{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Run where

import Cardano.BM.Data.LogItem (
  LOContent (..),
  LogObject (..),
  PrivacyAnnotation (..),
  mkLOMeta,
 )
import Cardano.BM.Data.Severity (Severity (..))
import Cardano.BM.Trace (Trace, logWarning)
import Cardano.Prelude
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Monad.Logger (
  LogLevel (..),
  LogSource,
  LoggingT,
  NoLoggingT,
  runLoggingT,
  runNoLoggingT,
 )
import Control.Monad.Trans.Resource (MonadUnliftIO)
import Control.Tracer (traceWith)
import Data.Pool (Pool, defaultPoolConfig, newPool, withResource)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Hasql.Connection as HsqlCon
import qualified Hasql.Connection.Setting as HsqlConS
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlS
import qualified Hasql.Statement as HsqlStmt
import Language.Haskell.TH.Syntax (Loc)
import System.Log.FastLogger (LogStr, fromLogStr)
import Prelude (error, userError)

import Cardano.Db.Error (DbCallStack (..), DbError (..), runOrThrowIO)
import Cardano.Db.PGConfig
import Cardano.Db.Statement.Function.Core (mkDbCallStack)
import Cardano.Db.Types (DbAction (..), DbEnv (..))

-----------------------------------------------------------------------------------------
-- Transaction Management
-----------------------------------------------------------------------------------------

data IsolationLevel
  = ReadUncommitted
  | ReadCommitted
  | RepeatableRead
  | Serializable
  deriving (Show, Eq)

-- | Convert isolation level to SQL string
isolationLevelToSql :: IsolationLevel -> Text
isolationLevelToSql ReadUncommitted = "READ UNCOMMITTED"
isolationLevelToSql ReadCommitted = "READ COMMITTED"
isolationLevelToSql RepeatableRead = "REPEATABLE READ"
isolationLevelToSql Serializable = "SERIALIZABLE"

-- | Begin transaction with isolation level
beginTransactionStmt :: IsolationLevel -> HsqlStmt.Statement () ()
beginTransactionStmt isolationLevel =
  HsqlStmt.Statement sql HsqlE.noParams HsqlD.noResult True
  where
    sql = "BEGIN ISOLATION LEVEL " <> encodeUtf8 (isolationLevelToSql isolationLevel)

-- | Commit transaction
commitTransactionStmt :: HsqlStmt.Statement () ()
commitTransactionStmt =
  HsqlStmt.Statement "COMMIT" HsqlE.noParams HsqlD.noResult True

-- | Rollback transaction
rollbackTransactionStmt :: HsqlStmt.Statement () ()
rollbackTransactionStmt =
  HsqlStmt.Statement "ROLLBACK" HsqlE.noParams HsqlD.noResult True

-- | Helper to convert SessionError to DbError
sessionErrorToDbError :: DbCallStack -> HsqlS.SessionError -> DbError
sessionErrorToDbError cs sessionErr =
  DbError cs ("Transaction error: " <> Text.pack (show sessionErr)) (Just sessionErr)

-----------------------------------------------------------------------------------------
-- Run DB actions with INTERRUPT HANDLING
-----------------------------------------------------------------------------------------

-- | Run a DbAction with explicit transaction and isolation level
-- This version properly handles interrupts (Ctrl+C) and ensures cleanup
runDbActionWithIsolation ::
  MonadUnliftIO m =>
  DbEnv ->
  IsolationLevel ->
  DbAction m a ->
  m (Either DbError a)
runDbActionWithIsolation dbEnv isolationLevel action = do
  withRunInIO $ \runInIO -> do
    mask $ \restore -> do
      -- Begin transaction
      beginResult <- beginTransaction dbEnv isolationLevel
      case beginResult of
        Left err -> pure (Left err)
        Right _ -> do
          -- Run the action with exception handling for interrupts
          result <-
            restore (runInIO $ runReaderT (runExceptT (runDbAction action)) dbEnv)
              `onException` do
                case dbTracer dbEnv of
                  Just tracer -> logWarning tracer "rolling back transaction, due to interrupt."
                  Nothing -> pure ()
                rollbackTransaction dbEnv
          case result of
            Left err -> do
              rollbackTransaction dbEnv
              pure (Left err)
            Right val -> do
              commitResult <- commitTransaction dbEnv
              case commitResult of
                Left commitErr -> do
                  rollbackTransaction dbEnv
                  pure (Left commitErr)
                Right _ -> pure (Right val)
  where
    beginTransaction :: DbEnv -> IsolationLevel -> IO (Either DbError ())
    beginTransaction env level = do
      let cs = mkDbCallStack "beginTransaction"
      result <- HsqlS.run (HsqlS.statement () (beginTransactionStmt level)) (dbConnection env)
      pure $ first (sessionErrorToDbError cs) result

    commitTransaction :: DbEnv -> IO (Either DbError ())
    commitTransaction env = do
      let cs = mkDbCallStack "commitTransaction"
      result <- HsqlS.run (HsqlS.statement () commitTransactionStmt) (dbConnection env)
      pure $ first (sessionErrorToDbError cs) result

    rollbackTransaction :: DbEnv -> IO ()
    rollbackTransaction env = do
      void $ HsqlS.run (HsqlS.statement () rollbackTransactionStmt) (dbConnection env)

runDbConnWithIsolation ::
  MonadUnliftIO m =>
  DbAction m a ->
  DbEnv ->
  IsolationLevel ->
  m a
runDbConnWithIsolation action dbEnv isolationLevel = do
  result <- runDbActionWithIsolation dbEnv isolationLevel action
  case result of
    Left err -> liftIO $ throwIO err
    Right val -> pure val

-- | Main functions with RepeatableRead isolation (matching original behavior)
runDbIohkLogging :: MonadUnliftIO m => Trace IO Text -> DbEnv -> DbAction (LoggingT m) a -> m a
runDbIohkLogging tracer dbEnv action =
  runIohkLogging tracer $
    runDbConnWithIsolation action dbEnv RepeatableRead

runDbIohkLoggingEither :: MonadUnliftIO m => Trace IO Text -> DbEnv -> DbAction (LoggingT m) a -> m (Either DbError a)
runDbIohkLoggingEither tracer dbEnv action = do
  runIohkLogging tracer $
    runDbActionWithIsolation dbEnv RepeatableRead action

runDbIohkNoLogging :: MonadUnliftIO m => DbEnv -> DbAction (NoLoggingT m) a -> m a
runDbIohkNoLogging dbEnv action =
  runNoLoggingT $
    runDbConnWithIsolation action dbEnv RepeatableRead

runPoolDbIohkLogging ::
  MonadUnliftIO m =>
  Pool HsqlCon.Connection ->
  Trace IO Text ->
  DbAction (LoggingT m) a ->
  m (Either DbError a)
runPoolDbIohkLogging connPool tracer action = do
  conn <- liftIO $ withResource connPool pure
  let dbEnv = mkDbEnv conn
  runIohkLogging tracer $
    runDbActionWithIsolation dbEnv RepeatableRead action
  where
    mkDbEnv conn =
      DbEnv
        { dbConnection = conn
        , dbEnableLogging = True
        , dbTracer = Just tracer
        }

runDbNoLogging :: MonadUnliftIO m => PGPassSource -> DbAction m a -> m a
runDbNoLogging source action = do
  pgconfig <- liftIO $ runOrThrowIO (readPGPass source)
  connSetting <- liftIO $ case toConnectionSetting pgconfig of
    Left err -> error err
    Right setting -> pure setting
  withRunInIO $ \runInIO ->
    bracket
      (acquireConnection [connSetting])
      HsqlCon.release
      ( \connection -> runInIO $ do
          let dbEnv = DbEnv connection False Nothing
          runDbConnWithIsolation action dbEnv RepeatableRead
      )

runDbNoLoggingEnv :: MonadUnliftIO m => DbAction m a -> m a
runDbNoLoggingEnv = runDbNoLogging PGPassDefaultEnv

runWithConnectionNoLogging :: PGPassSource -> DbAction (NoLoggingT IO) a -> IO a
runWithConnectionNoLogging source action = do
  pgConfig <- runOrThrowIO (readPGPass source)
  connSetting <- case toConnectionSetting pgConfig of
    Left err -> throwIO $ userError err
    Right setting -> pure setting
  bracket
    (acquireConnection [connSetting])
    HsqlCon.release
    ( \connection -> do
        let dbEnv = DbEnv connection False Nothing
        runNoLoggingT $ runDbConnWithIsolation action dbEnv RepeatableRead
    )

-- | Run a DB action with loggingT.
runIohkLogging :: Trace IO Text -> LoggingT m a -> m a
runIohkLogging tracer action =
  runLoggingT action toIohkLog
  where
    toIohkLog :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
    toIohkLog _loc _src level msg = do
      meta <- mkLOMeta (toIohkSeverity level) Public
      traceWith
        tracer
        (name, LogObject name meta (LogMessage . Text.decodeLatin1 $ fromLogStr msg))

    name :: Text
    name = "db-sync"

    toIohkSeverity :: LogLevel -> Severity
    toIohkSeverity =
      \case
        LevelDebug -> Debug
        LevelInfo -> Info
        LevelWarn -> Warning
        LevelError -> Error
        LevelOther _ -> Error

-- | Run a DbAction in IO, throwing an exception on error
runDbActionIO :: DbEnv -> DbAction IO a -> IO a
runDbActionIO dbEnv action = do
  result <- runReaderT (runExceptT (runDbAction action)) dbEnv
  case result of
    Left err -> throwIO err
    Right val -> pure val

acquireConnection :: MonadIO m => [HsqlConS.Setting] -> m HsqlCon.Connection
acquireConnection settings = liftIO $ do
  result <- HsqlCon.acquire settings
  case result of
    Left err -> throwIO $ userError $ "Connection error: " <> show err
    Right conn -> pure conn

-- Function to create a connection pool
createHasqlConnectionPool :: [HsqlConS.Setting] -> Int -> IO (Pool HsqlCon.Connection)
createHasqlConnectionPool settings numConnections = do
  newPool poolConfig
  where
    poolConfig =
      defaultPoolConfig
        acquireConn
        releaseConn
        30.0 -- cacheTTL (seconds)
        numConnections -- maxResources
    acquireConn = do
      result <- HsqlCon.acquire settings
      case result of
        Left err -> throwIO $ userError $ "Connection error: " <> show err
        Right conn -> pure conn
    releaseConn = HsqlCon.release
