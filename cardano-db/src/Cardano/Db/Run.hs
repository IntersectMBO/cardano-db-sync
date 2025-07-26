{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Db.Run where

import Cardano.BM.Data.LogItem (
  LOContent (..),
  LogObject (..),
  PrivacyAnnotation (..),
  mkLOMeta,
 )
import Cardano.BM.Data.Severity (Severity (..))
import Cardano.BM.Trace (Trace)
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
import Cardano.Db.Statement (runDbSession)
import Cardano.Db.Statement.Function.Core (mkDbCallStack)
import Cardano.Db.Types (DbAction (..), DbEnv (..))
import qualified Hasql.Session as HsqlSess

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

commitCurrentTransaction :: MonadIO m => DbAction m ()
commitCurrentTransaction = do
  runDbSession (mkDbCallStack "commitCurrentTransaction") $
    HsqlSess.statement () commitTransactionStmt

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

-- | Run a DbAction with explicit transaction control and isolation level
--
-- Transaction behavior:
-- * Begins transaction with specified isolation level
-- * Runs the action within the transaction
-- * Commits if action succeeds, rollback only on commit failure or async exceptions
-- * Returns Either for explicit error handling instead of throwing exceptions
--
-- Exception safety:
-- * Uses 'mask' to prevent async exceptions during transaction lifecycle
-- * Uses 'onException' to ensure rollback on interrupts (Ctrl+C, SIGTERM, etc.)
-- * Does NOT rollback on action errors - lets them commit (matches Persistent semantics)
--
-- Note: This follows Persistent's philosophy where successful function calls commit
-- their transactions regardless of the return value. Only async exceptions and
-- commit failures trigger rollbacks.
runDbActionWithIsolation ::
  MonadUnliftIO m =>
  DbEnv ->
  IsolationLevel ->
  DbAction m a ->
  m (Either DbError a)
runDbActionWithIsolation dbEnv isolationLevel action = do
  withRunInIO $ \runInIO -> do
    -- Use masking to prevent async exceptions during transaction management
    mask $ \restore -> do
      -- Begin transaction with specified isolation level
      beginResult <- beginTransaction dbEnv isolationLevel
      case beginResult of
        Left err -> pure (Left err)
        Right _ -> do
          -- Run action with async exception protection via onException
          -- If interrupted (Ctrl+C), the onException handler will rollback
          result <-
            onException
              (restore (runInIO $ runReaderT (runExceptT (runDbAction action)) dbEnv))
              (restore $ rollbackTransaction dbEnv)
          case result of
            -- Action returned error but ran successfully - commit the transaction
            -- This matches Persistent's behavior: successful calls always commit
            Left err -> pure (Left err)
            Right val -> do
              -- Attempt to commit the transaction
              commitResult <- commitTransaction dbEnv
              case commitResult of
                Left commitErr -> do
                  -- Commit failed - rollback and return the commit error
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
  let dbEnv = createDbEnv conn connPool (Just tracer)
  runIohkLogging tracer $
    runDbActionWithIsolation dbEnv RepeatableRead action

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
      ( \connection -> do
          pool <- createHasqlConnectionPool [connSetting] 4 -- 4 connections for reasonable parallelism
          runInIO $ do
            let dbEnv = createDbEnv connection pool Nothing
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
        pool <- createHasqlConnectionPool [connSetting] 4 -- 4 connections for reasonable parallelism
        let dbEnv = createDbEnv connection pool Nothing
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

-- Helper to create DbEnv with both single connection and pool
createDbEnv :: HsqlCon.Connection -> Pool HsqlCon.Connection -> Maybe (Trace IO Text) -> DbEnv
createDbEnv conn pool tracer =
  DbEnv
    { dbConnection = conn
    , dbPoolConnection = pool
    , dbTracer = tracer
    }

-- Pool-aware database action runners for async operations
runPoolDbAction :: forall a m. MonadUnliftIO m => DbEnv -> DbAction m a -> m a
runPoolDbAction dbEnv action = do
  withRunInIO $ \runInIO -> do
    conn <- withResource (dbPoolConnection dbEnv) pure
    let poolDbEnv = dbEnv {dbConnection = conn, dbTracer = Nothing} -- No logging for pool operations to avoid contention
    result <- runInIO $ runReaderT (runExceptT (runDbAction action)) poolDbEnv
    case result of
      Left err -> throwIO err
      Right val -> pure val

runPoolDbActionWithLogging :: forall a m. MonadUnliftIO m => DbEnv -> DbAction m a -> m a
runPoolDbActionWithLogging dbEnv action = do
  withRunInIO $ \runInIO -> do
    conn <- withResource (dbPoolConnection dbEnv) pure
    let poolDbEnv = dbEnv {dbConnection = conn} -- Keep original logging settings
    result <- runInIO $ runReaderT (runExceptT (runDbAction action)) poolDbEnv
    case result of
      Left err -> throwIO err
      Right val -> pure val
