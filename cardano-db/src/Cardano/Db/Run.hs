{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Db.Run where

import Cardano.BM.Data.LogItem (
  LOContent (..),
  LogObject (..),
  PrivacyAnnotation (..),
  mkLOMeta,
 )
import Cardano.BM.Data.Severity (Severity (..))
import Cardano.BM.Trace (Trace)
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
import Data.Pool (Pool, withResource, newPool, defaultPoolConfig)
import qualified Data.Text.Encoding as Text
import qualified Hasql.Connection as HsqlCon
import qualified Hasql.Connection.Setting as HsqlConS
import qualified Hasql.Session as HsqlSes
import Language.Haskell.TH.Syntax (Loc)
import System.Log.FastLogger (LogStr, fromLogStr)
import Cardano.Prelude
import Prelude (userError, error)

import Cardano.Db.Types (DbAction (..), DbEnv (..))
import Cardano.Db.Error (runOrThrowIO)
import Cardano.Db.PGConfig
import Cardano.Db.Statement.Function.Core (runDbSession, mkCallInfo)

-----------------------------------------------------------------------------------------
-- Transactions
-----------------------------------------------------------------------------------------
-- | Execute a transaction start
startTransaction :: MonadIO m => HsqlCon.Connection -> m ()
startTransaction conn = liftIO $
  HsqlSes.run beginTransaction conn >>= \case
    Left err -> throwIO $ userError $ "Error starting transaction: " <> show err
    Right _ -> pure ()

-- | Commit a transaction
commitAction :: MonadIO m => HsqlCon.Connection -> m ()
commitAction conn = liftIO $
  HsqlSes.run commitTransaction conn >>= \case
    Left err -> throwIO $ userError $ "Error committing: " <> show err
    Right _ -> pure ()

-- | Rollback a transaction
rollbackAction :: MonadIO m => HsqlCon.Connection -> m ()
rollbackAction conn = liftIO $
  HsqlSes.run rollbackTransaction conn >>= \case
    Left err -> throwIO $ userError $ "Error rolling back: " <> show err
    Right _ -> pure ()

-----------------------------------------------------------------------------------------
-- Run DB actions
-----------------------------------------------------------------------------------------
-- | Run a DB action logging via iohk-monitoring-framework.
runDbIohkLogging :: MonadUnliftIO m => Trace IO Text -> DbEnv ->  DbAction (LoggingT m) a -> m a
runDbIohkLogging tracer dbEnv@DbEnv{..} action = do
  runIohkLogging tracer $ do
    -- Start transaction
    startTransaction dbConnection
    -- Run action
    result <- runReaderT (runExceptT (runDbAction action)) dbEnv
    -- Commit or rollback
    case result of
      Left err -> do
        rollbackAction dbConnection
        throwIO err
      Right val -> do
        commitAction dbConnection
        pure val

-- | Run a DB action using a Pool via iohk-monitoring-framework.
runPoolDbIohkLogging ::
  (MonadUnliftIO m) =>
  Pool HsqlCon.Connection ->
  Trace IO Text ->
  DbAction (LoggingT m) a -> m a
runPoolDbIohkLogging connPool tracer action = do
  conn <- liftIO $ withResource connPool pure
  let dbEnv = DbEnv conn True (Just tracer)
  runDbIohkLogging tracer dbEnv action

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

-- | Run a DB action with NoLoggingT.
runDbIohkNoLogging :: MonadIO m => DbEnv -> DbAction (NoLoggingT m) a -> m a
runDbIohkNoLogging dbEnv@DbEnv{..} action = do
  runNoLoggingT $ do
    -- Start transaction
    startTransaction dbConnection
    -- Run action
    result <- runReaderT (runExceptT (runDbAction action)) dbEnv
    -- Commit or rollback
    case result of
      Left err -> do
        rollbackAction dbConnection
        throwIO err
      Right val -> do
        commitAction dbConnection
        pure val

createTransactionCheckpoint :: MonadIO m => DbAction m ()
createTransactionCheckpoint =
  runDbSession (mkCallInfo "createTransactionCheckpoint") beginTransaction

-- | Run a DB action without any logging, mainly for tests.
runDbNoLoggingEnv :: MonadIO m => DbAction m a -> m a
runDbNoLoggingEnv = runDbNoLogging PGPassDefaultEnv

runDbNoLogging :: MonadIO m => PGPassSource -> DbAction m a -> m a
runDbNoLogging source action = do
  pgconfig <- liftIO $ runOrThrowIO (readPGPass source)
  connSetting <- liftIO $ case toConnectionSetting pgconfig of
    Left err -> error err
    Right setting -> pure setting
  connection <- liftIO $ acquireConnection [connSetting]
  let dbEnv = DbEnv connection False Nothing
  -- Start transaction
  startTransaction connection
  -- Run action with exception handling
  actionResult <- runReaderT (runExceptT (runDbAction action)) dbEnv
  -- Process results, handle transaction completion
  case actionResult of
    Left err -> do
      -- On error, rollback and rethrow
      rollbackAction connection
      liftIO $ HsqlCon.release connection
      throwIO err
    Right val -> do
      -- On success, commit and return value
      commitAction connection
      liftIO $ HsqlCon.release connection
      pure val

runWithConnectionNoLogging :: PGPassSource -> DbAction (NoLoggingT IO) a -> IO a
runWithConnectionNoLogging source action = do
  pgConfig <- runOrThrowIO (readPGPass source)
  connSetting <- case toConnectionSetting pgConfig of
    Left err -> throwIO $ userError err
    Right setting -> pure setting
  bracket
    (acquireConnection [connSetting])
    HsqlCon.release
    (\connection -> do
      let dbEnv = DbEnv connection False Nothing
      runNoLoggingT $ do
        -- Start transaction
        startTransaction connection
        -- Run action
        result <- runReaderT (runExceptT (runDbAction action)) dbEnv
        -- Commit or rollback
        case result of
          Left err -> do
            rollbackAction connection
            throwIO err
          Right val -> do
            commitAction connection
            pure val
    )

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
        30.0          -- cacheTTL (seconds)
        numConnections -- maxResources
    acquireConn = do
      result <- HsqlCon.acquire settings
      case result of
        Left err -> throwIO $ userError $ "Connection error: " <> show err
        Right conn -> pure conn
    releaseConn = HsqlCon.release

-----------------------------------------------------------------------------------------
-- Transaction Sql
-----------------------------------------------------------------------------------------
beginTransaction :: HsqlSes.Session ()
beginTransaction = HsqlSes.sql "BEGIN ISOLATION LEVEL SERIALIZABLE"

commitTransaction :: HsqlSes.Session ()
commitTransaction = HsqlSes.sql "COMMIT"

rollbackTransaction :: HsqlSes.Session ()
rollbackTransaction = HsqlSes.sql "ROLLBACK"

checkpointTransaction :: HsqlSes.Session ()
checkpointTransaction = HsqlSes.sql "COMMIT; BEGIN ISOLATION LEVEL SERIALIZABLE"
