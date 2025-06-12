{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
import qualified Data.Text.Encoding as Text
import qualified Hasql.Connection as HsqlCon
import qualified Hasql.Connection.Setting as HsqlConS
import Language.Haskell.TH.Syntax (Loc)
import System.Log.FastLogger (LogStr, fromLogStr)
import Prelude (error, userError)

import Cardano.Db.Error (runOrThrowIO)
import Cardano.Db.PGConfig
import Cardano.Db.Types (DbAction (..), DbEnv (..))

-----------------------------------------------------------------------------------------
-- Run DB actions
-----------------------------------------------------------------------------------------

runDbIohkLogging :: MonadUnliftIO m => Trace IO Text -> DbEnv -> DbAction (LoggingT m) a -> m a
runDbIohkLogging tracer dbEnv action = do
  runIohkLogging tracer $ do
    -- Let Hasql handle transactions automatically
    result <- runReaderT (runExceptT (runDbAction action)) dbEnv
    case result of
      Left err -> throwIO err
      Right val -> pure val

-- runDbIohkLogging :: MonadUnliftIO m => Trace IO Text -> DbEnv -> DbAction (LoggingT m) a -> m a
-- runDbIohkLogging tracer dbEnv@DbEnv {..} action = do
--   runIohkLogging tracer $ do
--     -- Start transaction
--     startTransaction dbConnection
--     -- Run action
--     result <- runReaderT (runExceptT (runDbAction action)) dbEnv
--     -- Commit or rollback
--     case result of
--       Left err -> do
--         rollbackAction dbConnection
--         throwIO err
--       Right val -> do
--         commitAction dbConnection
--         pure val

runDbIohkNoLogging :: MonadIO m => DbEnv -> DbAction (NoLoggingT m) a -> m a
runDbIohkNoLogging dbEnv action = do
  runNoLoggingT $ do
    result <- runReaderT (runExceptT (runDbAction action)) dbEnv
    case result of
      Left err -> throwIO err
      Right val -> pure val

-- runDbIohkNoLogging :: MonadIO m => DbEnv -> DbAction (NoLoggingT m) a -> m a
-- runDbIohkNoLogging dbEnv@DbEnv {..} action = do
--   runNoLoggingT $ do
--     -- Start transaction
--     startTransaction dbConnection
--     -- Run action
--     result <- runReaderT (runExceptT (runDbAction action)) dbEnv
--     -- Commit or rollback
--     case result of
--       Left err -> do
--         rollbackAction dbConnection
--         throwIO err
--       Right val -> do
--         commitAction dbConnection
--         pure val

-- | Run a DB action using a Pool via iohk-monitoring-framework.
runPoolDbIohkLogging ::
  (MonadUnliftIO m) =>
  Pool HsqlCon.Connection ->
  Trace IO Text ->
  DbAction (LoggingT m) a ->
  m a
runPoolDbIohkLogging connPool tracer action = do
  conn <- liftIO $ withResource connPool pure
  let dbEnv = mkDbEnv conn
  runDbIohkLogging tracer dbEnv action
  where
    mkDbEnv conn =
      DbEnv
        { dbConnection = conn
        , dbEnableLogging = True
        , dbTracer = Just tracer
        }

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
  actionResult <- runReaderT (runExceptT (runDbAction action)) dbEnv
  case actionResult of
    Left err -> do
      liftIO $ HsqlCon.release connection
      throwIO err
    Right val -> do
      liftIO $ HsqlCon.release connection
      pure val

-- runDbNoLogging :: MonadIO m => PGPassSource -> DbAction m a -> m a
-- runDbNoLogging source action = do
--   pgconfig <- liftIO $ runOrThrowIO (readPGPass source)
--   connSetting <- liftIO $ case toConnectionSetting pgconfig of
--     Left err -> error err
--     Right setting -> pure setting
--   connection <- liftIO $ acquireConnection [connSetting]
--   let dbEnv = DbEnv connection False Nothing
--   -- Start transaction
--   startTransaction connection
--   -- Run action with exception handling
--   actionResult <- runReaderT (runExceptT (runDbAction action)) dbEnv
--   -- Process results, handle transaction completion
--   case actionResult of
--     Left err -> do
--       -- On error, rollback and rethrow
--       rollbackAction connection
--       liftIO $ HsqlCon.release connection
--       throwIO err
--     Right val -> do
--       -- On success, commit and return value
--       commitAction connection
--       liftIO $ HsqlCon.release connection
--       pure val

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
        runNoLoggingT $ do
          result <- runReaderT (runExceptT (runDbAction action)) dbEnv
          case result of
            Left err -> throwIO err
            Right val -> pure val
    )

-- runWithConnectionNoLogging :: PGPassSource -> DbAction (NoLoggingT IO) a -> IO a
-- runWithConnectionNoLogging source action = do
--   pgConfig <- runOrThrowIO (readPGPass source)
--   connSetting <- case toConnectionSetting pgConfig of
--     Left err -> throwIO $ userError err
--     Right setting -> pure setting
--   bracket
--     (acquireConnection [connSetting])
--     HsqlCon.release
--     ( \connection -> do
--         let dbEnv = DbEnv connection False Nothing
--         runNoLoggingT $ do
--           -- Start transaction
--           startTransaction connection
--           -- Run action
--           result <- runReaderT (runExceptT (runDbAction action)) dbEnv
--           -- Commit or rollback
--           case result of
--             Left err -> do
--               rollbackAction connection
--               throwIO err
--             Right val -> do
--               commitAction connection
--               pure val
--     )

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
