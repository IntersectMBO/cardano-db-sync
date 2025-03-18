{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Db.Run (
  runDbHandleLogger,
  runDbIohkLogging,
  runDbIohkNoLogging,
  runDbNoLogging,
  runDbNoLoggingEnv,
  runIohkLogging,
  transactionCommit,
  runWithConnectionLogging,
  runWithConnectionNoLogging,

  -- * Connection Pool variants
  runPoolDbIohkLogging,
) where

import Cardano.BM.Data.LogItem (
  LOContent (..),
  LogObject (..),
  PrivacyAnnotation (..),
  mkLOMeta,
 )
import Cardano.BM.Data.Severity (Severity (..))
import Cardano.BM.Trace (Trace)
import Cardano.Db.Error (DbError, runOrThrowIO)
import Cardano.Db.PGConfig
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (
  LogLevel (..),
  LogSource,
  LoggingT,
  NoLoggingT,
  defaultLogStr,
  runLoggingT,
  runNoLoggingT,
 )
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (MonadUnliftIO)
import Control.Tracer (traceWith)
import qualified Data.ByteString.Char8 as BS
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Database.Persist.Postgresql (
  ConnectionString,
  SqlBackend,
  withPostgresqlConn,
 )
import Database.Persist.Sql (
  IsolationLevel (..),
  runSqlConnWithIsolation,
  runSqlPoolWithIsolation,
  transactionSaveWithIsolation,
 )
import Language.Haskell.TH.Syntax (Loc)
import System.IO (Handle)
import System.Log.FastLogger (LogStr, fromLogStr)
import qualified Cardano.Db.Types as DB
import Cardano.Prelude (runExceptT, ReaderT (..), lift, throwIO, bracket, Exception)
import qualified Hasql.Connection as HsqlC
import qualified Hasql.Connection.Setting as HsqlC


-- | Run a DB action logging via the provided Handle.
-- runDbHandleLogger :: Handle -> PGPassSource -> ReaderT SqlBackend (LoggingT IO) a -> IO a
-- runDbHandleLogger logHandle source dbAction = do
--   pgconfig <- runOrThrowIODb (readPGPass source)
--   runHandleLoggerT
--     . withPostgresqlConn (toConnectionSetting pgconfig)
--     $ \backend ->
--       -- The 'runSqlConnWithIsolation' function starts a transaction, runs the 'dbAction'
--       -- and then commits the transaction.
--       runSqlConnWithIsolation dbAction backend Serializable
--   where
--     runHandleLoggerT :: LoggingT m a -> m a
--     runHandleLoggerT action =
--       runLoggingT action logOut

--     logOut :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
--     logOut loc src level msg =
--       BS.hPutStrLn logHandle . fromLogStr $ defaultLogStr loc src level msg

runDbHandleLogger :: Handle -> PGPassSource -> DB.DbAction (LoggingT IO) a -> IO a
runDbHandleLogger logHandle source action = do
  pgconfig <- runOrThrowIO (readPGPass source)
  connSetting <- case toConnectionSetting pgconfig of
    Left err -> throwIO $ userError err
    Right setting -> pure setting

  bracket
    (acquireConnection [connSetting])
    HsqlC.release
    (\connection -> do
      let dbEnv = DB.DbEnv connection True Nothing  -- No tracer needed
      runHandleLoggerT $
        runReaderT (runExceptT (DB.runDbAction action)) dbEnv >>= \case
          Left err -> liftIO $ throwIO err
          Right result -> pure result
    )
  where
    runHandleLoggerT :: LoggingT m a -> m a
    runHandleLoggerT actn =
      runLoggingT actn logOut

    logOut :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
    logOut loc src level msg =
      BS.hPutStrLn logHandle . fromLogStr $ defaultLogStr loc src level msg

runWithConnectionLogging ::
  ConnectionString -> Trace IO Text -> ReaderT SqlBackend (LoggingT IO) a -> IO a
runWithConnectionLogging dbConnString tracer dbAction = do
  runIohkLogging tracer
    . withPostgresqlConn dbConnString
    $ \backend ->
      runSqlConnWithIsolation dbAction backend Serializable

runWithConnectionNoLogging ::
  PGPassSource -> DB.DbAction (NoLoggingT IO) a -> IO a
runWithConnectionNoLogging source action = do
  pgConfig <- runOrThrowIO (readPGPass source)
  connSetting <- case toConnectionSetting pgConfig of
    Left err -> throwIO $ userError err
    Right setting -> pure setting

  bracket
    (acquireConnection [connSetting])
    HsqlC.release
    (\connection -> do
      let dbEnv = DB.DbEnv connection False Nothing
      runNoLoggingT $
        runReaderT (runExceptT (DB.runDbAction action)) dbEnv >>= \case
          Left err -> liftIO $ throwIO err
          Right result -> pure result
    )

-- runWithConnectionNoLogging ::
--   PGPassSource -> ReaderT SqlBackend (NoLoggingT IO) a -> IO a
-- runWithConnectionNoLogging source dbAction = do
--   pgConfig <- runOrThrowIODb (readPGPass source)
--   runNoLoggingT
--     . withPostgresqlConn (toConnectionSetting pgConfig)
--     $ \backend ->
--       runSqlConnWithIsolation dbAction backend Serializable

-- | Run a DB action logging via iohk-monitoring-framework.
-- runDbIohkLogging :: forall m a. MonadUnliftIO m => Trace IO Text -> DB.DbEnv -> DB.DbAction m a -> m (Either DbError a)
-- runDbIohkLogging tracer dbEnv action = do
--   runIohkLogging tracer $ runReaderT (runExceptT (DB.runDbAction action)) dbEnv

runDbIohkLogging ::
  MonadUnliftIO m =>
  Trace IO Text ->
  DB.DbEnv ->
  DB.DbAction m a ->
  m (Either DbError a)
runDbIohkLogging tracer dbEnv action =
  runIohkLogging tracer $
    lift $ runReaderT (runExceptT (DB.runDbAction action)) dbEnv

-- | Run a DB action using a Pool via iohk-monitoring-framework.
runPoolDbIohkLogging :: MonadUnliftIO m => Pool SqlBackend -> Trace IO Text -> ReaderT SqlBackend (LoggingT m) b -> m b
runPoolDbIohkLogging backend tracer dbAction = do
  runIohkLogging tracer $ runSqlPoolWithIsolation dbAction backend Serializable

-- | Run a DB action logging via iohk-monitoring-framework.
runDbIohkNoLogging :: MonadUnliftIO m => SqlBackend -> ReaderT SqlBackend (NoLoggingT m) a -> m a
runDbIohkNoLogging backend action = do
  runNoLoggingT $ runSqlConnWithIsolation action backend Serializable

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

-- | Run a DB action without any logging, mainly for tests.
-- runDbNoLoggingEnv ::
--   (MonadBaseControl IO m, MonadUnliftIO m) =>
--   ReaderT SqlBackend (NoLoggingT m) a ->
--   m a
-- runDbNoLoggingEnv = runDbNoLogging PGPassDefaultEnv

-- runDbNoLogging ::
--   (MonadBaseControl IO m, MonadUnliftIO m) =>
--   PGPassSource ->
--   ReaderT SqlBackend (NoLoggingT m) a ->
--   m a
-- runDbNoLogging source action = do
--   pgconfig <- liftIO $ runOrThrowIODb (readPGPass source)
--   runNoLoggingT
--     . withPostgresqlConn (toConnectionSetting pgconfig)
--     $ \backend ->
--       runSqlConnWithIsolation action backend Serializable

runDbNoLoggingEnv ::
  MonadIO m =>
  DB.DbAction m a ->
  m a
runDbNoLoggingEnv = runDbNoLogging PGPassDefaultEnv

runDbNoLogging ::
  MonadIO m =>
  PGPassSource ->
  DB.DbAction m a ->
  m a
runDbNoLogging source action = do
  pgconfig <- liftIO $ runOrThrowIO (readPGPass source)
  connSetting <- liftIO $ case toConnectionSetting pgconfig of
    Left err -> error err  -- or use a more appropriate error handling
    Right setting -> pure setting

  connection <- liftIO $ acquireConnection [connSetting]
  let dbEnv = DB.DbEnv connection False Nothing

  result <- runReaderT (runExceptT (DB.runDbAction action)) dbEnv
  liftIO $ HsqlC.release connection

  case result of
    Left err -> error (show err)  -- or use a more appropriate error handling
    Right val -> pure val

transactionCommit :: MonadIO m => ReaderT SqlBackend m ()
transactionCommit = transactionSaveWithIsolation Serializable

-- data HsqlConnectionException = HsqlConnectionException (Maybe BS.ByteString)
--   deriving (Show)

newtype HsqlConnectionException =
  HsqlConnectionException (Maybe BS.ByteString)
  deriving (Show)

instance Exception HsqlConnectionException

acquireConnection :: MonadIO m => [HsqlC.Setting] -> m HsqlC.Connection
acquireConnection settings = liftIO $ do
  result <- HsqlC.acquire settings
  case result of
    Left err -> throwIO $ HsqlConnectionException err
    Right conn -> pure conn
