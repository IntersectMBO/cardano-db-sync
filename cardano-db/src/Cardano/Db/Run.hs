{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Run (
  getBackendGhci,
  ghciDebugQuery,
  runDbHandleLogger,
  runDbIohkLogging,
  runDbIohkNoLogging,
  runDbNoLogging,
  runDbNoLoggingEnv,
  runDbStdoutLogging,
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
import Cardano.Db.Error (runOrThrowIODb)
import Cardano.Db.PGConfig
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (
  LogLevel (..),
  LogSource,
  LoggingT,
  NoLoggingT,
  defaultLogStr,
  defaultOutput,
  runLoggingT,
  runNoLoggingT,
  runStdoutLoggingT,
 )
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (MonadUnliftIO)
import Control.Tracer (traceWith)
import qualified Data.ByteString.Char8 as BS
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy.Builder as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import Database.Esqueleto.Experimental (SqlQuery)
import Database.Esqueleto.Internal.Internal (
  Mode (SELECT),
  SqlSelect,
  initialIdentState,
  toRawSql,
 )
import Database.Persist.Postgresql (
  ConnectionString,
  SqlBackend,
  openSimpleConn,
  withPostgresqlConn,
 )
import Database.Persist.Sql (
  IsolationLevel (..),
  runSqlConnWithIsolation,
  runSqlPoolWithIsolation,
  transactionSaveWithIsolation,
 )
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Language.Haskell.TH.Syntax (Loc)
import System.IO (Handle, stdout)
import System.Log.FastLogger (LogStr, fromLogStr)

-- | Run a DB action logging via the provided Handle.
runDbHandleLogger :: Handle -> PGPassSource -> ReaderT SqlBackend (LoggingT IO) a -> IO a
runDbHandleLogger logHandle source dbAction = do
  pgconfig <- runOrThrowIODb (readPGPass source)
  runHandleLoggerT
    . withPostgresqlConn (toConnectionString pgconfig)
    $ \backend ->
      -- The 'runSqlConnWithIsolation' function starts a transaction, runs the 'dbAction'
      -- and then commits the transaction.
      runSqlConnWithIsolation dbAction backend Serializable
  where
    runHandleLoggerT :: LoggingT m a -> m a
    runHandleLoggerT action =
      runLoggingT action logOut

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
  PGPassSource -> ReaderT SqlBackend (NoLoggingT IO) a -> IO a
runWithConnectionNoLogging source dbAction = do
  pgconfig <- runOrThrowIODb (readPGPass source)
  runNoLoggingT
    . withPostgresqlConn (toConnectionString pgconfig)
    $ \backend ->
      runSqlConnWithIsolation dbAction backend Serializable

-- | Run a DB action logging via iohk-monitoring-framework.
runDbIohkLogging :: MonadUnliftIO m => SqlBackend -> Trace IO Text -> ReaderT SqlBackend (LoggingT m) b -> m b
runDbIohkLogging backend tracer dbAction = do
  runIohkLogging tracer $ runSqlConnWithIsolation dbAction backend Serializable

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
runDbNoLoggingEnv ::
  (MonadBaseControl IO m, MonadUnliftIO m) =>
  ReaderT SqlBackend (NoLoggingT m) a ->
  m a
runDbNoLoggingEnv = runDbNoLogging PGPassDefaultEnv

runDbNoLogging ::
  (MonadBaseControl IO m, MonadUnliftIO m) =>
  PGPassSource ->
  ReaderT SqlBackend (NoLoggingT m) a ->
  m a
runDbNoLogging source action = do
  pgconfig <- liftIO $ runOrThrowIODb (readPGPass source)
  runNoLoggingT
    . withPostgresqlConn (toConnectionString pgconfig)
    $ \backend ->
      runSqlConnWithIsolation action backend Serializable

-- | Run a DB action with stdout logging. Mainly for debugging.
runDbStdoutLogging :: PGPassSource -> ReaderT SqlBackend (LoggingT IO) b -> IO b
runDbStdoutLogging source action = do
  pgconfig <- runOrThrowIODb (readPGPass source)
  runStdoutLoggingT
    . withPostgresqlConn (toConnectionString pgconfig)
    $ \backend ->
      runSqlConnWithIsolation action backend Serializable

getBackendGhci :: IO SqlBackend
getBackendGhci = do
  pgconfig <- runOrThrowIODb (readPGPass PGPassDefaultEnv)
  connection <- connectPostgreSQL (toConnectionString pgconfig)
  openSimpleConn (defaultOutput stdout) connection

ghciDebugQuery :: SqlSelect a r => SqlQuery a -> IO ()
ghciDebugQuery query = do
  pgconfig <- runOrThrowIODb (readPGPass PGPassDefaultEnv)
  runStdoutLoggingT
    . withPostgresqlConn (toConnectionString pgconfig)
    $ \backend -> do
      let (sql, params) = toRawSql SELECT (backend, initialIdentState) query
      liftIO $ do
        LazyText.putStr $ LazyText.toLazyText sql
        print params

transactionCommit :: MonadIO m => ReaderT SqlBackend m ()
transactionCommit = transactionSaveWithIsolation Serializable
