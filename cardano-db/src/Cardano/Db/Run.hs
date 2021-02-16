{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Run
  ( getBackendGhci
  , ghciDebugQuery
  , runDbAction
  , runDbHandleLogger
  , runDbIohkLogging
  , runDbIohkNoLogging
  , runDbNoLogging
  , runDbStdoutLogging
  , runIohkLogging
  ) where

import           Cardano.BM.Data.LogItem (LOContent (..), LogObject (..), PrivacyAnnotation (..),
                   mkLOMeta)
import           Cardano.BM.Data.Severity (Severity (..))
import           Cardano.BM.Trace (Trace)

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (LogLevel (..), LogSource, LoggingT, NoLoggingT,
                   defaultLogStr, runLoggingT, runNoLoggingT, runStdoutLoggingT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Tracer (traceWith)

import qualified Data.ByteString.Char8 as BS
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.Text.Lazy.IO as LT

import           Database.Persist.Postgresql (openSimpleConn, withPostgresqlConn)
import           Database.Persist.Sql (IsolationLevel (..), runSqlConnWithIsolation)
import           Database.PostgreSQL.Simple (connectPostgreSQL)

import           Database.Esqueleto
import           Database.Esqueleto.Internal.Internal (Mode (SELECT), SqlSelect, initialIdentState,
                   toRawSql)

import           Cardano.Db.PGConfig

import           Language.Haskell.TH.Syntax (Loc)

import           System.IO (Handle, stdout)
import           System.Log.FastLogger (LogStr, fromLogStr)

-- | Run a DB action logging via the provided Handle.
runDbHandleLogger :: Handle -> ReaderT SqlBackend (LoggingT IO) a -> IO a
runDbHandleLogger logHandle dbAction = do
    pgconf <- readPGPassFileEnv Nothing
    runHandleLoggerT .
      withPostgresqlConn (toConnectionString pgconf) $ \backend ->
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

-- Be explicit and send the @SqlBackend@ inside.
runDbAction :: SqlBackend -> Maybe (Trace IO Text) -> ReaderT SqlBackend (LoggingT IO) a -> IO a
runDbAction backend mLogging dbAction =
    case mLogging of
      Nothing ->
        runSilentLoggingT $ runSqlConnWithIsolation dbAction backend Serializable
      Just tracer ->
        runIohkLogging tracer $ runSqlConnWithIsolation dbAction backend Serializable
  where
    runSilentLoggingT :: LoggingT m a -> m a
    runSilentLoggingT action = runLoggingT action silentLog

    silentLog :: Monad m => Loc -> LogSource -> LogLevel -> LogStr -> m ()
    silentLog _loc _src _level _msg = pure ()

-- | Run a DB action logging via iohk-monitoring-framework.
runDbIohkLogging :: SqlBackend -> Trace IO Text -> ReaderT SqlBackend (LoggingT IO) b -> IO b
runDbIohkLogging backend tracer dbAction = do
    runIohkLogging tracer $ runSqlConnWithIsolation dbAction backend Serializable

-- | Run a DB action logging via iohk-monitoring-framework.
runDbIohkNoLogging:: SqlBackend -> ReaderT SqlBackend (NoLoggingT IO) a -> IO a
runDbIohkNoLogging backend action = do
    runNoLoggingT $ runSqlConnWithIsolation action backend Serializable

runIohkLogging :: Trace IO Text -> LoggingT m a -> m a
runIohkLogging tracer action =
    runLoggingT action toIohkLog
  where
    toIohkLog :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
    toIohkLog _loc _src level msg = do
      meta <- mkLOMeta (toIohkSeverity level) Public
      traceWith tracer
        (name, LogObject name meta (LogMessage . T.decodeLatin1 $ fromLogStr msg))

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

-- | Run a DB action without any logging. Mainly for tests.
runDbNoLogging :: ReaderT SqlBackend (NoLoggingT IO) a -> IO a
runDbNoLogging action = do
  pgconfig <- readPGPassFileEnv Nothing
  runNoLoggingT .
    withPostgresqlConn (toConnectionString pgconfig) $ \backend ->
      runSqlConnWithIsolation action backend Serializable

-- | Run a DB action with stdout logging. Mainly for debugging.
runDbStdoutLogging :: ReaderT SqlBackend (LoggingT IO) b -> IO b
runDbStdoutLogging action = do
  pgconfig <- readPGPassFileEnv Nothing
  runStdoutLoggingT .
    withPostgresqlConn (toConnectionString pgconfig) $ \backend ->
      runSqlConnWithIsolation action backend Serializable

-- from Control.Monad.Logger, wasnt exported
defaultOutput :: Handle
              -> Loc
              -> LogSource
              -> LogLevel
              -> LogStr
              -> IO ()
defaultOutput h loc src level msg =
    BS.hPutStr h $ defaultLogStrBS loc src level msg

defaultLogStrBS :: Loc
                -> LogSource
                -> LogLevel
                -> LogStr
                -> BS.ByteString
defaultLogStrBS a b c d =
  fromLogStr $ defaultLogStr a b c d

getBackendGhci :: IO SqlBackend
getBackendGhci = do
  pgconfig <- readPGPassFileEnv Nothing
  connection <- connectPostgreSQL (toConnectionString pgconfig)
  openSimpleConn (defaultOutput stdout) connection

ghciDebugQuery :: SqlSelect a r => SqlQuery a -> IO ()
ghciDebugQuery query = do
  pgconfig <- readPGPassFileEnv Nothing
  runStdoutLoggingT . withPostgresqlConn (toConnectionString pgconfig) $ \backend -> do
    let (sql,params) = toRawSql SELECT (backend, initialIdentState) query
    liftIO $ do
      LT.putStr $ LT.toLazyText sql
      print params
