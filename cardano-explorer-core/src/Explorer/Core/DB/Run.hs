{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Explorer.Core.DB.Run
  ( runDbHandleLogger
  , runDbIohkLogging
  , runDbNoLogging
  , runDbStdoutLogging
  ) where

import           Cardano.BM.Data.LogItem (LogObject (..), LOContent (..), PrivacyAnnotation (..), mkLOMeta)
import           Cardano.BM.Data.Severity (Severity (..))
import           Cardano.BM.Trace (Trace)

import           Control.Monad.Logger (LogLevel (..), LogSource, LoggingT, NoLoggingT,
                    defaultLogStr, runLoggingT, runNoLoggingT, runStdoutLoggingT)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Control.Tracer (traceWith)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Text (Text)

import           Database.Persist.Postgresql (withPostgresqlConn)
import           Database.Persist.Sql (SqlBackend)

import           Explorer.Core.DB.PGConfig

import           Language.Haskell.TH.Syntax (Loc)

import           System.IO (Handle)
import           System.Log.FastLogger (LogStr, fromLogStr)


-- | Run a DB action logging via the provided Handle.
runDbHandleLogger :: Handle -> ReaderT SqlBackend (LoggingT IO) a -> IO a
runDbHandleLogger logHandle dbAction = do
    pgconf <- readPGPassFileEnv
    runHandleLoggerT .
      withPostgresqlConn (toConnectionString pgconf) $ \backend ->
        runReaderT dbAction backend
  where
    runHandleLoggerT :: LoggingT m a -> m a
    runHandleLoggerT action =
      runLoggingT action logOut

    logOut :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
    logOut loc src level msg =
      BS.hPutStrLn logHandle . fromLogStr $ defaultLogStr loc src level msg


-- | Run a DB action logging via iohk-monitoring-framework.
runDbIohkLogging :: Trace IO Text -> ReaderT SqlBackend (LoggingT IO) b -> IO b
runDbIohkLogging tracer dbAction = do
    pgconf <- readPGPassFileEnv
    runIohkLogging .
      withPostgresqlConn (toConnectionString pgconf) $ \backend ->
        runReaderT dbAction backend
  where
    runIohkLogging :: LoggingT m a -> m a
    runIohkLogging action =
      runLoggingT action toIohkLog

    toIohkLog :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
    toIohkLog _loc _src level msg = do
      meta <- mkLOMeta (toIohkSeverity level) Public
      traceWith tracer $ LogObject "explorer-db" meta (LogStructured (LBS.fromStrict $ fromLogStr msg))


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
  pgconfig <- readPGPassFileEnv
  runNoLoggingT .
    withPostgresqlConn (toConnectionString pgconfig) $ \backend ->
      runReaderT action backend

-- | Run a DB action with stdout logging. Mainly for debugging.
runDbStdoutLogging :: ReaderT SqlBackend (LoggingT IO) b -> IO b
runDbStdoutLogging action = do
  pgconfig <- readPGPassFileEnv
  runStdoutLoggingT .
    withPostgresqlConn (toConnectionString pgconfig) $ \backend ->
      runReaderT action backend
