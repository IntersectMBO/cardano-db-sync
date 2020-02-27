{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Run
  ( getBackendGhci
  , ghciDebugQuery
  , runDbAction
  , runDbHandleLogger
  , runDbIohkLogging
  , runDbNoLogging
  , runDbStdoutLogging
  ) where

import           Cardano.BM.Data.LogItem (LogObject (..), LOContent (..), PrivacyAnnotation (..), mkLOMeta)
import           Cardano.BM.Data.Severity (Severity (..))
import           Cardano.BM.Trace (Trace)

import           Control.Monad.Logger (LogLevel (..), LogSource, LoggingT, NoLoggingT,
                    defaultLogStr, runLoggingT, runNoLoggingT, runStdoutLoggingT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Tracer (traceWith)
import           Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString.Char8 as BS
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Text.Lazy.Builder as LT

import           Database.Persist.Postgresql (withPostgresqlConn, openSimpleConn)
import           Database.PostgreSQL.Simple (connectPostgreSQL)
import           Database.Persist.Sql (SqlBackend, IsolationLevel (..), runSqlConnWithIsolation)

import           Database.Esqueleto
import           Database.Esqueleto.Internal.Sql

import           Cardano.Db.PGConfig

import           Language.Haskell.TH.Syntax (Loc)

import           System.IO (Handle, stdout)
import           System.Log.FastLogger (LogStr, fromLogStr)

-- | Run a DB action logging via the provided Handle.
runDbHandleLogger :: Handle -> ReaderT SqlBackend (LoggingT IO) a -> IO a
runDbHandleLogger logHandle dbAction = do
    pgconf <- readPGPassFileEnv
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

runDbAction :: Maybe (Trace IO Text) -> ReaderT SqlBackend (LoggingT IO) a -> IO a
runDbAction mLogging dbAction = do
    pgconf <- readPGPassFileEnv
    case mLogging of
      Nothing ->
        runSilentLoggingT .
          withPostgresqlConn (toConnectionString pgconf) $ \backend ->
            runSqlConnWithIsolation dbAction backend Serializable
      Just tracer ->
        runIohkLogging tracer .
          withPostgresqlConn (toConnectionString pgconf) $ \backend ->
            runSqlConnWithIsolation dbAction backend Serializable
  where
    runSilentLoggingT :: LoggingT m a -> m a
    runSilentLoggingT action = runLoggingT action silentLog

    silentLog :: Monad m => Loc -> LogSource -> LogLevel -> LogStr -> m ()
    silentLog _loc _src _level _msg = pure ()

-- | Run a DB action logging via iohk-monitoring-framework.
runDbIohkLogging :: Trace IO Text -> ReaderT SqlBackend (LoggingT IO) b -> IO b
runDbIohkLogging tracer dbAction = do
    pgconf <- readPGPassFileEnv
    runIohkLogging tracer .
      withPostgresqlConn (toConnectionString pgconf) $ \backend ->
        runSqlConnWithIsolation dbAction backend Serializable

runIohkLogging :: Trace IO Text -> LoggingT m a -> m a
runIohkLogging tracer action =
    runLoggingT action toIohkLog
  where
    toIohkLog :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
    toIohkLog _loc _src level msg = do
      meta <- mkLOMeta (toIohkSeverity level) Public
      traceWith tracer $
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
  pgconfig <- readPGPassFileEnv
  runNoLoggingT .
    withPostgresqlConn (toConnectionString pgconfig) $ \backend ->
      runSqlConnWithIsolation action backend Serializable

-- | Run a DB action with stdout logging. Mainly for debugging.
runDbStdoutLogging :: ReaderT SqlBackend (LoggingT IO) b -> IO b
runDbStdoutLogging action = do
  pgconfig <- readPGPassFileEnv
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
    BS.hPutStr h ls
  where
    ls = defaultLogStrBS loc src level msg

defaultLogStrBS :: Loc
                -> LogSource
                -> LogLevel
                -> LogStr
                -> BS.ByteString
defaultLogStrBS a b c d =
    toBS $ defaultLogStr a b c d
  where
    toBS = fromLogStr

getBackendGhci :: IO SqlBackend
getBackendGhci = do
  pgconfig <- readPGPassFileEnv
  connection <- connectPostgreSQL (toConnectionString pgconfig)
  openSimpleConn (\loc source level str -> defaultOutput stdout loc source level str) connection

ghciDebugQuery :: SqlSelect a r => SqlQuery a -> IO ()
ghciDebugQuery query = do
  pgconfig <- readPGPassFileEnv
  runStdoutLoggingT . withPostgresqlConn (toConnectionString pgconfig) $ \backend -> do
    let
      (sql,params) = toRawSql SELECT (backend, initialIdentState) query
    liftIO $ do
      LT.putStr $ LT.toLazyText sql
      print params
