{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Db.Run (
  runDbHandleLogger,
  runDbIohkLogging,
  runDbIohkNoLogging,
  runDbNoLogging,
  runDbNoLoggingEnv,
  runIohkLogging,
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
import Cardano.Prelude (ReaderT (..), bracket, lift, runExceptT, throwIO)
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
import Control.Monad.Trans.Resource (MonadUnliftIO)
import Control.Tracer (traceWith)
import qualified Data.ByteString.Char8 as BS
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Hasql.Connection as HsqlCon
import qualified Hasql.Connection.Setting as HsqlConS
import Language.Haskell.TH.Syntax (Loc)
import System.IO (Handle)
import System.Log.FastLogger (LogStr, fromLogStr)

import Cardano.Db.Error (DbError, runOrThrowIO)
import qualified Cardano.Db.PGConfig as PGC 
import qualified Cardano.Db.Types as DB


-- | Run a DB action logging via the provided Handle.
runDbHandleLogger :: Handle -> PGC.PGPassSource -> DB.DbAction (LoggingT IO) a -> IO a
runDbHandleLogger logHandle source action = do
  pgconfig <- runOrThrowIO (PGC.readPGPass source)
  connSetting <- case PGC.toConnectionSetting pgconfig of
    Left err -> throwIO $ userError err
    Right setting -> pure setting

  bracket
    (acquireConnection [connSetting])
    HsqlCon.release
    ( \connection -> do
        let dbEnv = DB.DbEnv connection True Nothing -- No tracer needed
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

runWithConnectionNoLogging ::
  PGC.PGPassSource -> DB.DbAction (NoLoggingT IO) a -> IO a
runWithConnectionNoLogging source action = do
  pgConfig <- runOrThrowIO (PGC.readPGPass source)
  connSetting <- case PGC.toConnectionSetting pgConfig of
    Left err -> throwIO $ userError err
    Right setting -> pure setting

  bracket
    (acquireConnection [connSetting])
    HsqlCon.release
    ( \connection -> do
        let dbEnv = DB.DbEnv connection False Nothing
        runNoLoggingT $
          runReaderT (runExceptT (DB.runDbAction action)) dbEnv >>= \case
            Left err -> liftIO $ throwIO err
            Right result -> pure result
    )

-- | Run a DB action logging via iohk-monitoring-framework.
runDbIohkLogging ::
  MonadUnliftIO m =>
  Trace IO Text ->
  DB.DbEnv ->
  DB.DbAction m a ->
  m (Either DbError a)
runDbIohkLogging tracer dbEnv action =
  runIohkLogging tracer $
    lift $
      runReaderT (runExceptT (DB.runDbAction action)) dbEnv

-- | Run a DB action using a Pool with iohk-monitoring-framework logging.
-- This function now expects a Pool of Hasql.Connection instead of SqlBackend
runPoolDbIohkLogging :: 
  MonadIO m =>
  Pool HsqlCon.Connection ->
  Trace IO Text ->
  DB.DbAction (LoggingT m) a ->
  m a
runPoolDbIohkLogging connPool tracer action = do
  -- Use withResource from Data.Pool which works with MonadIO
  conn <- liftIO $ withResource connPool pure

  let dbEnv = DB.DbEnv conn True (Just tracer)
  result <- runIohkLogging tracer $
              runReaderT (runExceptT (DB.runDbAction action)) dbEnv
  case result of
    Left err -> liftIO $ throwIO err
    Right val -> pure val

-- | Run a DB action with no logging.
runDbIohkNoLogging ::
  MonadIO m =>
  HsqlCon.Connection ->
  DB.DbAction (NoLoggingT m) a ->
  m a
runDbIohkNoLogging conn action = do
  let dbEnv = DB.DbEnv conn False Nothing
  result <- runNoLoggingT $ runReaderT (runExceptT (DB.runDbAction action)) dbEnv
  case result of
    Left err -> liftIO $ throwIO err
    Right val -> pure val

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
  MonadIO m =>
  DB.DbAction m a ->
  m a
runDbNoLoggingEnv = runDbNoLogging PGC.PGPassDefaultEnv

runDbNoLogging ::
  MonadIO m =>
  PGC.PGPassSource ->
  DB.DbAction m a ->
  m a
runDbNoLogging source action = do
  pgconfig <- liftIO $ runOrThrowIO (PGC.readPGPass source)
  connSetting <- liftIO $ case PGC.toConnectionSetting pgconfig of
    Left err -> error err -- or use a more appropriate error handling
    Right setting -> pure setting

  connection <- liftIO $ acquireConnection [connSetting]
  let dbEnv = DB.DbEnv connection False Nothing

  result <- runReaderT (runExceptT (DB.runDbAction action)) dbEnv
  liftIO $ HsqlCon.release connection

  case result of
    Left err -> error (show err) -- or use a more appropriate error handling
    Right val -> pure val

acquireConnection :: MonadIO m => [HsqlConS.Setting] -> m HsqlCon.Connection
acquireConnection settings = liftIO $ do
  result <- HsqlCon.acquire settings
  case result of
    Left err -> throwIO $ userError $ "Connection error: " <> show err
    Right conn -> pure conn
