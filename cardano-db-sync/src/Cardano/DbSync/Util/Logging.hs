{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Util.Logging (
  LogContext (..),
  logInfoCtx,
  logWarningCtx,
  logErrorCtx,
  logDebugCtx,
  initLogCtx,
  liftedLogExceptionCtx,
  logActionDurationCtx,
  logExceptionCtx,
) where

import Cardano.BM.Trace (Trace, logDebug, logError, logInfo, logWarning)
import Cardano.Prelude hiding (catch)
import Control.Exception.Lifted (catch)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text (pack)
import qualified Data.Time.Clock as Time
import Prelude hiding (show, unwords, (.))

data LogContext = LogContext
  { lcFunction :: Text
  , lcComponent :: Text
  , lcBlockNo :: Maybe Word64
  , lcSlotNo :: Maybe Word64
  , lcEpochNo :: Maybe Word64
  , lcMessage :: Text
  }

-- TODO: We could select what to show here with a debug flag!
formatLogMessage :: LogContext -> Text
formatLogMessage ctx =
  unwords
    [ lcMessage ctx
    , "[Function:"
    , lcFunction ctx
    , "| Component:"
    , lcComponent ctx
    , "| Block No:"
    , maybe "None" (pack . show) (lcBlockNo ctx)
    , "| Slot No:"
    , maybe "None" (pack . show) (lcSlotNo ctx)
    , "| Epoch No:"
    , maybe "None" (pack . show) (lcEpochNo ctx)
    , "]"
    ]

-- Wrapper functions using LogContext
logInfoCtx :: Trace IO Text -> LogContext -> IO ()
logInfoCtx trce ctx = logInfo trce (formatLogMessage ctx)

logWarningCtx :: Trace IO Text -> LogContext -> IO ()
logWarningCtx trce ctx = logWarning trce (formatLogMessage ctx)

logErrorCtx :: Trace IO Text -> LogContext -> IO ()
logErrorCtx trce ctx = logError trce (formatLogMessage ctx)

logDebugCtx :: Trace IO Text -> LogContext -> IO ()
logDebugCtx trce ctx = logDebug trce (formatLogMessage ctx)

initLogCtx :: Text -> Text -> LogContext
initLogCtx functionName componentName =
  LogContext
    { lcFunction = functionName
    , lcComponent = componentName
    , lcBlockNo = Nothing
    , lcSlotNo = Nothing
    , lcEpochNo = Nothing
    , lcMessage = ""
    }

-- | Needed when debugging disappearing exceptions.
liftedLogExceptionCtx :: (MonadBaseControl IO m, MonadIO m) => Trace IO Text -> Text -> m a -> m a
liftedLogExceptionCtx tracer txt action =
  action `catch` logger
  where
    logCtx = LogContext txt "Cardano.DbSync.Util" Nothing Nothing Nothing

    logger :: MonadIO m => SomeException -> m a
    logger e =
      liftIO $ do
        logErrorCtx tracer $ logCtx ("Caught exception: txt " <> show e)
        throwIO e

-- | Log the runtime duration of an action. Mainly for debugging.
logActionDurationCtx :: (MonadBaseControl IO m, MonadIO m) => Trace IO Text -> LogContext -> m a -> m a
logActionDurationCtx tracer logCtx action = do
  before <- liftIO Time.getCurrentTime
  a <- action
  after <- liftIO Time.getCurrentTime
  liftIO . logInfoCtx tracer $ logCtx {lcMessage = mconcat ["duration: ", textShow (Time.diffUTCTime after before)]}
  pure a

-- | ouroboros-network catches 'SomeException' and if a 'nullTracer' is passed into that
-- code, the caught exception will not be logged. Therefore wrap all cardano-db-sync code that
-- is called from network with an exception logger so at least the exception will be
-- logged (instead of silently swallowed) and then rethrown.
logExceptionCtx :: Trace IO Text -> LogContext -> IO a -> IO a
logExceptionCtx tracer logCtx action =
  action `catch` logger
  where
    logger :: SomeException -> IO a
    logger e = do
      logErrorCtx tracer $ logCtx {lcMessage = lcMessage logCtx <> textShow e}
      throwIO e
