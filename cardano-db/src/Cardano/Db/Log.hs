{-# LANGUAGE OverloadedStrings #-}

-- | Plain text log messages for db-sync and friends.
--
-- A 'LogMessage' is just a severity and the message text, routed through
-- the @trace-dispatcher@ framework. The severity is chosen at the call
-- site with 'logDebug', 'logInfo', 'logNotice', 'logWarning' or 'logError',
-- mirroring the old @iohk-monitoring@ API:
--
-- > liftIO $ logInfo trce "UTxO bootstrap migration done"
--
-- The severity carried inside the message is picked up by the dispatcher
-- (via 'severityFor') and is honoured by the configured severity filters.
module Cardano.Db.Log (
  LogMessage (..),
  logDebug,
  logInfo,
  logNotice,
  logWarning,
  logError,
) where

import Cardano.Logging.Trace (traceWith)
import Cardano.Logging.Types (
  LogFormatting (..),
  MetaTrace (..),
  Namespace (..),
  SeverityS (..),
  Trace,
 )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value (..), (.=))
import Data.Text (Text)

-- | A log message: the severity chosen at the call site and the text.
data LogMessage = LogMessage !SeverityS !Text
  deriving (Eq, Show)

instance LogFormatting LogMessage where
  forHuman (LogMessage _sev msg) = msg
  forMachine _dtal (LogMessage _sev msg) = mconcat ["message" .= String msg]

instance MetaTrace LogMessage where
  namespaceFor _ = Namespace [] ["Message"]
  severityFor _ (Just (LogMessage sev _msg)) = Just sev
  severityFor _ Nothing = Just Info
  documentFor _ = Just "A plain text log message"
  allNamespaces = [Namespace [] ["Message"]]

logDebug, logInfo, logNotice, logWarning, logError :: MonadIO m => Trace IO LogMessage -> Text -> m ()
logDebug tr = logWith tr Debug
logInfo tr = logWith tr Info
logNotice tr = logWith tr Notice
logWarning tr = logWith tr Warning
logError tr = logWith tr Error

logWith :: MonadIO m => Trace IO LogMessage -> SeverityS -> Text -> m ()
logWith tr sev msg = liftIO $ traceWith tr (LogMessage sev msg)
