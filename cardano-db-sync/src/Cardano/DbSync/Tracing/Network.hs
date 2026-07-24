{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphan @trace-dispatcher@ instances for network types which do not
-- provide them upstream (currently only 'SubscriptionTrace').
module Cardano.DbSync.Tracing.Network () where

import Cardano.Client.Subscription (SubscriptionTrace (..))
import Cardano.Logging.Types (
  LogFormatting (..),
  MetaTrace (..),
  Namespace (..),
  SeverityS (..),
 )
import Data.Aeson (Value (..), (.=))
import Data.Text (Text)
import qualified Data.Text as Text

instance Show a => LogFormatting (SubscriptionTrace a) where
  forMachine _dtal = \case
    SubscriptionResult a ->
      mconcat ["kind" .= String "SubscriptionResult", "result" .= String (textShow a)]
    SubscriptionError e ->
      mconcat ["kind" .= String "SubscriptionError", "error" .= String (textShow e)]
    SubscriptionReconnect ->
      mconcat ["kind" .= String "SubscriptionReconnect"]
    SubscriptionTerminate ->
      mconcat ["kind" .= String "SubscriptionTerminate"]

  forHuman = textShow

instance MetaTrace (SubscriptionTrace a) where
  namespaceFor =
    Namespace [] . \case
      SubscriptionResult {} -> ["Result"]
      SubscriptionError {} -> ["Error"]
      SubscriptionReconnect -> ["Reconnect"]
      SubscriptionTerminate -> ["Terminate"]

  severityFor (Namespace _ ns) _ =
    case ns of
      ["Result"] -> Just Info
      ["Error"] -> Just Error
      ["Reconnect"] -> Just Debug
      ["Terminate"] -> Just Debug
      _ -> Nothing

  documentFor _ = Just "Subscription (node connection) events"

  allNamespaces =
    map
      (Namespace [])
      [ ["Result"]
      , ["Error"]
      , ["Reconnect"]
      , ["Terminate"]
      ]

textShow :: Show a => a -> Text
textShow = Text.pack . show
