{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.DbSync.Tracing.ToObjectOrphans () where

import Cardano.BM.Data.Severity (Severity (..))
import Cardano.BM.Data.Tracer
import Cardano.Client.Subscription (SubscriptionTrace (..))
import Cardano.Tracing.OrphanInstances.Network ()
import Data.Aeson ((.=))
import Data.Text (Text)
import qualified Data.Text as Text
import Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import Ouroboros.Network.Block (Point (..), Tip (..))
import Ouroboros.Network.NodeToNode (TraceSendRecv (..))
import Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)

instance HasTextFormatter (TraceSendRecv (ChainSync blk (Point blk) (Tip blk))) where
  formatText _ = Text.pack . show

instance ToObject ByronBlock where
  toObject _verb msg =
    mconcat
      [ "kind" .= ("ByronBlock" :: String)
      , "event" .= show msg
      ]

instance Transformable Text IO (TraceSendRecv (ChainSync blk (Point blk) (Tip blk))) where
  trTransformer = trStructuredText

instance HasPrivacyAnnotation (SubscriptionTrace a)
instance HasSeverityAnnotation (SubscriptionTrace a) where
  getSeverityAnnotation (SubscriptionResult _) = Info
  getSeverityAnnotation (SubscriptionError _) = Error
  getSeverityAnnotation SubscriptionReconnect = Debug
  getSeverityAnnotation SubscriptionTerminate = Debug
instance Show a => ToObject (SubscriptionTrace a) where
  toObject _verb (SubscriptionResult result) =
    mconcat
      [ "kind" .= ("SubscriptionResult" :: String)
      , "result" .= show result
      ]
  toObject _verb (SubscriptionError err) =
    mconcat
      [ "kind" .= ("SubscriptionError" :: String)
      , "error" .= show err
      ]
  toObject _verb SubscriptionReconnect =
    mconcat
      [ "kind" .= ("SubscriptionReconnect" :: String)
      ]
  toObject _verb SubscriptionTerminate =
    mconcat
      [ "kind" .= ("SubscriptionTerminate" :: String)
      ]
instance HasTextFormatter (SubscriptionTrace a) where
  formatText _ = Text.pack . show
instance Show a => Transformable Text IO (SubscriptionTrace a) where
  trTransformer = trStructuredText
