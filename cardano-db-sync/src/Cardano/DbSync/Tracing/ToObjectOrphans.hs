{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Cardano.DbSync.Tracing.ToObjectOrphans () where

import           Data.Text (Text)
import           Data.Aeson ((.=))
import           Data.Functor.Identity (Identity (..))

import           Cardano.BM.Data.Tracer
import           Cardano.BM.Tracing

import           Cardano.Tracing.ToObjectOrphans (defaultTextTransformer)

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))
import           Ouroboros.Network.Block (Tip)
import           Ouroboros.Network.Codec (AnyMessage)
import           Ouroboros.Network.NodeToClient (LocalAddress, TraceSendRecv)
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import           Ouroboros.Network.Subscription (SubscriptionTrace (..))


instance HasPrivacyAnnotation (Identity (SubscriptionTrace LocalAddress))

instance HasSeverityAnnotation (Identity (SubscriptionTrace LocalAddress)) where
  getSeverityAnnotation (Identity ev) = case ev of
    SubscriptionTraceConnectStart {} -> Info
    SubscriptionTraceConnectEnd {} -> Info
    SubscriptionTraceConnectException {} -> Error
    SubscriptionTraceSocketAllocationException {} -> Error
    SubscriptionTraceTryConnectToPeer {} -> Info
    SubscriptionTraceSkippingPeer {} -> Info
    SubscriptionTraceSubscriptionRunning -> Debug
    SubscriptionTraceSubscriptionWaiting {} -> Debug
    SubscriptionTraceSubscriptionFailed -> Warning
    SubscriptionTraceSubscriptionWaitingNewConnection {} -> Debug
    SubscriptionTraceStart {} -> Debug
    SubscriptionTraceRestart {} -> Debug
    SubscriptionTraceConnectionExist {} -> Info
    SubscriptionTraceUnsupportedRemoteAddr {} -> Warning
    SubscriptionTraceMissingLocalAddress -> Warning
    SubscriptionTraceApplicationException {} -> Error
    SubscriptionTraceAllocateSocket {} -> Debug
    SubscriptionTraceCloseSocket {} -> Debug

instance Transformable Text IO (Identity (SubscriptionTrace LocalAddress)) where
  trTransformer = defaultTextTransformer

instance ToObject (Identity (SubscriptionTrace LocalAddress)) where
  toObject _verb (Identity ev) =
    mkObject [ "kind" .= ("SubscriptionTrace" :: String)
             , "event" .= show ev
             ]

instance Transformable Text IO (TraceSendRecv (ChainSync ByronBlock (Tip ByronBlock))) where
  trTransformer = defaultTextTransformer

instance HasPrivacyAnnotation (TraceSendRecv (ChainSync ByronBlock (Tip ByronBlock)))

instance HasSeverityAnnotation (TraceSendRecv (ChainSync ByronBlock (Tip ByronBlock)))

instance ToObject (AnyMessage (ChainSync ByronBlock (Tip ByronBlock))) where
  toObject _verb msg =
    mkObject [ "kind" .= ("TraceSendRecv" :: String)
             , "event" .= show msg
             ]
