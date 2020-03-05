{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Cardano.DbSync.Tracing.ToObjectOrphans () where

import qualified Codec.CBOR.Term as CBOR

import           Data.Text (Text, pack)
import           Data.Aeson ((.=))
import           Data.Functor.Identity (Identity (..))

import           Cardano.BM.Data.LogItem
import           Cardano.BM.Data.Tracer
import           Cardano.BM.Tracing

import           Cardano.Tracing.ToObjectOrphans ()

import           Control.Monad.IO.Class (MonadIO)


import           Network.Mux (WithMuxBearer (..))

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))
import           Ouroboros.Network.Block (Tip)
import           Ouroboros.Network.NodeToClient (ConnectionId, Handshake, LocalAddress,
                    NodeToClientVersion, TraceSendRecv)
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import           Ouroboros.Network.Subscription (SubscriptionTrace (..))


defaultTextTransformer
  :: ( MonadIO m
     , DefinePrivacyAnnotation b
     , DefineSeverity b
     , Show b
     , ToObject b)
  => TracingFormatting
  -> TracingVerbosity
  -> Trace m Text
  -> Tracer m b
defaultTextTransformer TextualRepresentation _verb tr =
  Tracer $ \s -> do
    meta <- mkLOMeta (defineSeverity s) (definePrivacyAnnotation s)
    traceWith tr (mempty, LogObject mempty meta (LogMessage $ pack $ show s))
defaultTextTransformer _ verb tr =
  trStructured verb tr

instance DefinePrivacyAnnotation (Identity (SubscriptionTrace LocalAddress))

instance DefineSeverity (Identity (SubscriptionTrace LocalAddress)) where
  defineSeverity (Identity ev) = case ev of
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

instance DefinePrivacyAnnotation (WithMuxBearer
                             (ConnectionId LocalAddress)
                             (TraceSendRecv (Handshake NodeToClientVersion CBOR.Term)))

instance DefineSeverity (WithMuxBearer
                             (ConnectionId LocalAddress)
                             (TraceSendRecv (Handshake NodeToClientVersion CBOR.Term)))

instance Transformable Text IO (WithMuxBearer
                             (ConnectionId LocalAddress)
                             (TraceSendRecv (Handshake NodeToClientVersion CBOR.Term))) where
  trTransformer = defaultTextTransformer


instance Transformable Text IO (TraceSendRecv (ChainSync ByronBlock (Tip ByronBlock))) where
  trTransformer = defaultTextTransformer


instance DefinePrivacyAnnotation (TraceSendRecv (ChainSync ByronBlock (Tip ByronBlock)))

instance DefineSeverity (TraceSendRecv (ChainSync ByronBlock (Tip ByronBlock)))

instance ToObject (WithMuxBearer
                             (ConnectionId LocalAddress)
                             (TraceSendRecv (Handshake NodeToClientVersion CBOR.Term))) where
  toObject _verb (WithMuxBearer b ev) =
    mkObject [ "kind" .= ("MuxTrace" :: String)
             , "bearer" .= show b
             , "event" .= show ev
             ]

instance ToObject (TraceSendRecv (ChainSync ByronBlock (Tip ByronBlock))) where
  toObject _verb msg =
    mkObject [ "kind" .= ("TraceSendRecv" :: String)
             , "event" .= show msg
             ]
