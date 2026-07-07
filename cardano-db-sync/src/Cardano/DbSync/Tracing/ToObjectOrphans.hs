{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.DbSync.Tracing.ToObjectOrphans () where

import Cardano.BM.Data.Severity (Severity (..))
import Cardano.BM.Data.Tracer
import Cardano.Client.Subscription (SubscriptionTrace (..))
import Data.Aeson ((.=))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.Mux as Mux
import Network.TypedProtocol.Codec (AnyMessage)
import Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import Ouroboros.Network.Block (Point (..), Tip (..))
import Ouroboros.Network.Driver (TraceSendRecv (..))
import Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)

-- The instances for 'TraceSendRecv' and 'Mux.WithBearer' used to come from
-- cardano-node's Cardano.Tracing.OrphanInstances.Network, which was removed
-- along with the rest of the legacy tracing system, so they are defined
-- locally here instead.

instance HasPrivacyAnnotation (TraceSendRecv a)
instance HasSeverityAnnotation (TraceSendRecv a) where
  getSeverityAnnotation _ = Debug

instance Show (AnyMessage ps) => ToObject (TraceSendRecv ps) where
  toObject _verb (TraceSendMsg msg) =
    mconcat
      [ "kind" .= ("Send" :: String)
      , "msg" .= show msg
      ]
  toObject _verb (TraceRecvMsg msg) =
    mconcat
      [ "kind" .= ("Recv" :: String)
      , "msg" .= show msg
      ]

instance HasTextFormatter (TraceSendRecv (ChainSync blk (Point blk) (Tip blk))) where
  formatText _ = Text.pack . show

instance
  Show (AnyMessage (ChainSync blk (Point blk) (Tip blk))) =>
  Transformable Text IO (TraceSendRecv (ChainSync blk (Point blk) (Tip blk)))
  where
  trTransformer = trStructuredText

instance HasPrivacyAnnotation (Mux.WithBearer peer a)
instance HasSeverityAnnotation (Mux.WithBearer peer a) where
  getSeverityAnnotation _ = Debug

instance (Show peer, Show a) => ToObject (Mux.WithBearer peer a) where
  toObject _verb (Mux.WithBearer peer ev) =
    mconcat
      [ "kind" .= ("WithBearer" :: String)
      , "bearer" .= show peer
      , "event" .= show ev
      ]

instance (Show peer, Show a) => HasTextFormatter (Mux.WithBearer peer a) where
  formatText (Mux.WithBearer peer ev) _o =
    "Bearer on " <> Text.pack (show peer) <> " event: " <> Text.pack (show ev)

instance (Show peer, Show a) => Transformable Text IO (Mux.WithBearer peer a) where
  trTransformer = trStructuredText

instance ToObject ByronBlock where
  toObject _verb msg =
    mconcat
      [ "kind" .= ("ByronBlock" :: String)
      , "event" .= show msg
      ]

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
