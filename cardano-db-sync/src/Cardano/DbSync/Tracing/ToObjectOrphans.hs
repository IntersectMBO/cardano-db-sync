{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Cardano.DbSync.Tracing.ToObjectOrphans () where

import           Data.Text (Text)
import           Data.Aeson ((.=))

import           Cardano.BM.Data.Tracer
import           Cardano.TracingOrphanInstances.Network ()

import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)

import           Ouroboros.Network.Block (Tip, StandardHash)
import           Ouroboros.Network.Codec (AnyMessage)
import           Ouroboros.Network.NodeToClient (TraceSendRecv)
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)


instance HasTextFormatter (TraceSendRecv (ChainSync tip (Tip blk)))

instance (StandardHash blk, Show blk) => ToObject (AnyMessage (ChainSync blk (Tip blk))) where
  toObject _verb msg =
    mkObject [ "kind" .= ("TraceSendRecv" :: String)
             , "event" .= show msg
             ]

instance ToObject ByronBlock where
  toObject _verb msg =
    mkObject [ "kind" .= ("ByronBlock" :: String)
             , "event" .= show msg
             ]

instance (StandardHash blk, Show blk) => Transformable Text IO (TraceSendRecv (ChainSync blk (Tip blk))) where
  trTransformer = trStructuredText
