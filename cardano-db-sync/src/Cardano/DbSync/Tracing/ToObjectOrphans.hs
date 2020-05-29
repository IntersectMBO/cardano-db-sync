{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Cardano.DbSync.Tracing.ToObjectOrphans () where

import           Data.Text (Text)
import           Data.Aeson ((.=))

import           Cardano.BM.Data.Tracer

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import           Ouroboros.Network.Block (Tip)
import           Ouroboros.Network.Codec (AnyMessage)
import           Ouroboros.Network.NodeToClient (TraceSendRecv)
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)

import           Cardano.TracingOrphanInstances.Network ()

instance HasTextFormatter (TraceSendRecv (ChainSync ByronBlock (Tip ByronBlock)))
instance Transformable Text IO (TraceSendRecv (ChainSync ByronBlock (Tip ByronBlock))) where
  trTransformer = trStructuredText

instance ToObject (AnyMessage (ChainSync ByronBlock (Tip ByronBlock))) where
  toObject _verb msg =
    mkObject [ "kind" .= ("TraceSendRecv" :: String)
             , "event" .= show msg
             ]
