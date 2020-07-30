{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Cardano.DbSync.Tracing.ToObjectOrphans () where

import           Data.Text (Text)
import           Data.Aeson ((.=))

import           Cardano.BM.Data.Tracer
import           Cardano.Tracing.OrphanInstances.Network ()

import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)

import           Ouroboros.Network.Block (Tip)
import           Ouroboros.Network.NodeToClient (TraceSendRecv)
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)


instance HasTextFormatter (TraceSendRecv (ChainSync tip (Tip blk)))

instance ToObject ByronBlock where
  toObject _verb msg =
    mkObject [ "kind" .= ("ByronBlock" :: String)
             , "event" .= show msg
             ]

instance Transformable Text IO (TraceSendRecv (ChainSync blk (Tip blk)))
