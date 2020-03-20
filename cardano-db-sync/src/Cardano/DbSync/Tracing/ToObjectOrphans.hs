{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Cardano.DbSync.Tracing.ToObjectOrphans () where

import           Data.Text (Text)
import           Data.Aeson ((.=))

import           Cardano.BM.Data.Tracer

import           Cardano.Tracing.ToObjectOrphans (defaultTextTransformer)

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import           Ouroboros.Network.Block (Tip)
import           Ouroboros.Network.Codec (AnyMessage)
import           Ouroboros.Network.NodeToClient (TraceSendRecv)
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)


instance Transformable Text IO (TraceSendRecv (ChainSync ByronBlock (Tip ByronBlock))) where
  trTransformer = defaultTextTransformer

instance HasPrivacyAnnotation (TraceSendRecv (ChainSync ByronBlock (Tip ByronBlock)))

instance HasSeverityAnnotation (TraceSendRecv (ChainSync ByronBlock (Tip ByronBlock)))

instance ToObject (AnyMessage (ChainSync ByronBlock (Tip ByronBlock))) where
  toObject _verb msg =
    mkObject [ "kind" .= ("TraceSendRecv" :: String)
             , "event" .= show msg
             ]
