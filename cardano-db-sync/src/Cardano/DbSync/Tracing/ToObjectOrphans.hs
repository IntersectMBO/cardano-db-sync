{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Cardano.DbSync.Tracing.ToObjectOrphans () where

import           Data.Aeson ((.=))
import           Data.Text (Text)
import qualified Data.Text as Text

import           Cardano.BM.Data.Tracer
import           Cardano.Tracing.OrphanInstances.Network ()

import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Network.Block (Point (..), Tip (..))
import           Ouroboros.Network.NodeToNode (TraceSendRecv (..))
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)


instance HasTextFormatter (TraceSendRecv (ChainSync blk (Point blk) (Tip blk))) where
  formatText _ = Text.pack . show

instance ToObject ByronBlock where
  toObject _verb msg =
    mkObject [ "kind" .= ("ByronBlock" :: String)
             , "event" .= show msg
             ]

instance Transformable Text IO (TraceSendRecv (ChainSync blk (Point blk) (Tip blk))) where
  trTransformer = trStructuredText
