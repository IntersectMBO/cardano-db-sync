{-# LANGUAGE OverloadedStrings #-}
module Cardano.DbSync.Era.Cardano.Util
  ( unChainHash
  ) where

import qualified Data.ByteString.Short as BSS

import           Cardano.Prelude

import           Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus

import           Ouroboros.Network.Block (ChainHash (..))


unChainHash :: ChainHash (CardanoBlock era) -> ByteString
unChainHash ch =
  case ch of
    GenesisHash -> "genesis"
    BlockHash bh -> BSS.fromShort (Consensus.getOneEraHash bh)


