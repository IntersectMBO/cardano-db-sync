{-# LANGUAGE OverloadedStrings #-}
module Cardano.DbSync.Era.Cardano.Util
  ( unChainHash
  ) where

import qualified Data.ByteString.Short as BSS

import           Cardano.DbSync.Config.Types

import           Cardano.Prelude

import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus

import           Ouroboros.Network.Block (ChainHash (..))


unChainHash :: ChainHash CardanoBlock -> ByteString
unChainHash ch =
  case ch of
    GenesisHash -> "genesis"
    BlockHash bh -> BSS.fromShort (Consensus.getOneEraHash bh)


