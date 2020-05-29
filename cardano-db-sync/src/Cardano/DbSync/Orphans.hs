{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.DbSync.Orphans
  (
  ) where

import           Cardano.Crypto (abstractHashFromDigest)

import           Crypto.Hash (digestFromByteString)

import           Data.Maybe (fromMaybe)

import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock, ByronHash (..))
import           Ouroboros.Consensus.HardFork.Combinator.Unary (FromRawHash (..))


instance FromRawHash ByronBlock where
  fromRawHash _ bs =
    ByronHash
      . abstractHashFromDigest
      . fromMaybe (error $ "Cardano.DbSync.Era.FromRawHash ByronHash: " <> show bs)
      $ digestFromByteString bs
