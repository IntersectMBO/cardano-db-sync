{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.DbSync.Era.Shelley.Generic.StakePoolKeyHash
  ( StakePoolKeyHash (..)
  , toStakePoolKeyHash
  ) where

import           Cardano.Prelude

import           Cardano.Crypto.Hash (hashToBytes)

import           Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))

import           Ouroboros.Consensus.Cardano.Block (StandardCrypto)


newtype StakePoolKeyHash
  = StakePoolKeyHash { unStakePoolKeyHash :: ByteString }
  deriving (Eq, Ord, Show)

toStakePoolKeyHash :: KeyHash 'StakePool StandardCrypto -> StakePoolKeyHash
toStakePoolKeyHash (KeyHash h) = StakePoolKeyHash $ hashToBytes h
