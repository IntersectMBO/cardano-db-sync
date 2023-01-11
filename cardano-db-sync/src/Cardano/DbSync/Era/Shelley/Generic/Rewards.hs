{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Generic.Rewards (
  Reward (..),
  Rewards (..),
  rewardsCount,
  rewardsTotalAda,
) where

import Cardano.Db (Ada, RewardSource (..), word64ToAda)
import Cardano.DbSync.Types
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Prelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import Ouroboros.Consensus.Cardano.CanHardFork ()

data Reward = Reward
  { rewardSource :: !RewardSource
  , rewardPool :: !(Strict.Maybe PoolKeyHash)
  , rewardAmount :: !Coin
  }
  deriving (Eq, Ord, Show)

-- The `ledger-specs` code defines a `RewardUpdate` type that is parameterised over
-- Shelley/Allegra/Mary. This is a huge pain in the neck for `db-sync` so we define a
-- generic one instead.
newtype Rewards = Rewards
  { unRewards :: Map StakeCred (Set Reward)
  }
  deriving (Eq, Show)

rewardsCount :: Rewards -> Int
rewardsCount = sum . map Set.size . Map.elems . unRewards

rewardsTotalAda :: Rewards -> Ada
rewardsTotalAda rwds =
  word64ToAda
    . fromIntegral
    . sum
    . concatMap (map (unCoin . rewardAmount) . Set.toList)
    $ Map.elems (unRewards rwds)
