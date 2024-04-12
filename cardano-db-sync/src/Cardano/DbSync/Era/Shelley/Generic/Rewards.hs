{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Generic.Rewards (
  Reward (..),
  Rewards (..),
  RewardRest (..),
  RewardRests (..),
  rewardsCount,
  rewardsTotalAda,
) where

import Cardano.Db (Ada, RewardSource (..), word64ToAda)
import Cardano.DbSync.Types
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Prelude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Ouroboros.Consensus.Cardano.CanHardFork ()

data Reward = Reward
  { rewardSource :: !RewardSource
  , rewardPool :: !PoolKeyHash
  , rewardAmount :: !Coin
  }
  deriving (Eq, Ord, Show)

newtype Rewards = Rewards
  { unRewards :: Map StakeCred (Set Reward)
  }
  deriving (Eq, Show)

data RewardRest = RewardRest
  { irSource :: !RewardSource
  , irAmount :: !Coin
  }
  deriving (Eq, Ord, Show)

newtype RewardRests = RewardRests
  { unIRewards :: Map StakeCred (Set Reward)
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
