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
  getRewardsUpdate,
) where

import Cardano.Db (Ada, RewardSource (..), word64ToAda)
import Cardano.DbSync.Types
import Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.LedgerState hiding (LedgerState)
import Cardano.Prelude
import qualified Data.Map.Strict as Map
import Data.SOP.Strict.NP
import qualified Data.Set as Set
import Ouroboros.Consensus.Cardano.Block (EraCrypto, LedgerState (..))
import Ouroboros.Consensus.Cardano.CanHardFork ()
import Ouroboros.Consensus.Config (TopLevelConfig (..))
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import Ouroboros.Consensus.HardFork.Combinator.Basics
import Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import Ouroboros.Consensus.Shelley.Ledger.Ledger
import Ouroboros.Consensus.Shelley.ShelleyHFC

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

getRewardsUpdate :: TopLevelConfig CardanoBlock -> ExtLedgerState CardanoBlock -> Maybe (RewardUpdate StandardCrypto)
getRewardsUpdate cfg els =
  case ledgerState els of
    LedgerStateByron _ -> Nothing
    LedgerStateShelley sls -> genericRewardUpdate cfg sls
    LedgerStateAllegra als -> genericRewardUpdate cfg als
    LedgerStateMary mls -> genericRewardUpdate cfg mls
    LedgerStateAlonzo als -> genericRewardUpdate cfg als
    LedgerStateBabbage bls -> genericRewardUpdate cfg bls
    LedgerStateConway cls -> genericRewardUpdate cfg cls

genericRewardUpdate ::
  forall era p.
  (EraCrypto era ~ StandardCrypto) =>
  TopLevelConfig CardanoBlock ->
  LedgerState (ShelleyBlock p era) ->
  Maybe (RewardUpdate StandardCrypto)
genericRewardUpdate cfg lstate = do
  pulsing <- strictMaybeToMaybe mPulsing
  case pulsing of
    Complete _ -> Nothing
    Pulsing _ _ -> do
      let Identity (rewardUpdate, _) = runReaderT (completeRupd pulsing) globals
      Just rewardUpdate
  where
    mPulsing = nesRu $ shelleyLedgerState lstate

    globals = case getPerEraLedgerConfig $ hardForkLedgerConfigPerEra $ topLevelConfigLedger cfg of
      _ :* wplc :* _ -> shelleyLedgerGlobals $ shelleyLedgerConfig $ unwrapPartialLedgerConfig wplc
