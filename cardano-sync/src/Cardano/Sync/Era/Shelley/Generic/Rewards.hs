{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Sync.Era.Shelley.Generic.Rewards
  ( Reward (..)
  , Rewards (..)
  , epochRewards
  , rewardsPoolHashKeys
  , rewardsStakeCreds
  ) where

import           Cardano.Db (RewardSource (..), rewardTypeToSource)

import qualified Cardano.Ledger.BaseTypes as Ledger
import           Cardano.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Credential as Ledger
import           Cardano.Ledger.Era (Crypto)
import qualified Cardano.Ledger.Keys as Ledger

import           Cardano.Slotting.Slot (EpochNo (..))

import           Cardano.Sync.Era.Shelley.Generic.StakeCred
import           Cardano.Sync.Types

import           Data.Bifunctor (bimap)
import           Data.Coerce (coerce)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set

import           Ouroboros.Consensus.Cardano.Block (LedgerState (..), StandardCrypto)
import           Ouroboros.Consensus.Cardano.CanHardFork ()
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus

import qualified Shelley.Spec.Ledger.LedgerState as Shelley
import qualified Shelley.Spec.Ledger.Rewards as Shelley

data Reward = Reward
  { rewardSource :: !RewardSource
  , rewardPool :: !(Maybe (Ledger.KeyHash 'Ledger.StakePool StandardCrypto))
  , rewardAmount :: !Coin
  } deriving (Eq, Ord, Show)

-- The `ledger-specs` code defines a `RewardUpdate` type that is parameterised over
-- Shelley/Allegra/Mary. This is a huge pain in the neck for `db-sync` so we define a
-- generic one instead.
data Rewards = Rewards
  { rwdEpoch :: !EpochNo
  , rwdRewards :: !(Map StakeCred (Set Reward))
  } deriving Eq

epochRewards :: Ledger.Network -> EpochNo -> ExtLedgerState CardanoBlock -> Maybe Rewards
epochRewards nw epoch lstate =
  case ledgerState lstate of
    LedgerStateByron _ -> Nothing
    LedgerStateShelley sls -> genericRewards nw Shelley epoch sls
    LedgerStateAllegra als -> genericRewards nw Allegra epoch als
    LedgerStateMary mls -> genericRewards nw Mary epoch mls
    LedgerStateAlonzo als -> genericRewards nw Alonzo epoch als

rewardsPoolHashKeys :: Rewards -> Set PoolKeyHash
rewardsPoolHashKeys rwds =
  Set.fromList . mapMaybe rewardPool
    $ concatMap Set.toList (Map.elems $ rwdRewards rwds)

rewardsStakeCreds :: Rewards -> Set StakeCred
rewardsStakeCreds = Map.keysSet . rwdRewards

-- -------------------------------------------------------------------------------------------------

genericRewards :: forall era. Ledger.Network -> BlockEra -> EpochNo -> LedgerState (ShelleyBlock era) -> Maybe Rewards
genericRewards network era epoch lstate =
    fmap cleanup rewardUpdate
  where
    cleanup :: Map StakeCred (Set Reward) -> Rewards
    cleanup rmap =
      Rewards
        { rwdEpoch = epoch - 1 -- Epoch in which rewards were earned.
        , rwdRewards = filterByEra era $ Map.filterWithKey validRewardAddress rmap
        }

    rewardUpdate :: Maybe (Map StakeCred (Set Reward))
    rewardUpdate =
      completeRewardUpdate =<< Ledger.strictMaybeToMaybe (Shelley.nesRu $ Consensus.shelleyLedgerState lstate)

    completeRewardUpdate :: Shelley.PulsingRewUpdate (Crypto era) -> Maybe (Map StakeCred (Set Reward))
    completeRewardUpdate x =
      case x of
        Shelley.Pulsing {} -> Nothing -- Should never happen.
        Shelley.Complete ru -> Just $ Map.unionWith mappend
                                        (convertRewardMap $ Shelley.rs ru)
                                        (getInstantaneousRewards network lstate)

    validRewardAddress :: StakeCred -> Set Reward -> Bool
    validRewardAddress addr _value = Set.member addr rewardAccounts

    rewardAccounts :: Set StakeCred
    rewardAccounts =
        Set.fromList . map (toStakeCred network) . Map.keys
          . Shelley._rewards . Shelley._dstate . Shelley._delegationState . Shelley.esLState
          . Shelley.nesEs $ Consensus.shelleyLedgerState lstate

    convertRewardMap
        :: Map (Ledger.Credential 'Ledger.Staking (Crypto era)) (Set (Shelley.Reward (Crypto era)))
        -> Map StakeCred (Set Reward)
    convertRewardMap = mapBimap (toStakeCred network) (Set.map convertReward)

    convertReward :: Shelley.Reward (Crypto era) -> Reward
    convertReward sr =
      Reward
        { rewardSource = rewardTypeToSource $ Shelley.rewardType sr
        , rewardAmount = Shelley.rewardAmount sr
        , -- Coerce is safe here because we are coercing away an un-needed phantom type parameter (era).
          rewardPool = Just $ coerce (Shelley.rewardPool sr)
        }


mapBimap :: Ord k2 => (k1 -> k2) -> (a1 -> a2) -> Map k1 a1 -> Map k2 a2
mapBimap fk fa = Map.fromAscList . map (bimap fk fa) . Map.toAscList


getInstantaneousRewards :: forall era. Ledger.Network -> LedgerState (ShelleyBlock era) -> Map StakeCred (Set Reward)
getInstantaneousRewards network lstate =
    Map.unionWith mappend
        (mapBimap (toStakeCred network) (convert RwdReserves) $ Shelley.iRReserves instRwds)
        (mapBimap (toStakeCred network) (convert RwdTreasury) $ Shelley.iRTreasury instRwds)
  where
    convert :: RewardSource -> Coin -> Set Reward
    convert rs coin =
      Set.singleton
        Reward
          { rewardSource = rs
          , rewardAmount = coin
          , rewardPool = Nothing
          }

    instRwds :: Shelley.InstantaneousRewards (Crypto era)
    instRwds =
      Shelley._irwd . Shelley._dstate . Shelley._delegationState
        . Shelley.esLState . Shelley.nesEs $ Consensus.shelleyLedgerState lstate

-- -------------------------------------------------------------------------------------------------

-- | `db-sync` needs to be match the implementation of the logic it `ledger-specs` even when that
-- logic is not actually correct (ie bug compatible). Eg it was intended that a single stake address
-- can receive rewards for being a pool owner and a pool member. However, due to a bug in
-- `ledger-specs` member rewards a accidentally dropped whenever the same address got a pool owner
-- reward (this was paid back later with a payment from the reserves).
filterByEra :: BlockEra -> Map StakeCred (Set Reward) -> Map StakeCred (Set Reward)
filterByEra be rmap =
  case be of
    Shelley -> Map.map shelleyFilter rmap
    Allegra -> rmap
    Mary -> rmap
    Alonzo -> rmap

shelleyFilter :: Set Reward -> Set Reward
shelleyFilter rs =
    if hasLeaderReward
      then Set.fromList $ filter (\x -> rewardSource x == RwdMember) xs
      else rs
  where
    xs :: [Reward]
    xs = Set.toList rs

    hasLeaderReward :: Bool
    hasLeaderReward = any (\x -> rewardSource x == RwdLeader) xs
