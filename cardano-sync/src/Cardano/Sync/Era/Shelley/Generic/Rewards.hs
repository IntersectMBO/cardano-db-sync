{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Sync.Era.Shelley.Generic.Rewards
  ( Reward (..)
  , Rewards (..)
  , epochRewards
  , rewardsPoolHashKeys
  , rewardsStakeCreds
  ) where

import           Cardano.Prelude

import           Cardano.Db (RewardSource (..), rewardTypeToSource, textShow)

import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import qualified Cardano.Ledger.BaseTypes as Ledger
import           Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import           Cardano.Ledger.Era (Crypto)
import qualified Cardano.Ledger.Keys as Ledger

import           Cardano.Slotting.Slot (EpochNo (..))

import           Cardano.Sync.Era.Shelley.Generic.StakeCred
import           Cardano.Sync.Types

import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Ouroboros.Consensus.Cardano.Block (LedgerState (..), StandardCrypto)
import           Ouroboros.Consensus.Cardano.CanHardFork ()
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus

import qualified Shelley.Spec.Ledger.LedgerState as Shelley
import qualified Shelley.Spec.Ledger.PParams as Shelley
import qualified Shelley.Spec.Ledger.Rewards as Shelley


-- The fields of this struct *must* remain in this ordering in order for the `Ord` instance
-- to work correctly for `takeFirstReward` to operate correctly.
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
      LedgerStateShelley sls -> genericRewards nw era epoch sls
      LedgerStateAllegra als -> genericRewards nw era epoch als
      LedgerStateMary mls -> genericRewards nw era epoch mls
      LedgerStateAlonzo als -> genericRewards nw era epoch als
  where
    era :: BlockEra
    era = rewardBlockEra $ rewardProtoVer lstate

rewardsPoolHashKeys :: Rewards -> Set PoolKeyHash
rewardsPoolHashKeys rwds =
  Set.fromList . mapMaybe rewardPool
    $ concatMap Set.toList (Map.elems $ rwdRewards rwds)

rewardsStakeCreds :: Rewards -> Set StakeCred
rewardsStakeCreds = Map.keysSet . rwdRewards

rewardBlockEra :: Shelley.ProtVer -> BlockEra
rewardBlockEra pv =
  case pv of
    Shelley.ProtVer 2 0 -> Shelley
    Shelley.ProtVer 3 0 -> Allegra
    Shelley.ProtVer 4 0 -> Mary
    Shelley.ProtVer 5 0 -> Alonzo
    x -> panic $ "rewardBlockEra: " <> textShow x

rewardProtoVer :: ExtLedgerState CardanoBlock -> Shelley.ProtVer
rewardProtoVer lstate =
    case ledgerState lstate of
      LedgerStateByron _ -> Shelley.ProtVer 1 0 -- Should never happen.
      LedgerStateShelley sls -> Shelley._protocolVersion $ previousPParams sls
      LedgerStateAllegra als -> Shelley._protocolVersion $ previousPParams als
      LedgerStateMary mls -> Shelley._protocolVersion $ previousPParams mls
      LedgerStateAlonzo als -> Alonzo._protocolVersion $ previousPParams als
  where
    -- Get the *previous* block's PParams by using `esPrevPp` `esPp`.
    previousPParams :: LedgerState (ShelleyBlock era) -> Ledger.PParams era
    previousPParams = Shelley.esPrevPp . Shelley.nesEs . Consensus.shelleyLedgerState

-- -------------------------------------------------------------------------------------------------

genericRewards :: forall era. Ledger.Network -> BlockEra -> EpochNo -> LedgerState (ShelleyBlock era) -> Maybe Rewards
genericRewards network era epoch lstate =
    fmap cleanup rewardUpdate
  where
    cleanup :: Map StakeCred (Set Reward) -> Rewards
    cleanup rmap =
      Rewards
        { rwdEpoch = epoch - 1 -- Epoch in which rewards were earned.
        , rwdRewards = filterByEra era rmap
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
-- `db-sync` needs to match the implementation of the logic in `ledger-specs` even when that logic
-- is not actually correct (ie it needs to be bug compatible). Eg it was intended that a single
-- stake address can receive rewards from more than one place (eg for being a pool owner and a
-- pool member or rewards from two separate pools going to the name stake address). However, due
-- to a bug in `ledger-specs` all rewards other than the first are accidentally dropped (caused by
-- the use of `Map.union` instead of `Map.unionWith mapppend`). These missing rewards have since
-- been paid back with payments from the reserves.

filterByEra :: BlockEra -> Map StakeCred (Set Reward) -> Map StakeCred (Set Reward)
filterByEra be rmap =
  case be of
    Shelley -> Map.map takeFirstReward rmap
    Allegra -> rmap
    Mary -> rmap
    Alonzo -> rmap

-- This emulates the `ledger-specs` bug by taking the first element of the reward Set.
-- The `Ord` instance on `Reward`, orders by `rewardSource` first, then `rewardPool` and then
-- `rewardAmount`.
takeFirstReward :: Set Reward -> Set Reward
takeFirstReward rs =
  -- The `toList` operation returns an ordered list.
  case Set.toList rs of
    [] -> mempty -- Should never happen.
    x:_ -> Set.singleton x
