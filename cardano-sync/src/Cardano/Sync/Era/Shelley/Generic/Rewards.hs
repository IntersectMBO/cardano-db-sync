{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Sync.Era.Shelley.Generic.Rewards
  ( Rewards (..)
  , epochRewards
  , rewardsPoolHashKeys
  , rewardsStakeCreds
  ) where

import           Cardano.Sync.Era.Shelley.Generic.StakeCred

import qualified Cardano.Ledger.BaseTypes as Ledger
import           Cardano.Ledger.Era (Crypto)
import qualified Cardano.Ledger.Keys as Ledger

import           Cardano.Slotting.Slot (EpochNo (..))

import           Cardano.Sync.Types

import           Data.Bifunctor (bimap)
import           Data.Coerce (coerce)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           Ouroboros.Consensus.Cardano.Block (LedgerState (..), StandardCrypto)
import           Ouroboros.Consensus.Cardano.CanHardFork ()
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus

import qualified Shelley.Spec.Ledger.Credential as Shelley
import qualified Shelley.Spec.Ledger.LedgerState as Shelley
import qualified Shelley.Spec.Ledger.Rewards as Shelley

-- The `ledger-specs` code defines a `RewardUpdate` type that is parameterised over
-- Shelley/Allegra/Mary. This is a huge pain in the neck for `db-sync` so we define a
-- generic one instead.
data Rewards = Rewards
  { rwdEpoch :: !EpochNo
  , rwdRewards :: !(Map StakeCred (Set (Shelley.Reward StandardCrypto)))
  , rwdOrphaned :: !(Map StakeCred (Set (Shelley.Reward StandardCrypto)))
  } deriving Eq

epochRewards :: Ledger.Network -> EpochNo -> ExtLedgerState CardanoBlock -> Maybe Rewards
epochRewards nw epoch lstate =
  case ledgerState lstate of
    LedgerStateByron _ -> Nothing
    LedgerStateShelley sls -> genericRewards nw epoch sls
    LedgerStateAllegra als -> genericRewards nw epoch als
    LedgerStateMary mls -> genericRewards nw epoch mls
    LedgerStateAlonzo als -> genericRewards nw epoch als

rewardsPoolHashKeys :: Rewards -> Set PoolKeyHash
rewardsPoolHashKeys rwds =
  Set.unions . map (Set.map Shelley.rewardPool) $
    Map.elems (rwdRewards rwds) ++ Map.elems (rwdOrphaned rwds)

rewardsStakeCreds :: Rewards -> Set StakeCred
rewardsStakeCreds rwds =
  Set.union (Map.keysSet $ rwdRewards rwds) (Map.keysSet $ rwdOrphaned rwds)

-- -------------------------------------------------------------------------------------------------

genericRewards :: forall era. Ledger.Network -> EpochNo -> LedgerState (ShelleyBlock era) -> Maybe Rewards
genericRewards network epoch lstate =
    fmap cleanup rewardUpdate
  where
    cleanup :: Map (Shelley.Credential 'Ledger.Staking (Crypto era)) (Set (Shelley.Reward (Crypto era))) -> Rewards
    cleanup rmap =
      let (rm, om) = Map.partitionWithKey validRewardAddress rmap in
      Rewards
        { rwdEpoch = epoch - 1 -- Epoch in whch rewards were earned.
        , rwdRewards = mapBimap (toStakeCred network) (Set.map convertReward) rm
        , rwdOrphaned = mapBimap (toStakeCred network) (Set.map convertReward) om
        }
    rewardAccounts :: Shelley.RewardAccounts (Crypto era)
    rewardAccounts =
      Shelley._rewards . Shelley._dstate . Shelley._delegationState . Shelley.esLState
        $ Shelley.nesEs (Consensus.shelleyLedgerState lstate)

    rewardUpdate :: Maybe (Map (Shelley.Credential 'Ledger.Staking (Crypto era)) (Set (Shelley.Reward (Crypto era))))
    rewardUpdate =
      completeRewardUpdate =<< Ledger.strictMaybeToMaybe (Shelley.nesRu $ Consensus.shelleyLedgerState lstate)

    completeRewardUpdate
        :: Shelley.PulsingRewUpdate crypto
        -> Maybe (Map (Shelley.Credential 'Ledger.Staking crypto) (Set (Shelley.Reward crypto)))
    completeRewardUpdate x =
      case x of
        Shelley.Pulsing {} -> Nothing -- Should never happen.
        Shelley.Complete ru -> Just $ Shelley.rs ru

    validRewardAddress :: Shelley.Credential 'Ledger.Staking (Crypto era) -> a -> Bool
    validRewardAddress addr _value = Map.member addr rewardAccounts

    convertReward :: Shelley.Reward (Crypto era) -> Shelley.Reward StandardCrypto
    convertReward sr =
      -- Coerce is safe because we are coercing away an un-needed phantom type parameter.
      sr { Shelley.rewardPool = coerce $ Shelley.rewardPool sr }

mapBimap :: Ord k2 => (k1 -> k2) -> (a1 -> a2) -> Map k1 a1 -> Map k2 a2
mapBimap fk fa = Map.fromAscList . map (bimap fk fa) . Map.toAscList
