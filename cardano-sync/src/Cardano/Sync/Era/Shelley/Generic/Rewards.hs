{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Sync.Era.Shelley.Generic.Rewards
  ( Rewards (..)
  , allegraRewards
  , maryRewards
  , shelleyRewards
  ) where

import           Cardano.Sync.Era.Shelley.Generic.StakeCred

import           Cardano.Ledger.Era (Crypto)

import           Data.Bifunctor (bimap)
import           Data.Coerce (coerce)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           Ouroboros.Consensus.Cardano.Block (LedgerState (..), StandardAllegra,
                   StandardCrypto, StandardMary, StandardShelley)
import           Ouroboros.Consensus.Cardano.CanHardFork ()
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus

import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import qualified Shelley.Spec.Ledger.Credential as Shelley
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.LedgerState as Shelley
import qualified Shelley.Spec.Ledger.Rewards as Shelley

-- The `ledger-specs` code defines a `RewardUpdate` type that is parameterised over
-- Shelley/Allegra/Mary. This is a huge pain in the neck for `db-sync` so we define a
-- generic one instead.
data Rewards = Rewards
  { rewards :: Map StakeCred (Set (Shelley.Reward StandardCrypto))
  , orphaned :: Map StakeCred (Set (Shelley.Reward StandardCrypto))
  }


allegraRewards :: Shelley.Network -> LedgerState (ShelleyBlock StandardAllegra) -> Maybe Rewards
allegraRewards = genericRewards

maryRewards :: Shelley.Network -> LedgerState (ShelleyBlock StandardMary) -> Maybe Rewards
maryRewards = genericRewards

shelleyRewards :: Shelley.Network -> LedgerState (ShelleyBlock StandardShelley) -> Maybe Rewards
shelleyRewards = genericRewards

-- -------------------------------------------------------------------------------------------------

genericRewards :: forall era. Shelley.Network -> LedgerState (ShelleyBlock era) -> Maybe Rewards
genericRewards network lstate =
    fmap cleanup rewardUpdate
  where
    cleanup :: Map (Shelley.Credential 'Shelley.Staking (Crypto era)) (Set (Shelley.Reward (Crypto era))) -> Rewards
    cleanup rmap =
      let (rm, om) = Map.partitionWithKey validRewardAddress rmap in
      Rewards
        { rewards = mapBimap (toStakeCred network) (Set.map convertReward) rm
        , orphaned = mapBimap (toStakeCred network) (Set.map convertReward) om
        }
    rewardAccounts :: Shelley.RewardAccounts (Crypto era)
    rewardAccounts =
      Shelley._rewards . Shelley._dstate . Shelley._delegationState . Shelley.esLState
        $ Shelley.nesEs (Consensus.shelleyLedgerState lstate)

    rewardUpdate :: Maybe (Map (Shelley.Credential 'Shelley.Staking (Crypto era)) (Set (Shelley.Reward (Crypto era))))
    rewardUpdate =
      completeRewardUpdate =<< Shelley.strictMaybeToMaybe (Shelley.nesRu $ Consensus.shelleyLedgerState lstate)

    completeRewardUpdate
        :: Shelley.PulsingRewUpdate crypto
        -> Maybe (Map (Shelley.Credential 'Shelley.Staking crypto) (Set (Shelley.Reward crypto)))
    completeRewardUpdate x =
      case x of
        Shelley.Pulsing {} -> Nothing -- Should never happen.
        Shelley.Complete ru -> Just $ Shelley.rs ru

    validRewardAddress :: Shelley.Credential 'Shelley.Staking (Crypto era) -> a -> Bool
    validRewardAddress addr _value = Map.member addr rewardAccounts

    convertReward :: Shelley.Reward (Crypto era) -> Shelley.Reward StandardCrypto
    convertReward sr =
      -- Coerce is safe because we are coercing away an un-needed phantom type parameter.
      sr { Shelley.rewardPool = coerce $ Shelley.rewardPool sr }

mapBimap :: Ord k2 => (k1 -> k2) -> (a1 -> a2) -> Map k1 a1 -> Map k2 a2
mapBimap fk fa = Map.fromList . map (bimap fk fa) . Map.toList
