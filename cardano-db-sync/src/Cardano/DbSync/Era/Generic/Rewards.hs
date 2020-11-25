{-# LANGUAGE DataKinds #-}
module Cardano.DbSync.Era.Generic.Rewards
  ( Rewards (..)
  , allegraRewards
  , maryRewards
  , shelleyRewards
  ) where

import           Cardano.DbSync.Config.Types (DbSyncEnv (..))
import           Cardano.DbSync.Era.Generic.StakeCred

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Ouroboros.Consensus.Cardano.Block (LedgerState (..), StandardAllegra, StandardMary,
                   StandardShelley)
import           Ouroboros.Consensus.Cardano.CanHardFork ()
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus

import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import           Shelley.Spec.Ledger.Coin (Coin (..))
import qualified Shelley.Spec.Ledger.LedgerState as Shelley

-- The `ledger-specs` code defines a `RewardUpdate` type that is parameterised over
-- Shelley/Allegra/Mary. This is a huge pain in the neck for `db-sync` so we define a
-- generic one instead.
newtype Rewards
  = Rewards { unRewards :: Map StakeCred Coin }

allegraRewards :: DbSyncEnv -> LedgerState (ShelleyBlock StandardAllegra) -> Maybe Rewards
allegraRewards env =
  fmap (Rewards . Map.mapKeys (toStakeCred env) . Shelley.rs)
    . Shelley.strictMaybeToMaybe . Shelley.nesRu . Consensus.shelleyLedgerState

maryRewards :: DbSyncEnv -> LedgerState (ShelleyBlock StandardMary) -> Maybe Rewards
maryRewards env =
  fmap (Rewards . Map.mapKeys (toStakeCred env) . Shelley.rs)
    . Shelley.strictMaybeToMaybe . Shelley.nesRu . Consensus.shelleyLedgerState

shelleyRewards :: DbSyncEnv -> LedgerState (ShelleyBlock StandardShelley) -> Maybe Rewards
shelleyRewards env =
  fmap (Rewards . Map.mapKeys (toStakeCred env) . Shelley.rs)
    . Shelley.strictMaybeToMaybe . Shelley.nesRu . Consensus.shelleyLedgerState
