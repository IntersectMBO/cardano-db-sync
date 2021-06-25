{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Sync.Era.Shelley.Generic.StakeDist
  ( StakeDist (..)
  , epochStakeDist
  , stakeDistPoolHashKeys
  , stakeDistStakeCreds
  ) where

import           Cardano.Prelude

import qualified Cardano.Ledger.BaseTypes as Ledger
import           Cardano.Ledger.Coin (Coin (..))
import           Cardano.Ledger.Credential (Credential)
import           Cardano.Ledger.Era (Crypto)
import           Cardano.Ledger.Keys (KeyHash, KeyRole (..))

import           Cardano.Slotting.Slot (EpochNo (..))

import           Cardano.Sync.Era.Shelley.Generic.StakeCred
import           Cardano.Sync.Types

import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Ouroboros.Consensus.Cardano.Block (LedgerState (..), StandardCrypto)

import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus

import qualified Shelley.Spec.Ledger.EpochBoundary as Shelley
import qualified Shelley.Spec.Ledger.LedgerState as Shelley hiding (_delegations)


data StakeDist = StakeDist
  { sdistEpochNo :: !EpochNo
  , sdistStakeMap :: !(Map StakeCred (Coin, KeyHash 'StakePool StandardCrypto))
  } deriving Eq

epochStakeDist :: Ledger.Network -> EpochNo -> ExtLedgerState CardanoBlock -> Maybe StakeDist
epochStakeDist network epoch els =
  case ledgerState els of
    LedgerStateByron _ -> Nothing
    LedgerStateShelley sls -> Just $ genericStakeDist network epoch sls
    LedgerStateAllegra als -> Just $ genericStakeDist network epoch als
    LedgerStateMary mls -> Just $ genericStakeDist network epoch mls
    LedgerStateAlonzo als -> Just $ genericStakeDist network epoch als

-- Use Set because they guarantee unique elements.
stakeDistPoolHashKeys :: StakeDist -> Set PoolKeyHash
stakeDistPoolHashKeys = Set.fromList . map snd . Map.elems . sdistStakeMap

stakeDistStakeCreds :: StakeDist -> Set StakeCred
stakeDistStakeCreds = Map.keysSet . sdistStakeMap

-- -------------------------------------------------------------------------------------------------

genericStakeDist :: forall era. Ledger.Network -> EpochNo -> LedgerState (ShelleyBlock era) -> StakeDist
genericStakeDist network epoch lstate =
    StakeDist
      { sdistEpochNo = epoch
      , sdistStakeMap = stakeMap
      }
  where
    stakeMap :: Map StakeCred (Coin, KeyHash 'StakePool StandardCrypto)
    stakeMap =
      Map.mapKeys (toStakeCred network) $
        Map.intersectionWith (,) stakeCoinMap stakePoolMap

    -- We used coerce here on a phanton type: Crypto era -> StandardCrypto.
    stakeCoinMap :: Map (Credential 'Staking StandardCrypto) Coin
    stakeCoinMap = Map.mapKeys coerce . Shelley.unStake $ Shelley._stake stakeSet

    stakePoolMap :: Map (Credential 'Staking StandardCrypto) (KeyHash 'StakePool StandardCrypto)
    stakePoolMap = mapBimap coerce coerce $ Shelley._delegations stakeSet

    -- We use '_pstakeSet' here instead of '_pstateMark' because the stake addresses for the
    -- later may not have been added to the database yet. That means that when these values
    -- are added to the database, the epoch number where they become active is the current
    -- epoch plus one.

    stakeSet :: Shelley.SnapShot (Crypto era)
    stakeSet = Shelley._pstakeSet . Shelley.esSnapshots . Shelley.nesEs
                $ Consensus.shelleyLedgerState lstate

-- Is there a better way to do this?
mapBimap :: Ord k2 => (k1 -> k2) -> (a1 -> a2) -> Map k1 a1 -> Map k2 a2
mapBimap fk fa = Map.fromAscList . map (bimap fk fa) . Map.toAscList
