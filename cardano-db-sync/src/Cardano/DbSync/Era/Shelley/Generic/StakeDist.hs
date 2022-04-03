{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.DbSync.Era.Shelley.Generic.StakeDist
  ( StakeDist (..)
  , epochStakeDist
  , stakeDistPoolHashKeys
  , stakeDistStakeCreds
  ) where

import           Cardano.Prelude

import           Cardano.Crypto.Hash (hashToBytes)

import qualified Cardano.Ledger.BaseTypes as Ledger
import           Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Compactible as Ledger
import           Cardano.Ledger.Credential (Credential)
import           Cardano.Ledger.Era (Crypto)
import           Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import qualified Cardano.Ledger.Shelley.EpochBoundary as Shelley
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley

import           Cardano.Slotting.Slot (EpochNo (..))

import           Cardano.DbSync.Era.Shelley.Generic.StakeCred
import           Cardano.DbSync.Era.Shelley.Generic.StakePoolKeyHash
import           Cardano.DbSync.Types

import qualified Data.Compact.VMap as VMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Ouroboros.Consensus.Cardano.Block (LedgerState (..))

import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus


data StakeDist = StakeDist
  { sdistEpochNo :: !EpochNo
  , sdistStakeMap :: !(Map StakeCred (Coin, StakePoolKeyHash))
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
stakeDistPoolHashKeys :: StakeDist -> Set StakePoolKeyHash
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
    stakeMap :: Map StakeCred (Coin, StakePoolKeyHash)
    stakeMap = Map.intersectionWith (,) stakeCoinMap stakePoolMap

    stakeCoinMap :: Map StakeCred Coin
    stakeCoinMap = mapBimap (toStakeCred network) Ledger.fromCompact stMap

    stMap :: Map (Credential 'Staking (Crypto era)) (Ledger.CompactForm Coin)
    stMap = VMap.toMap . Shelley.unStake $ Shelley._stake stakeSet

    stakePoolMap :: Map StakeCred StakePoolKeyHash
    stakePoolMap = mapBimap (toStakeCred network) convertStakePoolkeyHash delMap

    delMap :: Map (Credential 'Staking (Crypto era)) (KeyHash 'StakePool (Crypto era))
    delMap = VMap.toMap $ Shelley._delegations stakeSet

    -- We use '_pstakeSet' here instead of '_pstateMark' because the stake addresses for the
    -- later may not have been added to the database yet. That means that when these values
    -- are added to the database, the epoch number where they become active is the current
    -- epoch plus one.

    stakeSet :: Shelley.SnapShot (Crypto era)
    stakeSet = Shelley._pstakeSet . Shelley.esSnapshots . Shelley.nesEs
                $ Consensus.shelleyLedgerState lstate

    convertStakePoolkeyHash :: KeyHash 'StakePool (Crypto era) -> StakePoolKeyHash
    convertStakePoolkeyHash (KeyHash h) = StakePoolKeyHash $ hashToBytes h

-- Is there a better way to do this?
mapBimap :: Ord k2 => (k1 -> k2) -> (a1 -> a2) -> Map k1 a1 -> Map k2 a2
mapBimap fk fa = Map.fromAscList . map (bimap fk fa) . Map.toAscList


