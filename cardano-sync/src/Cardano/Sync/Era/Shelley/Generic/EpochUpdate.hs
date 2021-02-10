module Cardano.Sync.Era.Shelley.Generic.EpochUpdate
  ( EpochUpdate (..)
  , allegraEpochUpdate
  , maryEpochUpdate
  , shelleyEpochUpdate
  ) where

import           Cardano.Sync.Era.Shelley.Generic.ProtoParams
import           Cardano.Sync.Era.Shelley.Generic.Rewards
import           Cardano.Sync.Era.Shelley.Generic.StakeDist

import           Data.Maybe (fromMaybe)

import           Ouroboros.Consensus.Cardano.Block (LedgerState (..), StandardAllegra, StandardMary,
                   StandardShelley)
import           Ouroboros.Consensus.Cardano.CanHardFork ()
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)

import qualified Shelley.Spec.Ledger.BaseTypes as Shelley

data EpochUpdate = EpochUpdate
  { euProtoParams :: !ProtoParams
  , euRewards :: !(Maybe Rewards)
  , euStakeDistribution :: !StakeDist
  , euNonce :: !Shelley.Nonce
  }

allegraEpochUpdate :: Shelley.Network -> LedgerState (ShelleyBlock StandardAllegra) -> Maybe Rewards -> Maybe Shelley.Nonce -> EpochUpdate
allegraEpochUpdate network sls mRewards mNonce =
  EpochUpdate
    { euProtoParams = allegraProtoParams sls
    , euRewards = mRewards
    , euStakeDistribution = allegraStakeDist network sls
    , euNonce = fromMaybe Shelley.NeutralNonce mNonce
    }

maryEpochUpdate :: Shelley.Network -> LedgerState (ShelleyBlock StandardMary) -> Maybe Rewards -> Maybe Shelley.Nonce -> EpochUpdate
maryEpochUpdate network sls mRewards mNonce =
  EpochUpdate
    { euProtoParams = maryProtoParams sls
    , euRewards = mRewards
    , euStakeDistribution = maryStakeDist network sls
    , euNonce = fromMaybe Shelley.NeutralNonce mNonce
    }

shelleyEpochUpdate :: Shelley.Network -> LedgerState (ShelleyBlock StandardShelley) -> Maybe Rewards -> Maybe Shelley.Nonce -> EpochUpdate
shelleyEpochUpdate network sls mRewards mNonce =
  EpochUpdate
    { euProtoParams = shelleyProtoParams sls
    , euRewards = mRewards
    , euStakeDistribution = shelleyStakeDist network sls
    , euNonce = fromMaybe Shelley.NeutralNonce mNonce
    }
