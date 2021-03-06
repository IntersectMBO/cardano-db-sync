module Cardano.Sync.Era.Shelley.Generic.EpochUpdate
  ( NewEpoch (..)
  , EpochUpdate (..)
  , AdaPots (..)
  , allegraEpochUpdate
  , maryEpochUpdate
  , shelleyEpochUpdate
  ) where

import           Cardano.Sync.Era.Shelley.Generic.ProtoParams
import           Cardano.Sync.Era.Shelley.Generic.Rewards
import           Cardano.Sync.Era.Shelley.Generic.StakeDist

import           Data.Maybe (fromMaybe)

import           Ouroboros.Consensus.Block (EpochNo)
import           Ouroboros.Consensus.Cardano.Block (LedgerState (..), StandardAllegra, StandardMary,
                   StandardShelley)
import           Ouroboros.Consensus.Cardano.CanHardFork ()
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)

import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import           Shelley.Spec.Ledger.Coin (Coin (..))

data NewEpoch = NewEpoch
  { epoch :: !EpochNo
  , isEBB :: !Bool
  , adaPots :: !(Maybe AdaPots)
  , epochUpdate :: !(Maybe EpochUpdate)
  }

data EpochUpdate = EpochUpdate
  { euProtoParams :: !ProtoParams
  , euRewards :: !(Maybe Rewards)
  , euStakeDistribution :: !StakeDist
  , euNonce :: !Shelley.Nonce
  }

-- There is a similar type in ledger-spec, but it is not exported yet.
data AdaPots = AdaPots
  { apTreasury :: !Coin
  , apReserves :: !Coin
  , apRewards :: !Coin
  , apCirculation :: !Coin
  , apDeposits :: !Coin
  , apFees :: !Coin
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
