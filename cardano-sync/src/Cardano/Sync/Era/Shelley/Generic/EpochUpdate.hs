{-# LANGUAGE NoImplicitPrelude #-}
module Cardano.Sync.Era.Shelley.Generic.EpochUpdate
  ( NewEpoch (..)
  , EpochUpdate (..)
  , AdaPots (..)
  , epochUpdate
  , orphanedRewardCount
  , rewardCount
  , stakeDistributionCount
  ) where

import           Cardano.Prelude

import           Cardano.Slotting.Slot (EpochNo (..))

import           Cardano.Sync.Era.Shelley.Generic.ProtoParams
import           Cardano.Sync.Era.Shelley.Generic.Rewards
import           Cardano.Sync.Era.Shelley.Generic.StakeDist
import           Cardano.Sync.Types

import           Ouroboros.Consensus.Cardano.Block (LedgerState (..), StandardAllegra, StandardMary,
                   StandardShelley)
import           Ouroboros.Consensus.Cardano.CanHardFork ()
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)

import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import           Shelley.Spec.Ledger.Coin (Coin (..))

data NewEpoch = NewEpoch
  { epoch :: !EpochNo
  , isEBB :: !Bool
  , adaPots :: !(Maybe AdaPots)
  , neEpochUpdate :: !(Maybe EpochUpdate)
  , neProtoParams :: !(Maybe ProtoParams)
  , neNonce :: !Shelley.Nonce
  }

data EpochUpdate = EpochUpdate
  { euEpoch :: !EpochNo
  , euRewards :: !Rewards
  , euStakeDistribution :: !StakeDist
  }

-- There is a similar type in ledger-spec, but it is not exported yet.
data AdaPots = AdaPots
  { apTreasury :: !Coin
  , apReserves :: !Coin
  , apRewards :: !Coin
  , apUtxo :: !Coin
  , apDeposits :: !Coin
  , apFees :: !Coin
  }

-- Create an EpochUpdate from the current epoch state and the rewards from the last epoch.
epochUpdate :: Shelley.Network -> EpochNo -> ExtLedgerState CardanoBlock -> Rewards -> Maybe EpochUpdate
epochUpdate nw epochNo els rwds =
  case ledgerState els of
    LedgerStateByron _ -> Nothing
    LedgerStateShelley sls -> Just $ shelleyEpochUpdate nw epochNo sls rwds
    LedgerStateAllegra als -> Just $ allegraEpochUpdate nw epochNo als rwds
    LedgerStateMary mls -> Just $ maryEpochUpdate nw epochNo mls rwds

orphanedRewardCount :: EpochUpdate -> Int
orphanedRewardCount eu = length (rwdOrphaned $ euRewards eu)

rewardCount :: EpochUpdate -> Int
rewardCount eu = length (rwdRewards $ euRewards eu)

stakeDistributionCount :: EpochUpdate -> Int
stakeDistributionCount eu = length (unStakeDist $ euStakeDistribution eu)

-- -------------------------------------------------------------------------------------------------

allegraEpochUpdate :: Shelley.Network -> EpochNo -> LedgerState (ShelleyBlock StandardAllegra) -> Rewards -> EpochUpdate
allegraEpochUpdate nw epochNo als rwds =
  EpochUpdate
    { euEpoch = epochNo - 2
    , euRewards = rwds
    , euStakeDistribution = allegraStakeDist nw als
    }

maryEpochUpdate :: Shelley.Network -> EpochNo -> LedgerState (ShelleyBlock StandardMary) -> Rewards -> EpochUpdate
maryEpochUpdate nw epochNo mls rwds =
  EpochUpdate
    { euEpoch = epochNo - 2
    , euRewards = rwds
    , euStakeDistribution = maryStakeDist nw mls
    }

shelleyEpochUpdate :: Shelley.Network -> EpochNo -> LedgerState (ShelleyBlock StandardShelley) -> Rewards -> EpochUpdate
shelleyEpochUpdate nw epochNo sls rwds =
  EpochUpdate
    { euEpoch = epochNo - 2
    , euRewards = rwds
    , euStakeDistribution = shelleyStakeDist nw sls
    }
