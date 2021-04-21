{-# LANGUAGE NoImplicitPrelude #-}
module Cardano.Sync.Era.Shelley.Generic.EpochUpdate
  ( NewEpoch (..)
  , EpochUpdate (..)
  , AdaPots (..)
  , epochUpdate
  ) where

import           Cardano.Prelude hiding (Maybe (..), fromMaybe)

import           Cardano.Slotting.Slot (EpochNo (..))

import           Cardano.Sync.Era.Shelley.Generic.ProtoParams
import           Cardano.Sync.Era.Shelley.Generic.Rewards
import           Cardano.Sync.Era.Shelley.Generic.StakeDist
import           Cardano.Sync.Types

import           Data.Strict.Maybe (Maybe (..))

import           Ouroboros.Consensus.Cardano.Block (HardForkState (..))
import           Ouroboros.Consensus.Cardano.CanHardFork ()
import qualified Ouroboros.Consensus.HeaderValidation as Consensus
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import qualified Ouroboros.Consensus.Shelley.Protocol as Consensus

import qualified Shelley.Spec.Ledger.API.Protocol as Shelley
import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import           Shelley.Spec.Ledger.Coin (Coin (..))
import qualified Shelley.Spec.Ledger.STS.Tickn as Shelley

data NewEpoch = NewEpoch
  { neEpoch :: !EpochNo
  , neIsEBB :: !Bool
  , neAdaPots :: !(Maybe AdaPots)
  , neEpochUpdate :: !EpochUpdate
  }

data EpochUpdate = EpochUpdate
  { euProtoParams :: !(Maybe ProtoParams)
  , euRewards :: !(Maybe Rewards)
  , euStakeDistribution :: !StakeDist
  , euNonce :: !Shelley.Nonce
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

epochUpdate :: Shelley.Network -> EpochNo -> ExtLedgerState CardanoBlock -> Maybe Rewards -> EpochUpdate
epochUpdate network epochNo lstate mRewards =
    EpochUpdate
      { euProtoParams = maybeToStrict $ epochProtoParams lstate
      , euRewards = mRewards
      , euStakeDistribution = epochStakeDist network epochNo lstate
      , euNonce = extractEpochNonce lstate
      }

-- -------------------------------------------------------------------------------------------------

extractEpochNonce :: ExtLedgerState CardanoBlock -> Shelley.Nonce
extractEpochNonce extLedgerState =
    case Consensus.headerStateChainDep (headerState extLedgerState) of
      ChainDepStateByron _ -> Shelley.NeutralNonce
      ChainDepStateShelley st -> extractNonce st
      ChainDepStateAllegra st -> extractNonce st
      ChainDepStateMary st -> extractNonce st
  where
    extractNonce :: Consensus.TPraosState crypto -> Shelley.Nonce
    extractNonce =
      Shelley.ticknStateEpochNonce . Shelley.csTickn . Consensus.tpraosStateChainDepState

