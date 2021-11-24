{-# LANGUAGE NoImplicitPrelude #-}
module Cardano.DbSync.Era.Shelley.Generic.EpochUpdate
  ( NewEpoch (..)
  , EpochUpdate (..)
  , epochUpdate
  ) where

import           Cardano.Prelude hiding (Maybe (..), fromMaybe)

import           Cardano.Slotting.Slot (EpochNo (..))

import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Shelley.API.Protocol as Shelley
import qualified Cardano.Ledger.Shelley.API.Wallet as Shelley
import qualified Cardano.Protocol.TPraos.Rules.Tickn as Shelley

import           Cardano.DbSync.Era.Shelley.Generic.ProtoParams
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import           Data.Strict.Maybe (Maybe (..))

import           Ouroboros.Consensus.Cardano.Block (HardForkState (..))
import           Ouroboros.Consensus.Cardano.CanHardFork ()
import qualified Ouroboros.Consensus.HeaderValidation as Consensus
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import qualified Ouroboros.Consensus.Shelley.Protocol as Consensus


data NewEpoch = NewEpoch
  { neEpoch :: !EpochNo
  , neIsEBB :: !Bool
  , neAdaPots :: !(Maybe Shelley.AdaPots)
  , neEpochUpdate :: !EpochUpdate
  }

data EpochUpdate = EpochUpdate
  { euProtoParams :: !(Maybe ProtoParams)
  , euNonce :: !Ledger.Nonce
  }

epochUpdate :: ExtLedgerState CardanoBlock -> EpochUpdate
epochUpdate lstate =
  EpochUpdate
    { euProtoParams = maybeToStrict $ epochProtoParams lstate
    , euNonce = extractEpochNonce lstate
    }

-- -------------------------------------------------------------------------------------------------

extractEpochNonce :: ExtLedgerState CardanoBlock -> Ledger.Nonce
extractEpochNonce extLedgerState =
    case Consensus.headerStateChainDep (headerState extLedgerState) of
      ChainDepStateByron _ -> Ledger.NeutralNonce
      ChainDepStateShelley st -> extractNonce st
      ChainDepStateAllegra st -> extractNonce st
      ChainDepStateMary st -> extractNonce st
      ChainDepStateAlonzo st -> extractNonce st
  where
    extractNonce :: Consensus.TPraosState crypto -> Ledger.Nonce
    extractNonce =
      Shelley.ticknStateEpochNonce . Shelley.csTickn . Consensus.tpraosStateChainDepState

