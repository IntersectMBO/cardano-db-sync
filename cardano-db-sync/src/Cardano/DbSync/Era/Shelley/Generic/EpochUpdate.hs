{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Generic.EpochUpdate (
  NewEpoch (..),
  EpochUpdate (..),
  epochUpdate,
) where

import Cardano.DbSync.Era.Shelley.Generic.ProtoParams
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Shelley.API.Wallet as Shelley
import Cardano.Prelude hiding (Maybe (..), fromMaybe)
import qualified Cardano.Protocol.TPraos.API as Shelley
import qualified Cardano.Protocol.TPraos.Rules.Tickn as Shelley
import Cardano.Slotting.Slot (EpochNo (..))
import Data.Strict.Maybe (Maybe (..))
import Ouroboros.Consensus.Cardano.Block (HardForkState (..))
import Ouroboros.Consensus.Cardano.CanHardFork ()
import qualified Ouroboros.Consensus.HeaderValidation as Consensus
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import Ouroboros.Consensus.Protocol.Praos as Consensus
import qualified Ouroboros.Consensus.Protocol.TPraos as Consensus

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
    ChainDepStateBabbage st -> extractNoncePraos st
    ChainDepStateConway st -> extractNoncePraos st
  where
    extractNonce :: Consensus.TPraosState c -> Ledger.Nonce
    extractNonce =
      Shelley.ticknStateEpochNonce . Shelley.csTickn . Consensus.tpraosStateChainDepState

    extractNoncePraos :: Consensus.PraosState c -> Ledger.Nonce
    extractNoncePraos = praosStateEpochNonce
