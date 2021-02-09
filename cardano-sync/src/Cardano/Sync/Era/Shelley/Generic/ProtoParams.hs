{-# LANGUAGE NoImplicitPrelude #-}
module Cardano.Sync.Era.Shelley.Generic.ProtoParams
  ( ProtoParams (..)
  , allegraProtoParams
  , maryProtoParams
  , shelleyProtoParams
  ) where

import           Cardano.Prelude

import           Cardano.Slotting.Slot (EpochNo (..))

import           Ouroboros.Consensus.Cardano.Block (LedgerState (..), StandardAllegra, StandardMary,
                   StandardShelley)

import           Ouroboros.Consensus.Cardano (Nonce (..))
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus

import           Shelley.Spec.Ledger.BaseTypes (UnitInterval)
import           Shelley.Spec.Ledger.Coin (Coin (..))
import qualified Shelley.Spec.Ledger.LedgerState as Shelley
import           Shelley.Spec.Ledger.PParams (ProtVer)
import qualified Shelley.Spec.Ledger.PParams as Shelley

data ProtoParams = ProtoParams
  { ppMinfeeA :: !Natural
  , ppMinfeeB :: !Natural
  , ppMaxBBSize :: !Natural
  , ppMaxTxSize :: !Natural
  , ppMaxBHSize :: !Natural
  , ppKeyDeposit :: !Coin
  , ppPoolDeposit :: !Coin
  , ppMaxEpoch :: !EpochNo
  , ppOptialPoolCount :: !Natural
  , ppInfluence :: !Rational
  , ppMonetaryExpandRate :: !UnitInterval
  , ppTreasuryGrowthRate :: !UnitInterval
  , ppDecentralisation :: !UnitInterval
  , ppExtraEntropy :: !Nonce
  , ppProtocolVersion :: !ProtVer
  , ppMinUTxOValue :: !Coin
  , ppMinPoolCost :: !Coin
  }

allegraProtoParams :: LedgerState (ShelleyBlock StandardAllegra) -> ProtoParams
allegraProtoParams =
  toProtoParams . Shelley.esPp . Shelley.nesEs . Consensus.shelleyLedgerState

maryProtoParams :: LedgerState (ShelleyBlock StandardMary) -> ProtoParams
maryProtoParams =
  toProtoParams . Shelley.esPp . Shelley.nesEs . Consensus.shelleyLedgerState

shelleyProtoParams :: LedgerState (ShelleyBlock StandardShelley) -> ProtoParams
shelleyProtoParams =
  toProtoParams . Shelley.esPp . Shelley.nesEs . Consensus.shelleyLedgerState

-- -------------------------------------------------------------------------------------------------

toProtoParams :: Shelley.PParams' Identity era -> ProtoParams
toProtoParams params =
  ProtoParams
    { ppMinfeeA = Shelley._minfeeA params
    , ppMinfeeB = Shelley._minfeeB params
    , ppMaxBBSize = Shelley._maxBBSize params
    , ppMaxTxSize = Shelley._maxTxSize params
    , ppMaxBHSize = Shelley._maxBHSize params
    , ppKeyDeposit = Shelley._keyDeposit params
    , ppPoolDeposit = Shelley._poolDeposit params
    , ppMaxEpoch = Shelley._eMax params
    , ppOptialPoolCount = Shelley._nOpt params
    , ppInfluence = Shelley._a0 params
    , ppMonetaryExpandRate = Shelley._rho params
    , ppTreasuryGrowthRate = Shelley._tau params
    , ppDecentralisation  = Shelley._d params
    , ppExtraEntropy = Shelley._extraEntropy params
    , ppProtocolVersion = Shelley._protocolVersion params
    , ppMinUTxOValue = Shelley._minUTxOValue params
    , ppMinPoolCost = Shelley._minPoolCost params
    }
