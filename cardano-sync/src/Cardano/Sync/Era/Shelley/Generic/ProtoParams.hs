{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
module Cardano.Sync.Era.Shelley.Generic.ProtoParams
  ( ProtoParams (..)
  , epochProtoParams
  ) where

import           Cardano.Prelude

import qualified Cardano.Ledger.Alonzo as Alonzo
import           Cardano.Ledger.Alonzo.Language (Language)
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import           Cardano.Ledger.BaseTypes (UnitInterval)
import qualified Cardano.Ledger.BaseTypes as Ledger
import           Cardano.Ledger.Coin (Coin (..))
import           Cardano.Slotting.Slot (EpochNo (..))

import           Cardano.Sync.Types

import           Ouroboros.Consensus.Cardano.Block (LedgerState (..), StandardAllegra,
                   StandardAlonzo, StandardCrypto, StandardMary, StandardShelley)

import           Ouroboros.Consensus.Cardano (Nonce (..))
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus

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

  -- New for Alonzo.
  , ppCoinsPerUtxoWord :: !(Maybe Coin)
  , ppCostmdls :: !(Maybe (Map Language Alonzo.CostModel))
  , ppPriceMem :: !(Maybe Rational)
  , ppPriceStep :: !(Maybe Rational)
  , ppMaxTxExMem :: !(Maybe Word64)
  , ppMaxTxExSteps :: !(Maybe Word64)
  , ppMaxBlockExMem :: !(Maybe Word64)
  , ppMaxBlockExSteps :: !(Maybe Word64)
  , ppMaxValSize :: !(Maybe Natural)
  , ppCollateralPercentage :: !(Maybe Natural)
  , ppMaxCollateralInputs :: !(Maybe Natural)
  }

epochProtoParams :: ExtLedgerState CardanoBlock -> Maybe ProtoParams
epochProtoParams lstate =
    case ledgerState lstate of
      LedgerStateByron _ -> Nothing
      LedgerStateShelley sls -> Just $ shelleyProtoParams sls
      LedgerStateAllegra als -> Just $ allegraProtoParams als
      LedgerStateMary mls -> Just $ maryProtoParams mls
      LedgerStateAlonzo als -> Just $ alonzoProtoParams als

allegraProtoParams :: LedgerState (ShelleyBlock StandardAllegra) -> ProtoParams
allegraProtoParams =
  fromShelleyParams . Shelley.esPp . Shelley.nesEs . Consensus.shelleyLedgerState

alonzoProtoParams :: LedgerState (ShelleyBlock StandardAlonzo) -> ProtoParams
alonzoProtoParams =
  fromAlonzoParams . Shelley.esPp . Shelley.nesEs . Consensus.shelleyLedgerState

maryProtoParams :: LedgerState (ShelleyBlock StandardMary) -> ProtoParams
maryProtoParams =
  fromShelleyParams . Shelley.esPp . Shelley.nesEs . Consensus.shelleyLedgerState

shelleyProtoParams :: LedgerState (ShelleyBlock StandardShelley) -> ProtoParams
shelleyProtoParams =
  fromShelleyParams . Shelley.esPp . Shelley.nesEs . Consensus.shelleyLedgerState

-- -------------------------------------------------------------------------------------------------

fromAlonzoParams :: Alonzo.PParams (Alonzo.AlonzoEra StandardCrypto) -> ProtoParams
fromAlonzoParams params =
  ProtoParams
    { ppMinfeeA = Alonzo._minfeeA params
    , ppMinfeeB = Alonzo._minfeeB params
    , ppMaxBBSize = Alonzo._maxBBSize params
    , ppMaxTxSize = Alonzo._maxTxSize params
    , ppMaxBHSize = Alonzo._maxBHSize params
    , ppKeyDeposit = Alonzo._keyDeposit params
    , ppPoolDeposit = Alonzo._poolDeposit params
    , ppMaxEpoch = Alonzo._eMax params
    , ppOptialPoolCount = Alonzo._nOpt params
    , ppInfluence = Ledger.unboundRational $ Alonzo._a0 params
    , ppMonetaryExpandRate = Alonzo._rho params
    , ppTreasuryGrowthRate = Alonzo._tau params
    , ppDecentralisation  = Alonzo._d params
    , ppExtraEntropy = Alonzo._extraEntropy params
    , ppProtocolVersion = Alonzo._protocolVersion params
    , ppMinUTxOValue = Alonzo._coinsPerUTxOWord params
    , ppMinPoolCost = Alonzo._minPoolCost params
    , ppCoinsPerUtxoWord = Just $ Alonzo._coinsPerUTxOWord params
    , ppCostmdls = Just $ Alonzo._costmdls params
    , ppPriceMem = Just . Ledger.unboundRational $ Alonzo.prMem (Alonzo._prices params)
    , ppPriceStep = Just . Ledger.unboundRational $ Alonzo.prSteps (Alonzo._prices params)
    , ppMaxTxExMem = Just $ Alonzo.exUnitsMem (Alonzo._maxTxExUnits params)
    , ppMaxTxExSteps = Just $ Alonzo.exUnitsSteps (Alonzo._maxTxExUnits params)
    , ppMaxBlockExMem = Just $ Alonzo.exUnitsMem (Alonzo._maxBlockExUnits params)
    , ppMaxBlockExSteps = Just $ Alonzo.exUnitsSteps (Alonzo._maxBlockExUnits params)
    , ppMaxValSize = Just $ Alonzo._maxValSize params
    , ppCollateralPercentage = Just $ Alonzo._collateralPercentage params
    , ppMaxCollateralInputs = Just $ Alonzo._maxCollateralInputs params
    }

fromShelleyParams :: Shelley.PParams' Identity era -> ProtoParams
fromShelleyParams params =
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
    , ppInfluence = Ledger.unboundRational $ Shelley._a0 params
    , ppMonetaryExpandRate = Shelley._rho params
    , ppTreasuryGrowthRate = Shelley._tau params
    , ppDecentralisation  = Shelley._d params
    , ppExtraEntropy = Shelley._extraEntropy params
    , ppProtocolVersion = Shelley._protocolVersion params
    , ppMinUTxOValue = Shelley._minUTxOValue params
    , ppMinPoolCost = Shelley._minPoolCost params
    , ppCoinsPerUtxoWord = Nothing
    , ppCostmdls = Nothing
    , ppPriceMem = Nothing
    , ppPriceStep = Nothing
    , ppMaxTxExMem = Nothing
    , ppMaxTxExSteps = Nothing
    , ppMaxBlockExMem = Nothing
    , ppMaxBlockExSteps = Nothing
    , ppMaxValSize = Nothing
    , ppCollateralPercentage = Nothing
    , ppMaxCollateralInputs = Nothing
    }
