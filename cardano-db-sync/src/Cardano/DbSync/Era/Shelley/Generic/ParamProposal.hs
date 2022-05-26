{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Era.Shelley.Generic.ParamProposal
  ( ParamProposal (..)
  , convertParamProposal
  ) where

import           Cardano.Prelude

import           Cardano.DbSync.Era.Shelley.Generic.Util (unKeyHashRaw)
import           Cardano.DbSync.Era.Shelley.Generic.Witness (Witness (..))

import qualified Cardano.Ledger.Alonzo as Alonzo
import           Cardano.Ledger.Alonzo.Language (Language)
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import           Cardano.Ledger.Babbage (BabbageEra)
import qualified Cardano.Ledger.Babbage.PParams as Babbage
import           Cardano.Ledger.BaseTypes (UnitInterval, strictMaybeToMaybe)
import qualified Cardano.Ledger.BaseTypes as Ledger
import           Cardano.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Keys as Ledger
import           Cardano.Ledger.Shelley (ShelleyEra)
import qualified Cardano.Ledger.Shelley.PParams as Shelley
import qualified Cardano.Ledger.ShelleyMA as ShelleyMA

import           Cardano.Slotting.Slot (EpochNo (..))

import qualified Data.Map.Strict as Map

data ParamProposal = ParamProposal
  { pppEpochNo :: !EpochNo
  , pppKey :: !ByteString
  , pppMinFeeA :: !(Maybe Natural)
  , pppMinFeeB :: !(Maybe Natural)
  , pppMaxBlockSize :: !(Maybe Natural)
  , pppMaxTxSize :: !(Maybe Natural)
  , pppMaxBhSize :: !(Maybe Natural)
  , pppKeyDeposit :: !(Maybe Coin)
  , pppPoolDeposit :: !(Maybe Coin)
  , pppMaxEpoch :: !(Maybe EpochNo)
  , pppOptimalPoolCount :: !(Maybe Natural)
  , pppInfluence :: !(Maybe Rational)
  , pppMonetaryExpandRate :: !(Maybe UnitInterval)
  , pppTreasuryGrowthRate :: !(Maybe UnitInterval)
  , pppDecentralisation :: !(Maybe UnitInterval)
  , pppEntropy :: !(Maybe Ledger.Nonce)
  , pppProtocolVersion :: !(Maybe Ledger.ProtVer)
  , pppMinUtxoValue :: !(Maybe Coin)
  , pppMinPoolCost :: !(Maybe Coin)

  -- New for Alonzo.
  , pppCoinsPerUtxo :: !(Maybe Coin)
  , pppCostmdls :: !(Maybe (Map Language Alonzo.CostModel))
  , pppPriceMem :: !(Maybe Rational)
  , pppPriceStep :: !(Maybe Rational)
  , pppMaxTxExMem :: !(Maybe Word64)
  , pppMaxTxExSteps :: !(Maybe Word64)
  , pppMaxBlockExMem :: !(Maybe Word64)
  , pppMaxBlockExSteps :: !(Maybe Word64)
  , pppMaxValSize :: !(Maybe Natural)
  , pppCollateralPercentage :: !(Maybe Natural)
  , pppMaxCollateralInputs :: !(Maybe Natural)
  }

convertParamProposal :: Witness era -> Shelley.Update era -> [ParamProposal]
convertParamProposal witness (Shelley.Update pp epoch) =
  case witness of
    Shelley {} -> shelleyParamProposal epoch pp
    Allegra {} -> allegraOrMaryParamProposal epoch pp
    Mary {} -> allegraOrMaryParamProposal epoch pp
    Alonzo {} -> alonzoParamProposal epoch pp
    Babbage {} -> babbageParamProposal epoch pp

-- -------------------------------------------------------------------------------------------------

allegraOrMaryParamProposal :: EpochNo -> Shelley.ProposedPPUpdates (ShelleyMA.ShelleyMAEra a c) -> [ParamProposal]
allegraOrMaryParamProposal epochNo (Shelley.ProposedPPUpdates umap) =
    map (convertShelleyParamProposal epochNo) $ Map.toList umap

alonzoParamProposal :: EpochNo -> Shelley.ProposedPPUpdates (Alonzo.AlonzoEra c) -> [ParamProposal]
alonzoParamProposal epochNo (Shelley.ProposedPPUpdates umap) =
    map (convertAlonzoParamProposal epochNo) $ Map.toList umap

shelleyParamProposal :: EpochNo -> Shelley.ProposedPPUpdates (ShelleyEra c) -> [ParamProposal]
shelleyParamProposal epochNo (Shelley.ProposedPPUpdates umap) =
    map (convertShelleyParamProposal epochNo) $ Map.toList umap

babbageParamProposal :: EpochNo -> Shelley.ProposedPPUpdates (BabbageEra c) -> [ParamProposal]
babbageParamProposal epochNo (Shelley.ProposedPPUpdates umap) =
    map (convertBabbageParamProposal epochNo) $ Map.toList umap

-- -------------------------------------------------------------------------------------------------

convertBabbageParamProposal :: EpochNo -> (Ledger.KeyHash genesis crypto, Babbage.PParamsUpdate era) -> ParamProposal
convertBabbageParamProposal epochNo (key, pmap) =
  ParamProposal
    { pppEpochNo = epochNo
    , pppKey = unKeyHashRaw key
    , pppMinFeeA = strictMaybeToMaybe (Babbage._minfeeA pmap)
    , pppMinFeeB = strictMaybeToMaybe (Babbage._minfeeB pmap)
    , pppMaxBlockSize = strictMaybeToMaybe (Babbage._maxBBSize pmap)
    , pppMaxTxSize = strictMaybeToMaybe (Babbage._maxTxSize pmap)
    , pppMaxBhSize = strictMaybeToMaybe (Babbage._maxBHSize pmap)
    , pppKeyDeposit = strictMaybeToMaybe (Babbage._keyDeposit pmap)
    , pppPoolDeposit = strictMaybeToMaybe (Babbage._poolDeposit pmap)
    , pppMaxEpoch = strictMaybeToMaybe (Babbage._eMax pmap)
    , pppOptimalPoolCount = strictMaybeToMaybe (Babbage._nOpt pmap)
    , pppInfluence = Ledger.unboundRational <$> strictMaybeToMaybe (Babbage._a0 pmap)
    , pppMonetaryExpandRate = strictMaybeToMaybe (Babbage._rho pmap)
    , pppTreasuryGrowthRate = strictMaybeToMaybe (Babbage._tau pmap)
    , pppDecentralisation = Nothing -- Removed in Babbage
    , pppEntropy = Nothing -- Removed in Babbage
    , pppProtocolVersion = strictMaybeToMaybe (Babbage._protocolVersion pmap)
    , pppMinUtxoValue = Nothing -- Removed in Alonzo
    , pppMinPoolCost = strictMaybeToMaybe (Babbage._minPoolCost pmap)
    , pppCoinsPerUtxo = strictMaybeToMaybe (Babbage._coinsPerUTxOByte pmap)
    , pppCostmdls = strictMaybeToMaybe (Alonzo.unCostModels <$> Babbage._costmdls pmap)
    , pppPriceMem = Ledger.unboundRational . Alonzo.prMem <$> strictMaybeToMaybe (Babbage._prices pmap)
    , pppPriceStep = Ledger.unboundRational . Alonzo.prSteps <$> strictMaybeToMaybe (Babbage._prices pmap)
    , pppMaxTxExMem = fromIntegral . Alonzo.exUnitsMem <$> strictMaybeToMaybe (Babbage._maxTxExUnits pmap)
    , pppMaxTxExSteps = fromIntegral . Alonzo.exUnitsSteps <$> strictMaybeToMaybe (Babbage._maxTxExUnits pmap)
    , pppMaxBlockExMem = fromIntegral . Alonzo.exUnitsMem <$> strictMaybeToMaybe (Babbage._maxBlockExUnits pmap)
    , pppMaxBlockExSteps = fromIntegral . Alonzo.exUnitsSteps <$> strictMaybeToMaybe (Babbage._maxBlockExUnits pmap)
    , pppMaxValSize = strictMaybeToMaybe (Babbage._maxValSize pmap)
    , pppCollateralPercentage = strictMaybeToMaybe (Babbage._collateralPercentage pmap)
    , pppMaxCollateralInputs = strictMaybeToMaybe (Babbage._maxCollateralInputs pmap)
    }

convertAlonzoParamProposal :: EpochNo -> (Ledger.KeyHash genesis crypto, Alonzo.PParamsUpdate era) -> ParamProposal
convertAlonzoParamProposal epochNo (key, pmap) =
  ParamProposal
    { pppEpochNo = epochNo
    , pppKey = unKeyHashRaw key
    , pppMinFeeA = strictMaybeToMaybe (Alonzo._minfeeA pmap)
    , pppMinFeeB = strictMaybeToMaybe (Alonzo._minfeeB pmap)
    , pppMaxBlockSize = strictMaybeToMaybe (Alonzo._maxBBSize pmap)
    , pppMaxTxSize = strictMaybeToMaybe (Alonzo._maxTxSize pmap)
    , pppMaxBhSize = strictMaybeToMaybe (Alonzo._maxBHSize pmap)
    , pppKeyDeposit = strictMaybeToMaybe (Alonzo._keyDeposit pmap)
    , pppPoolDeposit = strictMaybeToMaybe (Alonzo._poolDeposit pmap)
    , pppMaxEpoch = strictMaybeToMaybe (Alonzo._eMax pmap)
    , pppOptimalPoolCount = strictMaybeToMaybe (Alonzo._nOpt pmap)
    , pppInfluence = Ledger.unboundRational <$> strictMaybeToMaybe (Alonzo._a0 pmap)
    , pppMonetaryExpandRate = strictMaybeToMaybe (Alonzo._rho pmap)
    , pppTreasuryGrowthRate = strictMaybeToMaybe (Alonzo._tau pmap)
    , pppDecentralisation = strictMaybeToMaybe (Alonzo._d pmap)
    , pppEntropy = strictMaybeToMaybe (Alonzo._extraEntropy pmap)
    , pppProtocolVersion = strictMaybeToMaybe (Alonzo._protocolVersion pmap)
    , pppMinUtxoValue = Nothing -- Removed in Alonzo
    , pppMinPoolCost = strictMaybeToMaybe (Alonzo._minPoolCost pmap)

    -- New for Alonzo.
    , pppCoinsPerUtxo = strictMaybeToMaybe (Alonzo._coinsPerUTxOWord pmap)
    , pppCostmdls = strictMaybeToMaybe (Alonzo.unCostModels <$> Alonzo._costmdls pmap)
    , pppPriceMem = Ledger.unboundRational . Alonzo.prMem <$> strictMaybeToMaybe (Alonzo._prices pmap)
    , pppPriceStep = Ledger.unboundRational . Alonzo.prSteps <$> strictMaybeToMaybe (Alonzo._prices pmap)
    , pppMaxTxExMem = fromIntegral . Alonzo.exUnitsMem <$> strictMaybeToMaybe (Alonzo._maxTxExUnits pmap)
    , pppMaxTxExSteps = fromIntegral . Alonzo.exUnitsSteps <$> strictMaybeToMaybe (Alonzo._maxTxExUnits pmap)
    , pppMaxBlockExMem = fromIntegral . Alonzo.exUnitsMem <$> strictMaybeToMaybe (Alonzo._maxBlockExUnits pmap)
    , pppMaxBlockExSteps = fromIntegral . Alonzo.exUnitsSteps <$> strictMaybeToMaybe (Alonzo._maxBlockExUnits pmap)
    , pppMaxValSize = strictMaybeToMaybe (Alonzo._maxValSize pmap)
    , pppCollateralPercentage = strictMaybeToMaybe (Alonzo._collateralPercentage pmap)
    , pppMaxCollateralInputs = strictMaybeToMaybe (Alonzo._maxCollateralInputs pmap)
    }

convertShelleyParamProposal :: EpochNo -> (Ledger.KeyHash genesis crypto, Shelley.PParams' Ledger.StrictMaybe era) -> ParamProposal
convertShelleyParamProposal epochNo (key, pmap) =
  ParamProposal
    { pppEpochNo = epochNo
    , pppKey = unKeyHashRaw key
    , pppMinFeeA = strictMaybeToMaybe (Shelley._minfeeA pmap)
    , pppMinFeeB = strictMaybeToMaybe (Shelley._minfeeB pmap)
    , pppMaxBlockSize = strictMaybeToMaybe (Shelley._maxBBSize pmap)
    , pppMaxTxSize = strictMaybeToMaybe (Shelley._maxTxSize pmap)
    , pppMaxBhSize = strictMaybeToMaybe (Shelley._maxBHSize pmap)
    , pppKeyDeposit = strictMaybeToMaybe (Shelley._keyDeposit pmap)
    , pppPoolDeposit = strictMaybeToMaybe (Shelley._poolDeposit pmap)
    , pppMaxEpoch = strictMaybeToMaybe (Shelley._eMax pmap)
    , pppOptimalPoolCount = strictMaybeToMaybe (Shelley._nOpt pmap)
    , pppInfluence = Ledger.unboundRational <$> strictMaybeToMaybe (Shelley._a0 pmap)
    , pppMonetaryExpandRate = strictMaybeToMaybe (Shelley._rho pmap)
    , pppTreasuryGrowthRate = strictMaybeToMaybe (Shelley._tau pmap)
    , pppDecentralisation = strictMaybeToMaybe (Shelley._d pmap)
    , pppEntropy = strictMaybeToMaybe (Shelley._extraEntropy pmap)
    , pppProtocolVersion = strictMaybeToMaybe (Shelley._protocolVersion pmap)
    , pppMinUtxoValue = strictMaybeToMaybe (Shelley._minUTxOValue pmap)
    , pppMinPoolCost = strictMaybeToMaybe (Shelley._minPoolCost pmap)

    -- The following are Alonzo related, hence Nothing.
    , pppCoinsPerUtxo = Nothing
    , pppCostmdls = Nothing
    , pppPriceMem = Nothing
    , pppPriceStep = Nothing
    , pppMaxTxExMem = Nothing
    , pppMaxTxExSteps = Nothing
    , pppMaxBlockExMem = Nothing
    , pppMaxBlockExSteps = Nothing
    , pppMaxValSize = Nothing
    , pppCollateralPercentage = Nothing
    , pppMaxCollateralInputs = Nothing
    }
