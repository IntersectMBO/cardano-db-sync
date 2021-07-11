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
import           Cardano.Ledger.BaseTypes (UnitInterval, strictMaybeToMaybe)
import qualified Cardano.Ledger.BaseTypes as Ledger
import           Cardano.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Keys as Ledger
import           Cardano.Ledger.Shelley (ShelleyEra)
import qualified Cardano.Ledger.ShelleyMA as ShelleyMA

import           Cardano.Slotting.Slot (EpochNo (..))

import qualified Data.Map.Strict as Map

import qualified Shelley.Spec.Ledger.PParams as Shelley

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
  , pppProtocolVersion :: !(Maybe Shelley.ProtVer)
  , pppMinUtxoValue :: !(Maybe Coin)
  , pppMinPoolCost :: !(Maybe Coin)

  -- New for Alonzo.
  , pppCoinsPerUtxoWord :: !(Maybe Coin)
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

-- -------------------------------------------------------------------------------------------------

convertAlonzoParamProposal :: EpochNo -> (Ledger.KeyHash genesis crypto, Alonzo.PParamsUpdate era) -> ParamProposal
convertAlonzoParamProposal  epochNo (key, pmap) =
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
    , pppCoinsPerUtxoWord = strictMaybeToMaybe (Alonzo._coinsPerUTxOWord pmap)
    , pppCostmdls = strictMaybeToMaybe (Alonzo._costmdls pmap)
    , pppPriceMem = Ledger.unboundRational . Alonzo.prMem <$> strictMaybeToMaybe (Alonzo._prices pmap)
    , pppPriceStep = Ledger.unboundRational . Alonzo.prSteps <$> strictMaybeToMaybe (Alonzo._prices pmap)
    , pppMaxTxExMem = Alonzo.exUnitsMem <$> strictMaybeToMaybe (Alonzo._maxTxExUnits pmap)
    , pppMaxTxExSteps = Alonzo.exUnitsSteps <$> strictMaybeToMaybe (Alonzo._maxTxExUnits pmap)
    , pppMaxBlockExMem = Alonzo.exUnitsMem <$> strictMaybeToMaybe (Alonzo._maxBlockExUnits pmap)
    , pppMaxBlockExSteps = Alonzo.exUnitsSteps <$> strictMaybeToMaybe (Alonzo._maxBlockExUnits pmap)
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
    , pppCoinsPerUtxoWord = Nothing
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
