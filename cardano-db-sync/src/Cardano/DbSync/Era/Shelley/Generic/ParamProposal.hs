{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Generic.ParamProposal (
  ParamProposal (..),
  convertParamProposal,
  convertConwayParamProposal,
) where

import Cardano.DbSync.Era.Shelley.Generic.Util (unKeyHashRaw)
import Cardano.DbSync.Era.Shelley.Generic.Witness (Witness (..))
import Cardano.Ledger.Alonzo.Core
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import Cardano.Ledger.BaseTypes (EpochInterval, UnitInterval, strictMaybeToMaybe)
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Coin (Coin, unCoin)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.PParams (ppuMinFeeRefScriptCostPerByteL)
import qualified Cardano.Ledger.Keys as Ledger
import Cardano.Ledger.Plutus.Language (Language)
import qualified Cardano.Ledger.Shelley.PParams as Shelley
import Cardano.Prelude
import Cardano.Slotting.Slot (EpochNo (..))
import qualified Data.Map.Strict as Map
import Lens.Micro ((^.))
import Ouroboros.Consensus.Cardano.Block (AlonzoEra, BabbageEra, ConwayEra)

data ParamProposal = ParamProposal
  { pppEpochNo :: !(Maybe EpochNo)
  , pppKey :: !(Maybe ByteString)
  , pppMinFeeA :: !(Maybe Natural)
  , pppMinFeeB :: !(Maybe Natural)
  , pppMaxBlockSize :: !(Maybe Word32)
  , pppMaxTxSize :: !(Maybe Word32)
  , pppMaxBhSize :: !(Maybe Word16)
  , pppKeyDeposit :: !(Maybe Coin)
  , pppPoolDeposit :: !(Maybe Coin)
  , pppMaxEpoch :: !(Maybe EpochInterval)
  , pppOptimalPoolCount :: !(Maybe Natural)
  , pppInfluence :: !(Maybe Rational)
  , pppMonetaryExpandRate :: !(Maybe UnitInterval)
  , pppTreasuryGrowthRate :: !(Maybe UnitInterval)
  , pppDecentralisation :: !(Maybe UnitInterval)
  , pppEntropy :: !(Maybe Ledger.Nonce)
  , pppProtocolVersion :: !(Maybe Ledger.ProtVer)
  , pppMinUtxoValue :: !(Maybe Coin)
  , pppMinPoolCost :: !(Maybe Coin)
  , -- New for Alonzo.
    pppCoinsPerUtxo :: !(Maybe Coin)
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
  , -- New for Conway.
    pppPoolVotingThresholds :: !(Maybe PoolVotingThresholds)
  , pppDRepVotingThresholds :: !(Maybe DRepVotingThresholds)
  , pppCommitteeMinSize :: !(Maybe Natural)
  , pppCommitteeMaxTermLength :: !(Maybe EpochInterval)
  , pppGovActionLifetime :: !(Maybe EpochInterval)
  , pppGovActionDeposit :: !(Maybe Natural)
  , pppDRepDeposit :: !(Maybe Natural)
  , pppDRepActivity :: !(Maybe EpochInterval)
  , pppMinFeeRefScriptCostPerByte :: !(Maybe Rational)
  }

convertParamProposal :: Witness era -> Shelley.Update era -> [ParamProposal]
convertParamProposal witness (Shelley.Update pp epoch) =
  case witness of
    Shelley {} -> shelleyParamProposal epoch pp
    Allegra {} -> shelleyParamProposal epoch pp
    Mary {} -> shelleyParamProposal epoch pp
    Alonzo {} -> alonzoParamProposal epoch pp
    Babbage {} -> babbageParamProposal epoch pp

-- -------------------------------------------------------------------------------------------------

shelleyParamProposal :: (EraPParams era, ProtVerAtMost era 4, ProtVerAtMost era 6, ProtVerAtMost era 8) => EpochNo -> Shelley.ProposedPPUpdates era -> [ParamProposal]
shelleyParamProposal epochNo (Shelley.ProposedPPUpdates umap) =
  map (convertShelleyParamProposal epochNo) $ Map.toList umap

alonzoParamProposal :: EpochNo -> Shelley.ProposedPPUpdates AlonzoEra -> [ParamProposal]
alonzoParamProposal epochNo (Shelley.ProposedPPUpdates umap) =
  map (convertAlonzoParamProposal epochNo) $ Map.toList umap

babbageParamProposal :: EpochNo -> Shelley.ProposedPPUpdates BabbageEra -> [ParamProposal]
babbageParamProposal epochNo (Shelley.ProposedPPUpdates umap) =
  map (convertBabbageParamProposal epochNo) $ Map.toList umap

-- -------------------------------------------------------------------------------------------------

convertConwayParamProposal :: PParamsUpdate ConwayEra -> ParamProposal
convertConwayParamProposal pmap =
  ParamProposal
    { pppEpochNo = Nothing
    , pppKey = Nothing
    , pppMinFeeA = fromIntegral . unCoin <$> strictMaybeToMaybe (pmap ^. ppuMinFeeAL)
    , pppMinFeeB = fromIntegral . unCoin <$> strictMaybeToMaybe (pmap ^. ppuMinFeeBL)
    , pppMaxBlockSize = strictMaybeToMaybe (pmap ^. ppuMaxBBSizeL)
    , pppMaxTxSize = strictMaybeToMaybe (pmap ^. ppuMaxTxSizeL)
    , pppMaxBhSize = strictMaybeToMaybe (pmap ^. ppuMaxBHSizeL)
    , pppKeyDeposit = strictMaybeToMaybe (pmap ^. ppuKeyDepositL)
    , pppPoolDeposit = strictMaybeToMaybe (pmap ^. ppuPoolDepositL)
    , pppMaxEpoch = strictMaybeToMaybe (pmap ^. ppuEMaxL)
    , pppOptimalPoolCount = fromIntegral <$> strictMaybeToMaybe (pmap ^. ppuNOptL)
    , pppInfluence = Ledger.unboundRational <$> strictMaybeToMaybe (pmap ^. ppuA0L)
    , pppMonetaryExpandRate = strictMaybeToMaybe (pmap ^. ppuRhoL)
    , pppTreasuryGrowthRate = strictMaybeToMaybe (pmap ^. ppuTauL)
    , pppDecentralisation = Nothing -- Removed in Babbage
    , pppEntropy = Nothing -- Removed in Babbage
    , pppProtocolVersion = Nothing -- Removed in Conway
    , pppMinUtxoValue = Nothing -- Removed in Alonzo
    , pppMinPoolCost = strictMaybeToMaybe (pmap ^. ppuMinPoolCostL)
    , pppCoinsPerUtxo = unCoinPerByte <$> strictMaybeToMaybe (pmap ^. ppuCoinsPerUTxOByteL)
    , pppCostmdls = strictMaybeToMaybe (Alonzo.costModelsValid <$> pmap ^. ppuCostModelsL)
    , pppPriceMem = Ledger.unboundRational . Alonzo.prMem <$> strictMaybeToMaybe (pmap ^. ppuPricesL)
    , pppPriceStep = Ledger.unboundRational . Alonzo.prSteps <$> strictMaybeToMaybe (pmap ^. ppuPricesL)
    , pppMaxTxExMem = fromIntegral . Alonzo.exUnitsMem <$> strictMaybeToMaybe (pmap ^. ppuMaxTxExUnitsL)
    , pppMaxTxExSteps = fromIntegral . Alonzo.exUnitsSteps <$> strictMaybeToMaybe (pmap ^. ppuMaxTxExUnitsL)
    , pppMaxBlockExMem = fromIntegral . Alonzo.exUnitsMem <$> strictMaybeToMaybe (pmap ^. ppuMaxBlockExUnitsL)
    , pppMaxBlockExSteps = fromIntegral . Alonzo.exUnitsSteps <$> strictMaybeToMaybe (pmap ^. ppuMaxBlockExUnitsL)
    , pppMaxValSize = strictMaybeToMaybe (pmap ^. ppuMaxValSizeL)
    , pppCollateralPercentage = strictMaybeToMaybe (pmap ^. ppuCollateralPercentageL)
    , pppMaxCollateralInputs = strictMaybeToMaybe (pmap ^. ppuMaxCollateralInputsL)
    , -- New for Conway.
      pppPoolVotingThresholds = strictMaybeToMaybe (pmap ^. ppuPoolVotingThresholdsL)
    , pppDRepVotingThresholds = strictMaybeToMaybe (pmap ^. ppuDRepVotingThresholdsL)
    , pppCommitteeMinSize = strictMaybeToMaybe (pmap ^. ppuCommitteeMinSizeL)
    , pppCommitteeMaxTermLength = strictMaybeToMaybe (pmap ^. ppuCommitteeMaxTermLengthL)
    , pppGovActionLifetime = strictMaybeToMaybe (pmap ^. ppuGovActionLifetimeL)
    , pppGovActionDeposit = fromIntegral . unCoin <$> strictMaybeToMaybe (pmap ^. ppuGovActionDepositL)
    , pppDRepDeposit = fromIntegral . unCoin <$> strictMaybeToMaybe (pmap ^. ppuDRepDepositL)
    , pppDRepActivity = strictMaybeToMaybe (pmap ^. ppuDRepActivityL)
    , pppMinFeeRefScriptCostPerByte = Ledger.unboundRational <$> strictMaybeToMaybe (pmap ^. ppuMinFeeRefScriptCostPerByteL)
    }

convertBabbageParamProposal :: EpochNo -> (Ledger.KeyHash genesis, PParamsUpdate BabbageEra) -> ParamProposal
convertBabbageParamProposal epochNo (key, pmap) =
  ParamProposal
    { pppEpochNo = Just epochNo
    , pppKey = Just $ unKeyHashRaw key
    , pppMinFeeA = fromIntegral . unCoin <$> strictMaybeToMaybe (pmap ^. ppuMinFeeAL)
    , pppMinFeeB = fromIntegral . unCoin <$> strictMaybeToMaybe (pmap ^. ppuMinFeeBL)
    , pppMaxBlockSize = strictMaybeToMaybe (pmap ^. ppuMaxBBSizeL)
    , pppMaxTxSize = strictMaybeToMaybe (pmap ^. ppuMaxTxSizeL)
    , pppMaxBhSize = strictMaybeToMaybe (pmap ^. ppuMaxBHSizeL)
    , pppKeyDeposit = strictMaybeToMaybe (pmap ^. ppuKeyDepositL)
    , pppPoolDeposit = strictMaybeToMaybe (pmap ^. ppuPoolDepositL)
    , pppMaxEpoch = strictMaybeToMaybe (pmap ^. ppuEMaxL)
    , pppOptimalPoolCount = fromIntegral <$> strictMaybeToMaybe (pmap ^. ppuNOptL)
    , pppInfluence = Ledger.unboundRational <$> strictMaybeToMaybe (pmap ^. ppuA0L)
    , pppMonetaryExpandRate = strictMaybeToMaybe (pmap ^. ppuRhoL)
    , pppTreasuryGrowthRate = strictMaybeToMaybe (pmap ^. ppuTauL)
    , pppDecentralisation = Nothing -- Removed in Babbage
    , pppEntropy = Nothing -- Removed in Babbage
    , pppProtocolVersion = strictMaybeToMaybe (pmap ^. ppuProtocolVersionL)
    , pppMinUtxoValue = Nothing -- Removed in Alonzo
    , pppMinPoolCost = strictMaybeToMaybe (pmap ^. ppuMinPoolCostL)
    , pppCoinsPerUtxo = unCoinPerByte <$> strictMaybeToMaybe (pmap ^. ppuCoinsPerUTxOByteL)
    , pppCostmdls = strictMaybeToMaybe (Alonzo.costModelsValid <$> pmap ^. ppuCostModelsL)
    , pppPriceMem = Ledger.unboundRational . Alonzo.prMem <$> strictMaybeToMaybe (pmap ^. ppuPricesL)
    , pppPriceStep = Ledger.unboundRational . Alonzo.prSteps <$> strictMaybeToMaybe (pmap ^. ppuPricesL)
    , pppMaxTxExMem = fromIntegral . Alonzo.exUnitsMem <$> strictMaybeToMaybe (pmap ^. ppuMaxTxExUnitsL)
    , pppMaxTxExSteps = fromIntegral . Alonzo.exUnitsSteps <$> strictMaybeToMaybe (pmap ^. ppuMaxTxExUnitsL)
    , pppMaxBlockExMem = fromIntegral . Alonzo.exUnitsMem <$> strictMaybeToMaybe (pmap ^. ppuMaxBlockExUnitsL)
    , pppMaxBlockExSteps = fromIntegral . Alonzo.exUnitsSteps <$> strictMaybeToMaybe (pmap ^. ppuMaxBlockExUnitsL)
    , pppMaxValSize = strictMaybeToMaybe (pmap ^. ppuMaxValSizeL)
    , pppCollateralPercentage = strictMaybeToMaybe (pmap ^. ppuCollateralPercentageL)
    , pppMaxCollateralInputs = strictMaybeToMaybe (pmap ^. ppuMaxCollateralInputsL)
    , pppPoolVotingThresholds = Nothing
    , pppDRepVotingThresholds = Nothing
    , pppCommitteeMinSize = Nothing
    , pppCommitteeMaxTermLength = Nothing
    , pppGovActionLifetime = Nothing
    , pppGovActionDeposit = Nothing
    , pppDRepDeposit = Nothing
    , pppDRepActivity = Nothing
    , pppMinFeeRefScriptCostPerByte = Nothing
    }

convertAlonzoParamProposal :: EpochNo -> (Ledger.KeyHash genesis, PParamsUpdate AlonzoEra) -> ParamProposal
convertAlonzoParamProposal epochNo (key, pmap) =
  ParamProposal
    { pppEpochNo = Just epochNo
    , pppKey = Just $ unKeyHashRaw key
    , pppMinFeeA = fromIntegral . unCoin <$> strictMaybeToMaybe (pmap ^. ppuMinFeeAL)
    , pppMinFeeB = fromIntegral . unCoin <$> strictMaybeToMaybe (pmap ^. ppuMinFeeBL)
    , pppMaxBlockSize = strictMaybeToMaybe (pmap ^. ppuMaxBBSizeL)
    , pppMaxTxSize = strictMaybeToMaybe (pmap ^. ppuMaxTxSizeL)
    , pppMaxBhSize = strictMaybeToMaybe (pmap ^. ppuMaxBHSizeL)
    , pppKeyDeposit = strictMaybeToMaybe (pmap ^. ppuKeyDepositL)
    , pppPoolDeposit = strictMaybeToMaybe (pmap ^. ppuPoolDepositL)
    , pppMaxEpoch = strictMaybeToMaybe (pmap ^. ppuEMaxL)
    , pppOptimalPoolCount = fromIntegral <$> strictMaybeToMaybe (pmap ^. ppuNOptL)
    , pppInfluence = Ledger.unboundRational <$> strictMaybeToMaybe (pmap ^. ppuA0L)
    , pppMonetaryExpandRate = strictMaybeToMaybe (pmap ^. ppuRhoL)
    , pppTreasuryGrowthRate = strictMaybeToMaybe (pmap ^. ppuTauL)
    , pppDecentralisation = strictMaybeToMaybe (pmap ^. ppuDL)
    , pppEntropy = strictMaybeToMaybe (pmap ^. ppuExtraEntropyL)
    , pppProtocolVersion = strictMaybeToMaybe (pmap ^. ppuProtocolVersionL)
    , pppMinUtxoValue = Nothing -- Removed in Alonzo
    , pppMinPoolCost = strictMaybeToMaybe (pmap ^. ppuMinPoolCostL)
    , -- New for Alonzo.
      pppCoinsPerUtxo = unCoinPerWord <$> strictMaybeToMaybe (pmap ^. ppuCoinsPerUTxOWordL)
    , pppCostmdls = strictMaybeToMaybe (Alonzo.costModelsValid <$> pmap ^. ppuCostModelsL)
    , pppPriceMem = Ledger.unboundRational . Alonzo.prMem <$> strictMaybeToMaybe (pmap ^. ppuPricesL)
    , pppPriceStep = Ledger.unboundRational . Alonzo.prSteps <$> strictMaybeToMaybe (pmap ^. ppuPricesL)
    , pppMaxTxExMem = fromIntegral . Alonzo.exUnitsMem <$> strictMaybeToMaybe (pmap ^. ppuMaxTxExUnitsL)
    , pppMaxTxExSteps = fromIntegral . Alonzo.exUnitsSteps <$> strictMaybeToMaybe (pmap ^. ppuMaxTxExUnitsL)
    , pppMaxBlockExMem = fromIntegral . Alonzo.exUnitsMem <$> strictMaybeToMaybe (pmap ^. ppuMaxBlockExUnitsL)
    , pppMaxBlockExSteps = fromIntegral . Alonzo.exUnitsSteps <$> strictMaybeToMaybe (pmap ^. ppuMaxBlockExUnitsL)
    , pppMaxValSize = strictMaybeToMaybe (pmap ^. ppuMaxValSizeL)
    , pppCollateralPercentage = strictMaybeToMaybe (pmap ^. ppuCollateralPercentageL)
    , pppMaxCollateralInputs = strictMaybeToMaybe (pmap ^. ppuMaxCollateralInputsL)
    , pppPoolVotingThresholds = Nothing
    , pppDRepVotingThresholds = Nothing
    , pppCommitteeMinSize = Nothing
    , pppCommitteeMaxTermLength = Nothing
    , pppGovActionLifetime = Nothing
    , pppGovActionDeposit = Nothing
    , pppDRepDeposit = Nothing
    , pppDRepActivity = Nothing
    , pppMinFeeRefScriptCostPerByte = Nothing
    }

-- | This works fine from Shelley to Mary. Not for Alonzo since 'ppuMinUTxOValueL' was removed
convertShelleyParamProposal :: (EraPParams era, ProtVerAtMost era 4, ProtVerAtMost era 6, ProtVerAtMost era 8) => EpochNo -> (Ledger.KeyHash genesis, PParamsUpdate era) -> ParamProposal
convertShelleyParamProposal epochNo (key, pmap) =
  ParamProposal
    { pppEpochNo = Just epochNo
    , pppKey = Just $ unKeyHashRaw key
    , pppMinFeeA = fromIntegral . unCoin <$> strictMaybeToMaybe (pmap ^. ppuMinFeeAL)
    , pppMinFeeB = fromIntegral . unCoin <$> strictMaybeToMaybe (pmap ^. ppuMinFeeBL)
    , pppMaxBlockSize = strictMaybeToMaybe (pmap ^. ppuMaxBBSizeL)
    , pppMaxTxSize = strictMaybeToMaybe (pmap ^. ppuMaxTxSizeL)
    , pppMaxBhSize = strictMaybeToMaybe (pmap ^. ppuMaxBHSizeL)
    , pppKeyDeposit = strictMaybeToMaybe (pmap ^. ppuKeyDepositL)
    , pppPoolDeposit = strictMaybeToMaybe (pmap ^. ppuPoolDepositL)
    , pppMaxEpoch = strictMaybeToMaybe (pmap ^. ppuEMaxL)
    , pppOptimalPoolCount = fromIntegral <$> strictMaybeToMaybe (pmap ^. ppuNOptL)
    , pppInfluence = Ledger.unboundRational <$> strictMaybeToMaybe (pmap ^. ppuA0L)
    , pppMonetaryExpandRate = strictMaybeToMaybe (pmap ^. ppuRhoL)
    , pppTreasuryGrowthRate = strictMaybeToMaybe (pmap ^. ppuTauL)
    , pppDecentralisation = strictMaybeToMaybe (pmap ^. ppuDL)
    , pppEntropy = strictMaybeToMaybe (pmap ^. ppuExtraEntropyL)
    , pppProtocolVersion = strictMaybeToMaybe (pmap ^. ppuProtocolVersionL)
    , pppMinUtxoValue = strictMaybeToMaybe (pmap ^. ppuMinUTxOValueL)
    , pppMinPoolCost = strictMaybeToMaybe (pmap ^. ppuMinPoolCostL)
    , -- The following are Alonzo related, hence Nothing.
      pppCoinsPerUtxo = Nothing
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
    , pppPoolVotingThresholds = Nothing
    , pppDRepVotingThresholds = Nothing
    , pppCommitteeMinSize = Nothing
    , pppCommitteeMaxTermLength = Nothing
    , pppGovActionLifetime = Nothing
    , pppGovActionDeposit = Nothing
    , pppDRepDeposit = Nothing
    , pppDRepActivity = Nothing
    , pppMinFeeRefScriptCostPerByte = Nothing
    }
