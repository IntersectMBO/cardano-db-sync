{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Generic.ParamProposal (
  ParamProposal (..),
  convertParamProposal,
  convertConwayParamProposal,
) where

import Cardano.DbSync.Era.Shelley.Generic.Util (unKeyHashRaw)
import Cardano.DbSync.Era.Shelley.Generic.Witness (Witness (..))
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Language (Language)
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import Cardano.Ledger.Babbage.Core (ppuCoinsPerUTxOByteL, unCoinPerByte)
import Cardano.Ledger.BaseTypes (UnitInterval, strictMaybeToMaybe)
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Coin (Coin, unCoin)
import Cardano.Ledger.Crypto
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Shelley.PParams as Shelley
import Cardano.Prelude
import Cardano.Slotting.Slot (EpochNo (..))
import qualified Data.Map.Strict as Map
#if __GLASGOW_HASKELL__ >= 906
import Data.Type.Equality (type (~))
#endif
import Lens.Micro ((^.))
import Ouroboros.Consensus.Cardano.Block (StandardAlonzo, StandardBabbage, StandardConway)

data ParamProposal = ParamProposal
  { pppEpochNo :: !(Maybe EpochNo)
  , pppKey :: !(Maybe ByteString)
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
  }

convertParamProposal :: EraCrypto era ~ StandardCrypto => Witness era -> Shelley.Update era -> [ParamProposal]
convertParamProposal witness (Shelley.Update pp epoch) =
  case witness of
    Shelley {} -> shelleyParamProposal epoch pp
    Allegra {} -> shelleyParamProposal epoch pp
    Mary {} -> shelleyParamProposal epoch pp
    Alonzo {} -> alonzoParamProposal epoch pp
    Babbage {} -> babbageParamProposal epoch pp

-- -------------------------------------------------------------------------------------------------

shelleyParamProposal :: (EraPParams era, ProtVerAtMost era 4, ProtVerAtMost era 6) => EpochNo -> Shelley.ProposedPPUpdates era -> [ParamProposal]
shelleyParamProposal epochNo (Shelley.ProposedPPUpdates umap) =
  map (convertShelleyParamProposal epochNo) $ Map.toList umap

alonzoParamProposal :: EpochNo -> Shelley.ProposedPPUpdates StandardAlonzo -> [ParamProposal]
alonzoParamProposal epochNo (Shelley.ProposedPPUpdates umap) =
  map (convertAlonzoParamProposal epochNo) $ Map.toList umap

babbageParamProposal :: EpochNo -> Shelley.ProposedPPUpdates StandardBabbage -> [ParamProposal]
babbageParamProposal epochNo (Shelley.ProposedPPUpdates umap) =
  map (convertBabbageParamProposal epochNo) $ Map.toList umap

-- -------------------------------------------------------------------------------------------------

convertConwayParamProposal :: PParamsUpdate StandardConway -> ParamProposal
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
    , pppOptimalPoolCount = strictMaybeToMaybe (pmap ^. ppuNOptL)
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
    }

convertBabbageParamProposal :: EpochNo -> (Ledger.KeyHash genesis StandardCrypto, PParamsUpdate StandardBabbage) -> ParamProposal
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
    , pppOptimalPoolCount = strictMaybeToMaybe (pmap ^. ppuNOptL)
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
    }

convertAlonzoParamProposal :: EpochNo -> (Ledger.KeyHash genesis crypto, PParamsUpdate StandardAlonzo) -> ParamProposal
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
    , pppOptimalPoolCount = strictMaybeToMaybe (pmap ^. ppuNOptL)
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
    }

-- | This works fine from Shelley to Mary. Not for Alonzo since 'ppuMinUTxOValueL' was removed
convertShelleyParamProposal :: (EraPParams era, ProtVerAtMost era 4, ProtVerAtMost era 6) => EpochNo -> (Ledger.KeyHash genesis crypto, PParamsUpdate era) -> ParamProposal
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
    , pppOptimalPoolCount = strictMaybeToMaybe (pmap ^. ppuNOptL)
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
    }
