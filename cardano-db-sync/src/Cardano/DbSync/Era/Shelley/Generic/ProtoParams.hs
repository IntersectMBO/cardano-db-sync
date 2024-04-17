{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Generic.ProtoParams (
  ProtoParams (..),
  Deposits (..),
  epochProtoParams,
  getDeposits,
) where

import Cardano.DbSync.Types
import Cardano.Ledger.Alonzo.Core
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import Cardano.Ledger.BaseTypes (EpochInterval, UnitInterval)
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.PParams (ppMinFeeRefScriptCostPerByteL)
import Cardano.Ledger.Plutus.Language (Language)
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Cardano.Prelude
import Lens.Micro ((^.))
import Ouroboros.Consensus.Cardano (Nonce (..))
import Ouroboros.Consensus.Cardano.Block hiding (CardanoBlock)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus

data ProtoParams = ProtoParams
  { ppMinfeeA :: !Natural
  , ppMinfeeB :: !Natural
  , ppMaxBBSize :: !Word32
  , ppMaxTxSize :: !Word32
  , ppMaxBHSize :: !Word16
  , ppKeyDeposit :: !Coin
  , ppPoolDeposit :: !Coin
  , ppMaxEpoch :: !EpochInterval
  , ppOptialPoolCount :: !Natural
  , ppInfluence :: !Rational
  , ppMonetaryExpandRate :: !UnitInterval
  , ppTreasuryGrowthRate :: !UnitInterval
  , ppDecentralisation :: !UnitInterval
  , ppExtraEntropy :: !Nonce
  , ppProtocolVersion :: !Ledger.ProtVer
  , ppMinUTxOValue :: !Coin
  , ppMinPoolCost :: !Coin
  , -- New for Alonzo.
    ppCoinsPerUtxo :: !(Maybe Coin)
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
  , -- New for Conway.
    ppPoolVotingThresholds :: !(Maybe PoolVotingThresholds)
  , ppDRepVotingThresholds :: !(Maybe DRepVotingThresholds)
  , ppCommitteeMinSize :: !(Maybe Natural)
  , ppCommitteeMaxTermLength :: !(Maybe EpochInterval)
  , ppGovActionLifetime :: !(Maybe EpochInterval)
  , ppGovActionDeposit :: !(Maybe Natural)
  , ppDRepDeposit :: !(Maybe Natural)
  , ppDRepActivity :: !(Maybe EpochInterval)
  , ppMinFeeRefScriptCostPerByte :: !(Maybe Rational)
  }

data Deposits = Deposits
  { stakeKeyDeposit :: Coin
  , poolDeposit :: Coin
  }

epochProtoParams :: ExtLedgerState CardanoBlock -> Maybe ProtoParams
epochProtoParams lstate =
  case ledgerState lstate of
    LedgerStateByron _ -> Nothing
    LedgerStateShelley st -> Just $ fromShelleyParams $ getProtoParams st
    LedgerStateAllegra st -> Just $ fromShelleyParams $ getProtoParams st
    LedgerStateMary st -> Just $ fromShelleyParams $ getProtoParams st
    LedgerStateAlonzo st -> Just $ fromAlonzoParams $ getProtoParams st
    LedgerStateBabbage st -> Just $ fromBabbageParams $ getProtoParams st
    LedgerStateConway st -> Just $ fromConwayParams $ getProtoParams st

getProtoParams ::
  EraGov era =>
  LedgerState (ShelleyBlock p era) ->
  PParams era
getProtoParams st = Shelley.nesEs (Consensus.shelleyLedgerState st) ^. Shelley.curPParamsEpochStateL

getDeposits :: ExtLedgerState CardanoBlock -> Maybe Deposits
getDeposits lstate =
  case ledgerState lstate of
    LedgerStateByron _ -> Nothing
    LedgerStateShelley st -> Just $ getDopositsShelley $ getProtoParams st
    LedgerStateAllegra st -> Just $ getDopositsShelley $ getProtoParams st
    LedgerStateMary st -> Just $ getDopositsShelley $ getProtoParams st
    LedgerStateAlonzo st -> Just $ getDopositsShelley $ getProtoParams st
    LedgerStateBabbage st -> Just $ getDopositsShelley $ getProtoParams st
    LedgerStateConway st -> Just $ getDopositsShelley $ getProtoParams st
  where
    getDopositsShelley :: EraPParams era => PParams era -> Deposits
    getDopositsShelley pp =
      Deposits
        { stakeKeyDeposit = pp ^. ppKeyDepositL
        , poolDeposit = pp ^. ppPoolDepositL
        }

-- -------------------------------------------------------------------------------------------------

fromConwayParams :: PParams StandardConway -> ProtoParams
fromConwayParams params =
  ProtoParams
    { ppMinfeeA = fromIntegral . unCoin $ params ^. ppMinFeeAL
    , ppMinfeeB = fromIntegral . unCoin $ params ^. ppMinFeeBL
    , ppMaxBBSize = params ^. ppMaxBBSizeL
    , ppMaxTxSize = params ^. ppMaxTxSizeL
    , ppMaxBHSize = params ^. ppMaxBHSizeL
    , ppKeyDeposit = params ^. ppKeyDepositL
    , ppPoolDeposit = params ^. ppPoolDepositL
    , ppMaxEpoch = params ^. ppEMaxL
    , ppOptialPoolCount = params ^. ppNOptL
    , ppInfluence = Ledger.unboundRational $ params ^. ppA0L
    , ppMonetaryExpandRate = params ^. ppRhoL
    , ppTreasuryGrowthRate = params ^. ppTauL
    , ppDecentralisation = minBound -- can't change in Babbage
    , ppExtraEntropy = NeutralNonce -- no extra entropy in Babbage
    , ppProtocolVersion = params ^. ppProtocolVersionL
    , ppMinUTxOValue = Coin 0
    , ppMinPoolCost = params ^. ppMinPoolCostL
    , ppCoinsPerUtxo = Just $ unCoinPerByte (params ^. ppCoinsPerUTxOByteL)
    , ppCostmdls = Just $ Alonzo.costModelsValid $ params ^. ppCostModelsL
    , ppPriceMem = Just . Ledger.unboundRational $ Alonzo.prMem (params ^. ppPricesL)
    , ppPriceStep = Just . Ledger.unboundRational $ Alonzo.prSteps (params ^. ppPricesL)
    , ppMaxTxExMem = Just . fromIntegral $ Alonzo.exUnitsMem (params ^. ppMaxTxExUnitsL)
    , ppMaxTxExSteps = Just . fromIntegral $ Alonzo.exUnitsSteps (params ^. ppMaxTxExUnitsL)
    , ppMaxBlockExMem = Just . fromIntegral $ Alonzo.exUnitsMem (params ^. ppMaxBlockExUnitsL)
    , ppMaxBlockExSteps = Just . fromIntegral $ Alonzo.exUnitsSteps (params ^. ppMaxBlockExUnitsL)
    , ppMaxValSize = Just $ params ^. ppMaxValSizeL
    , ppCollateralPercentage = Just $ params ^. ppCollateralPercentageL
    , ppMaxCollateralInputs = Just $ params ^. ppMaxCollateralInputsL
    , ppPoolVotingThresholds = Just $ params ^. ppPoolVotingThresholdsL
    , ppDRepVotingThresholds = Just $ params ^. ppDRepVotingThresholdsL
    , ppCommitteeMinSize = Just $ params ^. ppCommitteeMinSizeL
    , ppCommitteeMaxTermLength = Just $ params ^. ppCommitteeMaxTermLengthL
    , ppGovActionLifetime = Just $ params ^. ppGovActionLifetimeL
    , ppGovActionDeposit = Just . fromIntegral . unCoin $ params ^. ppGovActionDepositL
    , ppDRepDeposit = Just . fromIntegral . unCoin $ params ^. ppDRepDepositL
    , ppDRepActivity = Just $ params ^. ppDRepActivityL
    , ppMinFeeRefScriptCostPerByte = Just $ Ledger.unboundRational $ params ^. ppMinFeeRefScriptCostPerByteL
    }

fromBabbageParams :: PParams StandardBabbage -> ProtoParams
fromBabbageParams params =
  ProtoParams
    { ppMinfeeA = fromIntegral . unCoin $ params ^. ppMinFeeAL
    , ppMinfeeB = fromIntegral . unCoin $ params ^. ppMinFeeBL
    , ppMaxBBSize = params ^. ppMaxBBSizeL
    , ppMaxTxSize = params ^. ppMaxTxSizeL
    , ppMaxBHSize = params ^. ppMaxBHSizeL
    , ppKeyDeposit = params ^. ppKeyDepositL
    , ppPoolDeposit = params ^. ppPoolDepositL
    , ppMaxEpoch = params ^. ppEMaxL
    , ppOptialPoolCount = params ^. ppNOptL
    , ppInfluence = Ledger.unboundRational $ params ^. ppA0L
    , ppMonetaryExpandRate = params ^. ppRhoL
    , ppTreasuryGrowthRate = params ^. ppTauL
    , ppDecentralisation = minBound -- can't change in Babbage
    , ppExtraEntropy = NeutralNonce -- no extra entropy in Babbage
    , ppProtocolVersion = params ^. ppProtocolVersionL
    , ppMinUTxOValue = Coin 0
    , ppMinPoolCost = params ^. ppMinPoolCostL
    , ppCoinsPerUtxo = Just $ unCoinPerByte (params ^. ppCoinsPerUTxOByteL)
    , ppCostmdls = Just $ Alonzo.costModelsValid $ params ^. ppCostModelsL
    , ppPriceMem = Just . Ledger.unboundRational $ Alonzo.prMem (params ^. ppPricesL)
    , ppPriceStep = Just . Ledger.unboundRational $ Alonzo.prSteps (params ^. ppPricesL)
    , ppMaxTxExMem = Just . fromIntegral $ Alonzo.exUnitsMem (params ^. ppMaxTxExUnitsL)
    , ppMaxTxExSteps = Just . fromIntegral $ Alonzo.exUnitsSteps (params ^. ppMaxTxExUnitsL)
    , ppMaxBlockExMem = Just . fromIntegral $ Alonzo.exUnitsMem (params ^. ppMaxBlockExUnitsL)
    , ppMaxBlockExSteps = Just . fromIntegral $ Alonzo.exUnitsSteps (params ^. ppMaxBlockExUnitsL)
    , ppMaxValSize = Just $ params ^. ppMaxValSizeL
    , ppCollateralPercentage = Just $ params ^. ppCollateralPercentageL
    , ppMaxCollateralInputs = Just $ params ^. ppMaxCollateralInputsL
    , ppPoolVotingThresholds = Nothing
    , ppDRepVotingThresholds = Nothing
    , ppCommitteeMinSize = Nothing
    , ppCommitteeMaxTermLength = Nothing
    , ppGovActionLifetime = Nothing
    , ppGovActionDeposit = Nothing
    , ppDRepDeposit = Nothing
    , ppDRepActivity = Nothing
    , ppMinFeeRefScriptCostPerByte = Nothing
    }

fromAlonzoParams :: PParams StandardAlonzo -> ProtoParams
fromAlonzoParams params =
  ProtoParams
    { ppMinfeeA = fromIntegral . unCoin $ params ^. ppMinFeeAL
    , ppMinfeeB = fromIntegral . unCoin $ params ^. ppMinFeeBL
    , ppMaxBBSize = params ^. ppMaxBBSizeL
    , ppMaxTxSize = params ^. ppMaxTxSizeL
    , ppMaxBHSize = params ^. ppMaxBHSizeL
    , ppKeyDeposit = params ^. ppKeyDepositL
    , ppPoolDeposit = params ^. ppPoolDepositL
    , ppMaxEpoch = params ^. ppEMaxL
    , ppOptialPoolCount = params ^. ppNOptL
    , ppInfluence = Ledger.unboundRational $ params ^. ppA0L
    , ppMonetaryExpandRate = params ^. ppRhoL
    , ppTreasuryGrowthRate = params ^. ppTauL
    , ppDecentralisation = params ^. ppDL
    , ppExtraEntropy = params ^. ppExtraEntropyL
    , ppProtocolVersion = params ^. ppProtocolVersionL
    , ppMinUTxOValue = Coin 0
    , ppMinPoolCost = params ^. ppMinPoolCostL
    , ppCoinsPerUtxo = Just $ unCoinPerWord (params ^. ppCoinsPerUTxOWordL)
    , ppCostmdls = Just $ Alonzo.costModelsValid $ params ^. ppCostModelsL
    , ppPriceMem = Just . Ledger.unboundRational $ Alonzo.prMem (params ^. ppPricesL)
    , ppPriceStep = Just . Ledger.unboundRational $ Alonzo.prSteps (params ^. ppPricesL)
    , ppMaxTxExMem = Just . fromIntegral $ Alonzo.exUnitsMem (params ^. ppMaxTxExUnitsL)
    , ppMaxTxExSteps = Just . fromIntegral $ Alonzo.exUnitsSteps (params ^. ppMaxTxExUnitsL)
    , ppMaxBlockExMem = Just . fromIntegral $ Alonzo.exUnitsMem (params ^. ppMaxBlockExUnitsL)
    , ppMaxBlockExSteps = Just . fromIntegral $ Alonzo.exUnitsSteps (params ^. ppMaxBlockExUnitsL)
    , ppMaxValSize = Just $ params ^. ppMaxValSizeL
    , ppCollateralPercentage = Just $ params ^. ppCollateralPercentageL
    , ppMaxCollateralInputs = Just $ params ^. ppMaxCollateralInputsL
    , ppPoolVotingThresholds = Nothing
    , ppDRepVotingThresholds = Nothing
    , ppCommitteeMinSize = Nothing
    , ppCommitteeMaxTermLength = Nothing
    , ppGovActionLifetime = Nothing
    , ppGovActionDeposit = Nothing
    , ppDRepDeposit = Nothing
    , ppDRepActivity = Nothing
    , ppMinFeeRefScriptCostPerByte = Nothing
    }

fromShelleyParams :: (ProtVerAtMost era 6, ProtVerAtMost era 4, EraPParams era) => PParams era -> ProtoParams
fromShelleyParams params =
  ProtoParams
    { ppMinfeeA = fromIntegral . unCoin $ params ^. ppMinFeeAL
    , ppMinfeeB = fromIntegral . unCoin $ params ^. ppMinFeeBL
    , ppMaxBBSize = params ^. ppMaxBBSizeL
    , ppMaxTxSize = params ^. ppMaxTxSizeL
    , ppMaxBHSize = params ^. ppMaxBHSizeL
    , ppKeyDeposit = params ^. ppKeyDepositL
    , ppPoolDeposit = params ^. ppPoolDepositL
    , ppMaxEpoch = params ^. ppEMaxL
    , ppOptialPoolCount = params ^. ppNOptL
    , ppInfluence = Ledger.unboundRational $ params ^. ppA0L
    , ppMonetaryExpandRate = params ^. ppRhoL
    , ppTreasuryGrowthRate = params ^. ppTauL
    , ppDecentralisation = params ^. ppDL
    , ppExtraEntropy = params ^. ppExtraEntropyL
    , ppProtocolVersion = params ^. ppProtocolVersionL
    , ppMinUTxOValue = params ^. ppMinUTxOValueL
    , ppMinPoolCost = params ^. ppMinPoolCostL
    , ppCoinsPerUtxo = Nothing
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
    , ppPoolVotingThresholds = Nothing
    , ppDRepVotingThresholds = Nothing
    , ppCommitteeMinSize = Nothing
    , ppCommitteeMaxTermLength = Nothing
    , ppGovActionLifetime = Nothing
    , ppGovActionDeposit = Nothing
    , ppDRepDeposit = Nothing
    , ppDRepActivity = Nothing
    , ppMinFeeRefScriptCostPerByte = Nothing
    }
