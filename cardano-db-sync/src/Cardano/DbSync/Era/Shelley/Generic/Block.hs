{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Cardano.DbSync.Era.Shelley.Generic.Block
  ( Block (..)
  , BlockEra (..)
  , fromShelleyBasedBlock

  , blockHash
  , slotLeaderHash
  ) where

import qualified Cardano.Api.Shelley as Api
import qualified Cardano.Api.Byron as ByronApi

import qualified Cardano.Crypto.Hash as Crypto

import qualified Cardano.Db as Db
import           Cardano.DbSync.Era.Shelley.Generic.Tx
import           Cardano.DbSync.Era.Shelley.Generic.Util
import           Cardano.DbSync.Era.Shelley.Generic.ParamProposal

import           Cardano.Ledger.Alonzo ()
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import           Cardano.Ledger.Shelley.TxBody ()
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.Era (Crypto)
import qualified Cardano.Ledger.Mary.Value as Mary

import           Cardano.Prelude
import           Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LBS
import           Data.List (elemIndex)
import qualified Data.Map.Strict as M

import qualified Cardano.Protocol.TPraos.BHeader as Protocol

import           Cardano.DbSync.Types
import           Cardano.Slotting.Slot (SlotNo (..))

import           Ouroboros.Consensus.Cardano.Block (StandardCrypto, StandardAlonzo)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Consensus

import           Ouroboros.Network.Block (BlockNo (..))
import           Cardano.Api.Shelley (toShelleyLovelace)


data Block = Block
  { blkEra :: !Api.AnyCardanoEra
  , blkHash :: !ByteString
  , blkPreviousHash :: !(Maybe ByteString) -- Nothing is used for first block after Genesis.
  , blkSlotLeader :: !ByteString
  , blkSlotNo :: !SlotNo
  , blkBlockNo :: !BlockNo
  , blkSize :: !Word64
  , blkProto :: !Ledger.ProtVer
  , blkVrfKey :: !Text
  , blkOpCert :: !ByteString
  , blkOpCertCounter :: !Word64
  , blkTxs :: ![Tx]
  }

fromShelleyBasedBlock
  :: Ledger.PParams StandardAlonzo -> Api.ShelleyBasedEra era -> Api.Block era -> Block
fromShelleyBasedBlock pp sbe b@Api.ShelleyBlock{} =
  let Api.BlockHeader slotN (Api.HeaderHash bHeaderHash) bNumber = fst $ Api.getBlockHeaderAndTxs b
  in Block
       { blkEra = Api.blockToAnyCardanoEra b
       , blkHash = SBS.fromShort bHeaderHash
       , blkPreviousHash =
           case Api.previousBlockHeaderHash b of
             Just (Api.HeaderHash preHeadHash) -> Just $ SBS.fromShort preHeadHash
             Nothing -> Nothing
       , blkSlotLeader = Api.serialiseToRawBytes $ Api.getBlockIssuer sbe b
       , blkSlotNo = slotN
       , blkBlockNo = bNumber
       , blkSize = Api.getBlockSize b
       , blkProto = Api.getBlockProtocolVersion sbe b
       , blkVrfKey = Api.serialiseToBech32 $ Api.getBlockVrfVerificationKey sbe b
       , blkOpCert = Api.serialiseToRawBytes $ Api.getBlockOpCertKesHotKey sbe b
       , blkOpCertCounter = Api.getBlockOpCertCounter sbe b
       , blkTxs = map (fromShelleyBasedTx pp sbe) $ blockTxs b
       }

fromShelleyBasedTx
  :: Ledger.PParams StandardAlonzo -> Api.ShelleyBasedEra era -> (Word64, Api.Tx era) -> Tx
fromShelleyBasedTx pp era (blkIndex, tx@(Api.ShelleyTx _ _lTx)) =
  let (lowerBound, upperBound) = Api.getTxValidityInterval era tx
      allScripts = Api.getTxScripts era tx
      plutusScriptSizes = mapMaybe (fmap fromIntegral . getPlutuScriptSize) allScripts
      txouts = Api.getTxOuts tx
      txoutsSum = sum $ map (\(Api.TxOut _ v _) -> Api.txOutValueToLovelace v) txouts
      collateralTxIns = Api.txInsCollateral $ Api.getTxBodyContent $ Api.getTxBody tx
      txScriptValidity = Api.txScriptValidity $ Api.getTxBodyContent $ Api.getTxBody tx
  in Tx
      { txHash = Api.getTxHash tx
      , txBlockIndex = blkIndex
      , txSize = fromIntegral $ Api.getTxSize tx
      , txValidContract = fromApiScriptValidity txScriptValidity
      , txInputs = map (fromApiTxIn era (Api.getTxIns tx)) $ Api.getTxIns tx
      , txCollateralInputs = map (fromApiTxIn era (Api.getTxIns tx)) $ getTxCollateralIns collateralTxIns
      , txOutputs = zipWith (fromApiTxOut era) [0..] txouts
      , txFees = Api.toShelleyLovelace $ Api.getTxFee era tx
      , txOutSum = toShelleyLovelace txoutsSum
      , txInvalidBefore = fromApiLowerTxValidity lowerBound
      , txInvalidHereafter = fromApiUpperTxValidity upperBound
      , txWithdrawalSum = toShelleyLovelace . getTxWithdrawalSum $ Api.getTxWithdrawals era tx
      , txMetadata = fromApiTxMetadata $ Api.getTxMetadata tx
      , txCertificates = fromApiTxCertificates $ Api.getTxCertificates tx
      , txWithdrawals = fromApiTxWithdrawals era $ Api.getTxWithdrawals era tx
      , txParamProposal = fromApiUpdateProposal $ Api.getTxUpdateProposal era tx
      , txMint = fromApiMint $ Api.getTxMint tx
      , txRedeemer = getTxRedeemers pp tx
      , txData = getTxDatums era tx
      , txScriptSizes = plutusScriptSizes
      , txScripts = map fromApiPlutusScript allScripts
      , txScriptsFee = minScriptFees era tx pp
      , txExtraKeyWitnesses = fromApiExtraKeyWitnesses $ Api.getTxExtraKeyWitnesses tx
      }
 where
  getPlutuScriptSize :: Api.ScriptInAnyLang -> Maybe Int
  getPlutuScriptSize (Api.ScriptInAnyLang (Api.PlutusScriptLanguage _) (Api.PlutusScript _ s)) =
    Just $ Api.getScriptSize s
  getPlutuScriptSize _ = Nothing

getTxDatums :: Api.ShelleyBasedEra era -> Api.Tx era -> [TxDatum]
getTxDatums era (Api.Tx txbody _) =
  case txbody of
    ByronApi.ByronTxBody{} -> []
    Api.ShelleyTxBody _ _ _ datums _ _ ->
      case datums of
        Api.TxBodyNoScriptData -> []
        Api.TxBodyScriptData _ dats _ ->
          let datMap = obtainTypeableConstraint era $ datPatternSyn  dats
          in map (fromApiDatum . Api.fromAlonzoData) $ M.elems datMap
 where
  datPatternSyn (Ledger.TxDats datMap) = datMap

  obtainTypeableConstraint
    :: Api.ShelleyBasedEra era
    -> (Typeable (Api.ShelleyLedgerEra era) => a)
    -> a
  obtainTypeableConstraint Api.ShelleyBasedEraShelley f = f
  obtainTypeableConstraint Api.ShelleyBasedEraAllegra f = f
  obtainTypeableConstraint Api.ShelleyBasedEraMary f = f
  obtainTypeableConstraint Api.ShelleyBasedEraAlonzo f = f

getTxRedeemers :: Ledger.PParams StandardAlonzo ->  Api.Tx era -> [TxRedeemer]
getTxRedeemers _ ByronApi.ByronTx{} = []
getTxRedeemers pp t@(Api.ShelleyTx era _) =
  case era of
    Api.ShelleyBasedEraShelley -> []
    Api.ShelleyBasedEraAllegra -> []
    Api.ShelleyBasedEraMary -> []
    Api.ShelleyBasedEraAlonzo ->
      map mkRedeemer' $ Api.getTxRedeemers t
 where
  mkRedeemer'
    :: (Api.ScriptWitnessIndex, (Api.ScriptData, Api.ExecutionUnits)) -> TxRedeemer
  mkRedeemer' (sWitIndex, (datum, execUnits)) =
    let ptr@(Ledger.RdmrPtr tag index) = Api.toAlonzoRdmrPtr sWitIndex
    in TxRedeemer
         { txRedeemerMem = fromIntegral $ Api.executionMemory execUnits
         , txRedeemerSteps = fromIntegral $ Api.executionSteps execUnits
         , txRedeemerFee = Alonzo.txscriptfee (Alonzo._prices pp) $ Api.toAlonzoExUnits execUnits
         , txRedeemerPurpose = tag
         , txRedeemerIndex = index
         , txRedeemerScriptHash = findScriptHash ptr
         , txRedeemerDatum = fromApiDatum datum
         }

  -- TODO: Implement SerialiseAsRawBytes instance for Certificate in cardano-api
  -- This mimics 'Ledger.addOnlyCwitness'
  scriptHashCert :: Shelley.DCert StandardCrypto -> Maybe ByteString
  scriptHashCert cert =
    case cert of
      Shelley.DCertDeleg (Shelley.DeRegKey cred) ->
        getCredentialScriptHash cred
      Shelley.DCertDeleg (Shelley.Delegate (Shelley.Delegation cred _)) ->
        getCredentialScriptHash cred
      _ -> Nothing

  txWdrls = Api.getTxWithdrawals era t
  txCerts = Api.getTxCertificates t
  txMint' = Api.getTxMint t

  -- TODO: Expose similar function from cardano-api
  findScriptHash :: Ledger.RdmrPtr -> Maybe (Either TxIn ByteString)
  findScriptHash (Ledger.RdmrPtr tag index) =
    let txins = Api.getTxIns t
    in case tag of
         Alonzo.Spend ->
           -- We always use the real inputs here, instead of the collateral, because
           -- this just helps us find the script hash later, by resolving the input.
           Left . fromApiTxIn era txins
             <$> safeLookup txins index
         Alonzo.Rewrd ->
           case txWdrls of
             Api.TxWithdrawalsNone -> Nothing
             Api.TxWithdrawals _ wdrls ->
               let stakeAddrs = [ sAddr | (sAddr,_ ,_) <- wdrls]
               in Right <$> (Api.serialiseToRawBytes <$> safeLookup stakeAddrs index )
         Alonzo.Cert ->
           case txCerts of
             Api.TxCertificatesNone -> Nothing
             Api.TxCertificates _ certs _stakeCredMap ->
               Right <$> (scriptHashCert . Api.toShelleyCertificate =<< safeLookup certs index)

         Alonzo.Mint ->
           case txMint' of
             Api.TxMintNone -> Nothing
             Api.TxMintValue _ v _scriptWitMap ->
               let Api.ValueNestedRep bundle = Api.valueToNestedRep v
                   pids = [policyid | (Api.ValueNestedBundle policyid _) <- bundle]
               in Right . Api.serialiseToRawBytes <$> safeLookup pids index

safeLookup :: [a] -> Word64 -> Maybe a
safeLookup _ n | n < 0 = Nothing
safeLookup [] _ = Nothing
safeLookup (x:_) 0 = Just x
safeLookup (_:xs) n = safeLookup xs (n-1)

getTxCollateralIns :: Api.TxInsCollateral era -> [Api.TxIn]
getTxCollateralIns Api.TxInsCollateralNone = []
getTxCollateralIns (Api.TxInsCollateral _ ins) = ins

getTxWithdrawalSum :: Api.TxWithdrawals Api.ViewTx era -> Api.Lovelace
getTxWithdrawalSum Api.TxWithdrawalsNone = 0
getTxWithdrawalSum (Api.TxWithdrawals _ withdrwls) =
  foldl (\acc (_, rwd, _) -> acc + rwd) 0 withdrwls

-- TODO: Expose function from api that collects the transaction's script fee.
minScriptFees :: Api.ShelleyBasedEra era -> Api.Tx era -> Ledger.PParams StandardAlonzo  -> Ledger.Coin
minScriptFees sbe (Api.ShelleyTx _ tx) pp =
  case sbe of
   Api.ShelleyBasedEraShelley -> mempty
   Api.ShelleyBasedEraAllegra -> mempty
   Api.ShelleyBasedEraMary -> mempty
   Api.ShelleyBasedEraAlonzo ->
     Alonzo.txscriptfee (Alonzo._prices pp) (Alonzo.totExUnits tx)

fromApiDatum :: Api.ScriptData -> TxDatum
fromApiDatum datum =
   TxDatum (Api.serialiseToRawBytes $ Api.hashScriptData datum) (encodeDatum datum)
  where
   encodeDatum :: Api.ScriptData -> ByteString
   encodeDatum =
     LBS.toStrict . Aeson.encode .
       Api.scriptDataToJson Api.ScriptDataJsonDetailedSchema

fromApiScriptValidity :: Api.TxScriptValidity era -> Bool
fromApiScriptValidity Api.TxScriptValidityNone = True
fromApiScriptValidity (Api.TxScriptValidity _ sValidity) =
  case sValidity of
    Api.ScriptInvalid -> False
    Api.ScriptValid -> True

fromApiPlutusScript :: Api.ScriptInAnyLang -> TxScript
fromApiPlutusScript (Api.ScriptInAnyLang _ anyScript) =
  case anyScript of
    s@(Api.SimpleScript ver script) ->
      let scriptType = case ver of
                         Api.SimpleScriptV1 -> Db.MultiSig
                         Api.SimpleScriptV2 -> Db.Timelock
          scriptHash = Api.serialiseToRawBytesHex $ Api.hashScript s
          scriptAsCBOR = obtainIsLanguageConstraintSimple ver $ Api.serialiseToCBOR s
          scriptAsJSON = LBS.toStrict $ encode script
      in TxScript scriptHash scriptType Nothing (Just scriptAsJSON) (Just scriptAsCBOR)
    s@(Api.PlutusScript ver _script) ->
      -- TODO: Does db sync want to also display the Plutus script versions
      -- via ver?
      let scriptType = Db.Plutus
          scriptHash = Api.serialiseToRawBytesHex $ Api.hashScript s
          scriptAsCBOR = obtainIsLanguageConstraintPlutus ver $ Api.serialiseToCBOR s
          scriptAsJSON = Nothing -- Plutus scripts do not have a JSON representation
      in TxScript scriptHash scriptType Nothing scriptAsJSON (Just scriptAsCBOR)
 where
  obtainIsLanguageConstraintPlutus
    :: Api.PlutusScriptVersion lang
    -> (Api.IsScriptLanguage lang => a) -> a
  obtainIsLanguageConstraintPlutus Api.PlutusScriptV1 f = f
  obtainIsLanguageConstraintPlutus Api.PlutusScriptV2 f = f

  obtainIsLanguageConstraintSimple
    :: Api.SimpleScriptVersion lang
    -> (Api.IsScriptLanguage lang => a) -> a
  obtainIsLanguageConstraintSimple Api.SimpleScriptV1 f = f
  obtainIsLanguageConstraintSimple Api.SimpleScriptV2 f = f


fromApiLowerTxValidity :: Api.TxValidityLowerBound era -> Maybe SlotNo
fromApiLowerTxValidity Api.TxValidityNoLowerBound = Nothing
fromApiLowerTxValidity (Api.TxValidityLowerBound _ s) = Just s

fromApiUpperTxValidity :: Api.TxValidityUpperBound era -> Maybe SlotNo
fromApiUpperTxValidity (Api.TxValidityNoUpperBound _) = Nothing
fromApiUpperTxValidity (Api.TxValidityUpperBound _ s) = Just s

fromApiTxMetadata :: Api.TxMetadata -> Maybe (Map Word64 Api.TxMetadataValue)
fromApiTxMetadata (Api.TxMetadata m) = Just m

fromApiTxCertificates :: Api.TxCertificates Api.ViewTx era -> [TxCertificate]
fromApiTxCertificates Api.TxCertificatesNone = []
fromApiTxCertificates (Api.TxCertificates _ apiCerts _) =
  zipWith mkTxCertificate [0..] apiCerts
 where
   mkTxCertificate :: Word16 -> Api.Certificate -> TxCertificate
   mkTxCertificate n cert =
     let ledgerCert = Api.toShelleyCertificate cert
     in TxCertificate
       { txcIndex = n
       , txcRedeemerIndex = fromIntegral <$> elemIndex cert apiCerts
       , txcCert = ledgerCert
       }

fromApiTxWithdrawals :: Api.ShelleyBasedEra era -> Api.TxWithdrawals Api.ViewTx era -> [TxWithdrawal]
fromApiTxWithdrawals _ Api.TxWithdrawalsNone = []
fromApiTxWithdrawals sbe (Api.TxWithdrawals _ wdrls) =
  case sbe of
    Api.ShelleyBasedEraShelley -> map (mkTxWithdrawal Nothing) wdrls
    Api.ShelleyBasedEraAllegra -> map (mkTxWithdrawal Nothing) wdrls
    Api.ShelleyBasedEraMary -> map (mkTxWithdrawal Nothing) wdrls
    Api.ShelleyBasedEraAlonzo ->
      let rwdAccts = map (\(rwdAcct,_,_) -> rwdAcct) wdrls
      in map (\s@(sAddr,_,_) -> mkTxWithdrawal (fromIntegral <$> elemIndex sAddr rwdAccts) s ) wdrls
 where
  mkTxWithdrawal
    :: Maybe Word64
    -> ( Api.StakeAddress
       , Api.Lovelace
       , Api.BuildTxWith Api.ViewTx (Api.Witness Api.WitCtxStake era)
       )
    -> TxWithdrawal
  mkTxWithdrawal rIndex (sAddress, rewards,_) =
    TxWithdrawal
      { txwRedeemerIndex = rIndex
      , txwRewardAccount = Api.toShelleyStakeAddr sAddress
      , txwAmount = Api.toShelleyLovelace rewards
      }

fromApiMint :: Api.TxMintValue Api.ViewTx era -> Mary.Value StandardCrypto
fromApiMint Api.TxMintNone = mempty
fromApiMint (Api.TxMintValue _ v _sWits) = Api.toMaryValue v

fromApiUpdateProposal :: Api.TxUpdateProposal era -> [ParamProposal]
fromApiUpdateProposal Api.TxUpdateProposalNone = []
fromApiUpdateProposal (Api.TxUpdateProposal _ p) =
  let Api.UpdateProposal genKeyMap eNo = p
  in map (toParamProposal eNo) $ getUpdateProposal genKeyMap
 where
  getUpdateProposal
    :: Map (Api.Hash Api.GenesisKey) Api.ProtocolParametersUpdate
    -> [(Api.Hash Api.GenesisKey, Api.ProtocolParametersUpdate)]
  getUpdateProposal m = M.toList m

  toParamProposal
    :: Api.EpochNo
    -> (Api.Hash Api.GenesisKey, Api.ProtocolParametersUpdate) -> ParamProposal
  toParamProposal eNo (genKeyHash, ppu) =
    ParamProposal
      { pppEpochNo = eNo
      , pppKey = Api.serialiseToRawBytes genKeyHash
      , pppMinFeeA = Api.protocolUpdateTxFeePerByte ppu
      , pppMinFeeB = Api.protocolUpdateTxFeeFixed ppu
      , pppMaxBlockSize = Api.protocolUpdateMaxBlockBodySize ppu
      , pppMaxTxSize = Api.protocolUpdateMaxTxSize ppu
      , pppMaxBhSize = Api.protocolUpdateMaxBlockHeaderSize ppu
      , pppKeyDeposit = toShelleyLovelace <$> Api.protocolUpdateStakeAddressDeposit ppu
      , pppPoolDeposit = toShelleyLovelace <$> Api.protocolUpdateStakePoolDeposit ppu
      , pppMaxEpoch = Api.protocolUpdatePoolRetireMaxEpoch ppu
      , pppOptimalPoolCount = Api.protocolUpdateStakePoolTargetNum ppu
      , pppInfluence = Api.protocolUpdatePoolPledgeInfluence ppu
      , pppMonetaryExpandRate = Ledger.boundRational =<< Api.protocolUpdateMonetaryExpansion ppu
      , pppTreasuryGrowthRate = Ledger.boundRational =<<  Api.protocolUpdateTreasuryCut ppu
      , pppDecentralisation = Ledger.boundRational =<< Api.protocolUpdateDecentralization ppu
      , pppEntropy = Api.toLedgerNonce <$> Api.protocolUpdateExtraPraosEntropy ppu
      , pppProtocolVersion =
         Api.protocolUpdateProtocolVersion ppu >>= (\(max',min') -> Just $ Ledger.ProtVer max' min' )
      , pppMinUtxoValue = toShelleyLovelace <$> Api.protocolUpdateMinUTxOValue ppu
      , pppMinPoolCost = toShelleyLovelace <$> Api.protocolUpdateMinPoolCost ppu
      , pppCoinsPerUtxoWord = toShelleyLovelace <$> Api.protocolUpdateUTxOCostPerWord ppu
      , pppCostmdls = Just . Api.toAlonzoCostModels $ Api.protocolUpdateCostModels ppu
      , pppPriceMem = Api.priceExecutionMemory <$> Api.protocolUpdatePrices ppu
      , pppPriceStep = Api.priceExecutionSteps <$> Api.protocolUpdatePrices ppu
      , pppMaxTxExMem = fromIntegral . Api.executionMemory <$> Api.protocolUpdateMaxTxExUnits ppu
      , pppMaxTxExSteps = fromIntegral . Api.executionSteps <$> Api.protocolUpdateMaxTxExUnits ppu
      , pppMaxBlockExMem = fromIntegral . Api.executionMemory <$> Api.protocolUpdateMaxBlockExUnits ppu
      , pppMaxBlockExSteps = fromIntegral . Api.executionSteps <$> Api.protocolUpdateMaxBlockExUnits ppu
      , pppMaxValSize = Api.protocolUpdateMaxValueSize ppu
      , pppCollateralPercentage = Api.protocolUpdateCollateralPercent ppu
      , pppMaxCollateralInputs = Api.protocolUpdateMaxCollateralInputs ppu
      }

fromApiTxIn :: Api.ShelleyBasedEra era -> [Api.TxIn] -> Api.TxIn -> TxIn
fromApiTxIn sbe txins tIn@(Api.TxIn txid (Api.TxIx txIndex)) =
  TxIn { txInHash = Api.serialiseToRawBytes txid
       , txInIndex = fromIntegral txIndex
       , txInRedeemerIndex = txInRedeemerIndex' sbe txins tIn
       }
 where
   txInRedeemerIndex' :: Api.ShelleyBasedEra era -> [Api.TxIn] -> Api.TxIn -> Maybe Word64
   txInRedeemerIndex' Api.ShelleyBasedEraShelley _ _ = Nothing
   txInRedeemerIndex' Api.ShelleyBasedEraAllegra _ _ = Nothing
   txInRedeemerIndex' Api.ShelleyBasedEraMary _ _ = Nothing
   txInRedeemerIndex' Api.ShelleyBasedEraAlonzo allTxIns txin =
     fromIntegral <$> elemIndex txin allTxIns

-- TODO: The context should be UTxO as these txs are coming from blocks
fromApiTxOut
  :: Api.ShelleyBasedEra era -> Word16 -> Api.TxOut Api.CtxTx era -> TxOut
fromApiTxOut era index (Api.TxOut addrInEra val dat) =
  TxOut
    { txOutIndex = index
    , txOutAddress = Api.toShelleyAddr addrInEra
    , txOutAddressRaw = obtainIsCardanoEra era $ Api.serialiseToRawBytes addrInEra
    , txOutAdaValue = Api.toShelleyLovelace $ Api.txOutValueToLovelace val
    , txOutMaValue =
        let Mary.Value _ maMap =  Api.toMaryValue $ Api.txOutValueToValue val
        in maMap
    , txOutDataHash =
        case dat of
          Api.TxOutDatum _ sData ->
            Just . Api.serialiseToRawBytes $ Api.hashScriptData sData
          Api.TxOutDatumNone -> Nothing
          Api.TxOutDatumHash _ h -> Just $ Api.serialiseToRawBytes h
    }
 where
  obtainIsCardanoEra :: Api.ShelleyBasedEra era -> (Api.IsCardanoEra era => a) -> a
  obtainIsCardanoEra Api.ShelleyBasedEraShelley f = f
  obtainIsCardanoEra Api.ShelleyBasedEraAllegra f = f
  obtainIsCardanoEra Api.ShelleyBasedEraMary f = f
  obtainIsCardanoEra Api.ShelleyBasedEraAlonzo f = f

fromApiExtraKeyWitnesses :: Api.TxExtraKeyWitnesses era -> [ByteString]
fromApiExtraKeyWitnesses Api.TxExtraKeyWitnessesNone = []
fromApiExtraKeyWitnesses (Api.TxExtraKeyWitnesses _ pKeyHashes) =
  map Api.serialiseToRawBytes pKeyHashes

-- -------------------------------------------------------------------------------------------------

blockBody :: Consensus.ShelleyBasedEra era => ShelleyBlock era -> Protocol.BHBody (Crypto era)
blockBody = Protocol.bhbody . Ledger.bheader . Consensus.shelleyBlockRaw

blockHash :: ShelleyBlock era -> ByteString
blockHash =
  Crypto.hashToBytes . Protocol.unHashHeader
    . Consensus.unShelleyHash . Consensus.shelleyBlockHeaderHash

blockTxs :: Api.Block era -> [(Word64, Api.Tx era)]
blockTxs b = zip [0 ..] $ Api.getBlockTxs b

slotLeaderHash :: Consensus.ShelleyBasedEra era => ShelleyBlock era -> ByteString
slotLeaderHash = unKeyHashRaw . Protocol.issuerIDfromBHBody . blockBody

