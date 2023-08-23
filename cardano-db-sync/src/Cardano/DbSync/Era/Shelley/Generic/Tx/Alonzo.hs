{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Generic.Tx.Alonzo (
  fromAlonzoTx,
  dataHashToBytes,
  mkCollTxIn,
  mkTxData,
  mkTxScript,
  resolveRedeemers,
  extraKeyWits,
  getPlutusSizes,
  getScripts,
  txDataWitness,
  rmWdrl,
  rmCerts,
  rmInps,
) where

import qualified Cardano.Crypto.Hash as Crypto
import Cardano.Db (ScriptType (..))
import Cardano.DbSync.Era.Shelley.Generic.Metadata
import Cardano.DbSync.Era.Shelley.Generic.Script (fromTimelock)
import Cardano.DbSync.Era.Shelley.Generic.ScriptData (ScriptData (..))
import Cardano.DbSync.Era.Shelley.Generic.Tx.Allegra (getInterval)
import Cardano.DbSync.Era.Shelley.Generic.Tx.Shelley
import Cardano.DbSync.Era.Shelley.Generic.Tx.Types
import Cardano.DbSync.Era.Shelley.Generic.Util
import Cardano.DbSync.Era.Shelley.Generic.Witness
import Cardano.DbSync.Types (DataHash)
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), txscriptfee)
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData (..), getAlonzoTxAuxDataScripts)
import Cardano.Ledger.Alonzo.TxBody (AlonzoEraTxBody, AlonzoTxOut)
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWits as Alonzo
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..), policyID)
import qualified Cardano.Ledger.SafeHash as Ledger
import Cardano.Ledger.Shelley.Scripts (ScriptHash)
import qualified Cardano.Ledger.Shelley.Tx as ShelleyTx
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import Cardano.Ledger.Shelley.TxCert as Shelley
import Cardano.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
#if __GLASGOW_HASKELL__ >= 906
import Data.Type.Equality (type (~))
#endif
import Lens.Micro
import Ouroboros.Consensus.Cardano.Block (EraCrypto, StandardAlonzo, StandardCrypto)

fromAlonzoTx :: Bool -> Maybe Alonzo.Prices -> (Word64, Core.Tx StandardAlonzo) -> Tx
fromAlonzoTx ioExtraPlutus mprices (blkIndex, tx) =
  Tx
    { txHash = txHashId tx
    , txBlockIndex = blkIndex
    , txSize = fromIntegral $ tx ^. Core.sizeTxF
    , txValidContract = isValid2
    , txInputs =
        if not isValid2
          then collInputs
          else Map.elems $ rmInps finalMaps
    , txCollateralInputs = collInputs
    , txReferenceInputs = [] -- Alonzo does not have reference inputs
    , txOutputs =
        if not isValid2
          then []
          else outputs
    , txCollateralOutputs = [] -- Alonzo does not have collateral outputs
    , txFees =
        if not isValid2
          then Nothing
          else Just $ Alonzo.txfee' txBody
    , txOutSum =
        if not isValid2
          then Coin 0
          else sumTxOutCoin outputs
    , txInvalidBefore = invalidBefore
    , txInvalidHereafter = invalidAfter
    , txWithdrawalSum = calcWithdrawalSum txBody
    , txMetadata = fromAlonzoMetadata <$> getTxMetadata tx
    , txCertificates = snd <$> rmCerts finalMaps
    , txWithdrawals = Map.elems $ rmWdrl finalMaps
    , txParamProposal = mkTxParamProposal (Alonzo Standard) txBody
    , txMint = Alonzo.mint' txBody
    , txRedeemer = redeemers
    , txData = txDataWitness tx
    , txScriptSizes = getPlutusSizes tx
    , txScripts = getScripts tx
    , txExtraKeyWitnesses = extraKeyWits txBody
    , txVotingProcedure = []
    , txProposalProcedure = []
    }
  where
    txBody :: Alonzo.AlonzoTxBody StandardAlonzo
    txBody = tx ^. Core.bodyTxL

    outputs :: [TxOut]
    outputs = zipWith fromTxOut [0 ..] $ toList (Alonzo.outputs' txBody)

    fromTxOut :: Word64 -> AlonzoTxOut StandardAlonzo -> TxOut
    fromTxOut index txOut =
      TxOut
        { txOutIndex = index
        , txOutAddress = txOut ^. Core.addrTxOutL
        , txOutAddressRaw = SBS.fromShort bs
        , txOutAdaValue = Coin ada
        , txOutMaValue = maMap
        , txOutScript = Nothing
        , txOutDatum = getMaybeDatumHash $ strictMaybeToMaybe mDataHash
        }
      where
        bs = Ledger.unCompactAddr $ txOut ^. Core.compactAddrTxOutL
        MaryValue ada (MultiAsset maMap) = txOut ^. Core.valueTxOutL
        mDataHash = txOut ^. Alonzo.dataHashTxOutL

    (finalMaps, redeemers) = resolveRedeemers ioExtraPlutus mprices tx (Left . toShelleyCert)

    -- This is true if second stage contract validation passes or there are no contracts.
    isValid2 :: Bool
    isValid2 =
      case Alonzo.isValid tx of
        Alonzo.IsValid x -> x

    (invalidBefore, invalidAfter) = getInterval txBody

    collInputs = mkCollTxIn txBody

mkCollTxIn :: (EraCrypto era ~ StandardCrypto, AlonzoEraTxBody era) => Core.TxBody era -> [TxIn]
mkCollTxIn txBody = map fromTxIn . toList $ txBody ^. Alonzo.collateralInputsTxBodyL

getScripts ::
  forall era.
  ( EraCrypto era ~ StandardCrypto
  , Core.Script era ~ Alonzo.AlonzoScript era
  , Core.TxAuxData era ~ AlonzoTxAuxData era
  , Core.EraTx era
  ) =>
  Core.Tx era ->
  [TxScript]
getScripts tx =
  mkTxScript
    <$> ( Map.toList (tx ^. (Core.witsTxL . Core.scriptTxWitsL))
            ++ getAuxScripts (tx ^. Core.auxDataTxL)
        )
  where
    getAuxScripts ::
      StrictMaybe (AlonzoTxAuxData era) ->
      [(ScriptHash StandardCrypto, Alonzo.AlonzoScript era)]
    getAuxScripts maux =
      case strictMaybeToMaybe maux of
        Nothing -> []
        Just auxData ->
          map (\scr -> (Core.hashScript @era scr, scr)) $
            toList $
              getAlonzoTxAuxDataScripts auxData

resolveRedeemers ::
  forall era.
  ( EraCrypto era ~ StandardCrypto
  , Alonzo.AlonzoEraTxWits era
  , Core.EraTx era
  , Alonzo.MaryEraTxBody era
  ) =>
  Bool ->
  Maybe Alonzo.Prices ->
  Core.Tx era ->
  (TxCert era -> Cert) ->
  (RedeemerMaps, [(Word64, TxRedeemer)])
resolveRedeemers ioExtraPlutus mprices tx toCert =
  if not ioExtraPlutus
    then (initRedeemersMaps, [])
    else
      mkRdmrAndUpdateRec (initRedeemersMaps, []) $
        zip [0 ..] $
          Map.toList (Alonzo.unRedeemers (tx ^. (Core.witsTxL . Alonzo.rdmrsTxWitsL)))
  where
    txBody :: Core.TxBody era
    txBody = tx ^. Core.bodyTxL

    withdrawalsNoRedeemers :: Map (Shelley.RewardAcnt StandardCrypto) TxWithdrawal
    withdrawalsNoRedeemers =
      Map.mapWithKey (curry mkTxWithdrawal) $
        Shelley.unWithdrawals $
          txBody ^. Core.withdrawalsTxBodyL

    txCertsNoRedeemers :: [(Cert, TxCertificate)]
    txCertsNoRedeemers =
      zipWith (\n dcert -> (dcert, toTxCert n dcert)) [0 ..] $
        toList $
          toCert <$> (txBody ^. Core.certsTxBodyL)

    txInsMissingRedeemer :: Map (ShelleyTx.TxIn StandardCrypto) TxIn
    txInsMissingRedeemer = Map.fromList $ fmap (\inp -> (inp, fromTxIn inp)) $ toList $ txBody ^. Core.inputsTxBodyL

    initRedeemersMaps :: RedeemerMaps
    initRedeemersMaps = RedeemerMaps withdrawalsNoRedeemers txCertsNoRedeemers txInsMissingRedeemer

    mkRdmrAndUpdateRec ::
      (RedeemerMaps, [(Word64, TxRedeemer)]) ->
      [(Word64, (Alonzo.RdmrPtr, (Alonzo.Data era, ExUnits)))] ->
      (RedeemerMaps, [(Word64, TxRedeemer)])
    mkRdmrAndUpdateRec (rdmrMaps, rdmrsAcc) [] = (rdmrMaps, reverse rdmrsAcc)
    mkRdmrAndUpdateRec (rdmrMaps, rdmrsAcc) ((rdmrIx, rdmr) : rest) =
      let (txRdmr, rdmrMaps') = handleRedeemer rdmrIx rdmr rdmrMaps
       in mkRdmrAndUpdateRec (rdmrMaps', (rdmrIx, txRdmr) : rdmrsAcc) rest

    handleRedeemer ::
      Word64 ->
      (Alonzo.RdmrPtr, (Alonzo.Data era, ExUnits)) ->
      RedeemerMaps ->
      (TxRedeemer, RedeemerMaps)
    handleRedeemer rdmrIx (ptr@(Alonzo.RdmrPtr tag index), (dt, exUnits)) rdmrMps =
      (txRdmr, rdmrMps')
      where
        (rdmrMps', mScript) = case strictMaybeToMaybe (Alonzo.rdptrInv txBody ptr) of
          Just (Alonzo.Minting policyId) -> (rdmrMps, Just $ Right $ unScriptHash $ policyID policyId)
          Just (Alonzo.Spending txIn) -> handleTxInPtr rdmrIx txIn rdmrMps
          Just (Alonzo.Rewarding rwdAcnt) -> handleRewardPtr rdmrIx rwdAcnt rdmrMps
          Just prp@(Alonzo.Certifying dcert) -> case strictMaybeToMaybe (Alonzo.rdptr txBody prp) of
            Just ptr' | ptr == ptr' -> handleCertPtr rdmrIx (toCert dcert) rdmrMps
            _ -> (rdmrMps, Nothing)
          Nothing -> (rdmrMps, Nothing)

        txRdmr =
          TxRedeemer
            { txRedeemerMem = fromIntegral $ exUnitsMem exUnits
            , txRedeemerSteps = fromIntegral $ exUnitsSteps exUnits
            , txRedeemerFee = (`txscriptfee` exUnits) <$> mprices
            , txRedeemerPurpose = tag
            , txRedeemerIndex = index
            , txRedeemerScriptHash = mScript
            , txRedeemerData = mkTxData (Alonzo.hashData dt, dt)
            }

handleTxInPtr :: Word64 -> ShelleyTx.TxIn StandardCrypto -> RedeemerMaps -> (RedeemerMaps, Maybe (Either TxIn ByteString))
handleTxInPtr rdmrIx txIn mps = case Map.lookup txIn (rmInps mps) of
  Nothing -> (mps, Nothing)
  Just gtxIn ->
    let gtxIn' = gtxIn {txInRedeemerIndex = Just rdmrIx}
     in (mps {rmInps = Map.insert txIn gtxIn' (rmInps mps)}, Just (Left gtxIn'))

handleRewardPtr :: Word64 -> Shelley.RewardAcnt StandardCrypto -> RedeemerMaps -> (RedeemerMaps, Maybe (Either TxIn ByteString))
handleRewardPtr rdmrIx rwdAcnt mps = case Map.lookup rwdAcnt (rmWdrl mps) of
  Nothing -> (mps, Nothing)
  Just wdrl ->
    let wdrl' = wdrl {txwRedeemerIndex = Just rdmrIx}
     in (mps {rmWdrl = Map.insert rwdAcnt wdrl' (rmWdrl mps)}, Right <$> scriptHashAcnt rwdAcnt)

handleCertPtr :: Word64 -> Cert -> RedeemerMaps -> (RedeemerMaps, Maybe (Either TxIn ByteString))
handleCertPtr rdmrIx dcert mps =
  (mps {rmCerts = map f (rmCerts mps)}, Right <$> scriptHashCert dcert)
  where
    f (dcert', cert) | dcert' == dcert = (dcert, cert {txcRedeemerIndex = Just rdmrIx})
    f x = x

data RedeemerMaps = RedeemerMaps
  { rmWdrl :: Map (Shelley.RewardAcnt StandardCrypto) TxWithdrawal
  , rmCerts :: [(Cert, TxCertificate)]
  , rmInps :: Map (ShelleyTx.TxIn StandardCrypto) TxIn
  }

mkTxScript ::
  (Ledger.Era era) =>
  (ScriptHash StandardCrypto, Alonzo.AlonzoScript era) ->
  TxScript
mkTxScript (hsh, script) =
  TxScript
    { txScriptHash = unScriptHash hsh
    , txScriptType = getScriptType
    , txScriptPlutusSize = getPlutusScriptSize script
    , txScriptJson = timelockJsonScript
    , txScriptCBOR = plutusCborScript
    }
  where
    getScriptType :: ScriptType
    getScriptType =
      case script of
        Alonzo.TimelockScript {} -> Timelock
        Alonzo.PlutusScript Alonzo.PlutusV1 _s -> PlutusV1
        Alonzo.PlutusScript Alonzo.PlutusV2 _s -> PlutusV2
        Alonzo.PlutusScript Alonzo.PlutusV3 _s -> PlutusV3

    timelockJsonScript :: Maybe ByteString
    timelockJsonScript =
      case script of
        Alonzo.TimelockScript s ->
          Just . LBS.toStrict . Aeson.encode $ fromTimelock s
        Alonzo.PlutusScript {} -> Nothing

    plutusCborScript :: Maybe ByteString
    plutusCborScript =
      case script of
        Alonzo.TimelockScript {} -> Nothing
        plutusScript -> Just $ Ledger.originalBytes plutusScript

getPlutusSizes ::
  forall era.
  ( Core.EraTx era
  , Core.TxWits era ~ Alonzo.AlonzoTxWits era
  , Core.Script era ~ Alonzo.AlonzoScript era
  ) =>
  Core.Tx era ->
  [Word64]
getPlutusSizes tx =
  mapMaybe getPlutusScriptSize $
    Map.elems $
      tx ^. (Core.witsTxL . Alonzo.scriptAlonzoTxWitsL)

-- | Returns Nothing for non-plutus scripts.
getPlutusScriptSize :: Alonzo.AlonzoScript era -> Maybe Word64
getPlutusScriptSize script =
  case script of
    Alonzo.TimelockScript {} -> Nothing
    Alonzo.PlutusScript _lang sbs -> Just $ fromIntegral (SBS.length sbs)

txDataWitness ::
  (Core.TxWits era ~ Alonzo.AlonzoTxWits era, Core.EraTx era, EraCrypto era ~ StandardCrypto) =>
  Core.Tx era ->
  [PlutusData]
txDataWitness tx =
  mkTxData <$> Map.toList (Alonzo.unTxDats $ Alonzo.txdats' (tx ^. Core.witsTxL))

mkTxData :: (DataHash, Alonzo.Data era) -> PlutusData
mkTxData (dataHash, dt) = PlutusData dataHash (jsonData dt) (Ledger.originalBytes dt)
  where
    jsonData :: Alonzo.Data era -> ByteString
    jsonData =
      LBS.toStrict
        . Aeson.encode
        . ScriptData

extraKeyWits ::
  AlonzoEraTxBody era =>
  Core.TxBody era ->
  [ByteString]
extraKeyWits txBody =
  Set.toList $
    Set.map (\(Ledger.KeyHash h) -> Crypto.hashToBytes h) $
      txBody ^. Alonzo.reqSignerHashesTxBodyL

scriptHashAcnt :: Shelley.RewardAcnt StandardCrypto -> Maybe ByteString
scriptHashAcnt rewardAddr = getCredentialScriptHash $ Ledger.getRwdCred rewardAddr

scriptHashCert :: Cert -> Maybe ByteString
scriptHashCert cert = case cert of
  Left scert -> scriptHashCertShelley scert
  Right ccert -> scriptHashCertConway ccert

-- TODO: Conway
scriptHashCertConway :: ConwayCert -> Maybe ByteString
scriptHashCertConway _cert = Nothing

scriptHashCertShelley :: ShelleyCert -> Maybe ByteString
scriptHashCertShelley cert =
  case cert of
    Shelley.ShelleyTxCertDelegCert (Shelley.ShelleyUnRegCert cred) ->
      getCredentialScriptHash cred
    Shelley.ShelleyTxCertDelegCert (Shelley.ShelleyDelegCert cred _) ->
      getCredentialScriptHash cred
    _ -> Nothing
