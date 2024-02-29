{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
import Cardano.Ledger.Alonzo.Scripts (AsIndex (..), ExUnits (..), PlutusPurpose (..), txscriptfee, unPlutusBinary)
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData (..), getAlonzoTxAuxDataScripts)
import Cardano.Ledger.Alonzo.TxBody (AlonzoEraTxBody, AlonzoTxOut)
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWits as Alonzo
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Scripts (ConwayPlutusPurpose (..))
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Keys as Ledger
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..), policyID)
import qualified Cardano.Ledger.SafeHash as Ledger
import Cardano.Ledger.Shelley.Scripts (ScriptHash)
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import Cardano.Ledger.Shelley.TxCert as Shelley
import qualified Cardano.Ledger.TxIn as Ledger
import Cardano.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import qualified Data.Maybe.Strict as Strict
import qualified Data.Set as Set
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
        , txOutAdaValue = ada
        , txOutMaValue = maMap
        , txOutScript = Nothing
        , txOutDatum = getMaybeDatumHash $ strictMaybeToMaybe mDataHash
        }
      where
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
  , DBPlutusScript era
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
  , DBScriptPurpose era
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

    txInsMissingRedeemer :: Map (Ledger.TxIn StandardCrypto) TxIn
    txInsMissingRedeemer = Map.fromList $ fmap (\inp -> (inp, fromTxIn inp)) $ toList $ txBody ^. Core.inputsTxBodyL

    initRedeemersMaps :: RedeemerMaps
    initRedeemersMaps = RedeemerMaps withdrawalsNoRedeemers txCertsNoRedeemers txInsMissingRedeemer

    mkRdmrAndUpdateRec ::
      (RedeemerMaps, [(Word64, TxRedeemer)]) ->
      [(Word64, (Alonzo.PlutusPurpose AsIndex era, (Alonzo.Data era, ExUnits)))] ->
      (RedeemerMaps, [(Word64, TxRedeemer)])
    mkRdmrAndUpdateRec (rdmrMaps, rdmrsAcc) [] = (rdmrMaps, reverse rdmrsAcc)
    mkRdmrAndUpdateRec (rdmrMaps, rdmrsAcc) ((rdmrIx, rdmr) : rest) =
      let (txRdmr, rdmrMaps') = handleRedeemer rdmrIx rdmr rdmrMaps
       in mkRdmrAndUpdateRec (rdmrMaps', (rdmrIx, txRdmr) : rdmrsAcc) rest

    handleRedeemer ::
      Word64 ->
      (PlutusPurpose AsIndex era, (Alonzo.Data era, ExUnits)) ->
      RedeemerMaps ->
      (TxRedeemer, RedeemerMaps)
    handleRedeemer rdmrIx (ptr, (dt, exUnits)) rdmrMps =
      (txRdmr, rdmrMps')
      where
        (rdmrMps', mScript) = case mkPurpose $ Alonzo.redeemerPointerInverse txBody ptr of
          Just (Left (Alonzo.AlonzoMinting policyId, _)) -> (rdmrMps, Just $ Right $ unScriptHash $ policyID (Alonzo.unAsItem policyId))
          Just (Left (Alonzo.AlonzoSpending txIn, _)) -> handleTxInPtr rdmrIx (Alonzo.unAsItem txIn) rdmrMps
          Just (Left (Alonzo.AlonzoRewarding rwdAcnt, _)) -> handleRewardPtr rdmrIx (Alonzo.unAsItem rwdAcnt) rdmrMps
          Just (Left (Alonzo.AlonzoCertifying dcert, Just ptr')) ->
            if ptr == ptr'
              then handleCertPtr rdmrIx (toCert $ Alonzo.unAsItem dcert) rdmrMps
              else (rdmrMps, Nothing)
          Just (Left (Alonzo.AlonzoCertifying _, Nothing)) -> (rdmrMps, Nothing)
          Just (Right (ConwayMinting policyId)) -> (rdmrMps, Just $ Right $ unScriptHash $ policyID (Alonzo.unAsItem policyId))
          Just (Right (ConwaySpending txIn)) -> handleTxInPtr rdmrIx (Alonzo.unAsItem txIn) rdmrMps
          Just (Right (ConwayRewarding rwdAcnt)) -> handleRewardPtr rdmrIx (Alonzo.unAsItem rwdAcnt) rdmrMps
          Just (Right (ConwayCertifying dcert)) -> handleCertPtr rdmrIx (toCert $ Alonzo.unAsItem dcert) rdmrMps
          Just (Right (ConwayVoting voter)) -> (rdmrMps, Right <$> getConwayVotingScriptHash (Alonzo.unAsItem voter))
          Just (Right (ConwayProposing proposal)) -> (rdmrMps, Right <$> getConwayProposalScriptHash (Alonzo.unAsItem proposal))
          Nothing -> (rdmrMps, Nothing)

        (tag, idx) = getPurpose ptr
        txRdmr =
          TxRedeemer
            { txRedeemerMem = fromIntegral $ exUnitsMem exUnits
            , txRedeemerSteps = fromIntegral $ exUnitsSteps exUnits
            , txRedeemerFee = (`txscriptfee` exUnits) <$> mprices
            , txRedeemerPurpose = tag
            , txRedeemerIndex = fromIntegral idx
            , txRedeemerScriptHash = mScript
            , txRedeemerData = mkTxData (Alonzo.hashData dt, dt)
            }

        mkPurpose = \case
          Strict.SNothing -> Nothing
          Strict.SJust a -> toAlonzoPurpose txBody a

handleTxInPtr :: Word64 -> Ledger.TxIn StandardCrypto -> RedeemerMaps -> (RedeemerMaps, Maybe (Either TxIn ByteString))
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
  , rmInps :: Map (Ledger.TxIn StandardCrypto) TxIn
  }

mkTxScript ::
  DBPlutusScript era =>
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
        Alonzo.PlutusScript ps -> getPlutusScriptType ps

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
        plScript -> Just $ Ledger.originalBytes plScript

getPlutusSizes ::
  forall era.
  ( Core.EraTx era
  , Core.TxWits era ~ Alonzo.AlonzoTxWits era
  , Core.Script era ~ Alonzo.AlonzoScript era
  , AlonzoEraScript era
  ) =>
  Core.Tx era ->
  [Word64]
getPlutusSizes tx =
  mapMaybe getPlutusScriptSize $
    Map.elems $
      tx ^. (Core.witsTxL . Alonzo.scriptAlonzoTxWitsL)

-- | Returns Nothing for non-plutus scripts.
getPlutusScriptSize :: AlonzoEraScript era => Alonzo.AlonzoScript era -> Maybe Word64
getPlutusScriptSize script =
  case script of
    Alonzo.TimelockScript {} -> Nothing
    Alonzo.PlutusScript ps ->
      Just $ fromIntegral $ SBS.length $ unPlutusBinary $ Alonzo.plutusScriptBinary ps

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

scriptHashCertConway :: ConwayCert -> Maybe ByteString
scriptHashCertConway cert = unScriptHash <$> getScriptWitnessTxCert cert

scriptHashCertShelley :: ShelleyCert -> Maybe ByteString
scriptHashCertShelley cert = unScriptHash <$> getScriptWitnessTxCert cert

getConwayVotingScriptHash :: Voter StandardCrypto -> Maybe ByteString
getConwayVotingScriptHash = \case
  CommitteeVoter cred -> getCredentialScriptHash cred
  DRepVoter cred -> getCredentialScriptHash cred
  StakePoolVoter _ -> Nothing

getConwayProposalScriptHash :: EraCrypto era ~ StandardCrypto => ProposalProcedure era -> Maybe ByteString
getConwayProposalScriptHash pp = case pProcGovAction pp of
  ParameterChange _ _ p -> unScriptHash <$> strictMaybeToMaybe p
  TreasuryWithdrawals _ p -> unScriptHash <$> strictMaybeToMaybe p
  _ -> Nothing
