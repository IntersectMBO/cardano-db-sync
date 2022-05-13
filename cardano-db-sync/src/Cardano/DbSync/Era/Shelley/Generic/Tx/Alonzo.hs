{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbSync.Era.Shelley.Generic.Tx.Alonzo where

import           Cardano.Prelude

import qualified Cardano.Crypto.Hash as Crypto

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import           Cardano.Ledger.Alonzo.Scripts (ExUnits (..), txscriptfee)
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import           Cardano.Ledger.Alonzo.Tx (ValidatedTx (..))
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo
import           Cardano.Ledger.BaseTypes
import           Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.CompactAddress as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import           Cardano.Ledger.Mary.Value (Value (..), policyID)
import qualified Cardano.Ledger.SafeHash as Ledger
import           Cardano.Ledger.Shelley.Scripts (ScriptHash)
import qualified Cardano.Ledger.Shelley.Tx as ShelleyTx
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import qualified Cardano.Ledger.ShelleyMA.TxBody as ShelleyMa

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import           Data.Sequence.Strict (StrictSeq)
import qualified Data.Set as Set

import           Ouroboros.Consensus.Cardano.Block (StandardAlonzo, StandardCrypto)

import qualified Cardano.Api.Shelley as Api

import           Cardano.Db (ScriptType (..))

import           Cardano.DbSync.Era.Shelley.Generic.Metadata
import           Cardano.DbSync.Era.Shelley.Generic.ParamProposal
import           Cardano.DbSync.Era.Shelley.Generic.Tx.Allegra (getInterval)
import           Cardano.DbSync.Era.Shelley.Generic.Tx.Shelley (fromTxIn, getWithdrawalSum,
                   mkTxCertificate, mkTxWithdrawal)
import           Cardano.DbSync.Era.Shelley.Generic.Tx.Types
import           Cardano.DbSync.Era.Shelley.Generic.Util
import           Cardano.DbSync.Era.Shelley.Generic.Witness


fromAlonzoTx :: Alonzo.Prices -> (Word64, Ledger.Tx StandardAlonzo) -> Tx
fromAlonzoTx prices (blkIndex, tx) =
    Tx
      { txHash = Crypto.hashToBytes . Ledger.extractHash $ Ledger.hashAnnotated txBody
      , txBlockIndex = blkIndex
      , txSize = fromIntegral $ getField @"txsize" tx
      , txValidContract = isValid2
      , txInputs =
          if not isValid2
            then map fromTxIn . toList $ getField @"collateral" txBody
            else Map.elems $ rmInps finalMaps
      , txCollateralInputs = map fromTxIn . toList $ getField @"collateral" txBody
      , txReferenceInputs = [] -- Alonzo does not have reference inputs
      , txOutputs =
          if not isValid2
            then []
            else outputs
      , txCollateralOutputs = [] -- Alonzo does not have collateral outputs
      , txFees =
          if not isValid2
            then Nothing
            else Just $ getField @"txfee" txBody
      , txOutSum =
          if not isValid2
            then Coin 0
            else sumOutputs outputs
      , txInvalidBefore = invalidBefore
      , txInvalidHereafter = invalidAfter
      , txWithdrawalSum = getWithdrawalSum $ getField @"wdrls" txBody
      , txMetadata = fromAlonzoMetadata <$> strictMaybeToMaybe (getField @"auxiliaryData" tx)
      , txCertificates = snd <$> rmCerts finalMaps
      , txWithdrawals = Map.elems $ rmWdrl finalMaps
      , txParamProposal = maybe [] (convertParamProposal (Alonzo Standard)) $ strictMaybeToMaybe (getField @"update" txBody)
      , txMint = getField @"mint" txBody
      , txRedeemer = redeemers
      , txData = txDataWitness tx
      , txScriptSizes = getPlutusSizes tx
      , txScripts = getScripts tx
      , txScriptsFee = minFees
      , txExtraKeyWitnesses = extraKeyWits txBody
      }
  where
    outputs :: [TxOut]
    outputs = zipWith fromTxOut [0 .. ] $ toList $ getField @"outputs" txBody

    fromTxOut :: Word16 -> Alonzo.TxOut StandardAlonzo -> TxOut
    fromTxOut index txOut =
      TxOut
        { txOutIndex = index
        , txOutAddress = addr
        , txOutAddressRaw = SBS.fromShort caddr
        , txOutAdaValue = Coin ada
        , txOutMaValue = maMap
        , txOutScript = Nothing
        , txOutDatum = getMaybeDatumHash $ dataHashToBytes <$> strictMaybeToMaybe mDataHash
        }
      where
        Ledger.UnsafeCompactAddr caddr = Ledger.getTxOutCompactAddr txOut

        -- This pattern match also does the deserialisation of the address
        Alonzo.TxOut addr (Value ada maMap) mDataHash = txOut

    txBody :: Ledger.TxBody StandardAlonzo
    txBody = getField @"body" tx

    minFees :: Coin
    minFees = txscriptfee prices $ Alonzo.totExUnits tx

    (finalMaps, redeemers) = resolveRedeemers prices tx

    -- This is true if second stage contract validation passes or there are no contracts.
    isValid2 :: Bool
    isValid2 =
      case Alonzo.isValid tx of
        Alonzo.IsValid x -> x

    (invalidBefore, invalidAfter) = getInterval txBody

getScripts ::
    forall era.
    ( Ledger.Crypto era ~ StandardCrypto, Ledger.Era era
    , HasField "txscripts" (Ledger.Witnesses era) (Map (ScriptHash StandardCrypto) (Alonzo.Script era))
    , Ledger.Script era ~ Alonzo.Script era
    , Ledger.AuxiliaryData era ~ Alonzo.AuxiliaryData era
    , ShelleyTx.ValidateScript era
    ) => Ledger.Tx era -> [TxScript]
getScripts tx =
      mkTxScript
        <$> (Map.toList (getField @"txscripts" $ getField @"wits" tx)
            ++ getAuxScripts (getField @"auxiliaryData" tx))
  where
    getAuxScripts
        :: ShelleyMa.StrictMaybe (Alonzo.AuxiliaryData era)
        -> [(ScriptHash StandardCrypto, Alonzo.Script era)]
    getAuxScripts maux =
      case strictMaybeToMaybe maux of
        Nothing -> []
        Just (Alonzo.AuxiliaryData _ scrs) ->
          map (\scr -> (Ledger.hashScript @era scr, scr)) $ toList scrs

resolveRedeemers ::
    forall era.
    ( Ledger.Crypto era ~ StandardCrypto, Ledger.Era era,
      HasField "inputs" (Ledger.TxBody era) (Set (ShelleyTx.TxIn StandardCrypto)),
      HasField "wdrls" (Ledger.TxBody era) (Shelley.Wdrl StandardCrypto),
      HasField "certs" (Ledger.TxBody era) (StrictSeq (Shelley.DCert StandardCrypto)),
      HasField "txrdmrs" (Ledger.Witnesses era) (Alonzo.Redeemers era)
    ) =>
    Alonzo.Prices -> Ledger.Tx era -> (RedeemerMaps, [(Word64, TxRedeemer)])
resolveRedeemers prices tx =
    mkRdmrAndUpdateRec (initRedeemersMaps, [])
      $ zip [0..] $ Map.toList (Alonzo.unRedeemers (getField @"txrdmrs" (getField @"wits" tx)))
  where
    txBody = getField @"body" tx

    withdrawalsNoRedeemers :: Map (Shelley.RewardAcnt StandardCrypto) TxWithdrawal
    withdrawalsNoRedeemers =
      Map.mapWithKey (curry mkTxWithdrawal)
        $ Shelley.unWdrl $ getField @"wdrls" txBody

    txCertsNoRedeemers :: [(Shelley.DCert StandardCrypto, TxCertificate)]
    txCertsNoRedeemers = zipWith (\n dcert -> (dcert, mkTxCertificate n dcert)) [0..]
      $ toList $ getField @"certs" txBody

    txInsMissingRedeemer :: Map (ShelleyTx.TxIn StandardCrypto) TxIn
    txInsMissingRedeemer = Map.fromList $ fmap (\inp -> (inp, fromTxIn inp)) $ toList $ getField @"inputs" txBody

    initRedeemersMaps :: RedeemerMaps
    initRedeemersMaps = RedeemerMaps withdrawalsNoRedeemers txCertsNoRedeemers txInsMissingRedeemer

    mkRdmrAndUpdateRec :: (RedeemerMaps, [(Word64, TxRedeemer)])
                       -> [(Word64, (Alonzo.RdmrPtr, (Alonzo.Data era, ExUnits)))]
                       -> (RedeemerMaps, [(Word64, TxRedeemer)])
    mkRdmrAndUpdateRec (rdmrMaps, rdmrsAcc) [] = (rdmrMaps, reverse rdmrsAcc)
    mkRdmrAndUpdateRec (rdmrMaps, rdmrsAcc) ((rdmrIx, rdmr) : rest) =
      let (txRdmr, rdmrMaps') = handleRedeemer rdmrIx rdmr rdmrMaps
      in mkRdmrAndUpdateRec (rdmrMaps', (rdmrIx, txRdmr) : rdmrsAcc) rest

    handleRedeemer :: Word64 -> (Alonzo.RdmrPtr, (Alonzo.Data era, ExUnits))
                   -> RedeemerMaps -> (TxRedeemer, RedeemerMaps)
    handleRedeemer rdmrIx (ptr@(Alonzo.RdmrPtr tag index), (dt, exUnits)) rdmrMps =
        (txRdmr, rdmrMps')
      where
        (rdmrMps', mScript) = case strictMaybeToMaybe (Alonzo.rdptrInv txBody ptr) of
          Just (Alonzo.Minting policyId) -> (rdmrMps, Just $ Right $ unScriptHash $ policyID policyId)
          Just (Alonzo.Spending txIn) -> handleTxInPtr rdmrIx txIn rdmrMps
          Just (Alonzo.Rewarding rwdAcnt) -> handleRewardPtr rdmrIx rwdAcnt rdmrMps
          Just prp@(Alonzo.Certifying dcert) -> case strictMaybeToMaybe (Alonzo.rdptr txBody prp) of
            Just ptr' | ptr == ptr' -> handleCertPtr rdmrIx dcert rdmrMps
            _ -> (rdmrMps, Nothing)
          Nothing -> (rdmrMps, Nothing)

        txRdmr = TxRedeemer
          { txRedeemerMem = fromIntegral $ exUnitsMem exUnits
          , txRedeemerSteps = fromIntegral $ exUnitsSteps exUnits
          , txRedeemerFee = txscriptfee prices exUnits
          , txRedeemerPurpose = tag
          , txRedeemerIndex = index
          , txRedeemerScriptHash = mScript
          , txRedeemerData = PlutusData (dataHashToBytes $ Alonzo.hashData dt) (encodeData dt)
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

handleCertPtr :: Word64 -> Shelley.DCert StandardCrypto -> RedeemerMaps -> (RedeemerMaps, Maybe (Either TxIn ByteString))
handleCertPtr rdmrIx dcert mps =
    (mps {rmCerts = map f (rmCerts mps)}, Right <$> scriptHashCert dcert)
  where
    f (dcert', cert) | dcert' == dcert = (dcert, cert {txcRedeemerIndex = Just rdmrIx})
    f x = x

data RedeemerMaps =
  RedeemerMaps
    { rmWdrl :: Map (Shelley.RewardAcnt StandardCrypto) TxWithdrawal
    , rmCerts :: [(Shelley.DCert StandardCrypto, TxCertificate)]
    , rmInps :: Map (ShelleyTx.TxIn StandardCrypto) TxIn
    }

mkTxScript
    :: Ledger.Crypto era ~ StandardCrypto
    => (ScriptHash StandardCrypto, Alonzo.Script era) -> TxScript
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
        Alonzo.PlutusScript {} -> Plutus

    timelockJsonScript :: Maybe ByteString
    timelockJsonScript =
      case script of
        Alonzo.TimelockScript s ->
            Just . LBS.toStrict . Aeson.encode $ Api.fromAllegraTimelock Api.TimeLocksInSimpleScriptV2 s
        Alonzo.PlutusScript {} -> Nothing

    plutusCborScript :: Maybe ByteString
    plutusCborScript =
      case script of
        Alonzo.TimelockScript {} -> Nothing
        Alonzo.PlutusScript _lang s ->
          Just . Api.serialiseToCBOR . Api.PlutusScript Api.PlutusScriptV1 $ Api.PlutusScriptSerialised s

getPlutusSizes ::
    ( HasField "wits" tx (Alonzo.TxWitness era)
    , Ledger.Script era ~ Alonzo.Script era
    , Ledger.Era era
    ) => tx -> [Word64]
getPlutusSizes tx = mapMaybe getPlutusScriptSize $ Map.elems $ Alonzo.txscripts $ getField @"wits" tx

-- | Returns Nothing for non-plutus scripts.
getPlutusScriptSize :: Alonzo.Script era -> Maybe Word64
getPlutusScriptSize script =
  case script of
    Alonzo.TimelockScript {} ->  Nothing
    Alonzo.PlutusScript _lang sbs -> Just $ fromIntegral (SBS.length sbs)

encodeData :: Alonzo.Data era -> ByteString
encodeData dt = LBS.toStrict $ Aeson.encode $
    Api.scriptDataToJson Api.ScriptDataJsonDetailedSchema $ Api.fromAlonzoData dt

txDataWitness ::
    (HasField "wits" tx (Alonzo.TxWitness era), Ledger.Crypto era ~ StandardCrypto)
    => tx -> [PlutusData]
txDataWitness tx =
    mkTxData <$> Map.toList (Alonzo.unTxDats $ Alonzo.txdats' (getField @"wits" tx))

mkTxData :: (Ledger.DataHash StandardCrypto, Alonzo.Data era) -> PlutusData
mkTxData (dataHash, dt) = PlutusData (dataHashToBytes dataHash) (encodeData dt)

extraKeyWits :: HasField "reqSignerHashes" (Ledger.TxBody era) (Set (Ledger.KeyHash d c))
              => Ledger.TxBody era -> [ByteString]
extraKeyWits txBody = Set.toList $
  Set.map (\(Ledger.KeyHash h) -> Crypto.hashToBytes h) $
  getField @"reqSignerHashes" txBody

dataHashToBytes :: Ledger.DataHash crypto -> ByteString
dataHashToBytes dataHash = Crypto.hashToBytes (Ledger.extractHash dataHash)

elemAtSet :: forall a. Ord a => Word64 -> Set a -> Maybe a
elemAtSet n set =
    snd <$> find (\(index, _a) -> index == Just n) (Set.toList setWithIndexes)
  where
    setWithIndexes :: Set (Maybe Word64, a)
    setWithIndexes = Set.map (\a -> (strictMaybeToMaybe $ Alonzo.indexOf a set, a)) set

scriptHashAcnt :: Shelley.RewardAcnt StandardCrypto -> Maybe ByteString
scriptHashAcnt rewardAddr = getCredentialScriptHash $ Ledger.getRwdCred rewardAddr

scriptHashCert :: Shelley.DCert StandardCrypto -> Maybe ByteString
scriptHashCert cert =
  case cert of
    Shelley.DCertDeleg (Shelley.DeRegKey cred) ->
      getCredentialScriptHash cred
    Shelley.DCertDeleg (Shelley.Delegate (Shelley.Delegation cred _)) ->
      getCredentialScriptHash cred
    _ -> Nothing
