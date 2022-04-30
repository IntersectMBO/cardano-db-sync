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
import           Cardano.Ledger.Alonzo (AlonzoEra)
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import           Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Tag (..), txscriptfee)
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import           Cardano.Ledger.Alonzo.Tx (ValidatedTx (..))
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import           Cardano.Ledger.BaseTypes
import           Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.CompactAddress as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import           Cardano.Ledger.Mary.Value (Value (..))
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
import           Cardano.DbSync.Era.Shelley.Generic.Util
import           Cardano.DbSync.Era.Shelley.Generic.Tx.Shelley (mkTxWithdrawal)
import           Cardano.DbSync.Era.Shelley.Generic.Tx.Types
import           Cardano.DbSync.Era.Shelley.Generic.Witness


fromAlonzoTx :: Alonzo.Prices -> (Word64, Ledger.Tx StandardAlonzo) -> Tx
fromAlonzoTx prices (blkIndex, tx) =
    Tx
      { txHash = Crypto.hashToBytes . Ledger.extractHash $ Ledger.hashAnnotated txBody
      , txBlockIndex = blkIndex
      , txSize = fromIntegral $ getField @"txsize" tx
      , txValidContract = isValid2
      , txInputs = txIns
      , txCollateralInputs = map (fromTxIn Nothing) . toList $ getField @"collateral" txBody
      , txOutputs =
          if not isValid2
            then []
            else zipWith fromTxOut [0 .. ] . toList $ getField @"outputs" txBody
      , txFees = getField @"txfee" txBody
      , txOutSum =
          if not isValid2
            then Coin 0
            else Coin . sum $ map txOutValue (getField @"outputs" txBody)
      , txInvalidBefore = strictMaybeToMaybe . ShelleyMa.invalidBefore $ getField @"vldt" txBody
      , txInvalidHereafter = strictMaybeToMaybe . ShelleyMa.invalidHereafter $ getField @"vldt" txBody
      , txWithdrawalSum = Coin . sum . map unCoin . Map.elems
                            . Shelley.unWdrl $ getField @"wdrls" txBody
      , txMetadata = fromAlonzoMetadata <$> strictMaybeToMaybe (getField @"auxiliaryData" tx)
      , txCertificates = txCerts
      , txWithdrawals = txWdrls
      , txParamProposal = maybe [] (convertParamProposal (Alonzo Standard)) $ strictMaybeToMaybe (getField @"update" txBody)
      , txMint = getField @"mint" txBody
      , txRedeemer = redeemers
      , txData = txDataWitness
      , txScriptSizes = sizes
      , txScripts = scripts
      , txScriptsFee = minFees
      , txExtraKeyWitnesses = extraKeyWits txBody
      }
  where
    fromTxOut :: Word16 -> Alonzo.TxOut StandardAlonzo -> TxOut
    fromTxOut index txOut =
      TxOut
        { txOutIndex = index
        , txOutAddress = addr
        , txOutAddressRaw = SBS.fromShort caddr
        , txOutAdaValue = Coin ada
        , txOutMaValue = maMap
        , txOutDataHash = getDataHash <$> strictMaybeToMaybe mDataHash
        }
      where
        caddr :: SBS.ShortByteString
        caddr =
          case txOut of
            Alonzo.TxOutCompact (Ledger.UnsafeCompactAddr bs) _-> bs
            Alonzo.TxOutCompactDH (Ledger.UnsafeCompactAddr bs) _ _-> bs

        -- This pattern match also does the deserialisation of the address
        Alonzo.TxOut addr (Value ada maMap) mDataHash = txOut


    txBody :: Ledger.TxBody StandardAlonzo
    txBody = getField @"body" tx

    sizes :: [Word64]
    sizes = mapMaybe getScriptSize $ toList $ Ledger.txscripts $ getField @"wits" tx

    scripts :: [TxScript]
    scripts =
      mkTxScript
        <$> (Map.toList (getField @"txscripts" $ getField @"wits" tx)
            ++ getAuxScripts (getField @"auxiliaryData" tx))

    getAuxScripts
        :: ShelleyMa.StrictMaybe (Alonzo.AuxiliaryData StandardAlonzo)
        -> [(ScriptHash StandardCrypto, Alonzo.Script (AlonzoEra StandardCrypto))]
    getAuxScripts maux =
      case strictMaybeToMaybe maux of
        Nothing -> []
        Just (Alonzo.AuxiliaryData _ scrs) ->
          map (\scr -> (Ledger.hashScript @StandardAlonzo scr, scr)) $ toList scrs

    mkTxScript :: (ScriptHash StandardCrypto, Alonzo.Script (AlonzoEra StandardCrypto)) -> TxScript
    mkTxScript (hsh, script) =
      TxScript
        { txScriptHash = unScriptHash hsh
        , txScriptType = getScriptType script
        , txScriptPlutusSize = getScriptSize script
        , txScriptJson = timelockJsonScript script
        , txScriptCBOR = plutusCborScript script
        }

    timelockJsonScript :: Alonzo.Script (AlonzoEra StandardCrypto) -> Maybe ByteString
    timelockJsonScript script =
      case script of
        Alonzo.TimelockScript s ->
            Just . LBS.toStrict . Aeson.encode $ Api.fromAllegraTimelock Api.TimeLocksInSimpleScriptV2 s
        Alonzo.PlutusScript {} -> Nothing

    plutusCborScript :: Alonzo.Script (AlonzoEra StandardCrypto) -> Maybe ByteString
    plutusCborScript script =
      case script of
        Alonzo.TimelockScript {} -> Nothing
        Alonzo.PlutusScript _lang s ->
          Just . Api.serialiseToCBOR . Api.PlutusScript Api.PlutusScriptV1 $ Api.PlutusScriptSerialised s

    getScriptSize :: Alonzo.Script (AlonzoEra StandardCrypto) -> Maybe Word64
    getScriptSize script =
      case script of
        Alonzo.TimelockScript {} ->  Nothing
        Alonzo.PlutusScript _lang sbs -> Just $ fromIntegral (SBS.length sbs)

    getScriptType :: Alonzo.Script (AlonzoEra StandardCrypto) -> ScriptType
    getScriptType script =
      case script of
        Alonzo.TimelockScript {} -> Timelock
        Alonzo.PlutusScript {} -> Plutus

    minFees :: Coin
    minFees = txscriptfee prices $ Alonzo.totExUnits tx

    txOutValue :: Alonzo.TxOut StandardAlonzo -> Integer
    txOutValue (Alonzo.TxOut _addr (Value coin _ma) _dataHash) = coin

    certificates :: StrictSeq (Shelley.DCert StandardCrypto)
    certificates = getField @"certs" txBody

    mkTxCertificate :: Word16 -> Shelley.DCert StandardCrypto -> TxCertificate
    mkTxCertificate n cert =
      TxCertificate
        { txcIndex = n
        , txcRedeemerIndex = strictMaybeToMaybe $ Alonzo.indexOf cert certificates
        , txcCert = cert
        }

    txCerts :: [TxCertificate]
    txCerts = zipWith mkTxCertificate [0..] (toList certificates)

    withdrawals :: Map (Shelley.RewardAcnt StandardCrypto) Coin
    withdrawals = Shelley.unWdrl $ getField @"wdrls" txBody

    mkTxWithdrawal' :: (Shelley.RewardAcnt StandardCrypto, Coin) -> TxWithdrawal
    mkTxWithdrawal' (acnt, coin) =
      mkTxWithdrawal (strictMaybeToMaybe $ Alonzo.indexOf acnt withdrawals) (acnt, coin)

    txWdrls :: [TxWithdrawal]
    txWdrls = map mkTxWithdrawal' (Map.toList withdrawals)

    -- This is true if second stage contract validation passes.
    isValid2 :: Bool
    isValid2 =
      case Alonzo.isValid tx of
        Alonzo.IsValid x -> x

    txIns :: [TxIn]
    txIns =
      if isValid2 then
        let inputsSet = getField @"inputs" txBody
            withIndex txIn = fromTxIn (strictMaybeToMaybe $ Alonzo.indexOf txIn inputsSet) txIn
        in map withIndex $ toList inputsSet
      else
          let inputsSet = getField @"collateral" txBody
          in map (fromTxIn Nothing) $ toList inputsSet

    redeemers :: [TxRedeemer]
    redeemers =
      mkRedeemer <$> Map.toList (Ledger.unRedeemers (getField @"txrdmrs" (getField @"wits" tx)))

    mkRedeemer :: (Ledger.RdmrPtr, (Alonzo.Data StandardAlonzo, ExUnits)) -> TxRedeemer
    mkRedeemer (ptr@(Ledger.RdmrPtr tag index), (dt, exUnits)) = TxRedeemer
      { txRedeemerMem = fromIntegral $ exUnitsMem exUnits
      , txRedeemerSteps = fromIntegral $ exUnitsSteps exUnits
      , txRedeemerFee = txscriptfee prices exUnits
      , txRedeemerPurpose = tag
      , txRedeemerIndex = index
      , txRedeemerScriptHash = findScriptHash ptr
      , txRedeemerDatum = TxDatum (getDataHash $ Alonzo.hashData dt) (encodeData dt)
      }

    encodeData :: Alonzo.Data StandardAlonzo -> ByteString
    encodeData dt = LBS.toStrict $ Aeson.encode $
      Api.scriptDataToJson Api.ScriptDataJsonDetailedSchema $ Api.fromAlonzoData dt

    txDataWitness :: [TxDatum]
    txDataWitness =
      mkTxDatum <$> Map.toList (Ledger.unTxDats $ Ledger.txdats' (getField @"wits" tx))

    mkTxDatum :: (Ledger.SafeHash StandardCrypto a, Alonzo.Data StandardAlonzo) -> TxDatum
    mkTxDatum (dataHash, dt) = TxDatum (getDataHash dataHash) (encodeData dt)

    -- For 'Spend' script, we need to resolve the 'TxIn' to find the ScriptHash
    -- so we return 'Left TxIn' and resolve it later from the db. In other cases
    -- we can directly find the 'ScriptHash'.
    findScriptHash :: Ledger.RdmrPtr -> Maybe (Either TxIn ByteString)
    findScriptHash (Ledger.RdmrPtr tag index) =
      case tag of
        Spend ->
          -- We always use the real inputs here, instead of the collateral, because
          -- this just helps us find the script hash later, by resolving the input.
          Left . fromTxIn (Just index) <$> elemAtSet index (getField @"inputs" txBody)
        Rewrd ->
          Right <$> (scriptHashAcnt . txwRewardAccount =<< find (\wdrl -> txwRedeemerIndex wdrl == Just index) txWdrls)
        Cert ->
          Right <$> (scriptHashCert . txcCert =<< find (\cert -> txcRedeemerIndex cert == Just index) txCerts)
        Mint ->
          Right . unScriptHash <$> elemAtSet index (getField @"minted" txBody)

extraKeyWits :: HasField "reqSignerHashes" (Ledger.TxBody era) (Set (Ledger.KeyHash d c))
              => Ledger.TxBody era -> [ByteString]
extraKeyWits txBody = Set.toList $
  Set.map (\(Ledger.KeyHash h) -> Crypto.hashToBytes h) $
  getField @"reqSignerHashes" txBody

getDataHash :: Ledger.SafeHash crypto a -> ByteString
getDataHash dataHash = Crypto.hashToBytes (Ledger.extractHash dataHash)

fromTxIn :: Maybe Word64 -> ShelleyTx.TxIn StandardCrypto -> TxIn
fromTxIn setIndex (ShelleyTx.TxIn (ShelleyTx.TxId txid) (TxIx w16)) =
  TxIn
    { txInHash = Crypto.hashToBytes $ Ledger.extractHash txid
    , txInIndex = fromIntegral w16
    , txInRedeemerIndex = setIndex
    }

-- | 'Set.elemAt' is a partial function so we can't use it. This reverses the 'indexOf' of the
-- 'class Indexable elem container'.
elemAtSet :: forall a. Ord a => Word64 -> Set a -> Maybe a
elemAtSet n set =
    snd <$> find (\(index, _a) -> index == Just n) (Set.toList setWithIndexes)
  where
    setWithIndexes :: Set (Maybe Word64, a)
    setWithIndexes = Set.map (\a -> (strictMaybeToMaybe $ Alonzo.indexOf a set, a)) set

scriptHashAcnt :: Shelley.RewardAcnt StandardCrypto -> Maybe ByteString
scriptHashAcnt rewardAddr = getCredentialScriptHash $ Ledger.getRwdCred rewardAddr

-- This mimics 'Ledger.addOnlyCwitness'
scriptHashCert :: Shelley.DCert StandardCrypto -> Maybe ByteString
scriptHashCert cert =
  case cert of
    Shelley.DCertDeleg (Shelley.DeRegKey cred) ->
      getCredentialScriptHash cred
    Shelley.DCertDeleg (Shelley.Delegate (Shelley.Delegation cred _)) ->
      getCredentialScriptHash cred
    _ -> Nothing
