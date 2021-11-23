{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Era.Shelley.Generic.Tx
  ( Tx (..)
  , TxCertificate (..)
  , TxIn (..)
  , TxOut (..)
  , TxRedeemer (..)
  , TxWithdrawal (..)
  , TxScript (..)
  , TxDatum (..)
  , fromShelleyTx
  , fromAllegraTx
  , fromMaryTx
  , fromAlonzoTx
  ) where

import           Cardano.Prelude

import           Cardano.Api.Shelley (TxMetadataValue (..))
import qualified Cardano.Api.Shelley as Api

import qualified Cardano.Crypto.Hash as Crypto

import           Cardano.Db (ScriptType (..))

import           Cardano.DbSync.Era.Shelley.Generic.Metadata
import           Cardano.DbSync.Era.Shelley.Generic.ParamProposal
import           Cardano.DbSync.Era.Shelley.Generic.Util
import           Cardano.DbSync.Era.Shelley.Generic.Witness

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Allegra as Allegra
import           Cardano.Ledger.Alonzo (AlonzoEra)
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import           Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Tag (..), txscriptfee)
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import           Cardano.Ledger.Alonzo.Tx (ValidatedTx (..))
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import           Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Mary as Mary
import           Cardano.Ledger.Mary.Value (AssetName, PolicyID, Value (..))
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.Shelley.CompactAddr as Ledger
import           Cardano.Ledger.Shelley.Scripts (ScriptHash)
import qualified Cardano.Ledger.Shelley.Scripts as Shelley
import qualified Cardano.Ledger.Shelley.Tx as ShelleyTx
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as ShelleyMa
import qualified Cardano.Ledger.ShelleyMA.TxBody as ShelleyMa

import           Cardano.Slotting.Slot (SlotNo (..))

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import           Data.Maybe.Strict (strictMaybeToMaybe)
import           Data.MemoBytes (MemoBytes (..))
import           Data.Sequence.Strict (StrictSeq)
import qualified Data.Set as Set

import           Ouroboros.Consensus.Cardano.Block (StandardAllegra, StandardAlonzo, StandardCrypto,
                   StandardMary, StandardShelley)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBasedEra)


data Tx = Tx
  { txHash :: !ByteString
  , txBlockIndex :: !Word64
  , txSize :: !Word64
  , txValidContract :: !Bool
  , txInputs :: ![TxIn]
  , txCollateralInputs :: ![TxIn]
  , txOutputs :: ![TxOut]
  , txFees :: !Coin
  , txOutSum :: !Coin
  , txInvalidBefore :: !(Maybe SlotNo)
  , txInvalidHereafter :: !(Maybe SlotNo)
  , txWithdrawalSum :: !Coin
  , txMetadata :: !(Maybe (Map Word64 TxMetadataValue))
  , txCertificates :: ![TxCertificate]
  , txWithdrawals :: ![TxWithdrawal]
  , txParamProposal :: ![ParamProposal]
  , txMint :: !(Value StandardCrypto)
  , txRedeemer :: [TxRedeemer]
  , txData :: [TxDatum]
  , txScriptSizes :: [Word64] -- this contains only the sizes of plutus scripts in witnesses
  , txScripts :: [TxScript]
  , txScriptsFee :: Coin
  }

data TxCertificate = TxCertificate
  { txcRedeemerIndex :: !(Maybe Word64)
  , txcIndex :: !Word16
  , txcCert :: !(Shelley.DCert StandardCrypto)
  }

data TxWithdrawal = TxWithdrawal
  { txwRedeemerIndex :: !(Maybe Word64)
  , txwRewardAccount :: !(Shelley.RewardAcnt StandardCrypto)
  , txwAmount :: !Coin
  }

data TxIn = TxIn
  { txInHash :: !ByteString
  , txInIndex :: !Word16
  , txInRedeemerIndex :: !(Maybe Word64) -- This only has a meaning for Alonzo.
  }

data TxOut = TxOut
  { txOutIndex :: !Word16
  , txOutAddress :: !(Ledger.Addr StandardCrypto)
  , txOutAddressRaw :: !ByteString
  , txOutAdaValue :: !Coin
  , txOutMaValue :: !(Map (PolicyID StandardCrypto) (Map AssetName Integer))
  , txOutDataHash :: !(Maybe ByteString)
  }

data TxRedeemer = TxRedeemer
  { txRedeemerMem :: !Word64
  , txRedeemerSteps :: !Word64
  , txRedeemerPurpose :: !Tag
  , txRedeemerFee :: !Coin
  , txRedeemerIndex :: !Word64
  , txRedeemerScriptHash :: Maybe (Either TxIn ByteString)
  , txRedeemerDatum :: TxDatum
  }

data TxScript = TxScript
  { txScriptHash :: !ByteString
  , txScriptType :: ScriptType
  , txScriptPlutusSize :: Maybe Word64
  , txScriptJson :: Maybe ByteString
  , txScriptCBOR :: Maybe ByteString
  }

data TxDatum = TxDatum
  { txDatumHash :: !ByteString
  , txDatumValue :: !ByteString -- we turn this into json later.
  }

fromAllegraTx :: (Word64, ShelleyTx.Tx StandardAllegra) -> Tx
fromAllegraTx (blkIndex, tx) =
    Tx
      { txHash = txHashId tx
      , txBlockIndex = blkIndex
      , txSize = fromIntegral $ getField @"txsize" tx
      , txValidContract = True
      , txInputs = map (fromTxIn Nothing) (toList $ ShelleyMa.inputs rawTxBody)
      , txCollateralInputs = [] -- Allegra does not have collateral inputs
      , txOutputs = zipWith fromTxOut [0 .. ] $ toList (ShelleyMa.outputs rawTxBody)
      , txFees = ShelleyMa.txfee rawTxBody
      , txOutSum = Coin . sum $ map txOutValue (ShelleyMa.outputs rawTxBody)
      , txInvalidBefore = strictMaybeToMaybe . ShelleyMa.invalidBefore $ ShelleyMa.vldt rawTxBody
      , txInvalidHereafter = strictMaybeToMaybe . ShelleyMa.invalidHereafter $ ShelleyMa.vldt rawTxBody
      , txWithdrawalSum = Coin . sum . map unCoin . Map.elems
                            . Shelley.unWdrl $ ShelleyMa.wdrls rawTxBody
      , txMetadata = fromAllegraMetadata <$> txMeta tx
      , txCertificates = zipWith (TxCertificate Nothing) [0..] (toList $ ShelleyMa.certs rawTxBody)
      , txWithdrawals = map (mkTxWithdrawal Nothing) (Map.toList . Shelley.unWdrl $ ShelleyMa.wdrls rawTxBody)
      , txParamProposal = maybe [] (convertParamProposal (Allegra Standard)) $ strictMaybeToMaybe (ShelleyMa.update rawTxBody)
      , txMint = mempty     -- Allegra does not support Multi-Assets
      , txRedeemer = []     -- Allegra does not support Redeemers
      , txData = []
      , txScriptSizes = []    -- Allegra does not support scripts
      , txScripts = scripts
      , txScriptsFee = Coin 0 -- Allegra does not support scripts
      }
  where
    fromTxOut :: Word16 -> ShelleyTx.TxOut StandardAllegra -> TxOut
    fromTxOut index txOut =
      TxOut
        { txOutIndex = index
        , txOutAddress = addr
        , txOutAddressRaw = SBS.fromShort bs
        , txOutAdaValue = ada
        , txOutMaValue = mempty -- Allegra does not support Multi-Assets
        , txOutDataHash = mempty -- Allegra does not support scripts
        }
      where
        ShelleyTx.TxOutCompact (Ledger.UnsafeCompactAddr bs) _ = txOut
        -- This pattern match also does the deserialisation of the address
        ShelleyTx.TxOut addr ada = txOut


    txMeta :: ShelleyTx.Tx StandardAllegra -> Maybe (ShelleyMa.AuxiliaryData StandardAllegra)
    txMeta (ShelleyTx.Tx _body _wit md) = strictMaybeToMaybe md

    txOutValue :: ShelleyTx.TxOut StandardAllegra -> Integer
    txOutValue (ShelleyTx.TxOut _ (Coin coin)) = coin

    rawTxBody :: ShelleyMa.TxBodyRaw StandardAllegra
    rawTxBody =
      case tx of
        (ShelleyTx.Tx (ShelleyMa.TxBodyConstr txBody) _wit _md) -> memotype txBody

    scripts :: [TxScript]
    scripts =
      mkTxScript
        <$> (Map.toList (ShelleyTx.scriptWits $ getField @"wits" tx)
            ++ getAuxScripts (getField @"auxiliaryData" tx))

    getAuxScripts
        :: ShelleyMa.StrictMaybe (Allegra.AuxiliaryData StandardAllegra)
        -> [(ScriptHash StandardCrypto, Allegra.Script StandardAllegra)]
    getAuxScripts maux =
      case strictMaybeToMaybe maux of
        Nothing -> []
        Just (ShelleyMa.AuxiliaryData _ scrs) ->
          map (\scr -> (Ledger.hashScript @StandardAllegra scr, scr)) $ toList scrs

    mkTxScript :: (ScriptHash StandardCrypto, Allegra.Script StandardAllegra) -> TxScript
    mkTxScript (hsh, script) = TxScript
      { txScriptHash = unScriptHash hsh
      , txScriptType = Timelock
      , txScriptPlutusSize = Nothing
      , txScriptJson =
          Just . LBS.toStrict . Aeson.encode
            $ Api.fromAllegraTimelock Api.TimeLocksInSimpleScriptV2 script
      , txScriptCBOR = Nothing
      }

fromShelleyTx :: (Word64, ShelleyTx.Tx StandardShelley) -> Tx
fromShelleyTx (blkIndex, tx) =
    Tx
      { txHash = txHashId tx
      , txBlockIndex = blkIndex
      , txSize = fromIntegral $ getField @"txsize" tx
      , txValidContract = True
      , txInputs = map (fromTxIn Nothing) (toList . Shelley._inputs $ ShelleyTx.body tx)
      , txCollateralInputs = [] -- Shelley does not have collateral inputs
      , txOutputs = zipWith fromTxOut [0 .. ] $ toList (Shelley._outputs $ ShelleyTx.body tx)
      , txFees = Shelley._txfee (ShelleyTx.body tx)
      , txOutSum = Coin . sum $ map txOutValue (Shelley._outputs $ ShelleyTx.body tx)
      , txInvalidBefore = Nothing
      , txInvalidHereafter = Just $ Shelley._ttl (ShelleyTx.body tx)
      , txWithdrawalSum = Coin . sum . map unCoin . Map.elems
                            . Shelley.unWdrl $ Shelley._wdrls (ShelleyTx.body tx)
      , txMetadata = fromShelleyMetadata <$> strictMaybeToMaybe (getField @"auxiliaryData" tx)
      , txCertificates = zipWith (TxCertificate Nothing) [0..] (toList . Shelley._certs $ ShelleyTx.body tx)
      , txWithdrawals = map (mkTxWithdrawal Nothing) (Map.toList . Shelley.unWdrl . Shelley._wdrls $ ShelleyTx.body tx)
      , txParamProposal = maybe [] (convertParamProposal (Shelley Standard)) $ strictMaybeToMaybe (ShelleyTx._txUpdate $ ShelleyTx.body tx)
      , txMint = mempty     -- Shelley does not support Multi-Assets
      , txRedeemer = []     -- Shelley does not support Redeemer
      , txData = []
      , txScriptSizes = []    -- Shelley does not support scripts
      , txScripts = scripts
      , txScriptsFee = Coin 0 -- Shelley does not support scripts
      }
  where
    fromTxOut :: Word16 -> ShelleyTx.TxOut StandardShelley -> TxOut
    fromTxOut index txOut =
      TxOut
        { txOutIndex = index
        , txOutAddress = addr
        , txOutAddressRaw = SBS.fromShort bs
        , txOutAdaValue = ada
        , txOutMaValue = mempty -- Shelley does not support Multi-Assets
        , txOutDataHash = mempty -- Shelley does not support scripts
        }
      where
        ShelleyTx.TxOutCompact (Ledger.UnsafeCompactAddr bs) _ = txOut
        -- This pattern match also does the deserialisation of the address
        ShelleyTx.TxOut addr ada = txOut

    txOutValue :: ShelleyTx.TxOut StandardShelley -> Integer
    txOutValue (ShelleyTx.TxOut _ (Coin coin)) = coin

    scripts :: [TxScript]
    scripts =
      mkTxScript <$> Map.toList (ShelleyTx.scriptWits $ getField @"wits" tx)

    mkTxScript :: (ScriptHash StandardCrypto, Shelley.MultiSig StandardCrypto) -> TxScript
    mkTxScript (hsh, script) = TxScript
      { txScriptHash = unScriptHash hsh
      , txScriptType = MultiSig
      , txScriptPlutusSize = Nothing
      , txScriptJson = Just . LBS.toStrict . Aeson.encode $ Api.fromShelleyMultiSig script
      , txScriptCBOR = Nothing
      }

fromMaryTx :: (Word64, ShelleyTx.Tx StandardMary) -> Tx
fromMaryTx (blkIndex, tx) =
    Tx
      { txHash = txHashId tx
      , txBlockIndex = blkIndex
      , txSize = fromIntegral $ getField @"txsize" tx
      , txValidContract = True
      , txInputs = map (fromTxIn Nothing) (toList . ShelleyMa.inputs $ unTxBodyRaw tx)
      , txCollateralInputs = [] -- Mary does not have collateral inputs
      , txOutputs = zipWith fromTxOut [0 .. ] $ toList (ShelleyMa.outputs $ unTxBodyRaw tx)
      , txFees = ShelleyMa.txfee (unTxBodyRaw tx)
      , txOutSum = Coin . sum $ map txOutValue (ShelleyMa.outputs $ unTxBodyRaw tx)
      , txInvalidBefore = strictMaybeToMaybe . ShelleyMa.invalidBefore $ ShelleyMa.vldt (unTxBodyRaw tx)
      , txInvalidHereafter = strictMaybeToMaybe . ShelleyMa.invalidHereafter $ ShelleyMa.vldt (unTxBodyRaw tx)
      , txWithdrawalSum = Coin . sum . map unCoin . Map.elems
                            . Shelley.unWdrl $ ShelleyMa.wdrls (unTxBodyRaw tx)
      , txMetadata = fromMaryMetadata <$> txMeta tx
      , txCertificates = zipWith (TxCertificate Nothing) [0..] (toList . ShelleyMa.certs $ unTxBodyRaw tx)
      , txWithdrawals = map (mkTxWithdrawal Nothing) (Map.toList . Shelley.unWdrl . ShelleyMa.wdrls $ unTxBodyRaw tx)
      , txParamProposal = maybe [] (convertParamProposal (Mary Standard)) $ strictMaybeToMaybe (ShelleyMa.update $ unTxBodyRaw tx)
      , txMint = ShelleyMa.mint $ unTxBodyRaw tx
      , txRedeemer = []     -- Mary does not support Redeemer
      , txData = []
      , txScriptSizes = []    -- Mary does not support scripts
      , txScripts = scripts
      , txScriptsFee = Coin 0 -- Mary does not support scripts
      }
  where
    fromTxOut :: Word16 -> ShelleyTx.TxOut StandardMary -> TxOut
    fromTxOut index txOut =
      TxOut
        { txOutIndex = index
        , txOutAddress = addr
        , txOutAddressRaw = SBS.fromShort bs
        , txOutAdaValue = Coin ada
        , txOutMaValue = maMap
        , txOutDataHash = mempty -- Mary does not support scripts
        }
      where
        ShelleyTx.TxOutCompact (Ledger.UnsafeCompactAddr bs) _ = txOut
        -- This pattern match also does the deserialisation of the address
        ShelleyTx.TxOut addr (Value ada maMap) = txOut

    txMeta :: ShelleyTx.Tx StandardMary -> Maybe (ShelleyMa.AuxiliaryData StandardMary)
    txMeta (ShelleyTx.Tx _body _wit md) = strictMaybeToMaybe md

    txOutValue :: ShelleyTx.TxOut StandardMary -> Integer
    txOutValue (ShelleyTx.TxOut _ (Value coin _ma)) = coin

    unTxBodyRaw :: ShelleyTx.Tx StandardMary -> ShelleyMa.TxBodyRaw StandardMary
    unTxBodyRaw (ShelleyTx.Tx (ShelleyMa.TxBodyConstr txBody) _wit _md) = memotype txBody

    scripts :: [TxScript]
    scripts =
      mkTxScript
        <$> (Map.toList (ShelleyTx.scriptWits $ getField @"wits" tx)
            ++ getAuxScripts (getField @"auxiliaryData" tx))

    getAuxScripts
        :: ShelleyMa.StrictMaybe (ShelleyMa.AuxiliaryData StandardMary)
        -> [(ScriptHash StandardCrypto, Mary.Script StandardMary)]
    getAuxScripts maux =
      case strictMaybeToMaybe maux of
        Nothing -> []
        Just (ShelleyMa.AuxiliaryData _ scrs) ->
          map (\scr -> (Ledger.hashScript @StandardMary scr, scr)) $ toList scrs

    mkTxScript :: (ScriptHash StandardCrypto, Mary.Script StandardMary) -> TxScript
    mkTxScript (hsh, script) = TxScript
      { txScriptHash = unScriptHash hsh
      , txScriptType = Timelock
      , txScriptPlutusSize = Nothing
      , txScriptJson =
          Just . LBS.toStrict . Aeson.encode
            $ Api.fromAllegraTimelock Api.TimeLocksInSimpleScriptV2 script
      , txScriptCBOR = Nothing
      }

fromAlonzoTx :: Ledger.PParams StandardAlonzo -> (Word64, Ledger.Tx StandardAlonzo) -> Tx
fromAlonzoTx pp (blkIndex, tx) =
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

    getDataHash :: Ledger.SafeHash crypto a -> ByteString
    getDataHash dataHash = Crypto.hashToBytes (Ledger.extractHash dataHash)

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
    minFees = txscriptfee (Alonzo._prices pp) $ Alonzo.totExUnits tx

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
      , txRedeemerFee = txscriptfee (Alonzo._prices pp) exUnits
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


-- -------------------------------------------------------------------------------------------------

fromTxIn :: Maybe Word64 -> ShelleyTx.TxIn StandardCrypto -> TxIn
fromTxIn setIndex (ShelleyTx.TxIn (ShelleyTx.TxId txid) index) =
  TxIn
    { txInHash = Crypto.hashToBytes $ Ledger.extractHash txid
    , txInIndex = fromIntegral index
    , txInRedeemerIndex = setIndex
    }

mkTxWithdrawal :: Maybe Word64 -> (Shelley.RewardAcnt StandardCrypto, Coin) -> TxWithdrawal
mkTxWithdrawal rIndex (ra, c) =
  TxWithdrawal
    { txwRedeemerIndex = rIndex
    , txwRewardAccount = ra
    , txwAmount = c
    }

txHashId :: (Ledger.Crypto era ~ StandardCrypto, ShelleyBasedEra era) => ShelleyTx.Tx era -> ByteString
txHashId = Crypto.hashToBytes . Ledger.extractHash . Ledger.hashAnnotated . ShelleyTx.body

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
