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
  , fromShelleyTx
  , fromAllegraTx
  , fromMaryTx
  , fromAlonzoTx
  ) where

import           Cardano.Prelude

import           Cardano.Api.Shelley (TxMetadataValue (..))

import qualified Cardano.Crypto.Hash as Crypto

import           Cardano.DbSync.Era.Shelley.Generic.Metadata
import           Cardano.DbSync.Era.Shelley.Generic.ParamProposal
import           Cardano.DbSync.Era.Shelley.Generic.Util
import           Cardano.DbSync.Era.Shelley.Generic.Witness

import qualified Cardano.Ledger.Address as Ledger
import           Cardano.Ledger.Alonzo (AlonzoEra)
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import           Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Script (..), Tag (..), txscriptfee)
import           Cardano.Ledger.Alonzo.Tx (ValidatedTx (..))
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import           Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Era as Ledger
import           Cardano.Ledger.Mary.Value (AssetName, PolicyID, Value (..))
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as ShelleyMa
import qualified Cardano.Ledger.ShelleyMA.TxBody as ShelleyMa

import           Cardano.Slotting.Slot (SlotNo (..))

import qualified Data.ByteString.Short as SBS
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import           Data.Maybe.Strict (strictMaybeToMaybe)
import           Data.MemoBytes (MemoBytes (..))
import           Data.Sequence.Strict (StrictSeq)
import qualified Data.Set as Set

import           Ouroboros.Consensus.Cardano.Block (StandardAllegra, StandardAlonzo, StandardCrypto,
                   StandardMary, StandardShelley)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBasedEra)

import           Shelley.Spec.Ledger.Scripts (ScriptHash)
import qualified Shelley.Spec.Ledger.Tx as Shelley
import qualified Shelley.Spec.Ledger.TxBody as Shelley

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
  , txOutAdaValue :: !Coin
  , txOutMaValue :: !(Map (PolicyID StandardCrypto) (Map AssetName Integer))
  }

data TxRedeemer = TxRedeemer
  { txRedeemerMem :: !Word64
  , txRedeemerSteps :: !Word64
  , txRedeemerPurpose :: !Tag
  , txRedeemerFee :: !Coin
  , txRedeemerIndex :: !Word64
  , txRedeemerScriptHash :: Maybe (Either TxIn ByteString)
  }

data TxScript = TxScript
  { txScriptHash :: !ByteString
  , txScriptPlutusSize :: Maybe Word64
  }

fromAllegraTx :: (Word64, Shelley.Tx StandardAllegra) -> Tx
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
      , txCertificates = zipWith (TxCertificate Nothing) [0..] (map coerceCertificate . toList $ ShelleyMa.certs rawTxBody)
      , txWithdrawals = map (mkTxWithdrawal Nothing) (Map.toList . Shelley.unWdrl $ ShelleyMa.wdrls rawTxBody)
      , txParamProposal = maybe [] (convertParamProposal (Allegra Standard)) $ strictMaybeToMaybe (ShelleyMa.update rawTxBody)
      , txMint = mempty     -- Allegra does not support Multi-Assets
      , txRedeemer = []     -- Allegra does not support Redeemers
      , txScriptSizes = []    -- Allegra does not support scripts
      , txScripts = []        -- We don't populate scripts for Allegra
      , txScriptsFee = Coin 0 -- Allegra does not support scripts
      }
  where
    fromTxOut :: Word16 -> Shelley.TxOut StandardAllegra -> TxOut
    fromTxOut index (Shelley.TxOut addr ada) =
      TxOut
        { txOutIndex = index
        , txOutAddress = coerceAddress addr
        , txOutAdaValue = ada
        , txOutMaValue = mempty -- Allegra does not support Multi-Assets
        }

    txMeta :: Shelley.Tx StandardAllegra -> Maybe (ShelleyMa.AuxiliaryData StandardAllegra)
    txMeta (Shelley.Tx _body _wit md) = strictMaybeToMaybe md

    txOutValue :: Shelley.TxOut StandardAllegra -> Integer
    txOutValue (Shelley.TxOut _ (Coin coin)) = coin

    rawTxBody :: ShelleyMa.TxBodyRaw StandardAllegra
    rawTxBody =
      case tx of
        (Shelley.Tx (ShelleyMa.TxBodyConstr txBody) _wit _md) -> memotype txBody


fromShelleyTx :: (Word64, Shelley.Tx StandardShelley) -> Tx
fromShelleyTx (blkIndex, tx) =
    Tx
      { txHash = txHashId tx
      , txBlockIndex = blkIndex
      , txSize = fromIntegral $ getField @"txsize" tx
      , txValidContract = True
      , txInputs = map (fromTxIn Nothing) (toList . Shelley._inputs $ Shelley.body tx)
      , txCollateralInputs = [] -- Shelley does not have collateral inputs
      , txOutputs = zipWith fromTxOut [0 .. ] $ toList (Shelley._outputs $ Shelley.body tx)
      , txFees = Shelley._txfee (Shelley.body tx)
      , txOutSum = Coin . sum $ map txOutValue (Shelley._outputs $ Shelley.body tx)
      , txInvalidBefore = Nothing
      , txInvalidHereafter = Just $ Shelley._ttl (Shelley.body tx)
      , txWithdrawalSum = Coin . sum . map unCoin . Map.elems
                            . Shelley.unWdrl $ Shelley._wdrls (Shelley.body tx)
      , txMetadata = fromShelleyMetadata <$> strictMaybeToMaybe (getField @"auxiliaryData" tx)
      , txCertificates = zipWith (TxCertificate Nothing) [0..] (toList . Shelley._certs $ Shelley.body tx)
      , txWithdrawals = map (mkTxWithdrawal Nothing) (Map.toList . Shelley.unWdrl . Shelley._wdrls $ Shelley.body tx)
      , txParamProposal = maybe [] (convertParamProposal (Shelley Standard)) $ strictMaybeToMaybe (Shelley._txUpdate $ Shelley.body tx)
      , txMint = mempty     -- Shelley does not support Multi-Assets
      , txRedeemer = []     -- Shelley does not support Redeemer
      , txScriptSizes = []    -- Shelley does not support scripts
      , txScripts = []        -- We don't populate scripts for Shelley
      , txScriptsFee = Coin 0 -- Shelley does not support scripts
      }
  where
    fromTxOut :: Word16 -> Shelley.TxOut StandardShelley -> TxOut
    fromTxOut index (Shelley.TxOut addr ada) =
      TxOut
        { txOutIndex = index
        , txOutAddress = coerceAddress addr
        , txOutAdaValue = ada
        , txOutMaValue = mempty -- Shelley does not support Multi-Assets
        }

    txOutValue :: Shelley.TxOut StandardShelley -> Integer
    txOutValue (Shelley.TxOut _ (Coin coin)) = coin

fromMaryTx :: (Word64, Shelley.Tx StandardMary) -> Tx
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
      , txCertificates = zipWith (TxCertificate Nothing) [0..] (map coerceCertificate . toList . ShelleyMa.certs $ unTxBodyRaw tx)
      , txWithdrawals = map (mkTxWithdrawal Nothing) (Map.toList . Shelley.unWdrl . ShelleyMa.wdrls $ unTxBodyRaw tx)
      , txParamProposal = maybe [] (convertParamProposal (Mary Standard)) $ strictMaybeToMaybe (ShelleyMa.update $ unTxBodyRaw tx)
      , txMint = coerceMint (ShelleyMa.mint $ unTxBodyRaw tx)
      , txRedeemer = []     -- Mary does not support Redeemer
      , txScriptSizes = []    -- Mary does not support scripts
      , txScripts = []        -- We don't populate scripts for Mary
      , txScriptsFee = Coin 0 -- Mary does not support scripts
      }
  where
    fromTxOut :: Word16 -> Shelley.TxOut StandardMary -> TxOut
    fromTxOut index (Shelley.TxOut addr (Value ada maMap)) =
      TxOut
        { txOutIndex = index
        , txOutAddress = coerceAddress addr
        , txOutAdaValue = Coin ada
        , txOutMaValue = coerceMultiAsset maMap
        }

    txMeta :: Shelley.Tx StandardMary -> Maybe (ShelleyMa.AuxiliaryData StandardMary)
    txMeta (Shelley.Tx _body _wit md) = strictMaybeToMaybe md

    txOutValue :: Shelley.TxOut StandardMary -> Integer
    txOutValue (Shelley.TxOut _ (Value coin _ma)) = coin

    unTxBodyRaw :: Shelley.Tx StandardMary -> ShelleyMa.TxBodyRaw StandardMary
    unTxBodyRaw (Shelley.Tx (ShelleyMa.TxBodyConstr txBody) _wit _md) = memotype txBody

fromAlonzoTx :: Ledger.PParams StandardAlonzo -> (Word64, Ledger.Tx StandardAlonzo) -> Tx
fromAlonzoTx pp (blkIndex, tx) =
    Tx
      { txHash = Crypto.hashToBytes . Ledger.extractHash $ Ledger.hashAnnotated txBody
      , txBlockIndex = blkIndex
      , txSize = fromIntegral $ getField @"txsize" tx
      , txValidContract = isValid2
      , txInputs = txIns
      , txCollateralInputs = map (fromTxIn Nothing) . toList $ getField @"collateral" txBody
      , txOutputs = zipWith fromTxOut [0 .. ] . toList $ getField @"outputs" txBody
      , txFees = getField @"txfee" txBody
      , txOutSum = Coin . sum $ map txOutValue (getField @"outputs" txBody)
      , txInvalidBefore = strictMaybeToMaybe . ShelleyMa.invalidBefore $ getField @"vldt" txBody
      , txInvalidHereafter = strictMaybeToMaybe . ShelleyMa.invalidHereafter $ getField @"vldt" txBody
      , txWithdrawalSum = Coin . sum . map unCoin . Map.elems
                            . Shelley.unWdrl $ getField @"wdrls" txBody
      , txMetadata = fromAlonzoMetadata <$> strictMaybeToMaybe (getField @"auxiliaryData" tx)
      , txCertificates = txCerts
      , txWithdrawals = txWdrls
      , txParamProposal = maybe [] (convertParamProposal (Alonzo Standard)) $ strictMaybeToMaybe (getField @"update" txBody)
      , txMint = coerceMint (getField @"mint" txBody)
      , txRedeemer = redeemers
      , txScriptSizes = sizes
      , txScripts = scripts
      , txScriptsFee = minFees
      }
  where
    fromTxOut :: Word16 -> Alonzo.TxOut StandardAlonzo -> TxOut
    fromTxOut index (Alonzo.TxOut addr (Value ada maMap) _dataHash) =
      TxOut
        { txOutIndex = index
        , txOutAddress = coerceAddress addr
        , txOutAdaValue = Coin ada
        , txOutMaValue = coerceMultiAsset maMap
        }

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
        -> [(ScriptHash StandardCrypto, Script (AlonzoEra StandardCrypto))]
    getAuxScripts maux =
      case strictMaybeToMaybe maux of
        Nothing -> []
        Just (Alonzo.AuxiliaryData _ scrs) ->
          map (\scr -> (Ledger.hashScript @StandardAlonzo scr, scr)) $ toList scrs

    mkTxScript :: (ScriptHash StandardCrypto, Script (AlonzoEra StandardCrypto)) -> TxScript
    mkTxScript (hsh, script) = TxScript
      { txScriptHash = unScriptHash hsh
      , txScriptPlutusSize = getScriptSize script
      }

    getScriptSize :: Script (AlonzoEra StandardCrypto) -> Maybe Word64
    getScriptSize (TimelockScript _) = Nothing
    getScriptSize (PlutusScript sbs) = Just $ fromIntegral $ SBS.length sbs

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
    txCerts = zipWith mkTxCertificate [0..] (map coerceCertificate . toList $ certificates)

    withdrawals :: Map (Shelley.RewardAcnt StandardCrypto) Coin
    withdrawals = Shelley.unWdrl $ getField @"wdrls" txBody

    mkTxWithdrawal' :: (Shelley.RewardAcnt era, Coin) -> TxWithdrawal
    mkTxWithdrawal' (acnt, coin) =
      let acnt' = coerce acnt
      in mkTxWithdrawal (strictMaybeToMaybe $ Alonzo.indexOf acnt' withdrawals) (acnt', coin)

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
      mkRedeemer <$> Map.toList (snd <$> Ledger.unRedeemers (getField @"txrdmrs" (getField @"wits" tx)))

    mkRedeemer :: (Ledger.RdmrPtr, ExUnits) -> TxRedeemer
    mkRedeemer (ptr@(Ledger.RdmrPtr tag index), exUnits) = TxRedeemer
      { txRedeemerMem = exUnitsMem exUnits
      , txRedeemerSteps = exUnitsSteps exUnits
      , txRedeemerFee = txscriptfee (Alonzo._prices pp) exUnits
      , txRedeemerPurpose = tag
      , txRedeemerIndex = index
      , txRedeemerScriptHash = findScriptHash ptr
      }

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

-- Coerce is safe here because 'era' is a phantom type.
coerceAddress :: Ledger.Addr era -> Ledger.Addr StandardCrypto
coerceAddress saddr =
  case saddr of
    Ledger.Addr nw pcred sref -> Ledger.Addr nw (coerce pcred) (coerce sref)
    Ledger.AddrBootstrap addr -> Ledger.AddrBootstrap (coerce addr)

coerceCertificate :: Shelley.DCert era -> Shelley.DCert StandardCrypto
coerceCertificate cert =
  case cert of
    Shelley.DCertDeleg deleg -> Shelley.DCertDeleg (coerce deleg)
    Shelley.DCertPool pool -> Shelley.DCertPool (coercePoolCert pool)
    Shelley.DCertMir (Shelley.MIRCert pot target) -> Shelley.DCertMir (Shelley.MIRCert pot (coerceMIRTarget target))
    Shelley.DCertGenesis gen -> Shelley.DCertGenesis (coerce gen)

coerceMIRTarget :: Shelley.MIRTarget crypto -> Shelley.MIRTarget StandardCrypto
coerceMIRTarget mt =
  case mt of
    Shelley.StakeAddressesMIR m -> Shelley.StakeAddressesMIR (Map.mapKeys coerce m)
    Shelley.SendToOppositePotMIR c -> Shelley.SendToOppositePotMIR c

coerceMint :: Value era -> Value StandardCrypto
coerceMint (Value ada maMap) = Value ada (Map.mapKeys coerce maMap)

coerceMultiAsset
    :: Map (PolicyID era) (Map AssetName Integer)
    -> Map (PolicyID StandardCrypto) (Map AssetName Integer)
coerceMultiAsset = Map.mapKeys coerce

coercePoolCert :: Shelley.PoolCert era -> Shelley.PoolCert StandardCrypto
coercePoolCert pcert =
  case pcert of
    Shelley.RegPool cert -> Shelley.RegPool (coercePoolParams cert)
    Shelley.RetirePool kh e -> Shelley.RetirePool (coerce kh) e

coercePoolParams :: Shelley.PoolParams era -> Shelley.PoolParams StandardCrypto
coercePoolParams pp =
  Shelley.PoolParams
    { Shelley._poolId = coerce (Shelley._poolId pp)
    , Shelley._poolVrf = coerce (Shelley._poolVrf pp)
    , Shelley._poolPledge = Shelley._poolPledge pp
    , Shelley._poolCost  = Shelley._poolCost pp
    , Shelley._poolMargin = Shelley._poolMargin pp
    , Shelley._poolRAcnt = coerce (Shelley._poolRAcnt pp)
    , Shelley._poolOwners = Set.map coerce (Shelley._poolOwners pp)
    , Shelley._poolRelays = Shelley._poolRelays pp
    , Shelley._poolMD = Shelley._poolMD pp
    }

-- -------------------------------------------------------------------------------------------------

fromTxIn :: Maybe Word64 -> Shelley.TxIn StandardCrypto -> TxIn
fromTxIn setIndex (Shelley.TxIn (Shelley.TxId txid) index) =
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

txHashId :: ShelleyBasedEra era => Shelley.Tx era -> ByteString
txHashId = Crypto.hashToBytes . Ledger.extractHash . Ledger.hashAnnotated . Shelley.body

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
