{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Era.Shelley.Generic.Tx
  ( Tx (..)
  , TxCertificate (..)
  , TxIn (..)
  , TxOut (..)
  , TxWithdrawal (..)
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
import           Cardano.DbSync.Era.Shelley.Generic.Witness

import qualified Cardano.Ledger.Address as Ledger
import           Cardano.Ledger.Alonzo (AlonzoEra)
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import           Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Script (..), txscriptfee)
import           Cardano.Ledger.Alonzo.Tx (ValidatedTx (..))
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import           Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Ledger
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
import qualified Data.Set as Set

import           Ouroboros.Consensus.Cardano.Block (StandardAllegra, StandardAlonzo, StandardCrypto,
                   StandardMary, StandardShelley)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBasedEra)

import           Shelley.Spec.Ledger.Scripts ()
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
  , txExUnits :: [ExUnits]
  , scriptSizes :: [Int]
  , scriptsFee :: Coin
  }

data TxCertificate = TxCertificate
  { txcIndex :: !Word16
  , txcCert :: !(Shelley.DCert StandardCrypto)
  }

data TxWithdrawal = TxWithdrawal
  { txwRewardAccount :: !(Shelley.RewardAcnt StandardCrypto)
  , txwAmount :: !Coin
  }

data TxIn = TxIn
  { txInHash :: !ByteString
  , txInIndex :: !Word16
  }

data TxOut = TxOut
  { txOutIndex :: !Word16
  , txOutAddress :: !(Ledger.Addr StandardCrypto)
  , txOutAdaValue :: !Coin
  , txOutMaValue :: !(Map (PolicyID StandardCrypto) (Map AssetName Integer))
  }

fromAllegraTx :: (Word64, Shelley.Tx StandardAllegra) -> Tx
fromAllegraTx (blkIndex, tx) =
    Tx
      { txHash = txHashId tx
      , txBlockIndex = blkIndex
      , txSize = fromIntegral $ getField @"txsize" tx
      , txValidContract = True
      , txInputs = map fromTxIn (toList $ ShelleyMa.inputs rawTxBody)
      , txCollateralInputs = [] -- Allegra does not have collateral inputs
      , txOutputs = zipWith fromTxOut [0 .. ] $ toList (ShelleyMa.outputs rawTxBody)
      , txFees = ShelleyMa.txfee rawTxBody
      , txOutSum = Coin . sum $ map txOutValue (ShelleyMa.outputs rawTxBody)
      , txInvalidBefore = strictMaybeToMaybe . ShelleyMa.invalidBefore $ ShelleyMa.vldt rawTxBody
      , txInvalidHereafter = strictMaybeToMaybe . ShelleyMa.invalidHereafter $ ShelleyMa.vldt rawTxBody
      , txWithdrawalSum = Coin . sum . map unCoin . Map.elems
                            . Shelley.unWdrl $ ShelleyMa.wdrls rawTxBody
      , txMetadata = fromAllegraMetadata <$> txMeta tx
      , txCertificates = zipWith TxCertificate [0..] (map coerceCertificate . toList $ ShelleyMa.certs rawTxBody)
      , txWithdrawals = map mkTxWithdrawal (Map.toList . Shelley.unWdrl $ ShelleyMa.wdrls rawTxBody)
      , txParamProposal = maybe [] (convertParamProposal (Allegra Standard)) $ strictMaybeToMaybe (ShelleyMa.update rawTxBody)
      , txMint = mempty     -- Allegra does not support Multi-Assets
      , txExUnits = []      -- Allegra does not support ExUnits
      , scriptSizes = []    -- Allegra does not support scripts
      , scriptsFee = Coin 0 -- Allegra does not support scripts
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
      , txInputs = map fromTxIn (toList . Shelley._inputs $ Shelley.body tx)
      , txCollateralInputs = [] -- Shelley does not have collateral inputs
      , txOutputs = zipWith fromTxOut [0 .. ] $ toList (Shelley._outputs $ Shelley.body tx)
      , txFees = Shelley._txfee (Shelley.body tx)
      , txOutSum = Coin . sum $ map txOutValue (Shelley._outputs $ Shelley.body tx)
      , txInvalidBefore = Nothing
      , txInvalidHereafter = Just $ Shelley._ttl (Shelley.body tx)
      , txWithdrawalSum = Coin . sum . map unCoin . Map.elems
                            . Shelley.unWdrl $ Shelley._wdrls (Shelley.body tx)
      , txMetadata = fromShelleyMetadata <$> strictMaybeToMaybe (getField @"auxiliaryData" tx)
      , txCertificates = zipWith TxCertificate [0..] (toList . Shelley._certs $ Shelley.body tx)
      , txWithdrawals = map mkTxWithdrawal (Map.toList . Shelley.unWdrl . Shelley._wdrls $ Shelley.body tx)
      , txParamProposal = maybe [] (convertParamProposal (Shelley Standard)) $ strictMaybeToMaybe (Shelley._txUpdate $ Shelley.body tx)
      , txMint = mempty     -- Shelley does not support Multi-Assets
      , txExUnits = []      -- Shelley does not support ExUnits
      , scriptSizes = []    -- Shelley does not support scripts
      , scriptsFee = Coin 0 -- Shelley does not support scripts
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
      , txInputs = map fromTxIn (toList . ShelleyMa.inputs $ unTxBodyRaw tx)
      , txCollateralInputs = [] -- Mary does not have collateral inputs
      , txOutputs = zipWith fromTxOut [0 .. ] $ toList (ShelleyMa.outputs $ unTxBodyRaw tx)
      , txFees = ShelleyMa.txfee (unTxBodyRaw tx)
      , txOutSum = Coin . sum $ map txOutValue (ShelleyMa.outputs $ unTxBodyRaw tx)
      , txInvalidBefore = strictMaybeToMaybe . ShelleyMa.invalidBefore $ ShelleyMa.vldt (unTxBodyRaw tx)
      , txInvalidHereafter = strictMaybeToMaybe . ShelleyMa.invalidHereafter $ ShelleyMa.vldt (unTxBodyRaw tx)
      , txWithdrawalSum = Coin . sum . map unCoin . Map.elems
                            . Shelley.unWdrl $ ShelleyMa.wdrls (unTxBodyRaw tx)
      , txMetadata = fromMaryMetadata <$> txMeta tx
      , txCertificates = zipWith TxCertificate [0..] (map coerceCertificate . toList . ShelleyMa.certs $ unTxBodyRaw tx)
      , txWithdrawals = map mkTxWithdrawal (Map.toList . Shelley.unWdrl . ShelleyMa.wdrls $ unTxBodyRaw tx)
      , txParamProposal = maybe [] (convertParamProposal (Mary Standard)) $ strictMaybeToMaybe (ShelleyMa.update $ unTxBodyRaw tx)
      , txMint = coerceMint (ShelleyMa.mint $ unTxBodyRaw tx)
      , txExUnits = []      -- Mary does not support ExUnits
      , scriptSizes = []    -- Mary does not support scripts
      , scriptsFee = Coin 0 -- Mary does not support scripts
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
      , txValidContract = isValid
      , txInputs = map fromTxIn inputs
      , txCollateralInputs = map fromTxIn . toList $ getField @"collateral" txBody
      , txOutputs = zipWith fromTxOut [0 .. ] . toList $ getField @"outputs" txBody
      , txFees = getField @"txfee" txBody
      , txOutSum = Coin . sum $ map txOutValue (getField @"outputs" txBody)
      , txInvalidBefore = strictMaybeToMaybe . ShelleyMa.invalidBefore $ getField @"vldt" txBody
      , txInvalidHereafter = strictMaybeToMaybe . ShelleyMa.invalidHereafter $ getField @"vldt" txBody
      , txWithdrawalSum = Coin . sum . map unCoin . Map.elems
                            . Shelley.unWdrl $ getField @"wdrls" txBody
      , txMetadata = fromAlonzoMetadata <$> strictMaybeToMaybe (getField @"auxiliaryData" tx)
      , txCertificates = zipWith TxCertificate [0..] (map coerceCertificate . toList $ getField @"certs" txBody)
      , txWithdrawals = map mkTxWithdrawal (Map.toList . Shelley.unWdrl $ getField @"wdrls" txBody)
      , txParamProposal = maybe [] (convertParamProposal (Alonzo Standard)) $ strictMaybeToMaybe (getField @"update" txBody)
      , txMint = coerceMint (getField @"mint" txBody)
      , txExUnits = exUnits
      , scriptSizes = sizes
      , scriptsFee = minFees
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

    exUnits :: [ExUnits]
    exUnits =
      map snd . Map.elems . Ledger.unRedeemers $ getField @"txrdmrs" (getField @"wits" tx)

    sizes :: [Int]
    sizes = mapMaybe getScriptSize $ toList $ Ledger.txscripts $ getField @"wits" tx

    getScriptSize :: Script (AlonzoEra StandardCrypto) -> Maybe Int
    getScriptSize (TimelockScript _) = Nothing
    getScriptSize (PlutusScript sbs) = Just $ SBS.length sbs

    minFees :: Coin
    minFees = txscriptfee (Alonzo._prices pp) $ Alonzo.totExUnits tx

    txOutValue :: Alonzo.TxOut StandardAlonzo -> Integer
    txOutValue (Alonzo.TxOut _addr (Value coin _ma) _dataHash) = coin

    isValid :: Bool
    isValid =
      case Alonzo.isValidating tx of
        Alonzo.IsValidating x -> x

    inputs :: [Shelley.TxIn StandardCrypto]
    inputs =
      -- Chose which inputs will be spent depending in where the tx is valid.
      -- If the transaction is valid, the regular inputs will be consumed and if it is
      -- invalid, the collateral inputs will be consumed.
      if isValid
        then toList $ getField @"inputs" txBody
        else toList $ getField @"collateral" txBody

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

fromTxIn :: Shelley.TxIn StandardCrypto -> TxIn
fromTxIn (Shelley.TxIn (Shelley.TxId txid) index) =
  TxIn
    { txInHash = Crypto.hashToBytes $ Ledger.extractHash txid
    , txInIndex = fromIntegral index
    }

mkTxWithdrawal :: (Shelley.RewardAcnt era, Coin) -> TxWithdrawal
mkTxWithdrawal (ra, c) =
  TxWithdrawal
    { txwRewardAccount = coerce ra
    , txwAmount = c
    }

txHashId :: ShelleyBasedEra era => Shelley.Tx era -> ByteString
txHashId = Crypto.hashToBytes . Ledger.extractHash . Ledger.hashAnnotated . Shelley.body
