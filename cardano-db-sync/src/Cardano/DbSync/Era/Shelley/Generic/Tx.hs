{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
  ) where

import           Cardano.Prelude

import           Cardano.Api.Shelley (TxMetadataValue (..))

import qualified Cardano.Crypto.Hash as Crypto

import           Cardano.DbSync.Era.Shelley.Generic.Metadata

import           Cardano.Ledger.Mary.Value (AssetName, PolicyID, Value (..))
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as ShelleyMa
import qualified Cardano.Ledger.ShelleyMA.TxBody as ShelleyMa

import           Cardano.Slotting.Slot (SlotNo (..))

import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import           Data.MemoBytes (MemoBytes (..))
import qualified Data.Set as Set

import           Ouroboros.Consensus.Cardano.Block (StandardAllegra, StandardCrypto, StandardMary,
                   StandardShelley)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBasedEra)

import qualified Shelley.Spec.Ledger.Address as Shelley
import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import           Shelley.Spec.Ledger.Coin (Coin (..))
import qualified Shelley.Spec.Ledger.Hashing as Shelley
import qualified Shelley.Spec.Ledger.PParams as Shelley
import           Shelley.Spec.Ledger.Scripts ()
import qualified Shelley.Spec.Ledger.Tx as Shelley
import qualified Shelley.Spec.Ledger.TxBody as Shelley


data Tx = Tx
  { txHash :: !ByteString
  , txBlockIndex :: !Word64
  , txSize :: !Word64
  , txInputs :: ![TxIn]
  , txOutputs :: ![TxOut]
  , txFees :: !Coin
  , txOutSum :: !Coin
  , txInvalidBefore :: !(Maybe SlotNo)
  , txInvalidHereafter :: !(Maybe SlotNo)
  , txWithdrawalSum :: !Coin
  , txMetadata :: !(Maybe (Map Word64 TxMetadataValue))
  , txCertificates :: ![TxCertificate]
  , txWithdrawals :: ![TxWithdrawal]
  , txParamProposal :: !(Maybe (Shelley.Update StandardCrypto))
  , txMint :: !(Value StandardCrypto)
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
  , txOutAddress :: !(Shelley.Addr StandardCrypto)
  , txOutAdaValue :: !Coin
  , txOutMaValue :: !(Map (PolicyID StandardCrypto) (Map AssetName Integer))
  }

fromAllegraTx :: (Word64, Shelley.Tx StandardAllegra) -> Tx
fromAllegraTx (blkIndex, tx) =
    Tx
      { txHash = txHashId tx
      , txBlockIndex = blkIndex
      , txSize = fromIntegral $ LBS.length (Shelley.txFullBytes tx)
      , txInputs = map fromTxIn (toList . ShelleyMa.inputs $ unTxBodyRaw tx)
      , txOutputs = zipWith fromTxOut [0 .. ] $ toList (ShelleyMa.outputs $ unTxBodyRaw tx)
      , txFees = ShelleyMa.txfee (unTxBodyRaw tx)
      , txOutSum = Coin . sum $ map txOutValue (ShelleyMa.outputs $ unTxBodyRaw tx)
      , txInvalidBefore = Shelley.strictMaybeToMaybe . ShelleyMa.invalidBefore $ ShelleyMa.vldt (unTxBodyRaw tx)
      , txInvalidHereafter = Shelley.strictMaybeToMaybe . ShelleyMa.invalidHereafter $ ShelleyMa.vldt (unTxBodyRaw tx)
      , txWithdrawalSum = Coin . sum . map unCoin . Map.elems
                            . Shelley.unWdrl $ ShelleyMa.wdrls (unTxBodyRaw tx)
      , txMetadata = fromAllegraMetadata <$> txMeta tx
      , txCertificates = zipWith TxCertificate [0..] (map coerceCertificate . toList . ShelleyMa.certs $ unTxBodyRaw tx)
      , txWithdrawals = map mkTxWithdrawal (Map.toList . Shelley.unWdrl . ShelleyMa.wdrls $ unTxBodyRaw tx)
      , txParamProposal = coerceProtoUpdate <$> Shelley.strictMaybeToMaybe (ShelleyMa.update $ unTxBodyRaw tx)
      , txMint = mempty     -- Allegra does not support Multi-Assets
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
    txMeta (Shelley.Tx _body _wit md) = Shelley.strictMaybeToMaybe md

    txOutValue :: Shelley.TxOut StandardAllegra -> Integer
    txOutValue (Shelley.TxOut _ (Coin coin)) = coin

    unTxBodyRaw :: Shelley.Tx StandardAllegra -> ShelleyMa.TxBodyRaw StandardAllegra
    unTxBodyRaw (Shelley.Tx (ShelleyMa.TxBodyConstr txBody) _wit _md) = memotype txBody

fromShelleyTx :: (Word64, Shelley.Tx StandardShelley) -> Tx
fromShelleyTx (blkIndex, tx) =
    Tx
      { txHash = txHashId tx
      , txBlockIndex = blkIndex
      , txSize = fromIntegral $ LBS.length (Shelley.txFullBytes tx)
      , txInputs = map fromTxIn (toList . Shelley._inputs $ Shelley._body tx)
      , txOutputs = zipWith fromTxOut [0 .. ] $ toList (Shelley._outputs $ Shelley._body tx)
      , txFees = Shelley._txfee (Shelley._body tx)
      , txOutSum = Coin . sum $ map txOutValue (Shelley._outputs $ Shelley._body tx)
      , txInvalidBefore = Nothing
      , txInvalidHereafter = Just $ Shelley._ttl (Shelley._body tx)
      , txWithdrawalSum = Coin . sum . map unCoin . Map.elems
                            . Shelley.unWdrl $ Shelley._wdrls (Shelley._body tx)
      , txMetadata = fromShelleyMetadata <$> Shelley.strictMaybeToMaybe (Shelley._metadata tx)
      , txCertificates = zipWith TxCertificate [0..] (toList . Shelley._certs $ Shelley._body tx)
      , txWithdrawals = map mkTxWithdrawal (Map.toList . Shelley.unWdrl . Shelley._wdrls $ Shelley._body tx)
      , txParamProposal = coerceProtoUpdate <$> Shelley.strictMaybeToMaybe (Shelley._txUpdate $ Shelley._body tx)
      , txMint = mempty     -- Shelley does not support Multi-Assets
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
      , txSize = fromIntegral $ LBS.length (Shelley.txFullBytes tx)
      , txInputs = map fromTxIn (toList . ShelleyMa.inputs $ unTxBodyRaw tx)
      , txOutputs = zipWith fromTxOut [0 .. ] $ toList (ShelleyMa.outputs $ unTxBodyRaw tx)
      , txFees = ShelleyMa.txfee (unTxBodyRaw tx)
      , txOutSum = Coin . sum $ map txOutValue (ShelleyMa.outputs $ unTxBodyRaw tx)
      , txInvalidBefore = Shelley.strictMaybeToMaybe . ShelleyMa.invalidBefore $ ShelleyMa.vldt (unTxBodyRaw tx)
      , txInvalidHereafter = Shelley.strictMaybeToMaybe . ShelleyMa.invalidHereafter $ ShelleyMa.vldt (unTxBodyRaw tx)
      , txWithdrawalSum = Coin . sum . map unCoin . Map.elems
                            . Shelley.unWdrl $ ShelleyMa.wdrls (unTxBodyRaw tx)
      , txMetadata = fromMaryMetadata <$> txMeta tx
      , txCertificates = zipWith TxCertificate [0..] (map coerceCertificate . toList . ShelleyMa.certs $ unTxBodyRaw tx)
      , txWithdrawals = map mkTxWithdrawal (Map.toList . Shelley.unWdrl . ShelleyMa.wdrls $ unTxBodyRaw tx)
      , txParamProposal = coerceProtoUpdate <$> Shelley.strictMaybeToMaybe (ShelleyMa.update $ unTxBodyRaw tx)
      , txMint = coerceMint (ShelleyMa.mint $ unTxBodyRaw tx)
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
    txMeta (Shelley.Tx _body _wit md) = Shelley.strictMaybeToMaybe md

    txOutValue :: Shelley.TxOut StandardMary -> Integer
    txOutValue (Shelley.TxOut _ (Value coin _ma)) = coin

    unTxBodyRaw :: Shelley.Tx StandardMary -> ShelleyMa.TxBodyRaw StandardMary
    unTxBodyRaw (Shelley.Tx (ShelleyMa.TxBodyConstr txBody) _wit _md) = memotype txBody

-- -------------------------------------------------------------------------------------------------

-- Coerce is safe here because 'era' is a phantom type.
coerceAddress :: Shelley.Addr era -> Shelley.Addr StandardCrypto
coerceAddress saddr =
  case saddr of
    Shelley.Addr nw pcred sref -> Shelley.Addr nw (coerce pcred) (coerce sref)
    Shelley.AddrBootstrap addr -> Shelley.AddrBootstrap (coerce addr)

coerceCertificate :: Shelley.DCert era -> Shelley.DCert StandardCrypto
coerceCertificate cert =
  case cert of
    Shelley.DCertDeleg deleg -> Shelley.DCertDeleg (coerce deleg)
    Shelley.DCertPool pool -> Shelley.DCertPool (coercePoolCert pool)
    Shelley.DCertMir (Shelley.MIRCert pot rwds) -> Shelley.DCertMir (Shelley.MIRCert pot (Map.mapKeys coerce rwds))
    Shelley.DCertGenesis gen -> Shelley.DCertGenesis (coerce gen)

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

coerceProtoUpdate :: Shelley.Update era -> Shelley.Update StandardCrypto
coerceProtoUpdate (Shelley.Update pp epoch) =
    Shelley.Update (coerceProposal pp) epoch
  where
    coerceProposal :: Shelley.ProposedPPUpdates era -> Shelley.ProposedPPUpdates StandardCrypto
    coerceProposal (Shelley.ProposedPPUpdates p) =
      Shelley.ProposedPPUpdates $ Map.map coerce (Map.mapKeys coerce p)

-- -------------------------------------------------------------------------------------------------

fromTxIn :: Shelley.TxIn StandardCrypto -> TxIn
fromTxIn (Shelley.TxIn (Shelley.TxId txid) index) =
  TxIn
    { txInHash = Crypto.hashToBytes txid
    , txInIndex = fromIntegral index
    }

mkTxWithdrawal :: (Shelley.RewardAcnt era, Coin) -> TxWithdrawal
mkTxWithdrawal (ra, c) =
  TxWithdrawal
    { txwRewardAccount = coerce ra
    , txwAmount = c
    }

txHashId :: ShelleyBasedEra era => Shelley.Tx era -> ByteString
txHashId = Crypto.hashToBytes . Shelley.hashAnnotated . Shelley._body
