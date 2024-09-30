{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Cardano.Db.Operations.Types where

import Cardano.Db.Schema.BaseSchema
import qualified Cardano.Db.Schema.Core.TxOut as C
import qualified Cardano.Db.Schema.Variant.TxOut as V
import Cardano.Db.Types (DbLovelace (..), DbWord64)
import Cardano.Prelude (ByteString, Text, Word64, mapMaybe)
import Data.Kind (Type)
import Database.Esqueleto.Experimental (PersistEntity (..))
import Database.Persist.Sql (PersistField)

data TxOutTableType = TxOutCore | TxOutVariantAddress
  deriving (Eq, Show)

-- | A wrapper for TxOut that allows us to handle both Core and Variant TxOuts
data TxOutW
  = CTxOutW !C.TxOut
  | VTxOutW !V.TxOut !(Maybe V.Address)

-- Pattern synonyms for easier construction
pattern CoreTxOut :: C.TxOut -> TxOutW
pattern CoreTxOut txOut = CTxOutW txOut

pattern VariantTxOutWithAddr :: V.TxOut -> V.Address -> TxOutW
pattern VariantTxOutWithAddr txOut address = VTxOutW txOut (Just address)

pattern VariantTxOutNoAddr :: V.TxOut -> Maybe V.Address -> TxOutW
pattern VariantTxOutNoAddr txOut maybeAddress = VTxOutW txOut maybeAddress

-- | A wrapper for TxOutId
data TxOutIdW
  = CTxOutIdW !C.TxOutId
  | VTxOutIdW !V.TxOutId
  deriving (Show)

-- Pattern synonyms for easier construction
pattern CoreTxOutId :: C.TxOutId -> TxOutIdW
pattern CoreTxOutId txOutId = CTxOutIdW txOutId

pattern VariantTxOutId :: V.TxOutId -> TxOutIdW
pattern VariantTxOutId txOutId = VTxOutIdW txOutId

-- | A wrapper for MaTxOut
data MaTxOutW
  = CMaTxOutW !C.MaTxOut
  | VMaTxOutW !V.MaTxOut
  deriving (Show)

pattern CoreMaTxOut :: C.MaTxOut -> MaTxOutW
pattern CoreMaTxOut maTxOut = CMaTxOutW maTxOut

pattern VariantMaTxOut :: V.MaTxOut -> MaTxOutW
pattern VariantMaTxOut maTxOut = VMaTxOutW maTxOut

-- | A wrapper for MaTxOut
data MaTxOutIdW
  = CMaTxOutIdW !C.MaTxOutId
  | VMaTxOutIdW !V.MaTxOutId
  deriving (Show)

pattern CoreMaTxOutId :: C.MaTxOutId -> MaTxOutIdW
pattern CoreMaTxOutId maTxOutId = CMaTxOutIdW maTxOutId

pattern VariantMaTxOutId :: V.MaTxOutId -> MaTxOutIdW
pattern VariantMaTxOutId maTxOutId = VMaTxOutIdW maTxOutId

-- | UtxoQueryResult which has utxoAddress that can come from Core or Variant TxOut
data UtxoQueryResult = UtxoQueryResult
  { utxoTxOutW :: TxOutW
  , utxoAddress :: Text
  , utxoTxHash :: ByteString
  }

--------------------------------------------------------------------------------
-- TxOut fields for a given TxOutTableType
--------------------------------------------------------------------------------
class (PersistEntity (TxOutTable a), PersistField (TxOutIdFor a)) => TxOutFields (a :: TxOutTableType) where
  type TxOutTable a :: Type
  type TxOutIdFor a :: Type
  txOutTxIdField :: EntityField (TxOutTable a) TxId
  txOutIndexField :: EntityField (TxOutTable a) Word64
  txOutValueField :: EntityField (TxOutTable a) DbLovelace
  txOutIdField :: EntityField (TxOutTable a) (TxOutIdFor a)
  txOutDataHashField :: EntityField (TxOutTable a) (Maybe ByteString)
  txOutInlineDatumIdField :: EntityField (TxOutTable a) (Maybe DatumId)
  txOutReferenceScriptIdField :: EntityField (TxOutTable a) (Maybe ScriptId)
  txOutConsumedByTxIdField :: EntityField (TxOutTable a) (Maybe TxId)

--------------------------------------------------------------------------------
-- Multi-asset fields for a given TxOutTableType
--------------------------------------------------------------------------------
class (PersistEntity (MaTxOutTable a)) => MaTxOutFields (a :: TxOutTableType) where
  type MaTxOutTable a :: Type
  type MaTxOutIdFor a :: Type
  maTxOutTxOutIdField :: EntityField (MaTxOutTable a) (TxOutIdFor a)
  maTxOutIdentField :: EntityField (MaTxOutTable a) MultiAssetId
  maTxOutQuantityField :: EntityField (MaTxOutTable a) DbWord64

--------------------------------------------------------------------------------
-- Address-related fields for TxOutVariantAddress only
--------------------------------------------------------------------------------
class AddressFields (a :: TxOutTableType) where
  type AddressTable a :: Type
  type AddressIdFor a :: Type
  addressField :: EntityField (AddressTable a) Text
  addressRawField :: EntityField (AddressTable a) ByteString
  addressHasScriptField :: EntityField (AddressTable a) Bool
  addressPaymentCredField :: EntityField (AddressTable a) (Maybe ByteString)
  addressStakeAddressIdField :: EntityField (AddressTable a) (Maybe StakeAddressId)
  addressIdField :: EntityField (AddressTable a) (AddressIdFor a)

--------------------------------------------------------------------------------
-- Instances for TxOutCore
--------------------------------------------------------------------------------
instance TxOutFields 'TxOutCore where
  type TxOutTable 'TxOutCore = C.TxOut
  type TxOutIdFor 'TxOutCore = C.TxOutId
  txOutTxIdField = C.TxOutTxId
  txOutIndexField = C.TxOutIndex
  txOutValueField = C.TxOutValue
  txOutIdField = C.TxOutId
  txOutDataHashField = C.TxOutDataHash
  txOutInlineDatumIdField = C.TxOutInlineDatumId
  txOutReferenceScriptIdField = C.TxOutReferenceScriptId
  txOutConsumedByTxIdField = C.TxOutConsumedByTxId

instance MaTxOutFields 'TxOutCore where
  type MaTxOutTable 'TxOutCore = C.MaTxOut
  type MaTxOutIdFor 'TxOutCore = C.MaTxOutId
  maTxOutTxOutIdField = C.MaTxOutTxOutId
  maTxOutIdentField = C.MaTxOutIdent
  maTxOutQuantityField = C.MaTxOutQuantity

--------------------------------------------------------------------------------
-- Instances for TxOutVariantAddress
--------------------------------------------------------------------------------
instance TxOutFields 'TxOutVariantAddress where
  type TxOutTable 'TxOutVariantAddress = V.TxOut
  type TxOutIdFor 'TxOutVariantAddress = V.TxOutId
  txOutTxIdField = V.TxOutTxId
  txOutIndexField = V.TxOutIndex
  txOutValueField = V.TxOutValue
  txOutIdField = V.TxOutId
  txOutDataHashField = V.TxOutDataHash
  txOutInlineDatumIdField = V.TxOutInlineDatumId
  txOutReferenceScriptIdField = V.TxOutReferenceScriptId
  txOutConsumedByTxIdField = V.TxOutConsumedByTxId

instance MaTxOutFields 'TxOutVariantAddress where
  type MaTxOutTable 'TxOutVariantAddress = V.MaTxOut
  type MaTxOutIdFor 'TxOutVariantAddress = V.MaTxOutId
  maTxOutTxOutIdField = V.MaTxOutTxOutId
  maTxOutIdentField = V.MaTxOutIdent
  maTxOutQuantityField = V.MaTxOutQuantity

instance AddressFields 'TxOutVariantAddress where
  type AddressTable 'TxOutVariantAddress = V.Address
  type AddressIdFor 'TxOutVariantAddress = V.AddressId
  addressField = V.AddressAddress
  addressRawField = V.AddressRaw
  addressHasScriptField = V.AddressHasScript
  addressPaymentCredField = V.AddressPaymentCred
  addressStakeAddressIdField = V.AddressStakeAddressId
  addressIdField = V.AddressId

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------
extractCoreTxOut :: TxOutW -> C.TxOut
extractCoreTxOut (CTxOutW txOut) = txOut
-- this will never error as we can only have either CoreTxOut or VariantTxOut
extractCoreTxOut (VTxOutW _ _) = error "Unexpected VTxOut in CoreTxOut list"

extractVariantTxOut :: TxOutW -> V.TxOut
extractVariantTxOut (VTxOutW txOut _) = txOut
-- this will never error as we can only have either CoreTxOut or VariantTxOut
extractVariantTxOut (CTxOutW _) = error "Unexpected CTxOut in VariantTxOut list"

extractVariantAddress :: TxOutW -> Maybe V.Address
extractVariantAddress (VTxOutW _ address) = address
-- this will never error as we can only have either CoreTxOut or VariantTxOut
extractVariantAddress (CTxOutW _) = error "Unexpected CTxOut in VariantTxOut list"

convertTxOutIdCore :: [TxOutIdW] -> [C.TxOutId]
convertTxOutIdCore = mapMaybe unwrapCore
  where
    unwrapCore (CTxOutIdW txOutid) = Just txOutid
    unwrapCore _ = Nothing

convertTxOutIdVariant :: [TxOutIdW] -> [V.TxOutId]
convertTxOutIdVariant = mapMaybe unwrapVariant
  where
    unwrapVariant (VTxOutIdW txOutid) = Just txOutid
    unwrapVariant _ = Nothing

convertMaTxOutIdCore :: [MaTxOutIdW] -> [C.MaTxOutId]
convertMaTxOutIdCore = mapMaybe unwrapCore
  where
    unwrapCore (CMaTxOutIdW maTxOutId) = Just maTxOutId
    unwrapCore _ = Nothing

convertMaTxOutIdVariant :: [MaTxOutIdW] -> [V.MaTxOutId]
convertMaTxOutIdVariant = mapMaybe unwrapVariant
  where
    unwrapVariant (VMaTxOutIdW maTxOutId) = Just maTxOutId
    unwrapVariant _ = Nothing

isTxOutCore :: TxOutTableType -> Bool
isTxOutCore TxOutCore = True
isTxOutCore TxOutVariantAddress = False

isTxOutVariantAddress :: TxOutTableType -> Bool
isTxOutVariantAddress TxOutVariantAddress = True
isTxOutVariantAddress TxOutCore = False
