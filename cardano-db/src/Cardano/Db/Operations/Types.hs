{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

--------------------------------------------------------------------------------
-- TxOut
--------------------------------------------------------------------------------

-- | A wrapper for TxOut that allows us to handle both Core and Variant TxOuts
data TxOutW
  = CTxOutW !C.TxOut
  | VTxOutW !V.TxOut !(Maybe V.Address)

-- | A wrapper for TxOutId
data TxOutIdW
  = CTxOutIdW !C.TxOutId
  | VTxOutIdW !V.TxOutId
  deriving (Show)

-- TxOut fields for a given TxOutTableType
class (PersistEntity (TxOutTable a), PersistField (TxOutIdFor a)) => TxOutFields (a :: TxOutTableType) where
  type TxOutTable a :: Type
  type TxOutIdFor a :: Type
  txOutIdField :: EntityField (TxOutTable a) (TxOutIdFor a)
  txOutTxIdField :: EntityField (TxOutTable a) TxId
  txOutIndexField :: EntityField (TxOutTable a) Word64
  txOutValueField :: EntityField (TxOutTable a) DbLovelace
  txOutDataHashField :: EntityField (TxOutTable a) (Maybe ByteString)
  txOutInlineDatumIdField :: EntityField (TxOutTable a) (Maybe DatumId)
  txOutReferenceScriptIdField :: EntityField (TxOutTable a) (Maybe ScriptId)
  txOutConsumedByTxIdField :: EntityField (TxOutTable a) (Maybe TxId)

-- TxOutCore fields
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

-- TxOutVariantAddress fields
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

--------------------------------------------------------------------------------
-- Address
-- related fields for TxOutVariantAddress only
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

-- TxOutVariant fields
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
-- MaTxOut
--------------------------------------------------------------------------------

-- | A wrapper for MaTxOut
data MaTxOutW
  = CMaTxOutW !C.MaTxOut
  | VMaTxOutW !V.MaTxOut
  deriving (Show)

-- | A wrapper for MaTxOutId
data MaTxOutIdW
  = CMaTxOutIdW !C.MaTxOutId
  | VMaTxOutIdW !V.MaTxOutId
  deriving (Show)

-- MaTxOut fields for a given TxOutTableType
class (PersistEntity (MaTxOutTable a)) => MaTxOutFields (a :: TxOutTableType) where
  type MaTxOutTable a :: Type
  type MaTxOutIdFor a :: Type
  maTxOutTxOutIdField :: EntityField (MaTxOutTable a) (TxOutIdFor a)
  maTxOutIdentField :: EntityField (MaTxOutTable a) MultiAssetId
  maTxOutQuantityField :: EntityField (MaTxOutTable a) DbWord64

-- TxOutCore fields
instance MaTxOutFields 'TxOutCore where
  type MaTxOutTable 'TxOutCore = C.MaTxOut
  type MaTxOutIdFor 'TxOutCore = C.MaTxOutId
  maTxOutTxOutIdField = C.MaTxOutTxOutId
  maTxOutIdentField = C.MaTxOutIdent
  maTxOutQuantityField = C.MaTxOutQuantity

-- TxOutVariantAddress fields
instance MaTxOutFields 'TxOutVariantAddress where
  type MaTxOutTable 'TxOutVariantAddress = V.MaTxOut
  type MaTxOutIdFor 'TxOutVariantAddress = V.MaTxOutId
  maTxOutTxOutIdField = V.MaTxOutTxOutId
  maTxOutIdentField = V.MaTxOutIdent
  maTxOutQuantityField = V.MaTxOutQuantity

-- | UtxoQueryResult which has utxoAddress that can come from Core or Variant TxOut
data UtxoQueryResult = UtxoQueryResult
  { utxoTxOutW :: TxOutW
  , utxoAddress :: Text
  , utxoTxHash :: ByteString
  }

--------------------------------------------------------------------------------
-- CollateralTxOut fields for a given TxOutTableType
--------------------------------------------------------------------------------
data CollateralTxOutW
  = CCollateralTxOutW !C.CollateralTxOut
  | VCollateralTxOutW !V.CollateralTxOut
  deriving (Show)

-- | A wrapper for TxOutId
data CollateralTxOutIdW
  = CCollateralTxOutIdW !C.CollateralTxOutId
  | VCollateralTxOutIdW !V.CollateralTxOutId
  deriving (Show)

class (PersistEntity (CollateralTxOutTable a)) => CollateralTxOutFields (a :: TxOutTableType) where
  type CollateralTxOutTable a :: Type
  type CollateralTxOutIdFor a :: Type
  collateralTxOutIdField :: EntityField (CollateralTxOutTable a) (CollateralTxOutIdFor a)
  collateralTxOutTxIdField :: EntityField (TxOutTable a) TxId
  collateralTxOutIndexField :: EntityField (TxOutTable a) DbWord64
  collateralTxOutAddressField :: EntityField (TxOutTable a) Text
  collateralTxOutAddressHasScriptField :: EntityField (TxOutTable a) Bool

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
