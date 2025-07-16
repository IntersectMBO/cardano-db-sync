{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Cardano.Db.Operations.Types where

import Cardano.Db.Schema.BaseSchema
import qualified Cardano.Db.Schema.Variants.TxOutAddress as VA
import qualified Cardano.Db.Schema.Variants.TxOutCore as VC
import Cardano.Db.Types (DbLovelace (..), DbWord64)
import Cardano.Prelude (ByteString, Text, Word64, mapMaybe)
import Data.Kind (Type)
import Database.Esqueleto.Experimental (PersistEntity (..))
import Database.Persist.Sql (PersistField)

data TxOutVariantType = TxOutVariantCore | TxOutVariantAddress
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- TxOut
--------------------------------------------------------------------------------

-- | A wrapper for TxOut that allows us to handle both Core and Variant TxOuts
data TxOutW
  = CTxOutW !VC.TxOut
  | VTxOutW !VA.TxOut !(Maybe VA.Address)

-- | A wrapper for TxOutId
data TxOutIdW
  = CTxOutIdW !VC.TxOutId
  | VTxOutIdW !VA.TxOutId
  deriving (Show)

-- TxOut fields for a given TxOutVariantType
class (PersistEntity (TxOutTable a), PersistField (TxOutIdFor a)) => TxOutFields (a :: TxOutVariantType) where
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

-- TxOutVariantCore fields
instance TxOutFields 'TxOutVariantCore where
  type TxOutTable 'TxOutVariantCore = VC.TxOut
  type TxOutIdFor 'TxOutVariantCore = VC.TxOutId
  txOutTxIdField = VC.TxOutTxId
  txOutIndexField = VC.TxOutIndex
  txOutValueField = VC.TxOutValue
  txOutIdField = VC.TxOutId
  txOutDataHashField = VC.TxOutDataHash
  txOutInlineDatumIdField = VC.TxOutInlineDatumId
  txOutReferenceScriptIdField = VC.TxOutReferenceScriptId
  txOutConsumedByTxIdField = VC.TxOutConsumedByTxId

-- TxOutVariantAddress fields
instance TxOutFields 'TxOutVariantAddress where
  type TxOutTable 'TxOutVariantAddress = VA.TxOut
  type TxOutIdFor 'TxOutVariantAddress = VA.TxOutId
  txOutTxIdField = VA.TxOutTxId
  txOutIndexField = VA.TxOutIndex
  txOutValueField = VA.TxOutValue
  txOutIdField = VA.TxOutId
  txOutDataHashField = VA.TxOutDataHash
  txOutInlineDatumIdField = VA.TxOutInlineDatumId
  txOutReferenceScriptIdField = VA.TxOutReferenceScriptId
  txOutConsumedByTxIdField = VA.TxOutConsumedByTxId

--------------------------------------------------------------------------------
-- Address
-- related fields for TxOutVariantAddress only
--------------------------------------------------------------------------------
class AddressFields (a :: TxOutVariantType) where
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
  type AddressTable 'TxOutVariantAddress = VA.Address
  type AddressIdFor 'TxOutVariantAddress = VA.AddressId
  addressField = VA.AddressAddress
  addressRawField = VA.AddressRaw
  addressHasScriptField = VA.AddressHasScript
  addressPaymentCredField = VA.AddressPaymentCred
  addressStakeAddressIdField = VA.AddressStakeAddressId
  addressIdField = VA.AddressId

--------------------------------------------------------------------------------
-- MaTxOut
--------------------------------------------------------------------------------

-- | A wrapper for MaTxOut
data MaTxOutW
  = CMaTxOutW !VC.MaTxOut
  | VMaTxOutW !VA.MaTxOut
  deriving (Show)

-- | A wrapper for MaTxOutId
data MaTxOutIdW
  = CMaTxOutIdW !VC.MaTxOutId
  | VMaTxOutIdW !VA.MaTxOutId
  deriving (Show)

-- MaTxOut fields for a given TxOutVariantType
class PersistEntity (MaTxOutTable a) => MaTxOutFields (a :: TxOutVariantType) where
  type MaTxOutTable a :: Type
  type MaTxOutIdFor a :: Type
  maTxOutTxOutIdField :: EntityField (MaTxOutTable a) (TxOutIdFor a)
  maTxOutIdentField :: EntityField (MaTxOutTable a) MultiAssetId
  maTxOutQuantityField :: EntityField (MaTxOutTable a) DbWord64

-- TxOutVariantCore fields
instance MaTxOutFields 'TxOutVariantCore where
  type MaTxOutTable 'TxOutVariantCore = VC.MaTxOut
  type MaTxOutIdFor 'TxOutVariantCore = VC.MaTxOutId
  maTxOutTxOutIdField = VC.MaTxOutTxOutId
  maTxOutIdentField = VC.MaTxOutIdent
  maTxOutQuantityField = VC.MaTxOutQuantity

-- TxOutVariantAddress fields
instance MaTxOutFields 'TxOutVariantAddress where
  type MaTxOutTable 'TxOutVariantAddress = VA.MaTxOut
  type MaTxOutIdFor 'TxOutVariantAddress = VA.MaTxOutId
  maTxOutTxOutIdField = VA.MaTxOutTxOutId
  maTxOutIdentField = VA.MaTxOutIdent
  maTxOutQuantityField = VA.MaTxOutQuantity

-- | UtxoQueryResult which has utxoAddress that can come from Core or Variant TxOut
data UtxoQueryResult = UtxoQueryResult
  { utxoTxOutW :: TxOutW
  , utxoAddress :: Text
  , utxoTxHash :: ByteString
  }

--------------------------------------------------------------------------------
-- CollateralTxOut fields for a given TxOutVariantType
--------------------------------------------------------------------------------
data CollateralTxOutW
  = CCollateralTxOutW !VC.CollateralTxOut
  | VCollateralTxOutW !VA.CollateralTxOut
  deriving (Show)

-- | A wrapper for TxOutId
data CollateralTxOutIdW
  = CCollateralTxOutIdW !VC.CollateralTxOutId
  | VCollateralTxOutIdW !VA.CollateralTxOutId
  deriving (Show)

class PersistEntity (CollateralTxOutTable a) => CollateralTxOutFields (a :: TxOutVariantType) where
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
extractCoreTxOut :: TxOutW -> VC.TxOut
extractCoreTxOut (CTxOutW txOut) = txOut
-- this will never error as we can only have either CoreTxOut or VariantTxOut
extractCoreTxOut (VTxOutW _ _) = error "Unexpected VTxOut in CoreTxOut list"

extractVariantTxOut :: TxOutW -> VA.TxOut
extractVariantTxOut (VTxOutW txOut _) = txOut
-- this will never error as we can only have either CoreTxOut or VariantTxOut
extractVariantTxOut (CTxOutW _) = error "Unexpected CTxOut in VariantTxOut list"

convertTxOutIdCore :: [TxOutIdW] -> [VC.TxOutId]
convertTxOutIdCore = mapMaybe unwrapCore
  where
    unwrapCore (CTxOutIdW txOutid) = Just txOutid
    unwrapCore _ = Nothing

convertTxOutIdVariant :: [TxOutIdW] -> [VA.TxOutId]
convertTxOutIdVariant = mapMaybe unwrapVariant
  where
    unwrapVariant (VTxOutIdW txOutid) = Just txOutid
    unwrapVariant _ = Nothing

convertMaTxOutIdCore :: [MaTxOutIdW] -> [VC.MaTxOutId]
convertMaTxOutIdCore = mapMaybe unwrapCore
  where
    unwrapCore (CMaTxOutIdW maTxOutId) = Just maTxOutId
    unwrapCore _ = Nothing

convertMaTxOutIdVariant :: [MaTxOutIdW] -> [VA.MaTxOutId]
convertMaTxOutIdVariant = mapMaybe unwrapVariant
  where
    unwrapVariant (VMaTxOutIdW maTxOutId) = Just maTxOutId
    unwrapVariant _ = Nothing

isTxOutVariantCore :: TxOutVariantType -> Bool
isTxOutVariantCore TxOutVariantCore = True
isTxOutVariantCore TxOutVariantAddress = False

isTxOutVariantAddress :: TxOutVariantType -> Bool
isTxOutVariantAddress TxOutVariantAddress = True
isTxOutVariantAddress TxOutVariantCore = False
