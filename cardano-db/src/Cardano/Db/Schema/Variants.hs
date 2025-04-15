module Cardano.Db.Schema.Variants where

import qualified Cardano.Db.Schema.Ids as Id
import qualified Cardano.Db.Schema.Variants.TxOutAddress as VA
import qualified Cardano.Db.Schema.Variants.TxOutCore as VC
import Cardano.Prelude (ByteString, Text)

--------------------------------------------------------------------------------
-- TxOutVariantType
--------------------------------------------------------------------------------
data TxOutVariantType = TxOutVariantCore | TxOutVariantAddress
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- TxOutW
--------------------------------------------------------------------------------
data TxOutW
  = VCTxOutW !VC.TxOutCore
  | VATxOutW !VA.TxOutAddress !(Maybe VA.Address)
  deriving (Eq, Show)

data TxOutIdW
  = VCTxOutIdW !Id.TxOutCoreId
  | VATxOutIdW !Id.TxOutAddressId
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- MaTxOutW
--------------------------------------------------------------------------------
data MaTxOutW
  = CMaTxOutW !VC.MaTxOutCore
  | VMaTxOutW !VA.MaTxOutAddress
  deriving (Eq, Show)

data MaTxOutIdW
  = CMaTxOutIdW !Id.MaTxOutCoreId
  | VMaTxOutIdW !Id.MaTxOutAddressId
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- CollateralTxOutW
--------------------------------------------------------------------------------
data CollateralTxOutW
  = CCollateralTxOutW !VC.CollateralTxOutCore
  | VCollateralTxOutW !VA.CollateralTxOutAddress
  deriving (Eq, Show)

data CollateralTxOutIdW
  = CCollateralTxOutIdW !Id.CollateralTxOutCoreId
  | VCollateralTxOutIdW !Id.CollateralTxOutAddressId
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- UTXOQueryResult
--------------------------------------------------------------------------------

-- | UtxoQueryResult which has utxoAddress that can come from Core or Variant TxOut
data UtxoQueryResult = UtxoQueryResult
  { utxoTxOutW :: !TxOutW
  , utxoAddress :: !Text
  , utxoTxHash :: !ByteString
  }

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

-- convertTxOutIdCore :: [TxOutIdW] -> [Id.TxOutCoreId]
-- convertTxOutIdCore = mapMaybe unwrapTxOutIdCore

unwrapTxOutIdCore :: TxOutIdW -> Maybe Id.TxOutCoreId
unwrapTxOutIdCore (VCTxOutIdW txOutid) = Just txOutid
unwrapTxOutIdCore _ = Nothing

-- --------------------------------------------------------------------------------
-- convertTxOutIdAddress :: [TxOutIdW] -> [Id.TxOutAddressId]
-- convertTxOutIdAddress = mapMaybe unwrapTxOutIdAddress

unwrapTxOutIdAddress :: TxOutIdW -> Maybe Id.TxOutAddressId
unwrapTxOutIdAddress (VATxOutIdW txOutid) = Just txOutid
unwrapTxOutIdAddress _ = Nothing

-- --------------------------------------------------------------------------------
-- convertMaTxOutIdCore :: [MaTxOutIdW] -> [Id.MaTxOutCoreId]
-- convertMaTxOutIdCore = mapMaybe unwrapMaTxOutIdCore

unwrapMaTxOutIdCore :: MaTxOutIdW -> Maybe Id.MaTxOutCoreId
unwrapMaTxOutIdCore (CMaTxOutIdW maTxOutId) = Just maTxOutId
unwrapMaTxOutIdCore _ = Nothing

-- --------------------------------------------------------------------------------
-- convertMaTxOutIdAddress :: [MaTxOutIdW] -> [Id.MaTxOutAddressId]
-- convertMaTxOutIdAddress = mapMaybe unwrapMaTxOutIdAddress

unwrapMaTxOutIdAddress :: MaTxOutIdW -> Maybe Id.MaTxOutAddressId
unwrapMaTxOutIdAddress (VMaTxOutIdW maTxOutId) = Just maTxOutId
unwrapMaTxOutIdAddress _ = Nothing

-- --------------------------------------------------------------------------------
-- convertCollateralTxOutIdCore :: [CollateralTxOutIdW] -> [Id.CollateralTxOutCoreId]
-- convertCollateralTxOutIdCore = mapMaybe unwrapCollateralTxOutIdCore

unwrapCollateralTxOutIdCore :: CollateralTxOutIdW -> Maybe Id.CollateralTxOutCoreId
unwrapCollateralTxOutIdCore (CCollateralTxOutIdW iD) = Just iD
unwrapCollateralTxOutIdCore _ = Nothing

-- --------------------------------------------------------------------------------
-- convertCollateralTxOutIdAddress :: [CollateralTxOutIdW] -> [Id.CollateralTxOutAddressId]
-- convertCollateralTxOutIdAddress = mapMaybe unwrapCollateralTxOutIdAddress

unwrapCollateralTxOutIdAddress :: CollateralTxOutIdW -> Maybe Id.CollateralTxOutAddressId
unwrapCollateralTxOutIdAddress (VCollateralTxOutIdW iD) = Just iD
unwrapCollateralTxOutIdAddress _ = Nothing

--------------------------------------------------------------------------------
isTxOutCore :: TxOutVariantType -> Bool
isTxOutCore TxOutVariantCore = True
isTxOutCore _ = False

isTxOutAddress :: TxOutVariantType -> Bool
isTxOutAddress TxOutVariantAddress = True
isTxOutAddress _ = False
