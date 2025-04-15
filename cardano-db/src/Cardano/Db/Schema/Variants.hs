module Cardano.Db.Schema.Variants (
  TxOutTableType (..),
  TxOutW (..),
  TxOutIdW (..),
  MaTxOutW (..),
  MaTxOutIdW (..),
  CollateralTxOutW (..),
  CollateralTxOutIdW (..),
  UtxoQueryResult (..),
  convertTxOutIdCore,
  convertTxOutIdAddress,
  convertMaTxOutIdCore,
  convertMaTxOutIdAddress,
  convertCollateralTxOutIdCore,
  convertCollateralTxOutIdAddress,
  isTxOutCore,
  isTxOutAddress,
  module X,
) where

import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Schema.Variants.TxOutAddress as X
import qualified Cardano.Db.Schema.Variants.TxOutAddress as V
import Cardano.Db.Schema.Variants.TxOutCore as X
import qualified Cardano.Db.Schema.Variants.TxOutCore as C
import Cardano.Db.Schema.Variants.TxOutUtxoHd as X
import Cardano.Db.Schema.Variants.TxOutUtxoHdAddress as X
import Cardano.Prelude (ByteString, Text, mapMaybe)

--------------------------------------------------------------------------------
-- TxOutTableType
--------------------------------------------------------------------------------
data TxOutTableType = TxOutTableCore | TxOutTableVariantAddress
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- TxOutW
--------------------------------------------------------------------------------
data TxOutW
  = CTxOutW !C.TxOutCore
  | VTxOutW !V.TxOutAddress !(Maybe V.Address)
  deriving (Eq, Show)

data TxOutIdW
  = CTxOutIdW !Id.TxOutCoreId
  | VTxOutIdW !Id.TxOutAddressId
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- MaTxOutW
--------------------------------------------------------------------------------
data MaTxOutW
  = CMaTxOutW !C.MaTxOutCore
  | VMaTxOutW !V.MaTxOutAddress
  deriving (Eq, Show)

data MaTxOutIdW
  = CMaTxOutIdW !Id.MaTxOutCoreId
  | VMaTxOutIdW !Id.MaTxOutAddressId
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- CollateralTxOutW
--------------------------------------------------------------------------------
data CollateralTxOutW
  = CCollateralTxOutW !C.CollateralTxOutCore
  | VCollateralTxOutW !V.CollateralTxOutAddress
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
convertTxOutIdCore :: [TxOutIdW] -> [Id.TxOutCoreId]
convertTxOutIdCore = mapMaybe unwrapCore
  where
    unwrapCore (CTxOutIdW txOutid) = Just txOutid
    unwrapCore _ = Nothing

convertTxOutIdAddress :: [TxOutIdW] -> [Id.TxOutAddressId]
convertTxOutIdAddress = mapMaybe unwrapVariant
  where
    unwrapVariant (VTxOutIdW txOutid) = Just txOutid
    unwrapVariant _ = Nothing

convertMaTxOutIdCore :: [MaTxOutIdW] -> [Id.MaTxOutCoreId]
convertMaTxOutIdCore = mapMaybe unwrapCore
  where
    unwrapCore (CMaTxOutIdW maTxOutId) = Just maTxOutId
    unwrapCore _ = Nothing

convertMaTxOutIdAddress :: [MaTxOutIdW] -> [Id.MaTxOutAddressId]
convertMaTxOutIdAddress = mapMaybe unwrapVariant
  where
    unwrapVariant (VMaTxOutIdW maTxOutId) = Just maTxOutId
    unwrapVariant _ = Nothing

convertCollateralTxOutIdCore :: [CollateralTxOutIdW] -> [Id.CollateralTxOutCoreId]
convertCollateralTxOutIdCore = mapMaybe unwrapCore
  where
    unwrapCore (CCollateralTxOutIdW iD) = Just iD
    unwrapCore _ = Nothing

convertCollateralTxOutIdAddress :: [CollateralTxOutIdW] -> [Id.CollateralTxOutAddressId]
convertCollateralTxOutIdAddress = mapMaybe unwrapVariant
  where
    unwrapVariant (VCollateralTxOutIdW iD) = Just iD
    unwrapVariant _ = Nothing

isTxOutCore :: TxOutTableType -> Bool
isTxOutCore TxOutTableCore = True
isTxOutCore _ = False

isTxOutAddress :: TxOutTableType -> Bool
isTxOutAddress TxOutTableVariantAddress = True
isTxOutAddress _ = False
