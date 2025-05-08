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
  unwrapTxOutIdCore,
  convertTxOutIdAddress,
  unwrapTxOutIdAddress,
  convertMaTxOutIdCore,
  unwrapMaTxOutIdCore,
  convertMaTxOutIdAddress,
  unwrapMaTxOutIdAddress,
  convertCollateralTxOutIdCore,
  unwrapCollateralTxOutIdCore,
  convertCollateralTxOutIdAddress,
  unwrapCollateralTxOutIdAddress,
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
convertTxOutIdCore = mapMaybe unwrapTxOutIdCore

unwrapTxOutIdCore :: TxOutIdW -> Maybe Id.TxOutCoreId
unwrapTxOutIdCore (CTxOutIdW txOutid) = Just txOutid
unwrapTxOutIdCore _ = Nothing

--------------------------------------------------------------------------------
convertTxOutIdAddress :: [TxOutIdW] -> [Id.TxOutAddressId]
convertTxOutIdAddress = mapMaybe unwrapTxOutIdAddress

unwrapTxOutIdAddress :: TxOutIdW -> Maybe Id.TxOutAddressId
unwrapTxOutIdAddress (VTxOutIdW txOutid) = Just txOutid
unwrapTxOutIdAddress _ = Nothing

--------------------------------------------------------------------------------
convertMaTxOutIdCore :: [MaTxOutIdW] -> [Id.MaTxOutCoreId]
convertMaTxOutIdCore = mapMaybe unwrapMaTxOutIdCore

unwrapMaTxOutIdCore :: MaTxOutIdW -> Maybe Id.MaTxOutCoreId
unwrapMaTxOutIdCore (CMaTxOutIdW maTxOutId) = Just maTxOutId
unwrapMaTxOutIdCore _ = Nothing

--------------------------------------------------------------------------------
convertMaTxOutIdAddress :: [MaTxOutIdW] -> [Id.MaTxOutAddressId]
convertMaTxOutIdAddress = mapMaybe unwrapMaTxOutIdAddress

unwrapMaTxOutIdAddress :: MaTxOutIdW -> Maybe Id.MaTxOutAddressId
unwrapMaTxOutIdAddress (VMaTxOutIdW maTxOutId) = Just maTxOutId
unwrapMaTxOutIdAddress _ = Nothing

--------------------------------------------------------------------------------
convertCollateralTxOutIdCore :: [CollateralTxOutIdW] -> [Id.CollateralTxOutCoreId]
convertCollateralTxOutIdCore = mapMaybe unwrapCollateralTxOutIdCore

unwrapCollateralTxOutIdCore :: CollateralTxOutIdW -> Maybe Id.CollateralTxOutCoreId
unwrapCollateralTxOutIdCore (CCollateralTxOutIdW iD) = Just iD
unwrapCollateralTxOutIdCore _ = Nothing

--------------------------------------------------------------------------------
convertCollateralTxOutIdAddress :: [CollateralTxOutIdW] -> [Id.CollateralTxOutAddressId]
convertCollateralTxOutIdAddress = mapMaybe unwrapCollateralTxOutIdAddress

unwrapCollateralTxOutIdAddress :: CollateralTxOutIdW -> Maybe Id.CollateralTxOutAddressId
unwrapCollateralTxOutIdAddress (VCollateralTxOutIdW iD) = Just iD
unwrapCollateralTxOutIdAddress _ = Nothing

--------------------------------------------------------------------------------
isTxOutCore :: TxOutTableType -> Bool
isTxOutCore TxOutTableCore = True
isTxOutCore _ = False

isTxOutAddress :: TxOutTableType -> Bool
isTxOutAddress TxOutTableVariantAddress = True
isTxOutAddress _ = False
