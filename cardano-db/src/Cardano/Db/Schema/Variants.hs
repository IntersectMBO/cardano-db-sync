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
  = VCCollateralTxOutW !VC.CollateralTxOutCore
  | VACollateralTxOutW !VA.CollateralTxOutAddress
  deriving (Eq, Show)

data CollateralTxOutIdW
  = VCCollateralTxOutIdW !Id.CollateralTxOutCoreId
  | VACollateralTxOutIdW !Id.CollateralTxOutAddressId
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

unwrapTxOutIdCore :: TxOutIdW -> Maybe Id.TxOutCoreId
unwrapTxOutIdCore (VCTxOutIdW txOutid) = Just txOutid
unwrapTxOutIdCore _ = Nothing

----------------------------------------------------------------------------------

unwrapTxOutIdAddress :: TxOutIdW -> Maybe Id.TxOutAddressId
unwrapTxOutIdAddress (VATxOutIdW txOutid) = Just txOutid
unwrapTxOutIdAddress _ = Nothing

----------------------------------------------------------------------------------

unwrapMaTxOutIdCore :: MaTxOutIdW -> Maybe Id.MaTxOutCoreId
unwrapMaTxOutIdCore (CMaTxOutIdW maTxOutId) = Just maTxOutId
unwrapMaTxOutIdCore _ = Nothing

----------------------------------------------------------------------------------

unwrapMaTxOutIdAddress :: MaTxOutIdW -> Maybe Id.MaTxOutAddressId
unwrapMaTxOutIdAddress (VMaTxOutIdW maTxOutId) = Just maTxOutId
unwrapMaTxOutIdAddress _ = Nothing

----------------------------------------------------------------------------------

unwrapCollateralTxOutIdAddress :: CollateralTxOutIdW -> Maybe Id.CollateralTxOutAddressId
unwrapCollateralTxOutIdAddress (VACollateralTxOutIdW iD) = Just iD
unwrapCollateralTxOutIdAddress _ = Nothing
