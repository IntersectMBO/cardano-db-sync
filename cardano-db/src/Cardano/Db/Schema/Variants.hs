module Cardano.Db.Schema.Variants where

import qualified Cardano.Db.Schema.Ids as Id
import qualified Cardano.Db.Schema.Variants.TxOutAddress as VA
import qualified Cardano.Db.Schema.Variants.TxOutCore as VC
import Cardano.Prelude (ByteString, Text)

-- |
-- This module implements a schema variants system that allows db-sync to support different
-- database layouts for the same logical data. Currently used for TxOut-related tables.
--
-- Two variants exist:
-- - Core (default): Stores address data inline in tx_out tables (denormalised)
-- - Address: Normalises addresses into a separate 'address' table for storage efficiency
--
-- Users configure this via 'tx_out.use_address_table' in the config file (see doc/configuration.md).
-- Since the schema variant is determined by runtime configuration rather than compile time,
-- we cannot use the type system alone to handle the different schemas. The wrapper types
-- (TxOutW, TxOutIdW, etc.) use sum types to provide runtime polymorphism, allowing code to
-- work with either variant through pattern matching. This design can be extended to other
-- tables following the same pattern.

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
