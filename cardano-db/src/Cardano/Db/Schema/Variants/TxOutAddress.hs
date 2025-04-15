{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Db.Schema.Variants.TxOutAddress where

import Contravariant.Extras (contrazip3, contrazip9)
import Data.ByteString.Char8 (ByteString)
import Data.Functor.Contravariant ((>$<))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Statement.Function.Core (bulkEncoder)
import Cardano.Db.Statement.Types (DbInfo (..), Entity (..), Key)
import Cardano.Db.Types (DbLovelace, DbWord64 (..), dbLovelaceDecoder, dbLovelaceEncoder, dbLovelaceValueEncoder)

-----------------------------------------------------------------------------------------------
-- TxOutAddress
-----------------------------------------------------------------------------------------------
data TxOutAddress = TxOutAddress
  { txOutAddressTxId :: !Id.TxId
  , txOutAddressIndex :: !Word64
  , txOutAddressStakeAddressId :: !(Maybe Id.StakeAddressId)
  , txOutAddressValue :: !DbLovelace
  , txOutAddressDataHash :: !(Maybe ByteString)
  , txOutAddressInlineDatumId :: !(Maybe Id.DatumId)
  , txOutAddressReferenceScriptId :: !(Maybe Id.ScriptId)
  , txOutAddressConsumedByTxId :: !(Maybe Id.TxId)
  , txOutAddressAddressId :: !Id.AddressId
  }
  deriving (Eq, Show, Generic)

type instance Key TxOutAddress = Id.TxOutAddressId

instance DbInfo TxOutAddress where
  tableName _ = "tx_out"
  columnNames _ =
    NE.fromList
      [ "tx_id"
      , "index"
      , "stake_address_id"
      , "value"
      , "data_hash"
      , "inline_datum_id"
      , "reference_script_id"
      , "consumed_by_tx_id"
      , "address_id"
      ]

entityTxOutAddressDecoder :: D.Row (Entity TxOutAddress)
entityTxOutAddressDecoder =
  Entity
    <$> Id.idDecoder Id.TxOutAddressId -- entityTxOutAddressId
    <*> txOutAddressDecoder -- entityTxOutAddress

txOutAddressDecoder :: D.Row TxOutAddress
txOutAddressDecoder =
  TxOutAddress
    <$> Id.idDecoder Id.TxId -- txOutAddressTxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- txOutAddressIndex
    <*> Id.maybeIdDecoder Id.StakeAddressId -- txOutAddressStakeAddressId
    <*> dbLovelaceDecoder -- txOutAddressValue
    <*> D.column (D.nullable D.bytea) -- txOutAddressDataHash
    <*> Id.maybeIdDecoder Id.DatumId -- txOutAddressInlineDatumId
    <*> Id.maybeIdDecoder Id.ScriptId -- txOutAddressReferenceScriptId
    <*> Id.maybeIdDecoder Id.TxId -- txOutAddressConsumedByTxId
    <*> Id.idDecoder Id.AddressId -- txOutAddressAddressId

txOutAddressEncoder :: E.Params TxOutAddress
txOutAddressEncoder =
  mconcat
    [ txOutAddressTxId >$< Id.idEncoder Id.getTxId
    , txOutAddressIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , txOutAddressStakeAddressId >$< Id.maybeIdEncoder Id.getStakeAddressId
    , txOutAddressValue >$< dbLovelaceEncoder
    , txOutAddressDataHash >$< E.param (E.nullable E.bytea)
    , txOutAddressInlineDatumId >$< Id.maybeIdEncoder Id.getDatumId
    , txOutAddressReferenceScriptId >$< Id.maybeIdEncoder Id.getScriptId
    , txOutAddressConsumedByTxId >$< Id.maybeIdEncoder Id.getTxId
    , txOutAddressAddressId >$< Id.idEncoder Id.getAddressId
    ]

txOutAddressBulkEncoder :: E.Params ([Id.TxId], [Word64], [Maybe Id.StakeAddressId], [DbLovelace], [Maybe ByteString], [Maybe Id.DatumId], [Maybe Id.ScriptId], [Maybe Id.TxId], [Id.AddressId])
txOutAddressBulkEncoder =
  contrazip9
    (bulkEncoder $ E.nonNullable $ Id.getTxId >$< E.int8) -- txOutAddressTxId
    (bulkEncoder $ E.nonNullable $ fromIntegral >$< E.int8) -- txOutAddressIndex
    (bulkEncoder $ E.nullable $ Id.getStakeAddressId >$< E.int8) -- txOutAddressStakeAddressId
    (bulkEncoder dbLovelaceValueEncoder) -- txOutAddressValue
    (bulkEncoder $ E.nullable E.bytea) -- txOutAddressDataHash
    (bulkEncoder $ E.nullable $ Id.getDatumId >$< E.int8) -- txOutAddressInlineDatumId
    (bulkEncoder $ E.nullable $ Id.getScriptId >$< E.int8) -- txOutAddressReferenceScriptId
    (bulkEncoder $ E.nullable $ Id.getTxId >$< E.int8) -- txOutAddressConsumedByTxId
    (bulkEncoder $ E.nonNullable $ Id.getAddressId >$< E.int8) -- txOutAddressAddressId

-----------------------------------------------------------------------------------------------
-- CollateralTxOutAddress
-----------------------------------------------------------------------------------------------
data CollateralTxOutAddress = CollateralTxOutAddress
  { collateralTxOutAddressTxId :: !Id.TxId
  , collateralTxOutAddressIndex :: !Word64
  , collateralTxOutAddressStakeAddressId :: !(Maybe Id.StakeAddressId)
  , collateralTxOutAddressValue :: !DbLovelace
  , collateralTxOutAddressDataHash :: !(Maybe ByteString)
  , collateralTxOutAddressMultiAssetsDescr :: !Text
  , collateralTxOutAddressInlineDatumId :: !(Maybe Id.DatumId)
  , collateralTxOutAddressReferenceScriptId :: !(Maybe Id.ScriptId)
  , collateralTxOutAddressId :: !Id.AddressId
  }
  deriving (Eq, Show, Generic)

type instance Key CollateralTxOutAddress = Id.CollateralTxOutAddressId

instance DbInfo CollateralTxOutAddress where
  tableName _ = "collateral_tx_out"
  columnNames _ =
    NE.fromList
      [ "tx_id"
      , "index"
      , "stake_address_id"
      , "value"
      , "data_hash"
      , "multi_assets_descr"
      , "inline_datum_id"
      , "reference_script_id"
      , "address_id"
      ]

entityCollateralTxOutAddressDecoder :: D.Row (Entity CollateralTxOutAddress)
entityCollateralTxOutAddressDecoder =
  Entity
    <$> Id.idDecoder Id.CollateralTxOutAddressId -- entityCollateralTxOutAddressId
    <*> collateralTxOutAddressDecoder -- entityCollateralTxOutAddress

collateralTxOutAddressDecoder :: D.Row CollateralTxOutAddress
collateralTxOutAddressDecoder =
  CollateralTxOutAddress
    <$> Id.idDecoder Id.TxId -- collateralTxOutAddressTxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- collateralTxOutAddressIndex
    <*> Id.maybeIdDecoder Id.StakeAddressId -- collateralTxOutAddressStakeAddressId
    <*> dbLovelaceDecoder -- collateralTxOutAddressValue
    <*> D.column (D.nullable D.bytea) -- collateralTxOutAddressDataHash
    <*> D.column (D.nonNullable D.text) -- collateralTxOutAddressMultiAssetsDescr
    <*> Id.maybeIdDecoder Id.DatumId -- collateralTxOutAddressInlineDatumId
    <*> Id.maybeIdDecoder Id.ScriptId -- collateralTxOutAddressReferenceScriptId
    <*> Id.idDecoder Id.AddressId -- collateralTxOutAddressId

collateralTxOutAddressEncoder :: E.Params CollateralTxOutAddress
collateralTxOutAddressEncoder =
  mconcat
    [ collateralTxOutAddressTxId >$< Id.idEncoder Id.getTxId
    , collateralTxOutAddressIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , collateralTxOutAddressStakeAddressId >$< Id.maybeIdEncoder Id.getStakeAddressId
    , collateralTxOutAddressValue >$< dbLovelaceEncoder
    , collateralTxOutAddressDataHash >$< E.param (E.nullable E.bytea)
    , collateralTxOutAddressMultiAssetsDescr >$< E.param (E.nonNullable E.text)
    , collateralTxOutAddressInlineDatumId >$< Id.maybeIdEncoder Id.getDatumId
    , collateralTxOutAddressReferenceScriptId >$< Id.maybeIdEncoder Id.getScriptId
    , collateralTxOutAddressId >$< Id.idEncoder Id.getAddressId
    ]

-----------------------------------------------------------------------------------------------
-- Address
-----------------------------------------------------------------------------------------------
data Address = Address
  { addressAddress :: !Text
  , addressRaw :: !ByteString
  , addressHasScript :: !Bool
  , addressPaymentCred :: !(Maybe ByteString)
  , addressStakeAddressId :: !(Maybe Id.StakeAddressId)
  }
  deriving (Eq, Show, Generic)

type instance Key Address = Id.AddressId
instance DbInfo Address

entityAddressDecoder :: D.Row (Entity Address)
entityAddressDecoder =
  Entity
    <$> Id.idDecoder Id.AddressId -- entityAddressId
    <*> addressDecoder -- entityAddress

addressDecoder :: D.Row Address
addressDecoder =
  Address
    <$> D.column (D.nonNullable D.text) -- addressAddress
    <*> D.column (D.nonNullable D.bytea) -- addressRaw
    <*> D.column (D.nonNullable D.bool) -- addressHasScript
    <*> D.column (D.nullable D.bytea) -- addressPaymentCred
    <*> Id.maybeIdDecoder Id.StakeAddressId -- addressStakeAddressId

addressEncoder :: E.Params Address
addressEncoder =
  mconcat
    [ addressAddress >$< E.param (E.nonNullable E.text)
    , addressRaw >$< E.param (E.nonNullable E.bytea)
    , addressHasScript >$< E.param (E.nonNullable E.bool)
    , addressPaymentCred >$< E.param (E.nullable E.bytea)
    , addressStakeAddressId >$< Id.maybeIdEncoder Id.getStakeAddressId
    ]

-----------------------------------------------------------------------------------------------
-- MultiAssetTxOut
-----------------------------------------------------------------------------------------------
data MaTxOutAddress = MaTxOutAddress
  { maTxOutAddressIdent :: !Id.MultiAssetId
  , maTxOutAddressQuantity :: !DbWord64
  , maTxOutAddressTxOutId :: !Id.TxOutAddressId
  }
  deriving (Eq, Show, Generic)

type instance Key MaTxOutAddress = Id.MaTxOutAddressId

instance DbInfo MaTxOutAddress where
  tableName _ = "ma_tx_out"
  columnNames _ =
    NE.fromList
      [ "ident"
      , "quantity"
      , "tx_out_id"
      ]

entityMaTxOutAddressDecoder :: D.Row (Entity MaTxOutAddress)
entityMaTxOutAddressDecoder =
  Entity
    <$> Id.idDecoder Id.MaTxOutAddressId -- entityMaTxOutAddressId
    <*> maTxOutAddressDecoder -- entityMaTxOutAddress

maTxOutAddressDecoder :: D.Row MaTxOutAddress
maTxOutAddressDecoder =
  MaTxOutAddress
    <$> Id.idDecoder Id.MultiAssetId -- maTxOutAddressIdent
    <*> D.column (D.nonNullable $ DbWord64 . fromIntegral <$> D.int8) -- maTxOutAddressQuantity
    <*> Id.idDecoder Id.TxOutAddressId -- maTxOutAddressTxOutId

maTxOutAddressEncoder :: E.Params MaTxOutAddress
maTxOutAddressEncoder =
  mconcat
    [ maTxOutAddressIdent >$< Id.idEncoder Id.getMultiAssetId
    , maTxOutAddressQuantity >$< E.param (E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8)
    , maTxOutAddressTxOutId >$< Id.idEncoder Id.getTxOutAddressId
    ]

maTxOutAddressBulkEncoder :: E.Params ([Id.MultiAssetId], [DbWord64], [Id.TxOutAddressId])
maTxOutAddressBulkEncoder =
  contrazip3
    (bulkEncoder $ E.nonNullable $ Id.getMultiAssetId >$< E.int8) -- maTxOutAddressIdent
    (bulkEncoder $ E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8) -- maTxOutAddressQuantity
    (bulkEncoder $ E.nonNullable $ Id.getTxOutAddressId >$< E.int8) -- maTxOutAddressTxOutId

-- share
--   [ mkPersist sqlSettings
--   , mkMigrate "migrateVariantAddressCardanoDb"
--   , mkEntityDefList "entityDefsTxOutAddress"
--   , deriveShowFields
--   ]
--   [persistLowerCase|
-- ----------------------------------------------
-- -- Variant Address TxOutAddress
-- ----------------------------------------------
--   TxOutAddress
--     addressId           AddressId           noreference
--     consumedByTxId      TxId Maybe          noreference
--     dataHash            ByteString Maybe    sqltype=hash32type
--     index               Word64              sqltype=txindex
--     inlineDatumId       DatumId Maybe       noreference
--     referenceScriptId   ScriptId Maybe      noreference
--     stakeAddressId      Id.StakeAddressId Maybe noreference
--     txId                TxId                noreference
--     value               DbLovelace          sqltype=lovelace
--     UniqueTxout         txId index          -- The (tx_id, index) pair must be unique.

--   CollateralTxOutAddress
--     txId                TxId                noreference     -- This type is the primary key for the 'tx' table.
--     index               Word64              sqltype=txindex
--     addressId           AddressId
--     stakeAddressId      Id.StakeAddressId Maybe noreference
--     value               DbLovelace          sqltype=lovelace
--     dataHash            ByteString Maybe    sqltype=hash32type
--     multiAssetsDescr    Text
--     inlineDatumId       DatumId Maybe       noreference
--     referenceScriptId   ScriptId Maybe      noreference
--     deriving Show

--   Address
--     address             Text
--     raw                 ByteString
--     hasScript           Bool
--     paymentCred         ByteString Maybe    sqltype=hash28type
--     stakeAddressId      Id.StakeAddressId Maybe noreference

-- ----------------------------------------------
-- -- MultiAsset
-- ----------------------------------------------
--   MaTxOutAddress
--     ident               MultiAssetId        noreference
--     quantity            DbWord64            sqltype=word64type
--     txOutAddressId             TxOutAddressId             noreference
--     deriving Show

-- | ]

-- schemaDocsTxOutAddress :: [EntityDef]
-- schemaDocsTxOutAddress =
--   document entityDefsTxOutAddress $ do
--     TxOutAddress --^ do
--       "A table for transaction outputs."
--       TxOutAddressId # "The Address table index for the output address."
--       TxOutAddressConsumedByTxId # "The Tx table index of the transaction that consumes this transaction output. Not populated by default, can be activated via tx-out configs."
--       TxOutAddressDataHash # "The hash of the transaction output datum. (NULL for Txs without scripts)."
--       TxOutAddressIndex # "The index of this transaction output with the transaction."
--       TxOutAddressInlineDatumId # "The inline datum of the output, if it has one. New in v13."
--       TxOutAddressReferenceScriptId # "The reference script of the output, if it has one. New in v13."
--       TxOutAddressValue # "The output value (in Lovelace) of the transaction output."
--       TxOutAddressTxId # "The Tx table index of the transaction that contains this transaction output."

--     CollateralTxOutAddress --^ do
--       "A table for transaction collateral outputs. New in v13."
--       CollateralTxOutAddressTxId # "The Address table index for the output address."
--       CollateralTxOutAddressIndex # "The index of this transaction output with the transaction."
--       CollateralTxOutAddressId # "The human readable encoding of the output address. Will be Base58 for Byron era addresses and Bech32 for Shelley era."
--       CollateralTxOutAddressStakeAddressId # "The StakeAddress table index for the stake address part of the Shelley address. (NULL for Byron addresses)."
--       CollateralTxOutAddressValue # "The output value (in Lovelace) of the transaction output."
--       CollateralTxOutAddressDataHash # "The hash of the transaction output datum. (NULL for Txs without scripts)."
--       CollateralTxOutAddressMultiAssetsDescr # "This is a description of the multiassets in collateral output. Since the output is not really created, we don't need to add them in separate tables."
--       CollateralTxOutAddressInlineDatumId # "The inline datum of the output, if it has one. New in v13."
--       CollateralTxOutAddressReferenceScriptId # "The reference script of the output, if it has one. New in v13."

--     Address --^ do
--       "A table for addresses that appear in outputs."
--       AddressAddress # "The human readable encoding of the output address. Will be Base58 for Byron era addresses and Bech32 for Shelley era."
--       AddressRaw # "The raw binary address."
--       AddressHasScript # "Flag which shows if this address is locked by a script."
--       AddressPaymentCred # "The payment credential part of the Shelley address. (NULL for Byron addresses). For a script-locked address, this is the script hash."
--       AddressStakeAddressId # "The StakeAddress table index for the stake address part of the Shelley address. (NULL for Byron addresses)."

--     MaTxOutAddress --^ do
--       "A table containing Multi-Asset transaction outputs."
--       MaTxOutAddressIdent # "The MultiAsset table index specifying the asset."
--       MaTxOutAddressQuantity # "The Multi Asset transaction output amount (denominated in the Multi Asset)."
--       MaTxOutAddressTxOutAddressId # "The TxOutAddress table index for the transaction that this Multi Asset transaction output."
