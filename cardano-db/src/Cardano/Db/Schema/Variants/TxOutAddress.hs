{-# LANGUAGE DeriveGeneric #-}

module Cardano.Db.Schema.Variants.TxOutAddress where

import Cardano.Db.Schema.Ids
import Cardano.Db.Types (DbLovelace, DbWord64 (..), dbLovelaceDecoder, dbLovelaceEncoder)
import Data.ByteString.Char8 (ByteString)
import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

-----------------------------------------------------------------------------------------------
-- TxOutAddress
-----------------------------------------------------------------------------------------------
data TxOutAddress = TxOutAddress
  { txOutAddressId :: !TxOutAddressId
  , txOutAddressTxId :: !TxId
  , txOutAddressIndex :: !Word64
  , txOutAddressStakeAddressId :: !(Maybe StakeAddressId)
  , txOutAddressValue :: !DbLovelace
  , txOutAddressDataHash :: !(Maybe ByteString)
  , txOutAddressInlineDatumId :: !(Maybe DatumId)
  , txOutAddressReferenceScriptId :: !(Maybe ScriptId)
  , txOutAddressConsumedByTxId :: !(Maybe TxId)
  , txOutAddressAddressId :: !AddressId
  }
  deriving (Eq, Show, Generic)

txOutAddressDecoder :: D.Row TxOutAddress
txOutAddressDecoder =
  TxOutAddress
    <$> idDecoder TxOutAddressId -- txOutAddressId
    <*> idDecoder TxId -- txOutAddressTxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- txOutAddressIndex
    <*> maybeIdDecoder StakeAddressId -- txOutAddressStakeAddressId
    <*> dbLovelaceDecoder -- txOutAddressValue
    <*> D.column (D.nullable D.bytea) -- txOutAddressDataHash
    <*> maybeIdDecoder DatumId -- txOutAddressInlineDatumId
    <*> maybeIdDecoder ScriptId -- txOutAddressReferenceScriptId
    <*> maybeIdDecoder TxId -- txOutAddressConsumedByTxId
    <*> idDecoder AddressId -- txOutAddressAddressId

txOutAddressEncoder :: E.Params TxOutAddress
txOutAddressEncoder =
  mconcat
    [ txOutAddressId >$< idEncoder getTxOutAddressId
    , txOutAddressTxId >$< idEncoder getTxId
    , txOutAddressIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , txOutAddressStakeAddressId >$< maybeIdEncoder getStakeAddressId
    , txOutAddressValue >$< dbLovelaceEncoder
    , txOutAddressDataHash >$< E.param (E.nullable E.bytea)
    , txOutAddressInlineDatumId >$< maybeIdEncoder getDatumId
    , txOutAddressReferenceScriptId >$< maybeIdEncoder getScriptId
    , txOutAddressConsumedByTxId >$< maybeIdEncoder getTxId
    , txOutAddressAddressId >$< idEncoder getAddressId
    ]

-----------------------------------------------------------------------------------------------
-- CollateralTxOutAddress
-----------------------------------------------------------------------------------------------
data CollateralTxOutAddress = CollateralTxOutAddress
  { colateralTxOutAddressId :: !TxOutAddressId
  , collateralTxOutAddressTxId :: !TxId
  , collateralTxOutAddressIndex :: !Word64
  , collateralTxOutAddressStakeAddressId :: !(Maybe StakeAddressId)
  , collateralTxOutAddressValue :: !DbLovelace
  , collateralTxOutAddressDataHash :: !(Maybe ByteString)
  , collateralTxOutAddressMultiAssetsDescr :: !Text
  , collateralTxOutAddressInlineDatumId :: !(Maybe DatumId)
  , collateralTxOutAddressReferenceScriptId :: !(Maybe ScriptId)
  , collateralTxOutAddressId :: !AddressId
  }
  deriving (Eq, Show, Generic)

collateralTxOutAddressDecoder :: D.Row CollateralTxOutAddress
collateralTxOutAddressDecoder =
  CollateralTxOutAddress
    <$> idDecoder TxOutAddressId -- colateralTxOutAddressId
    <*> idDecoder TxId -- collateralTxOutAddressTxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- collateralTxOutAddressIndex
    <*> maybeIdDecoder StakeAddressId -- collateralTxOutAddressStakeAddressId
    <*> dbLovelaceDecoder -- collateralTxOutAddressValue
    <*> D.column (D.nullable D.bytea) -- collateralTxOutAddressDataHash
    <*> D.column (D.nonNullable D.text) -- collateralTxOutAddressMultiAssetsDescr
    <*> maybeIdDecoder DatumId -- collateralTxOutAddressInlineDatumId
    <*> maybeIdDecoder ScriptId -- collateralTxOutAddressReferenceScriptId
    <*> idDecoder AddressId -- collateralTxOutAddressId

collateralTxOutAddressEncoder :: E.Params CollateralTxOutAddress
collateralTxOutAddressEncoder =
  mconcat
    [ colateralTxOutAddressId >$< idEncoder getTxOutAddressId
    , collateralTxOutAddressTxId >$< idEncoder getTxId
    , collateralTxOutAddressIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , collateralTxOutAddressStakeAddressId >$< maybeIdEncoder getStakeAddressId
    , collateralTxOutAddressValue >$< dbLovelaceEncoder
    , collateralTxOutAddressDataHash >$< E.param (E.nullable E.bytea)
    , collateralTxOutAddressMultiAssetsDescr >$< E.param (E.nonNullable E.text)
    , collateralTxOutAddressInlineDatumId >$< maybeIdEncoder getDatumId
    , collateralTxOutAddressReferenceScriptId >$< maybeIdEncoder getScriptId
    , collateralTxOutAddressId >$< idEncoder getAddressId
    ]

-----------------------------------------------------------------------------------------------
-- MultiAssetTxOutAddress
-----------------------------------------------------------------------------------------------
data MaTxOutAddress = MaTxOutAddress
  { maTxOutAddressId :: !MaTxOutAddressId
  , maTxOutAddressIdent :: !MultiAssetId
  , maTxOutAddressQuantity :: !DbWord64
  , maTxOutAddressTxOutAddressId :: !TxOutAddressId
  }
  deriving (Eq, Show, Generic)

maTxOutAddressDecoder :: D.Row MaTxOutAddress
maTxOutAddressDecoder =
  MaTxOutAddress
    <$> idDecoder MaTxOutAddressId -- maTxOutAddressId
    <*> idDecoder MultiAssetId -- maTxOutAddressIdent
    <*> D.column (D.nonNullable $ DbWord64 . fromIntegral <$> D.int8) -- maTxOutAddressQuantity
    <*> idDecoder TxOutAddressId -- maTxOutAddressTxOutAddressId

maTxOutAddressEncoder :: E.Params MaTxOutAddress
maTxOutAddressEncoder =
  mconcat
    [ maTxOutAddressId >$< idEncoder getMaTxOutAddressId
    , maTxOutAddressIdent >$< idEncoder getMultiAssetId
    , maTxOutAddressQuantity >$< E.param (E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8)
    , maTxOutAddressTxOutAddressId >$< idEncoder getTxOutAddressId
    ]

-----------------------------------------------------------------------------------------------
-- Address
-----------------------------------------------------------------------------------------------
data Address = Address
  { addressId :: !AddressId
  , addressAddress :: !Text
  , addressRaw :: !ByteString
  , addressHasScript :: !Bool
  , addressPaymentCred :: !(Maybe ByteString)
  , addressStakeAddressId :: !(Maybe StakeAddressId)
  }
  deriving (Eq, Show, Generic)

addressDecoder :: D.Row Address
addressDecoder =
  Address
    <$> idDecoder AddressId -- addressId
    <*> D.column (D.nonNullable D.text) -- addressAddress
    <*> D.column (D.nonNullable D.bytea) -- addressRaw
    <*> D.column (D.nonNullable D.bool) -- addressHasScript
    <*> D.column (D.nullable D.bytea) -- addressPaymentCred
    <*> maybeIdDecoder StakeAddressId -- addressStakeAddressId

addressEncoder :: E.Params Address
addressEncoder =
  mconcat
    [ addressId >$< idEncoder getAddressId
    , addressAddress >$< E.param (E.nonNullable E.text)
    , addressRaw >$< E.param (E.nonNullable E.bytea)
    , addressHasScript >$< E.param (E.nonNullable E.bool)
    , addressPaymentCred >$< E.param (E.nullable E.bytea)
    , addressStakeAddressId >$< maybeIdEncoder getStakeAddressId
    ]

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
--     stakeAddressId      StakeAddressId Maybe noreference
--     txId                TxId                noreference
--     value               DbLovelace          sqltype=lovelace
--     UniqueTxout         txId index          -- The (tx_id, index) pair must be unique.

--   CollateralTxOutAddress
--     txId                TxId                noreference     -- This type is the primary key for the 'tx' table.
--     index               Word64              sqltype=txindex
--     addressId           AddressId
--     stakeAddressId      StakeAddressId Maybe noreference
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
--     stakeAddressId      StakeAddressId Maybe noreference

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
