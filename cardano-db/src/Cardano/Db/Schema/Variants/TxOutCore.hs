{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Db.Schema.Variants.TxOutCore where

import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Statement.Function.Core (bulkEncoder)
import Cardano.Db.Statement.Types (DbInfo (..), Entity (..), Key)
import Cardano.Db.Types (DbLovelace, DbWord64 (..), dbLovelaceDecoder, dbLovelaceEncoder, dbLovelaceValueEncoder)
import Contravariant.Extras (contrazip11, contrazip3)
import Data.ByteString.Char8 (ByteString)
import Data.Functor.Contravariant ((>$<))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

-----------------------------------------------------------------------------------------------
-- TxOut
-----------------------------------------------------------------------------------------------
data TxOutCore = TxOutCore
  { txOutCoreAddress :: !Text
  , txOutCoreAddressHasScript :: !Bool
  , txOutCoreDataHash :: !(Maybe ByteString)
  , txOutCoreConsumedByTxId :: !(Maybe Id.TxId)
  , txOutCoreIndex :: !Word64
  , txOutCoreInlineDatumId :: !(Maybe Id.DatumId)
  , txOutCorePaymentCred :: !(Maybe ByteString)
  , txOutCoreReferenceScriptId :: !(Maybe Id.ScriptId)
  , txOutCoreStakeAddressId :: !(Maybe Id.StakeAddressId)
  , txOutCoreTxId :: !Id.TxId
  , txOutCoreValue :: !DbLovelace
  }
  deriving (Eq, Show, Generic)

type instance Key TxOutCore = Id.TxOutCoreId

instance DbInfo TxOutCore where
  tableName _ = "tx_out"
  columnNames _ =
    NE.fromList
      [ "address"
      , "address_has_script"
      , "data_hash"
      , "consumed_by_tx_id"
      , "index"
      , "inline_datum_id"
      , "payment_cred"
      , "reference_script_id"
      , "stake_address_id"
      , "tx_id"
      , "value"
      ]

entityTxOutCoreDecoder :: D.Row (Entity TxOutCore)
entityTxOutCoreDecoder =
  Entity
    <$> Id.idDecoder Id.TxOutCoreId
    <*> txOutCoreDecoder

txOutCoreDecoder :: D.Row TxOutCore
txOutCoreDecoder =
  TxOutCore
    <$> D.column (D.nonNullable D.text) -- txOutCoreAddress
    <*> D.column (D.nonNullable D.bool) -- txOutCoreAddressHasScript
    <*> D.column (D.nullable D.bytea) -- txOutCoreDataHash
    <*> Id.maybeIdDecoder Id.TxId -- txOutCoreConsumedByTxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- txOutCoreIndex
    <*> Id.maybeIdDecoder Id.DatumId -- txOutCoreInlineDatumId
    <*> D.column (D.nullable D.bytea) -- txOutCorePaymentCred
    <*> Id.maybeIdDecoder Id.ScriptId -- txOutCoreReferenceScriptId
    <*> Id.maybeIdDecoder Id.StakeAddressId -- txOutCoreStakeAddressId
    <*> Id.idDecoder Id.TxId -- txOutCoreTxId
    <*> dbLovelaceDecoder -- txOutCoreValue

txOutCoreEncoder :: E.Params TxOutCore
txOutCoreEncoder =
  mconcat
    [ txOutCoreAddress >$< E.param (E.nonNullable E.text)
    , txOutCoreAddressHasScript >$< E.param (E.nonNullable E.bool)
    , txOutCoreDataHash >$< E.param (E.nullable E.bytea)
    , txOutCoreConsumedByTxId >$< Id.maybeIdEncoder Id.getTxId
    , txOutCoreIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , txOutCoreInlineDatumId >$< Id.maybeIdEncoder Id.getDatumId
    , txOutCorePaymentCred >$< E.param (E.nullable E.bytea)
    , txOutCoreReferenceScriptId >$< Id.maybeIdEncoder Id.getScriptId
    , txOutCoreStakeAddressId >$< Id.maybeIdEncoder Id.getStakeAddressId
    , txOutCoreTxId >$< Id.idEncoder Id.getTxId
    , txOutCoreValue >$< dbLovelaceEncoder
    ]

txOutCoreBulkEncoder :: E.Params ([Text], [Bool], [Maybe ByteString], [Maybe Id.TxId], [Word64], [Maybe Id.DatumId], [Maybe ByteString], [Maybe Id.ScriptId], [Maybe Id.StakeAddressId], [Id.TxId], [DbLovelace])
txOutCoreBulkEncoder =
  contrazip11
    (bulkEncoder $ E.nonNullable E.text)
    (bulkEncoder $ E.nonNullable E.bool)
    (bulkEncoder $ E.nullable E.bytea)
    (bulkEncoder $ E.nullable $ Id.getTxId >$< E.int8)
    (bulkEncoder $ E.nonNullable $ fromIntegral >$< E.int8)
    (bulkEncoder $ E.nullable $ Id.getDatumId >$< E.int8)
    (bulkEncoder $ E.nullable E.bytea)
    (bulkEncoder $ E.nullable $ Id.getScriptId >$< E.int8)
    (bulkEncoder $ E.nullable $ Id.getStakeAddressId >$< E.int8)
    (bulkEncoder $ E.nonNullable $ Id.getTxId >$< E.int8)
    (bulkEncoder dbLovelaceValueEncoder)

-----------------------------------------------------------------------------------------------
-- CollateralTxOut
-----------------------------------------------------------------------------------------------
data CollateralTxOutCore = CollateralTxOutCore
  { collateralTxOutCoreTxId :: !Id.TxId
  , collateralTxOutCoreIndex :: !Word64
  , collateralTxOutCoreAddress :: !Text
  , collateralTxOutCoreAddressHasScript :: !Bool
  , collateralTxOutCorePaymentCred :: !(Maybe ByteString)
  , collateralTxOutCoreStakeAddressId :: !(Maybe Id.StakeAddressId)
  , collateralTxOutCoreValue :: !DbLovelace
  , collateralTxOutCoreDataHash :: !(Maybe ByteString)
  , collateralTxOutCoreMultiAssetsDescr :: !Text
  , collateralTxOutCoreInlineDatumId :: !(Maybe Id.DatumId)
  , collateralTxOutCoreReferenceScriptId :: !(Maybe Id.ScriptId)
  }
  deriving (Eq, Show, Generic)

type instance Key CollateralTxOutCore = Id.CollateralTxOutCoreId

instance DbInfo CollateralTxOutCore where
  tableName _ = "collateral_tx_out"
  columnNames _ =
    NE.fromList
      [ "tx_id"
      , "index"
      , "address"
      , "address_has_script"
      , "payment_cred"
      , "stake_address_id"
      , "value"
      , "data_hash"
      , "multi_assets_descr"
      , "inline_datum_id"
      , "reference_script_id"
      ]

entityCollateralTxOutCoreDecoder :: D.Row (Entity CollateralTxOutCore)
entityCollateralTxOutCoreDecoder =
  Entity
    <$> Id.idDecoder Id.CollateralTxOutCoreId
    <*> collateralTxOutCoreDecoder

collateralTxOutCoreDecoder :: D.Row CollateralTxOutCore
collateralTxOutCoreDecoder =
  CollateralTxOutCore
    <$> Id.idDecoder Id.TxId -- collateralTxOutCoreTxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- collateralTxOutCoreIndex
    <*> D.column (D.nonNullable D.text) -- collateralTxOutCoreAddress
    <*> D.column (D.nonNullable D.bool) -- collateralTxOutCoreAddressHasScript
    <*> D.column (D.nullable D.bytea) -- collateralTxOutCorePaymentCred
    <*> Id.maybeIdDecoder Id.StakeAddressId -- collateralTxOutCoreStakeAddressId
    <*> dbLovelaceDecoder -- collateralTxOutCoreValue
    <*> D.column (D.nullable D.bytea) -- collateralTxOutCoreDataHash
    <*> D.column (D.nonNullable D.text) -- collateralTxOutCoreMultiAssetsDescr
    <*> Id.maybeIdDecoder Id.DatumId -- collateralTxOutCoreInlineDatumId
    <*> Id.maybeIdDecoder Id.ScriptId -- collateralTxOutCoreReferenceScriptId

collateralTxOutCoreEncoder :: E.Params CollateralTxOutCore
collateralTxOutCoreEncoder =
  mconcat
    [ collateralTxOutCoreTxId >$< Id.idEncoder Id.getTxId
    , collateralTxOutCoreIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , collateralTxOutCoreAddress >$< E.param (E.nonNullable E.text)
    , collateralTxOutCoreAddressHasScript >$< E.param (E.nonNullable E.bool)
    , collateralTxOutCorePaymentCred >$< E.param (E.nullable E.bytea)
    , collateralTxOutCoreStakeAddressId >$< Id.maybeIdEncoder Id.getStakeAddressId
    , collateralTxOutCoreValue >$< dbLovelaceEncoder
    , collateralTxOutCoreDataHash >$< E.param (E.nullable E.bytea)
    , collateralTxOutCoreMultiAssetsDescr >$< E.param (E.nonNullable E.text)
    , collateralTxOutCoreInlineDatumId >$< Id.maybeIdEncoder Id.getDatumId
    , collateralTxOutCoreReferenceScriptId >$< Id.maybeIdEncoder Id.getScriptId
    ]

-----------------------------------------------------------------------------------------------
-- MultiAssetTxOut
-----------------------------------------------------------------------------------------------
data MaTxOutCore = MaTxOutCore
  { maTxOutCoreIdent :: !Id.MultiAssetId
  , maTxOutCoreQuantity :: !DbWord64
  , maTxOutCoreTxOutId :: !Id.TxOutCoreId
  }
  deriving (Eq, Show, Generic)

type instance Key MaTxOutCore = Id.MaTxOutCoreId

instance DbInfo MaTxOutCore where
  tableName _ = "ma_tx_out"
  columnNames _ =
    NE.fromList
      [ "ident"
      , "quantity"
      , "tx_out_id"
      ]

entityMaTxOutCoreDecoder :: D.Row (Entity MaTxOutCore)
entityMaTxOutCoreDecoder =
  Entity
    <$> Id.idDecoder Id.MaTxOutCoreId
    <*> maTxOutCoreDecoder

maTxOutCoreDecoder :: D.Row MaTxOutCore
maTxOutCoreDecoder =
  MaTxOutCore
    <$> Id.idDecoder Id.MultiAssetId -- maTxOutCoreIdent
    <*> D.column (D.nonNullable $ DbWord64 . fromIntegral <$> D.int8) -- maTxOutCoreQuantity
    <*> Id.idDecoder Id.TxOutCoreId -- maTxOutCoreTxOutId

maTxOutCoreEncoder :: E.Params MaTxOutCore
maTxOutCoreEncoder =
  mconcat
    [ maTxOutCoreIdent >$< Id.idEncoder Id.getMultiAssetId
    , maTxOutCoreQuantity >$< E.param (E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8)
    , maTxOutCoreTxOutId >$< Id.idEncoder Id.getTxOutCoreId
    ]

maTxOutCoreBulkEncoder :: E.Params ([Id.MultiAssetId], [DbWord64], [Id.TxOutCoreId])
maTxOutCoreBulkEncoder =
  contrazip3
    (bulkEncoder $ E.nonNullable $ Id.getMultiAssetId >$< E.int8)
    (bulkEncoder $ E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8)
    (bulkEncoder $ E.nonNullable $ Id.getTxOutCoreId >$< E.int8)

-- share
--   [ mkPersist sqlSettings
--   , mkMigrate "migrateCoreTxOutCardanoDb"
--   , mkEntityDefList "entityDefsTxOutCore"
--   , deriveShowFields
--   ]
--   [persistLowerCase|
-- ----------------------------------------------
-- -- Core TxOut
-- ----------------------------------------------
--   TxOut
--     address             Text
--     addressHasScript    Bool
--     dataHash            ByteString Maybe    sqltype=hash32type
--     consumedByTxId      TxId Maybe          noreference
--     index               Word64              sqltype=txindex
--     inlineDatumId       DatumId Maybe       noreference
--     paymentCred         ByteString Maybe    sqltype=hash28type
--     referenceScriptId   ScriptId Maybe      noreference
--     stakeAddressId      StakeAddressId Maybe noreference
--     txId                TxId                noreference
--     value               DbLovelace          sqltype=lovelace
--     UniqueTxout         txId index          -- The (tx_id, index) pair must be unique.

-- ----------------------------------------------
-- -- Core CollateralTxOut
-- ----------------------------------------------
--   CollateralTxOut
--     txId                TxId                noreference     -- This type is the primary key for the 'tx' table.
--     index               Word64              sqltype=txindex
--     address             Text
--     addressHasScript    Bool
--     paymentCred         ByteString Maybe    sqltype=hash28type
--     stakeAddressId      StakeAddressId Maybe noreference
--     value               DbLovelace          sqltype=lovelace
--     dataHash            ByteString Maybe    sqltype=hash32type
--     multiAssetsDescr    Text
--     inlineDatumId       DatumId Maybe       noreference
--     referenceScriptId   ScriptId Maybe      noreference
--     deriving Show

-- ----------------------------------------------
-- -- MultiAsset
-- ----------------------------------------------
--   MaTxOutCore
--     ident               MultiAssetId        noreference
--     quantity            DbWord64            sqltype=word64type
--     txOutCoreId             TxOutId             noreference
--     deriving Show

-- | ]

-- schemaDocsTxOutCore :: [EntityDef]
-- schemaDocsTxOutCore =
--   document entityDefsTxOutCore $ do
--     TxOut --^ do
--       "A table for transaction outputs."
--       TxOutAddress # "The human readable encoding of the output address. Will be Base58 for Byron era addresses and Bech32 for Shelley era."
--       TxOutAddressHasScript # "Flag which shows if this address is locked by a script."
--       TxOutConsumedByTxId # "The Tx table index of the transaction that consumes this transaction output. Not populated by default, can be activated via tx-out configs."
--       TxOutDataHash # "The hash of the transaction output datum. (NULL for Txs without scripts)."
--       TxOutIndex # "The index of this transaction output with the transaction."
--       TxOutInlineDatumId # "The inline datum of the output, if it has one. New in v13."
--       TxOutPaymentCred # "The payment credential part of the Shelley address. (NULL for Byron addresses). For a script-locked address, this is the script hash."
--       TxOutReferenceScriptId # "The reference script of the output, if it has one. New in v13."
--       TxOutStakeAddressId # "The StakeAddress table index for the stake address part of the Shelley address. (NULL for Byron addresses)."
--       TxOutValue # "The output value (in Lovelace) of the transaction output."

--       TxOutTxId # "The Tx table index of the transaction that contains this transaction output."

--     CollateralTxOut --^ do
--       "A table for transaction collateral outputs. New in v13."
--       CollateralTxOutTxId # "The Tx table index of the transaction that contains this transaction output."
--       CollateralTxOutIndex # "The index of this transaction output with the transaction."
--       CollateralTxOutAddress # "The human readable encoding of the output address. Will be Base58 for Byron era addresses and Bech32 for Shelley era."
--       CollateralTxOutAddressHasScript # "Flag which shows if this address is locked by a script."
--       CollateralTxOutPaymentCred # "The payment credential part of the Shelley address. (NULL for Byron addresses). For a script-locked address, this is the script hash."
--       CollateralTxOutStakeAddressId # "The StakeAddress table index for the stake address part of the Shelley address. (NULL for Byron addresses)."
--       CollateralTxOutValue # "The output value (in Lovelace) of the transaction output."
--       CollateralTxOutDataHash # "The hash of the transaction output datum. (NULL for Txs without scripts)."
--       CollateralTxOutMultiAssetsDescr # "This is a description of the multiassets in collateral output. Since the output is not really created, we don't need to add them in separate tables."
--       CollateralTxOutInlineDatumId # "The inline datum of the output, if it has one. New in v13."
--       CollateralTxOutReferenceScriptId # "The reference script of the output, if it has one. New in v13."

--     MaTxOutCore --^ do
--       "A table containing Multi-Asset transaction outputs."
--       MaTxOutCoreIdent # "The MultiAsset table index specifying the asset."
--       MaTxOutCoreQuantity # "The Multi Asset transaction output amount (denominated in the Multi Asset)."
--       MaTxOutCoreTxOutId # "The TxOut table index for the transaction that this Multi Asset transaction output."
