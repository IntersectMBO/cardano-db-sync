{-# LANGUAGE DeriveGeneric #-}

module Cardano.Db.Schema.Variants.TxOutCore where

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
-- TxOut
-----------------------------------------------------------------------------------------------
data TxOutCore = TxOutCore
  { txOutCoreId :: !TxOutCoreId
  , txOutCoreAddress :: !Text
  , txOutCoreAddressHasScript :: !Bool
  , txOutCoreDataHash :: !(Maybe ByteString)
  , txOutCoreConsumedByTxId :: !(Maybe TxId)
  , txOutCoreIndex :: !Word64
  , txOutCoreInlineDatumId :: !(Maybe DatumId)
  , txOutCorePaymentCred :: !(Maybe ByteString)
  , txOutCoreReferenceScriptId :: !(Maybe ScriptId)
  , txOutCoreStakeAddressId :: !(Maybe StakeAddressId)
  , txOutCoreTxId :: !TxId
  , txOutCoreValue :: !DbLovelace
  }
  deriving (Eq, Show, Generic)

txOutCoreCoreDecoder :: D.Row TxOutCore
txOutCoreCoreDecoder =
  TxOutCore
    <$> idDecoder TxOutCoreId -- txOutCoreId
    <*> D.column (D.nonNullable D.text) -- txOutCoreAddress
    <*> D.column (D.nonNullable D.bool) -- txOutCoreAddressHasScript
    <*> D.column (D.nullable D.bytea) -- txOutCoreDataHash
    <*> maybeIdDecoder TxId -- txOutCoreConsumedByTxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- txOutCoreIndex
    <*> maybeIdDecoder DatumId -- txOutCoreInlineDatumId
    <*> D.column (D.nullable D.bytea) -- txOutCorePaymentCred
    <*> maybeIdDecoder ScriptId -- txOutCoreReferenceScriptId
    <*> maybeIdDecoder StakeAddressId -- txOutCoreStakeAddressId
    <*> idDecoder TxId -- txOutCoreTxId
    <*> dbLovelaceDecoder -- txOutCoreValue

txOutCoreCoreEncoder :: E.Params TxOutCore
txOutCoreCoreEncoder =
  mconcat
    [ txOutCoreId >$< idEncoder getTxOutCoreId
    , txOutCoreAddress >$< E.param (E.nonNullable E.text)
    , txOutCoreAddressHasScript >$< E.param (E.nonNullable E.bool)
    , txOutCoreDataHash >$< E.param (E.nullable E.bytea)
    , txOutCoreConsumedByTxId >$< maybeIdEncoder getTxId
    , txOutCoreIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , txOutCoreInlineDatumId >$< maybeIdEncoder getDatumId
    , txOutCorePaymentCred >$< E.param (E.nullable E.bytea)
    , txOutCoreReferenceScriptId >$< maybeIdEncoder getScriptId
    , txOutCoreStakeAddressId >$< maybeIdEncoder getStakeAddressId
    , txOutCoreTxId >$< idEncoder getTxId
    , txOutCoreValue >$< dbLovelaceEncoder
    ]

-----------------------------------------------------------------------------------------------
-- CollateralTxOut
-----------------------------------------------------------------------------------------------
data CollateralTxOutCore = CollateralTxOutCore
  { collateralTxOutCoreId :: !TxOutCoreId
  , collateralTxOutCoreTxId :: !TxId
  , collateralTxOutCoreIndex :: !Word64
  , collateralTxOutCoreAddress :: !Text
  , collateralTxOutCoreAddressHasScript :: !Bool
  , collateralTxOutCorePaymentCred :: !(Maybe ByteString)
  , collateralTxOutCoreStakeAddressId :: !(Maybe StakeAddressId)
  , collateralTxOutCoreValue :: !DbLovelace
  , collateralTxOutCoreDataHash :: !(Maybe ByteString)
  , collateralTxOutCoreMultiAssetsDescr :: !Text
  , collateralTxOutCoreInlineDatumId :: !(Maybe DatumId)
  , collateralTxOutCoreReferenceScriptId :: !(Maybe ScriptId)
  }
  deriving (Eq, Show, Generic)

collateralTxOutCoreDecoder :: D.Row CollateralTxOutCore
collateralTxOutCoreDecoder =
  CollateralTxOutCore
    <$> idDecoder TxOutCoreId -- collateralTxOutCoreId
    <*> idDecoder TxId -- collateralTxOutCoreTxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- collateralTxOutCoreIndex
    <*> D.column (D.nonNullable D.text) -- collateralTxOutCoreAddress
    <*> D.column (D.nonNullable D.bool) -- collateralTxOutCoreAddressHasScript
    <*> D.column (D.nullable D.bytea) -- collateralTxOutCorePaymentCred
    <*> maybeIdDecoder StakeAddressId -- collateralTxOutCoreStakeAddressId
    <*> dbLovelaceDecoder -- collateralTxOutCoreValue
    <*> D.column (D.nullable D.bytea) -- collateralTxOutCoreDataHash
    <*> D.column (D.nonNullable D.text) -- collateralTxOutCoreMultiAssetsDescr
    <*> maybeIdDecoder DatumId -- collateralTxOutCoreInlineDatumId
    <*> maybeIdDecoder ScriptId -- collateralTxOutCoreReferenceScriptId

collateralTxOutCoreEncoder :: E.Params CollateralTxOutCore
collateralTxOutCoreEncoder =
  mconcat
    [ collateralTxOutCoreId >$< idEncoder getTxOutCoreId
    , collateralTxOutCoreTxId >$< idEncoder getTxId
    , collateralTxOutCoreIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , collateralTxOutCoreAddress >$< E.param (E.nonNullable E.text)
    , collateralTxOutCoreAddressHasScript >$< E.param (E.nonNullable E.bool)
    , collateralTxOutCorePaymentCred >$< E.param (E.nullable E.bytea)
    , collateralTxOutCoreStakeAddressId >$< maybeIdEncoder getStakeAddressId
    , collateralTxOutCoreValue >$< dbLovelaceEncoder
    , collateralTxOutCoreDataHash >$< E.param (E.nullable E.bytea)
    , collateralTxOutCoreMultiAssetsDescr >$< E.param (E.nonNullable E.text)
    , collateralTxOutCoreInlineDatumId >$< maybeIdEncoder getDatumId
    , collateralTxOutCoreReferenceScriptId >$< maybeIdEncoder getScriptId
    ]

-----------------------------------------------------------------------------------------------
-- MultiAssetTxOut
-----------------------------------------------------------------------------------------------
data MaTxOutCore = MaTxOutCore
  { maTxOutCoreId :: !MaTxOutCoreId
  , maTxOutCoreIdent :: !MultiAssetId
  , maTxOutCoreQuantity :: !DbWord64
  , maTxOutCoreTxOutId :: !TxOutCoreId
  }
  deriving (Eq, Show, Generic)

maTxOutCoreDecoder :: D.Row MaTxOutCore
maTxOutCoreDecoder =
  MaTxOutCore
    <$> idDecoder MaTxOutCoreId -- maTxOutCoreId
    <*> idDecoder MultiAssetId -- maTxOutCoreIdent
    <*> D.column (D.nonNullable $ DbWord64 . fromIntegral <$> D.int8) -- maTxOutCoreQuantity
    <*> idDecoder TxOutCoreId -- maTxOutCoreTxOutId

maTxOutCoreEncoder :: E.Params MaTxOutCore
maTxOutCoreEncoder =
  mconcat
    [ maTxOutCoreId >$< idEncoder getMaTxOutCoreId
    , maTxOutCoreIdent >$< idEncoder getMultiAssetId
    , maTxOutCoreQuantity >$< E.param (E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8)
    , maTxOutCoreTxOutId >$< idEncoder getTxOutCoreId
    ]

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
