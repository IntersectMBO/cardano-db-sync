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

-- |
-- Table Name: tx_out
-- Description: Represents a transaction output in the Cardano blockchain.
data TxOutCore = TxOutCore
  { txOutCoreTxId :: !Id.TxId
  , txOutCoreIndex :: !Word64
  , txOutCoreAddress :: !Text
  , txOutCoreAddressHasScript :: !Bool
  , txOutCorePaymentCred :: !(Maybe ByteString)
  , txOutCoreStakeAddressId :: !(Maybe Id.StakeAddressId)
  , txOutCoreValue :: !DbLovelace
  , txOutCoreDataHash :: !(Maybe ByteString)
  , txOutCoreInlineDatumId :: !(Maybe Id.DatumId)
  , txOutCoreReferenceScriptId :: !(Maybe Id.ScriptId)
  , txOutCoreConsumedByTxId :: !(Maybe Id.TxId)
  }
  deriving (Eq, Show, Generic)

type instance Key TxOutCore = Id.TxOutCoreId

instance DbInfo TxOutCore where
  tableName _ = "tx_out"
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
      , "inline_datum_id"
      , "reference_script_id"
      , "consumed_by_tx_id"
      ]
  unnestParamTypes _ =
    [ ("tx_id", "bigint[]")
    , ("index", "bigint[]")
    , ("address", "text[]")
    , ("address_has_script", "boolean[]")
    , ("payment_cred", "bytea[]")
    , ("stake_address_id", "bigint[]")
    , ("value", "numeric[]")
    , ("data_hash", "bytea[]")
    , ("inline_datum_id", "bigint[]")
    , ("reference_script_id", "bigint[]")
    , ("consumed_by_tx_id", "bigint[]")
    ]

entityTxOutCoreDecoder :: D.Row (Entity TxOutCore)
entityTxOutCoreDecoder =
  Entity
    <$> Id.idDecoder Id.TxOutCoreId
    <*> txOutCoreDecoder

txOutCoreDecoder :: D.Row TxOutCore
txOutCoreDecoder =
  TxOutCore
    <$> Id.idDecoder Id.TxId
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8)
    <*> D.column (D.nonNullable D.text)
    <*> D.column (D.nonNullable D.bool)
    <*> D.column (D.nullable D.bytea)
    <*> Id.maybeIdDecoder Id.StakeAddressId
    <*> dbLovelaceDecoder
    <*> D.column (D.nullable D.bytea)
    <*> Id.maybeIdDecoder Id.DatumId
    <*> Id.maybeIdDecoder Id.ScriptId
    <*> Id.maybeIdDecoder Id.TxId

txOutCoreEncoder :: E.Params TxOutCore
txOutCoreEncoder =
  mconcat
    [ txOutCoreTxId >$< Id.idEncoder Id.getTxId
    , txOutCoreIndex >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    , txOutCoreAddress >$< E.param (E.nonNullable E.text)
    , txOutCoreAddressHasScript >$< E.param (E.nonNullable E.bool)
    , txOutCorePaymentCred >$< E.param (E.nullable E.bytea)
    , txOutCoreStakeAddressId >$< Id.maybeIdEncoder Id.getStakeAddressId
    , txOutCoreValue >$< dbLovelaceEncoder
    , txOutCoreDataHash >$< E.param (E.nullable E.bytea)
    , txOutCoreInlineDatumId >$< Id.maybeIdEncoder Id.getDatumId
    , txOutCoreReferenceScriptId >$< Id.maybeIdEncoder Id.getScriptId
    , txOutCoreConsumedByTxId >$< Id.maybeIdEncoder Id.getTxId
    ]

txOutCoreBulkEncoder :: E.Params ([Id.TxId], [Word64], [Text], [Bool], [Maybe ByteString], [Maybe Id.StakeAddressId], [DbLovelace], [Maybe ByteString], [Maybe Id.DatumId], [Maybe Id.ScriptId], [Maybe Id.TxId])
txOutCoreBulkEncoder =
  contrazip11
    (bulkEncoder $ E.nonNullable $ Id.getTxId >$< E.int8)
    (bulkEncoder $ E.nonNullable $ fromIntegral >$< E.int8)
    (bulkEncoder $ E.nonNullable E.text)
    (bulkEncoder $ E.nonNullable E.bool)
    (bulkEncoder $ E.nullable E.bytea)
    (bulkEncoder $ E.nullable $ Id.getStakeAddressId >$< E.int8)
    (bulkEncoder dbLovelaceValueEncoder)
    (bulkEncoder $ E.nullable E.bytea)
    (bulkEncoder $ E.nullable $ Id.getDatumId >$< E.int8)
    (bulkEncoder $ E.nullable $ Id.getScriptId >$< E.int8)
    (bulkEncoder $ E.nullable $ Id.getTxId >$< E.int8)

-- |
-- Table Name: collateral_tx_out
-- Description: Represents a collateral transaction output in the Cardano blockchain.
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

-- |
-- Table Name: ma_tx_out
-- Description: Represents a multi-asset transaction output in the Cardano blockchain.
data MaTxOutCore = MaTxOutCore
  { maTxOutCoreQuantity :: !DbWord64
  , maTxOutCoreTxOutId :: !Id.TxOutCoreId
  , maTxOutCoreIdent :: !Id.MultiAssetId
  }
  deriving (Eq, Show, Generic)

type instance Key MaTxOutCore = Id.MaTxOutCoreId

instance DbInfo MaTxOutCore where
  tableName _ = "ma_tx_out"
  columnNames _ =
    NE.fromList
      [ "quantity"
      , "tx_out_id"
      , "ident"
      ]

entityMaTxOutCoreDecoder :: D.Row (Entity MaTxOutCore)
entityMaTxOutCoreDecoder =
  Entity
    <$> Id.idDecoder Id.MaTxOutCoreId
    <*> maTxOutCoreDecoder

maTxOutCoreDecoder :: D.Row MaTxOutCore
maTxOutCoreDecoder =
  MaTxOutCore
    <$> D.column (D.nonNullable $ DbWord64 . fromIntegral <$> D.int8) -- maTxOutCoreQuantity
    <*> Id.idDecoder Id.TxOutCoreId -- maTxOutCoreTxOutId
    <*> Id.idDecoder Id.MultiAssetId -- maTxOutCoreIdent

maTxOutCoreEncoder :: E.Params MaTxOutCore
maTxOutCoreEncoder =
  mconcat
    [ maTxOutCoreQuantity >$< E.param (E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8)
    , maTxOutCoreTxOutId >$< Id.idEncoder Id.getTxOutCoreId
    , maTxOutCoreIdent >$< Id.idEncoder Id.getMultiAssetId
    ]

maTxOutCoreBulkEncoder :: E.Params ([DbWord64], [Id.TxOutCoreId], [Id.MultiAssetId])
maTxOutCoreBulkEncoder =
  contrazip3
    (bulkEncoder $ E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8)
    (bulkEncoder $ E.nonNullable $ Id.getTxOutCoreId >$< E.int8)
    (bulkEncoder $ E.nonNullable $ Id.getMultiAssetId >$< E.int8)
