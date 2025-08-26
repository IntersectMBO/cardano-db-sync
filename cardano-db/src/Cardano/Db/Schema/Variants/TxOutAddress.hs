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
import Cardano.Db.Schema.Types (textDecoder)
import Cardano.Db.Statement.Function.Core (bulkEncoder)
import Cardano.Db.Statement.Types (DbInfo (..), Key)
import Cardano.Db.Types (DbLovelace, DbWord64 (..), dbLovelaceDecoder, dbLovelaceEncoder, dbLovelaceValueEncoder, dbWord64ValueEncoder)

-- |
-- Table Name: tx_out
-- Description: Represents the outputs of transactions, including addresses and values.
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
  unnestParamTypes _ =
    [ ("tx_id", "bigint[]")
    , ("index", "bigint[]")
    , ("stake_address_id", "bigint[]")
    , ("value", "numeric[]")
    , ("data_hash", "bytea[]")
    , ("inline_datum_id", "bigint[]")
    , ("reference_script_id", "bigint[]")
    , ("consumed_by_tx_id", "bigint[]")
    , ("address_id", "bigint[]")
    ]
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

-- |
-- Table Name: collateral_tx_out
-- Description: Represents collateral transaction outputs, which are used to cover transaction fees in case of failure.
data CollateralTxOutAddress = CollateralTxOutAddress
  { collateralTxOutAddressTxId :: !Id.TxId
  , collateralTxOutAddressIndex :: !Word64
  , collateralTxOutAddressStakeAddressId :: !(Maybe Id.StakeAddressId)
  , collateralTxOutAddressValue :: !DbLovelace
  , collateralTxOutAddressDataHash :: !(Maybe ByteString)
  , collateralTxOutAddressMultiAssetsDescr :: !Text
  , collateralTxOutAddressInlineDatumId :: !(Maybe Id.DatumId)
  , collateralTxOutAddressReferenceScriptId :: !(Maybe Id.ScriptId)
  , collateralTxOutAddressAddressId :: !Id.AddressId
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
    , collateralTxOutAddressAddressId >$< Id.idEncoder Id.getAddressId
    ]

-- |
-- Table Name: address
-- Description: Represents addresses used in transactions, including their raw representation and associated scripts.
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

addressDecoder :: D.Row Address
addressDecoder =
  Address
    <$> D.column (D.nonNullable textDecoder) -- addressAddress
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

-- |
-- Table Name: ma_tx_out
-- Description: Represents multi-asset transaction outputs, which include various assets and their quantities.
data MaTxOutAddress = MaTxOutAddress
  { maTxOutAddressIdent :: !Id.MultiAssetId
  , maTxOutAddressQuantity :: !DbWord64
  , maTxOutAddressTxOutId :: !Id.TxOutAddressId
  }
  deriving (Eq, Show, Generic)

type instance Key MaTxOutAddress = Id.MaTxOutAddressId

instance DbInfo MaTxOutAddress where
  tableName _ = "ma_tx_out"
  columnNames _ = NE.fromList ["quantity", "tx_out_id", "ident"]
  unnestParamTypes _ = [("ident", "bigint[]"), ("quantity", "numeric[]"), ("tx_out_id", "bigint[]")]

maTxOutAddressBulkEncoder :: E.Params ([Id.MultiAssetId], [DbWord64], [Id.TxOutAddressId])
maTxOutAddressBulkEncoder =
  contrazip3
    (bulkEncoder $ E.nonNullable $ Id.getMultiAssetId >$< E.int8) -- maTxOutAddressIdent
    (bulkEncoder $ E.nonNullable dbWord64ValueEncoder) -- maTxOutAddressQuantity
    (bulkEncoder $ E.nonNullable $ Id.getTxOutAddressId >$< E.int8) -- maTxOutAddressTxOutId
