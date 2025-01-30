{-# LANGUAGE DeriveGeneric #-}

module Cardano.Db.Schema.Variants.TxOutCore where

import Cardano.Db.Types (DbLovelace, DbWord64 (..), dbLovelaceDecoder, dbLovelaceEncoder)
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Cardano.Db.Schema.Ids
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import Data.Functor.Contravariant ((>$<))

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
