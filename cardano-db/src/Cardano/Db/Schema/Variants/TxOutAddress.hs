{-# LANGUAGE DeriveGeneric #-}

module Cardano.Db.Schema.Variants.TxOutAddress where

import Cardano.Db.Types (DbLovelace, DbWord64 (..), dbLovelaceEncoder, dbLovelaceDecoder)
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Word (Word64)
import Cardano.Db.Schema.Ids
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import GHC.Generics (Generic)
import Data.Functor.Contravariant ((>$<))

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
