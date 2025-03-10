{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Db.Schema.Core.MultiAsset where

import Contravariant.Extras (contrazip3)
import Data.ByteString.Char8 (ByteString)
import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import GHC.Generics (Generic)
import Hasql.Decoders as D
import Hasql.Encoders as E

import Cardano.Db.Schema.Ids
import Cardano.Db.Statement.Function.Core (manyEncoder)
import Cardano.Db.Statement.Types (DbInfo (..), Entity (..), Key)
import Cardano.Db.Types (DbInt65, dbInt65Decoder, dbInt65Encoder)

-----------------------------------------------------------------------------------------------------------------------------------
-- MULTI ASSETS
-- These tables manage governance-related data, including DReps, committees, and voting procedures.
-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: multi_asset
-- Description: Contains information about multi-assets, including the policy and name of the asset.
data MultiAsset = MultiAsset
  { multiAssetPolicy :: !ByteString -- sqltype=hash28type
  , multiAssetName :: !ByteString -- sqltype=asset32type
  , multiAssetFingerprint :: !Text
  }
  deriving (Eq, Show, Generic)

type instance Key MultiAsset = MultiAssetId
instance DbInfo MultiAsset where
  uniqueFields _ = ["policy", "name"]

entityNameMultiAssetDecoder :: D.Row (Entity MultiAsset)
entityNameMultiAssetDecoder =
  Entity
    <$> idDecoder MultiAssetId
    <*> multiAssetDecoder

multiAssetDecoder :: D.Row MultiAsset
multiAssetDecoder =
  MultiAsset
    <$> D.column (D.nonNullable D.bytea) -- multiAssetPolicy
    <*> D.column (D.nonNullable D.bytea) -- multiAssetName
    <*> D.column (D.nonNullable D.text) -- multiAssetFingerprint

entityNameMultiAssetEncoder :: E.Params (Entity MultiAsset)
entityNameMultiAssetEncoder =
  mconcat
    [ entityKey >$< idEncoder getMultiAssetId
    , entityVal >$< multiAssetEncoder
    ]

multiAssetEncoder :: E.Params MultiAsset
multiAssetEncoder =
  mconcat
    [ multiAssetPolicy >$< E.param (E.nonNullable E.bytea)
    , multiAssetName >$< E.param (E.nonNullable E.bytea)
    , multiAssetFingerprint >$< E.param (E.nonNullable E.text)
    ]

multiAssetInsertEncoder :: E.Params MultiAsset
multiAssetInsertEncoder =
  mconcat
    [ multiAssetPolicy >$< E.param (E.nonNullable E.bytea)
    , multiAssetName >$< E.param (E.nonNullable E.bytea)
    , multiAssetFingerprint >$< E.param (E.nonNullable E.text)
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: ma_tx_mint
-- Description: Contains information about the minting of multi-assets, including the quantity of the asset and the transaction in which it was minted.
data MaTxMint = MaTxMint
  { maTxMintQuantity :: !DbInt65 -- sqltype=int65type
  , maTxMintIdent :: !MultiAssetId -- noreference
  , maTxMintTxId :: !TxId -- noreference
  }
  deriving (Eq, Show, Generic)

type instance Key MaTxMint = MaTxMintId
instance DbInfo MaTxMint

entityNameMaTxMintDecoder :: D.Row (Entity MaTxMint)
entityNameMaTxMintDecoder =
  Entity
    <$> idDecoder MaTxMintId
    <*> maTxMintDecoder

maTxMintDecoder :: D.Row MaTxMint
maTxMintDecoder =
  MaTxMint
    <$> D.column (D.nonNullable dbInt65Decoder)
    <*> idDecoder MultiAssetId
    <*> idDecoder TxId

entityNameMaTxMintEncoder :: E.Params (Entity MaTxMint)
entityNameMaTxMintEncoder =
  mconcat
    [ entityKey >$< idEncoder getMaTxMintId
    , entityVal >$< maTxMintEncoder
    ]

maTxMintEncoder :: E.Params MaTxMint
maTxMintEncoder =
  mconcat
    [ maTxMintQuantity >$< E.param (E.nonNullable dbInt65Encoder)
    , maTxMintIdent >$< idEncoder getMultiAssetId
    , maTxMintTxId >$< idEncoder getTxId
    ]

maTxMintBulkEncoder :: E.Params ([DbInt65], [MultiAssetId], [TxId])
maTxMintBulkEncoder =
  contrazip3
    (manyEncoder $ E.nonNullable dbInt65Encoder)
    (manyEncoder $ E.nonNullable $ getMultiAssetId >$< E.int8)
    (manyEncoder $ E.nonNullable $ getTxId >$< E.int8)
