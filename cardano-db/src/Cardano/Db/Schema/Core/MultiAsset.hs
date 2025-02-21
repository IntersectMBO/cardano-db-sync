{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Db.Schema.Core.MultiAsset where

import Cardano.Db.Schema.Ids
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
-- import Database.Persist.Class (Unique)
-- import Database.Persist.Documentation (deriveShowFields, document, (#), (--^))
-- import Database.Persist.EntityDef.Internal (EntityDef (..))
import GHC.Generics (Generic)

import Hasql.Decoders as D
import Hasql.Encoders as E
import Cardano.Db.Types (DbInt65, dbInt65Decoder, dbInt65Encoder)
import Data.Functor.Contravariant ((>$<))

-----------------------------------------------------------------------------------------------------------------------------------
-- MULTI ASSETS
-- These tables manage governance-related data, including DReps, committees, and voting procedures.
-----------------------------------------------------------------------------------------------------------------------------------

{-|
Table Name: multi_asset
Description: Contains information about multi-assets, including the policy and name of the asset.
-}
data MultiAsset = MultiAsset
  { multiAssetId :: !MultiAssetId
  , multiAssetPolicy :: !ByteString -- sqltype=hash28type
  , multiAssetName :: !ByteString   -- sqltype=asset32type
  , multiAssetFingerprint :: !Text
  } deriving (Eq, Show, Generic)
-- UniqueMultiAsset  policy name

multiAssetDecoder :: D.Row MultiAsset
multiAssetDecoder =
  MultiAsset
    <$> idDecoder MultiAssetId -- multiAssetId
    <*> D.column (D.nonNullable D.bytea) -- multiAssetPolicy
    <*> D.column (D.nonNullable D.bytea) -- multiAssetName
    <*> D.column (D.nonNullable D.text) -- multiAssetFingerprint

multiAssetEncoder :: E.Params MultiAsset
multiAssetEncoder =
  mconcat
    [ multiAssetId >$< idEncoder getMultiAssetId
    , multiAssetPolicy >$< E.param (E.nonNullable E.bytea)
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
{-|
Table Name: ma_tx_mint
Description: Contains information about the minting of multi-assets, including the quantity of the asset and the transaction in which it was minted.
-}
data MaTxMint = MaTxMint
  { maTxMintId :: !MaTxMintId
  , maTxMintIdent :: !MultiAssetId -- noreference
  , maTxMintQuantity :: !DbInt65   -- sqltype=int65type
  , maTxMintTxId :: !TxId          -- noreference
  } deriving (Eq, Show, Generic)

maTxMintDecoder :: D.Row MaTxMint
maTxMintDecoder =
  MaTxMint
    <$> idDecoder MaTxMintId
    <*> idDecoder MultiAssetId
    <*> D.column (D.nonNullable dbInt65Decoder)
    <*> idDecoder TxId

maTxMintEncoder :: E.Params MaTxMint
maTxMintEncoder =
  mconcat
    [ maTxMintId >$< idEncoder getMaTxMintId
    , maTxMintIdent >$< idEncoder getMultiAssetId
    , maTxMintQuantity >$< E.param (E.nonNullable dbInt65Encoder)
    , maTxMintTxId >$< idEncoder getTxId
    ]
