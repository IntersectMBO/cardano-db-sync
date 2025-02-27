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
{-# LANGUAGE OverloadedStrings #-}

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
import Cardano.Db.Types (DbInt65, dbInt65Decoder, dbInt65Encoder, HasDbInfo (..))
import Data.Functor.Contravariant ((>$<))
import Contravariant.Extras (contrazip3)
import Cardano.Db.Statement.Helpers (manyEncoder)

-----------------------------------------------------------------------------------------------------------------------------------
-- MULTI ASSETS
-- These tables manage governance-related data, including DReps, committees, and voting procedures.
-----------------------------------------------------------------------------------------------------------------------------------

{-|
Table Name: multi_asset
Description: Contains information about multi-assets, including the policy and name of the asset.
-}
data MultiAsset = MultiAsset
  { multiAsset_Id :: !MultiAssetId
  , multiAsset_Policy :: !ByteString -- sqltype=hash28type
  , multiAsset_Name :: !ByteString   -- sqltype=asset32type
  , multiAsset_Fingerprint :: !Text
  } deriving (Eq, Show, Generic)
-- UniqueMultiAsset  policy name

instance HasDbInfo MultiAsset

multiAssetDecoder :: D.Row MultiAsset
multiAssetDecoder =
  MultiAsset
    <$> idDecoder MultiAssetId -- multiAsset_Id
    <*> D.column (D.nonNullable D.bytea) -- multiAsset_Policy
    <*> D.column (D.nonNullable D.bytea) -- multiAsset_Name
    <*> D.column (D.nonNullable D.text) -- multiAsset_Fingerprint

multiAssetEncoder :: E.Params MultiAsset
multiAssetEncoder =
  mconcat
    [ multiAsset_Id >$< idEncoder getMultiAssetId
    , multiAsset_Policy >$< E.param (E.nonNullable E.bytea)
    , multiAsset_Name >$< E.param (E.nonNullable E.bytea)
    , multiAsset_Fingerprint >$< E.param (E.nonNullable E.text)
    ]

multiAssetInsertEncoder :: E.Params MultiAsset
multiAssetInsertEncoder =
  mconcat
    [ multiAsset_Policy >$< E.param (E.nonNullable E.bytea)
    , multiAsset_Name >$< E.param (E.nonNullable E.bytea)
    , multiAsset_Fingerprint >$< E.param (E.nonNullable E.text)
    ]


-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: ma_tx_mint
Description: Contains information about the minting of multi-assets, including the quantity of the asset and the transaction in which it was minted.
-}
data MaTxMint = MaTxMint
  { maTxMint_Id :: !MaTxMintId
  , maTxMint_Quantity :: !DbInt65   -- sqltype=int65type
  , maTxMint_Ident :: !MultiAssetId -- noreference
  , maTxMint_TxId :: !TxId          -- noreference
  } deriving (Eq, Show, Generic)

instance HasDbInfo MaTxMint

maTxMintDecoder :: D.Row MaTxMint
maTxMintDecoder =
  MaTxMint
    <$> idDecoder MaTxMintId
    <*> D.column (D.nonNullable dbInt65Decoder)
    <*> idDecoder MultiAssetId
    <*> idDecoder TxId

maTxMintEncoder :: E.Params MaTxMint
maTxMintEncoder =
  mconcat
    [ maTxMint_Quantity >$< E.param (E.nonNullable dbInt65Encoder)
    , maTxMint_Ident >$< idEncoder getMultiAssetId
    , maTxMint_TxId >$< idEncoder getTxId
    ]

maTxMintEncoderMany :: E.Params ([DbInt65], [MultiAssetId], [TxId])
maTxMintEncoderMany =
  contrazip3
    (manyEncoder $ E.nonNullable dbInt65Encoder)
    (manyEncoder $ E.nonNullable $ getMultiAssetId >$< E.int8)
    (manyEncoder $ E.nonNullable $ getTxId >$< E.int8)
