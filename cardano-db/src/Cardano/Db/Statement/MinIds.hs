{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.Db.Statement.MinIds where

import Cardano.Prelude
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Pipeline as HsqlP
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

import Cardano.Db.Error (mkDbCallStack)
import qualified Cardano.Db.Schema.Core.Base as SCB
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Schema.MinIds (MinIds (..), extractCoreMaTxOutId, extractCoreTxOutId, extractVariantMaTxOutId, extractVariantTxOutId)
import qualified Cardano.Db.Schema.MinIds as SM
import Cardano.Db.Schema.Variants (MaTxOutIdW (..), TxOutIdW (..))
import qualified Cardano.Db.Schema.Variants.TxOutAddress as VA
import qualified Cardano.Db.Schema.Variants.TxOutCore as VC
import Cardano.Db.Statement.Function.Core (runSession)
import Cardano.Db.Statement.Types (DbInfo (..), Key, tableName, validateColumn)
import Cardano.Db.Types (DbM)

---------------------------------------------------------------------------
-- RAW INT64 QUERIES (for rollback operations)
---------------------------------------------------------------------------

-- | Find the minimum ID in a table - returns raw Int64
queryMinRefIdStmt ::
  forall a b.
  DbInfo a =>
  -- | Field name to filter on
  Text.Text ->
  -- | Parameter encoder
  HsqlE.Params b ->
  -- | Raw ID decoder (Int64)
  HsqlD.Row Int64 ->
  HsqlStmt.Statement b (Maybe Int64)
queryMinRefIdStmt fieldName encoder idDecoder =
  HsqlStmt.Statement sql encoder decoder True
  where
    validCol = validateColumn @a fieldName
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id"
          , " FROM " <> tableName (Proxy @a)
          , " WHERE " <> validCol <> " >= $1"
          , " ORDER BY id ASC"
          , " LIMIT 1"
          ]
    decoder = HsqlD.rowMaybe idDecoder

queryMinRefId ::
  forall a b.
  DbInfo a =>
  -- | Field name
  Text.Text ->
  -- | Value to compare against
  b ->
  -- | Parameter encoder
  HsqlE.Params b ->
  DbM (Maybe Int64)
queryMinRefId fieldName value encoder =
  runSession mkDbCallStack $ HsqlSes.statement value (queryMinRefIdStmt @a fieldName encoder rawInt64Decoder)
  where
    rawInt64Decoder = HsqlD.column (HsqlD.nonNullable HsqlD.int8)

---------------------------------------------------------------------------
-- NULLABLE QUERIES (Raw Int64)
---------------------------------------------------------------------------

queryMinRefIdNullableStmt ::
  forall a b.
  DbInfo a =>
  -- | Field name to filter on
  Text.Text ->
  -- | Parameter encoder
  HsqlE.Params b ->
  -- | Raw ID decoder (Int64)
  HsqlD.Row Int64 ->
  HsqlStmt.Statement b (Maybe Int64)
queryMinRefIdNullableStmt fieldName encoder idDecoder =
  HsqlStmt.Statement sql encoder decoder True
  where
    validCol = validateColumn @a fieldName
    decoder = HsqlD.rowMaybe idDecoder
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id"
          , " FROM " <> tableName (Proxy @a)
          , " WHERE " <> validCol <> " IS NOT NULL"
          , " AND " <> validCol <> " >= $1"
          , " ORDER BY id ASC"
          , " LIMIT 1"
          ]

queryMinRefIdNullable ::
  forall a b.
  DbInfo a =>
  -- | Field name
  Text.Text ->
  -- | Value to compare against
  b ->
  -- | Parameter encoder
  HsqlE.Params b ->
  DbM (Maybe Int64)
queryMinRefIdNullable fieldName value encoder =
  runSession mkDbCallStack $ HsqlSes.statement value (queryMinRefIdNullableStmt @a fieldName encoder rawInt64Decoder)
  where
    rawInt64Decoder = HsqlD.column (HsqlD.nonNullable HsqlD.int8)

---------------------------------------------------------------------------
-- TYPED KEY QUERIES (for MinIds operations)
---------------------------------------------------------------------------

-- | Find the minimum ID in a table - returns typed Key
queryMinRefIdKeyStmt ::
  forall a b.
  DbInfo a =>
  -- | Field name to filter on
  Text.Text ->
  -- | Parameter encoder
  HsqlE.Params b ->
  -- | Key decoder (nullable)
  HsqlD.Row (Maybe (Key a)) ->
  HsqlStmt.Statement b (Maybe (Key a))
queryMinRefIdKeyStmt fieldName encoder keyDecoder =
  HsqlStmt.Statement sql encoder decoder True
  where
    validCol = validateColumn @a fieldName
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT MIN(id)"
          , " FROM " <> tableName (Proxy @a)
          , " WHERE " <> validCol <> " >= $1"
          ]
    decoder = HsqlD.singleRow keyDecoder

queryMinRefIdKey ::
  forall a b.
  DbInfo a =>
  -- | Field name
  Text.Text ->
  -- | Value to compare against
  b ->
  -- | Parameter encoder
  HsqlE.Params b ->
  -- | Key decoder (nullable)
  HsqlD.Row (Maybe (Key a)) ->
  DbM (Maybe (Key a))
queryMinRefIdKey fieldName value encoder keyDecoder =
  runSession mkDbCallStack $
    HsqlSes.statement value (queryMinRefIdKeyStmt @a fieldName encoder keyDecoder)

whenNothingQueryMinRefId ::
  forall a b.
  DbInfo a =>
  Maybe (Key a) -> -- Existing key value
  Text.Text -> -- Field name
  b -> -- Value to compare
  HsqlE.Params b -> -- Encoder for value
  HsqlD.Row (Maybe (Key a)) -> -- Decoder for key (nullable)
  DbM (Maybe (Key a))
whenNothingQueryMinRefId mKey fieldName value encoder keyDecoder =
  case mKey of
    Just k -> pure $ Just k
    Nothing -> queryMinRefIdKey fieldName value encoder keyDecoder

---------------------------------------------------------------------------
-- MINIDS COMPLETION FUNCTIONS
---------------------------------------------------------------------------

completeMinId ::
  Maybe Id.TxId ->
  SM.MinIdsWrapper ->
  DbM SM.MinIdsWrapper
completeMinId mTxId mIdW = case mIdW of
  SM.CMinIdsWrapper minIds -> do
    res <- completeMinIdCore mTxId minIds
    pure $ SM.CMinIdsWrapper res
  SM.VMinIdsWrapper minIds -> do
    res <- completeMinIdVariant mTxId minIds
    pure $ SM.VMinIdsWrapper res

completeMinIdCore :: Maybe Id.TxId -> MinIds -> DbM MinIds
completeMinIdCore mTxId minIds = do
  case mTxId of
    Nothing -> pure mempty
    Just txId -> do
      (mTxInId, mTxOutId) <- runSession mkDbCallStack $ HsqlSes.pipeline $ do
        txInResult <- case minTxInId minIds of
          Just k -> pure $ Just k
          Nothing -> HsqlP.statement txId (queryMinRefIdKeyStmt @SCB.TxIn "tx_in_id" (Id.idEncoder Id.getTxId) (Id.maybeIdDecoder Id.TxInId))

        txOutResult <- case extractCoreTxOutId $ minTxOutId minIds of
          Just k -> pure $ Just k
          Nothing -> HsqlP.statement txId (queryMinRefIdKeyStmt @VC.TxOutCore "tx_id" (Id.idEncoder Id.getTxId) (Id.maybeIdDecoder Id.TxOutCoreId))

        pure (txInResult, txOutResult)

      mMaTxOutId <- case mTxOutId of
        Nothing -> pure Nothing
        Just txOutId ->
          case extractCoreMaTxOutId $ minMaTxOutId minIds of
            Just k -> pure $ Just k
            Nothing -> runSession mkDbCallStack $ HsqlSes.statement txOutId (queryMinRefIdKeyStmt @VC.MaTxOutCore "tx_out_id" (Id.idEncoder Id.getTxOutCoreId) (Id.maybeIdDecoder Id.MaTxOutCoreId))

      pure $
        MinIds
          { minTxInId = mTxInId
          , minTxOutId = VCTxOutIdW <$> mTxOutId
          , minMaTxOutId = CMaTxOutIdW <$> mMaTxOutId
          }

completeMinIdVariant :: Maybe Id.TxId -> MinIds -> DbM MinIds
completeMinIdVariant mTxId minIds = do
  case mTxId of
    Nothing -> pure mempty
    Just txId -> do
      (mTxInId, mTxOutId) <- runSession mkDbCallStack $ HsqlSes.pipeline $ do
        txInResult <- case minTxInId minIds of
          Just k -> pure $ Just k
          Nothing -> HsqlP.statement txId (queryMinRefIdKeyStmt @SCB.TxIn "tx_in_id" (Id.idEncoder Id.getTxId) (Id.maybeIdDecoder Id.TxInId))

        txOutResult <- case extractVariantTxOutId $ minTxOutId minIds of
          Just k -> pure $ Just k
          Nothing -> HsqlP.statement txId (queryMinRefIdKeyStmt @VA.TxOutAddress "tx_id" (Id.idEncoder Id.getTxId) (Id.maybeIdDecoder Id.TxOutAddressId))

        pure (txInResult, txOutResult)

      mMaTxOutId <- case mTxOutId of
        Nothing -> pure Nothing
        Just txOutId ->
          case extractVariantMaTxOutId $ minMaTxOutId minIds of
            Just k -> pure $ Just k
            Nothing -> runSession mkDbCallStack $ HsqlSes.statement txOutId (queryMinRefIdKeyStmt @VA.MaTxOutAddress "tx_out_id" (Id.idEncoder Id.getTxOutAddressId) (Id.maybeIdDecoder Id.MaTxOutAddressId))

      pure $
        MinIds
          { minTxInId = mTxInId
          , minTxOutId = VATxOutIdW <$> mTxOutId
          , minMaTxOutId = VMaTxOutIdW <$> mMaTxOutId
          }
