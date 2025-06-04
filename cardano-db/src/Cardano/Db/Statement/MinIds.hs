{-# LANGUAGE AllowAmbiguousTypes #-}
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
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

import qualified Cardano.Db.Schema.Core.Base as SCB
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Schema.MinIds (MinIds (..), extractCoreMaTxOutId, extractCoreTxOutId, extractVariantMaTxOutId, extractVariantTxOutId)
import qualified Cardano.Db.Schema.MinIds as SM
import Cardano.Db.Schema.Variants (MaTxOutIdW (..), TxOutIdW (..))
import qualified Cardano.Db.Schema.Variants.TxOutAddress as VA
import qualified Cardano.Db.Schema.Variants.TxOutCore as VC
import Cardano.Db.Statement.Function.Core (mkDbCallStack, runDbSession)
import Cardano.Db.Statement.Types (DbInfo (..), Key, tableName, validateColumn)
import Cardano.Db.Types (DbAction)

---------------------------------------------------------------------------
-- RAW INT64 QUERIES (for rollback operations)
---------------------------------------------------------------------------

-- | Find the minimum ID in a table - returns raw Int64
queryMinRefIdStmt ::
  forall a b.
  (DbInfo a) =>
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
  forall a b m.
  (DbInfo a, MonadIO m) =>
  -- | Field name
  Text.Text ->
  -- | Value to compare against
  b ->
  -- | Parameter encoder
  HsqlE.Params b ->
  DbAction m (Maybe Int64)
queryMinRefId fieldName value encoder =
  runDbSession (mkDbCallStack "queryMinRefId") $
    HsqlSes.statement value (queryMinRefIdStmt @a fieldName encoder rawInt64Decoder)
  where
    rawInt64Decoder = HsqlD.column (HsqlD.nonNullable HsqlD.int8)

---------------------------------------------------------------------------
-- NULLABLE QUERIES (Raw Int64)
---------------------------------------------------------------------------

queryMinRefIdNullableStmt ::
  forall a b.
  (DbInfo a) =>
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
  forall a b m.
  (DbInfo a, MonadIO m) =>
  -- | Field name
  Text.Text ->
  -- | Value to compare against
  b ->
  -- | Parameter encoder
  HsqlE.Params b ->
  DbAction m (Maybe Int64)
queryMinRefIdNullable fieldName value encoder =
  runDbSession (mkDbCallStack "queryMinRefIdNullable") $
    HsqlSes.statement value (queryMinRefIdNullableStmt @a fieldName encoder rawInt64Decoder)
  where
    rawInt64Decoder = HsqlD.column (HsqlD.nonNullable HsqlD.int8)

---------------------------------------------------------------------------
-- TYPED KEY QUERIES (for MinIds operations)
---------------------------------------------------------------------------

-- | Find the minimum ID in a table - returns typed Key
queryMinRefIdKeyStmt ::
  forall a b.
  (DbInfo a) =>
  -- | Field name to filter on
  Text.Text ->
  -- | Parameter encoder
  HsqlE.Params b ->
  -- | Key decoder
  HsqlD.Row (Key a) ->
  HsqlStmt.Statement b (Maybe (Key a))
queryMinRefIdKeyStmt fieldName encoder keyDecoder =
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
    decoder = HsqlD.rowMaybe keyDecoder

queryMinRefIdKey ::
  forall a b m.
  (DbInfo a, MonadIO m) =>
  -- | Field name
  Text.Text ->
  -- | Value to compare against
  b ->
  -- | Parameter encoder
  HsqlE.Params b ->
  -- | Key decoder
  HsqlD.Row (Key a) ->
  DbAction m (Maybe (Key a))
queryMinRefIdKey fieldName value encoder keyDecoder =
  runDbSession (mkDbCallStack "queryMinRefIdKey") $
    HsqlSes.statement value (queryMinRefIdKeyStmt @a fieldName encoder keyDecoder)

whenNothingQueryMinRefId ::
  forall a b m.
  (DbInfo a, MonadIO m) =>
  Maybe (Key a) -> -- Existing key value
  Text.Text -> -- Field name
  b -> -- Value to compare
  HsqlE.Params b -> -- Encoder for value
  HsqlD.Row (Key a) -> -- Decoder for key
  DbAction m (Maybe (Key a))
whenNothingQueryMinRefId mKey fieldName value encoder keyDecoder =
  case mKey of
    Just k -> pure $ Just k
    Nothing -> queryMinRefIdKey fieldName value encoder keyDecoder

---------------------------------------------------------------------------
-- NULLABLE KEY QUERIES (for MinIds operations)
---------------------------------------------------------------------------

queryMinRefIdNullableKeyStmt ::
  forall a b.
  (DbInfo a) =>
  -- | Field name to filter on
  Text.Text ->
  -- | Parameter encoder
  HsqlE.Params b ->
  -- | Key decoder
  HsqlD.Row (Key a) ->
  HsqlStmt.Statement b (Maybe (Key a))
queryMinRefIdNullableKeyStmt fieldName encoder keyDecoder =
  HsqlStmt.Statement sql encoder decoder True
  where
    validCol = validateColumn @a fieldName
    decoder = HsqlD.rowMaybe keyDecoder
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

queryMinRefIdNullableKey ::
  forall a b m.
  (DbInfo a, MonadIO m) =>
  -- | Field name
  Text.Text ->
  -- | Value to compare against
  b ->
  -- | Parameter encoder
  HsqlE.Params b ->
  -- | Key decoder
  HsqlD.Row (Key a) ->
  DbAction m (Maybe (Key a))
queryMinRefIdNullableKey fieldName value encoder keyDecoder =
  runDbSession (mkDbCallStack "queryMinRefIdNullableKey") $
    HsqlSes.statement value (queryMinRefIdNullableKeyStmt @a fieldName encoder keyDecoder)

---------------------------------------------------------------------------
-- MAX QUERIES (for completeness)
---------------------------------------------------------------------------

queryMaxRefIdStmt ::
  forall a b.
  (DbInfo a) =>
  -- | Field name to filter on
  Text.Text ->
  -- | Equal or strictly less
  Bool ->
  -- | Parameter encoder
  HsqlE.Params b ->
  -- | Key decoder
  HsqlD.Row (Key a) ->
  HsqlStmt.Statement b (Maybe (Key a))
queryMaxRefIdStmt fieldName eq encoder keyDecoder =
  HsqlStmt.Statement sql encoder decoder True
  where
    validCol = validateColumn @a fieldName
    op = if eq then "<=" else "<"
    decoder = HsqlD.rowMaybe keyDecoder
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id"
          , " FROM " <> tableName (Proxy @a)
          , " WHERE " <> validCol <> " " <> op <> " $1"
          , " ORDER BY id DESC"
          , " LIMIT 1"
          ]

queryMaxRefId ::
  forall a b m.
  (DbInfo a, MonadIO m) =>
  -- | Field name
  Text.Text ->
  -- | Value to compare against
  b ->
  -- | Equal or strictly less
  Bool ->
  -- | Parameter encoder
  HsqlE.Params b ->
  -- | Key decoder
  HsqlD.Row (Key a) ->
  DbAction m (Maybe (Key a))
queryMaxRefId fieldName value eq encoder keyDecoder =
  runDbSession (mkDbCallStack "queryMaxRefId") $
    HsqlSes.statement value (queryMaxRefIdStmt @a fieldName eq encoder keyDecoder)

---------------------------------------------------------------------------
-- MINIDS COMPLETION FUNCTIONS
---------------------------------------------------------------------------

completeMinId ::
  (MonadIO m) =>
  Maybe Id.TxId ->
  SM.MinIdsWrapper ->
  DbAction m SM.MinIdsWrapper
completeMinId mTxId mIdW = case mIdW of
  SM.CMinIdsWrapper minIds -> SM.CMinIdsWrapper <$> completeMinIdCore mTxId minIds
  SM.VMinIdsWrapper minIds -> SM.VMinIdsWrapper <$> completeMinIdVariant mTxId minIds

completeMinIdCore :: MonadIO m => Maybe Id.TxId -> MinIds -> DbAction m MinIds
completeMinIdCore mTxId minIds = do
  case mTxId of
    Nothing -> pure mempty
    Just txId -> do
      mTxInId <-
        whenNothingQueryMinRefId @SCB.TxIn
          (minTxInId minIds)
          "tx_in_id"
          txId
          (Id.idEncoder Id.getTxId)
          (Id.idDecoder Id.TxInId)

      mTxOutId <-
        whenNothingQueryMinRefId @VC.TxOutCore
          (extractCoreTxOutId $ minTxOutId minIds)
          "tx_id"
          txId
          (Id.idEncoder Id.getTxId)
          (Id.idDecoder Id.TxOutCoreId)

      mMaTxOutId <- case mTxOutId of
        Nothing -> pure Nothing
        Just txOutId ->
          whenNothingQueryMinRefId @VC.MaTxOutCore
            (extractCoreMaTxOutId $ minMaTxOutId minIds)
            "tx_out_id"
            txOutId
            (Id.idEncoder Id.getTxOutCoreId)
            (Id.idDecoder Id.MaTxOutCoreId)

      pure $
        MinIds
          { minTxInId = mTxInId
          , minTxOutId = VCTxOutIdW <$> mTxOutId
          , minMaTxOutId = CMaTxOutIdW <$> mMaTxOutId
          }

completeMinIdVariant :: MonadIO m => Maybe Id.TxId -> MinIds -> DbAction m MinIds
completeMinIdVariant mTxId minIds = do
  case mTxId of
    Nothing -> pure mempty
    Just txId -> do
      mTxInId <-
        whenNothingQueryMinRefId @SCB.TxIn
          (minTxInId minIds)
          "tx_in_id"
          txId
          (Id.idEncoder Id.getTxId)
          (Id.idDecoder Id.TxInId)

      mTxOutId <-
        whenNothingQueryMinRefId @VA.TxOutAddress
          (extractVariantTxOutId $ minTxOutId minIds)
          "tx_id"
          txId
          (Id.idEncoder Id.getTxId)
          (Id.idDecoder Id.TxOutAddressId)

      mMaTxOutId <- case mTxOutId of
        Nothing -> pure Nothing
        Just txOutId ->
          whenNothingQueryMinRefId @VA.MaTxOutAddress
            (extractVariantMaTxOutId $ minMaTxOutId minIds)
            "tx_out_id"
            txOutId
            (Id.idEncoder Id.getTxOutAddressId)
            (Id.idDecoder Id.MaTxOutAddressId)

      pure $
        MinIds
          { minTxInId = mTxInId
          , minTxOutId = VATxOutIdW <$> mTxOutId
          , minMaTxOutId = VMaTxOutIdW <$> mMaTxOutId
          }
