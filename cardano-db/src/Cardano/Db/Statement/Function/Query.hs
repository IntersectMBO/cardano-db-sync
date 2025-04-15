{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Db.Statement.Function.Query where

import Cardano.Prelude (MonadIO, Proxy (..), Word64, fromMaybe)
import Data.Fixed (Fixed (..))
import Data.Functor.Contravariant (Contravariant (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

import Cardano.Db.Statement.Function.Core (ResultType (..), mkCallInfo, runDbSession)
import Cardano.Db.Statement.Types (DbInfo (..), Entity, Key, validateColumn)
import Cardano.Db.Types (Ada (..), DbAction, lovelaceToAda)

replace ::
  forall a.
  (DbInfo a) =>
  HsqlE.Params (Key a) -> -- ID encoder
  HsqlE.Params a -> -- Record encoder
  HsqlStmt.Statement (Key a, a) ()
replace keyEncoder recordEncoder =
  HsqlStmt.Statement sql encoder HsqlD.noResult True
  where
    table = tableName (Proxy @a)
    colNames = NE.toList $ columnNames (Proxy @a)

    setClause =
      Text.intercalate ", " $
        zipWith
          (\col i -> col <> " = $" <> Text.pack (show (i + (1 :: Integer))))
          colNames
          [1 ..]

    encoder = contramap fst keyEncoder <> contramap snd recordEncoder

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "UPDATE " <> table
          , " SET " <> setClause
          , " WHERE id = $1"
          ]

selectByField ::
  forall a b.
  (DbInfo a) =>
  Text.Text -> -- Field name
  HsqlE.Params b -> -- Parameter encoder (not Value)
  HsqlD.Row (Entity a) -> -- Entity decoder
  HsqlStmt.Statement b (Maybe (Entity a))
selectByField fieldName paramEncoder entityDecoder =
  HsqlStmt.Statement
    ( TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT * FROM " <> tableName (Proxy @a)
          , " WHERE " <> fieldName <> " = $1"
          ]
    )
    paramEncoder -- Direct use of paramEncoder
    (HsqlD.rowMaybe entityDecoder)
    True

-- | Checks if a record with a specific ID exists in a table.
--
-- This function performs an EXISTS check on a given table, using the record's ID.
--
-- === Example
-- @
-- queryVotingAnchorIdStmt :: HsqlStmt.Statement Id.VotingAnchorId Bool
-- queryVotingAnchorIdStmt = existsById @VotingAnchor
--   (Id.idEncoder Id.getVotingAnchorId)
--   (WithResult (HsqlD.singleRow $ HsqlD.column (HsqlD.nonNullable HsqlD.bool)))
-- @
existsById ::
  forall a r.
  (DbInfo a, Key a ~ Key a) =>
  HsqlE.Params (Key a) -> -- Key encoder
  ResultType Bool r -> -- Whether to return Entity and decoder
  HsqlStmt.Statement (Key a) r
existsById encoder resultType =
  HsqlStmt.Statement sql encoder decoder True
  where
    decoder = case resultType of
      NoResult -> HsqlD.noResult
      WithResult dec -> dec

    table = tableName (Proxy @a)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT EXISTS (SELECT 1 FROM " <> table
          , " WHERE id = $1)"
          ]

-- | Statement to check if a row exists with a specific value in a given column
--
-- === Example
-- @
-- existsWhereStmt :: HsqlStmt.Statement ByteString Bool
-- existsWhereStmt = existsWhere @DelistedPool "hash_raw" (HsqlE.param (HsqlE.nonNullable HsqlE.bytea)) (WithResult boolDecoder)
-- @
existsWhere ::
  forall a r.
  (DbInfo a, Key a ~ Key a) =>
  -- | Column name to filter on
  Text.Text ->
  -- | Parameter encoder
  HsqlE.Params (Key a) ->
  -- | Whether to return result and decoder
  ResultType Bool r ->
  HsqlStmt.Statement (Key a) r
existsWhere colName encoder resultType =
  HsqlStmt.Statement sql encoder decoder True
  where
    decoder = case resultType of
      NoResult -> HsqlD.noResult
      WithResult dec -> dec

    table = tableName (Proxy @a)
    validCol = validateColumn @a colName

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT EXISTS ("
          , "  SELECT 1"
          , "  FROM " <> table
          , "  WHERE " <> validCol <> " = $1"
          , ")"
          ]

-- | Statement to check if a row exists with a specific value in a given column
--
-- === Example
-- @
-- existsWhereByColumnStmt :: HsqlStmt.Statement ByteString Bool
-- existsWhereByColumnStmt = existsWhereByColumn @DelistedPool "hash_raw" (HsqlE.param (HsqlE.nonNullable HsqlE.bytea)) (WithResult boolDecoder)
-- @
existsWhereByColumn ::
  forall a b r.
  (DbInfo a) =>
  -- | Column name to filter on
  Text.Text ->
  -- | Parameter encoder for the column value
  HsqlE.Params b ->
  -- | Whether to return result and decoder
  ResultType Bool r ->
  HsqlStmt.Statement b r
existsWhereByColumn colName encoder resultType =
  HsqlStmt.Statement sql encoder decoder True
  where
    decoder = case resultType of
      NoResult -> HsqlD.noResult
      WithResult dec -> dec

    table = tableName (Proxy @a)
    validCol = validateColumn @a colName

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT EXISTS ("
          , "  SELECT 1"
          , "  FROM " <> table
          , "  WHERE " <> validCol <> " = $1"
          , ")"
          ]

-- | Creates a statement to replace a record with a new value
--
-- === Example
-- @
-- replaceVotingAnchor :: MonadIO m => VotingAnchorId -> VotingAnchor -> DbAction m ()
-- replaceVotingAnchor key record =
--   runDbSession (mkCallInfo "replaceVotingAnchor") $
--     HsqlStmt.statement (key, record) $ replaceRecord
--       @VotingAnchor
--       (idEncoder getVotingAnchorId)
--       votingAnchorEncoder
-- @
replaceRecord ::
  forall a.
  (DbInfo a) =>
  HsqlE.Params (Key a) -> -- Key encoder
  HsqlE.Params a -> -- Record encoder
  HsqlStmt.Statement (Key a, a) () -- Returns a statement to replace a record
replaceRecord keyEnc recordEnc =
  HsqlStmt.Statement sql encoder HsqlD.noResult True
  where
    table = tableName (Proxy @a)
    colsNames = NE.toList $ columnNames (Proxy @a)

    setClause =
      Text.intercalate ", " $
        zipWith
          (\col idx -> col <> " = $" <> Text.pack (show idx))
          colsNames
          [2 .. (length colsNames + 1)]

    -- Combined encoder for the (key, record) tuple
    encoder = contramap fst keyEnc <> contramap snd recordEnc

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "UPDATE " <> table
          , " SET " <> setClause
          , " WHERE id = $1"
          ]

-- | Creates a statement to count rows in a table where a column matches a condition
--
-- The function validates that the column exists in the table schema
-- and throws an error if it doesn't.
--
-- === Example
-- @
-- queryTxOutUnspentCount :: MonadIO m => TxOutVariantType -> DbAction m Word64
-- queryTxOutUnspentCount txOutVariantType =
--   case txOutVariantType of
--     TxOutVariantCore ->
--       runDbSession (mkCallInfo "queryTxOutUnspentCountCore") $
--         HsqlSes.statement () (countWhere @TxOutCore "consumed_by_tx_id" "IS NULL")
--
--     TxOutVariantAddress ->
--       runDbSession (mkCallInfo "queryTxOutUnspentCountAddress") $
--         HsqlSes.statement () (countWhere @TxOutAddress "consumed_by_tx_id" "IS NULL")
-- @
countWhere ::
  forall a.
  (DbInfo a) =>
  -- | Column name to filter on
  Text.Text ->
  -- | SQL condition to apply (e.g., "IS NULL", "= $1", "> 100")
  Text.Text ->
  -- | Returns a statement that counts matching rows
  HsqlStmt.Statement () Word64
countWhere colName condition =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    decoder = HsqlD.singleRow (HsqlD.column $ HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
    -- Validate the column name
    validCol = validateColumn @a colName

    -- SQL statement to count rows matching the condition
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COUNT(*)::bigint"
          , " FROM " <> tableName (Proxy @a)
          , " WHERE " <> validCol <> " " <> condition
          ]

-- | Creates a statement to count rows matching a parameterized condition
parameterisedCountWhere ::
  forall a p.
  (DbInfo a) =>
  -- | Column name to filter on
  Text.Text ->
  -- | SQL condition with parameter placeholders
  Text.Text ->
  -- | Parameter encoder
  HsqlE.Params p ->
  HsqlStmt.Statement p Word64
parameterisedCountWhere colName condition encoder =
  HsqlStmt.Statement sql encoder decoder True
  where
    decoder = HsqlD.singleRow (HsqlD.column $ HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
    -- Validate the column name
    validCol = validateColumn @a colName

    -- SQL statement to count rows matching the condition
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COUNT(*)::bigint"
          , " FROM " <> tableName (Proxy @a)
          , " WHERE " <> validCol <> " " <> condition
          ]

-- | Creates a statement to count all rows in a table
--
-- === Example
-- @
-- queryTableCount :: MonadIO m => DbAction m Word64
-- queryTableCount =
--   runDbSession (mkCallInfo "queryTableCount") $
--     HsqlSes.statement () (countAll @TxOutCore)
-- @
countAll ::
  forall a.
  (DbInfo a) =>
  -- | Returns a statement that counts all rows
  HsqlStmt.Statement () Word64
countAll =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    table = tableName (Proxy @a)
    decoder = HsqlD.singleRow (HsqlD.column $ HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COUNT(*)::bigint"
          , " FROM " <> table
          ]

---------------------------------------------------------------------------
-- REFERENCE ID QUERIES
---------------------------------------------------------------------------

-- | Find the minimum ID in a table
queryMinRefIdStmt ::
  forall a b.
  (DbInfo a) =>
  -- | Field name to filter on
  Text.Text ->
  -- | Parameter encoder
  HsqlE.Params b ->
  -- | Key decoder
  HsqlD.Row (Key a) ->
  HsqlStmt.Statement b (Maybe (Key a))
queryMinRefIdStmt fieldName encoder keyDecoder =
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

queryMinRefId ::
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
queryMinRefId fieldName value encoder keyDecoder =
  runDbSession (mkCallInfo "queryMinRefId") $
    HsqlSes.statement value (queryMinRefIdStmt @a fieldName encoder keyDecoder)

---------------------------------------------------------------------------
queryMinRefIdNullableStmt ::
  forall a b.
  (DbInfo a) =>
  -- | Field name to filter on
  Text.Text ->
  -- | Parameter encoder
  HsqlE.Params b ->
  -- | Key decoder
  HsqlD.Row (Key a) ->
  HsqlStmt.Statement b (Maybe (Key a))
queryMinRefIdNullableStmt fieldName encoder keyDecoder =
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

queryMinRefIdNullable ::
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
queryMinRefIdNullable fieldName value encoder keyDecoder =
  runDbSession (mkCallInfo "queryMinRefIdNullable") $
    HsqlSes.statement value (queryMinRefIdNullableStmt @a fieldName encoder keyDecoder)

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
  runDbSession (mkCallInfo "queryMaxRefId") $
    HsqlSes.statement value (queryMaxRefIdStmt @a fieldName eq encoder keyDecoder)

---------------------------------------------------------------------------
-- QUERY HELPERS
---------------------------------------------------------------------------

-- Decoder for Ada amounts from database int8 values
adaDecoder :: HsqlD.Row Ada
adaDecoder = do
  amount <- HsqlD.column (HsqlD.nonNullable HsqlD.int8)
  pure $ lovelaceToAda (MkFixed $ fromIntegral amount)

-- Decoder for summed Ada amounts with null handling
adaSumDecoder :: HsqlD.Row Ada
adaSumDecoder = do
  amount <- HsqlD.column (HsqlD.nullable HsqlD.int8)
  case amount of
    Just value -> pure $ lovelaceToAda (MkFixed $ fromIntegral value)
    Nothing -> pure $ Ada 0

-- | Get the UTxO set after the specified 'BlockNo' has been applied to the chain.
-- Unfortunately the 'sum_' operation above returns a 'PersistRational' so we need
-- to un-wibble it.
unValueSumAda :: HsqlD.Result Ada
unValueSumAda =
  HsqlD.singleRow $
    fromMaybe (Ada 0) <$> HsqlD.column (HsqlD.nullable (Ada . fromIntegral <$> HsqlD.int8))
