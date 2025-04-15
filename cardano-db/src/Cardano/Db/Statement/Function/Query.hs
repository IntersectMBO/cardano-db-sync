{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Db.Statement.Function.Query where

import qualified Data.Text as Text
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Statement as HsqlS

import qualified Data.Text.Encoding as TextEnc

import Cardano.Db.Statement.Function.Core (ResultType (..))
import Cardano.Db.Statement.Types (DbInfo (..), Entity, Key, validateColumn)
import Cardano.Prelude (Proxy (..), Word64)
import Data.Functor.Contravariant (Contravariant (..))
import qualified Data.List.NonEmpty as NE

replace ::
  forall a.
  (DbInfo a) =>
  HsqlE.Params (Key a) -> -- ID encoder
  HsqlE.Params a -> -- Record encoder
  HsqlS.Statement (Key a, a) ()
replace keyEncoder recordEncoder =
  HsqlS.Statement sql encoder HsqlD.noResult True
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
  HsqlS.Statement b (Maybe (Entity a))
selectByField fieldName paramEncoder entityDecoder =
  HsqlS.Statement
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
-- queryVotingAnchorIdStmt :: HsqlS.Statement Id.VotingAnchorId Bool
-- queryVotingAnchorIdStmt = existsById @VotingAnchor
--   (Id.idEncoder Id.getVotingAnchorId)
--   (WithResult (HsqlD.singleRow $ HsqlD.column (HsqlD.nonNullable HsqlD.bool)))
-- @
existsById ::
  forall a r.
  (DbInfo a, Key a ~ Key a) =>
  HsqlE.Params (Key a) -> -- Key encoder
  ResultType Bool r -> -- Whether to return Entity and decoder
  HsqlS.Statement (Key a) r
existsById encoder resultType =
  HsqlS.Statement sql encoder decoder True
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

-- | Creates a statement to replace a record with a new value
--
-- === Example
-- @
-- replaceVotingAnchor :: MonadIO m => VotingAnchorId -> VotingAnchor -> DbAction m ()
-- replaceVotingAnchor key record =
--   runDbSession (mkCallInfo "replaceVotingAnchor") $
--     HsqlS.statement (key, record) $ replaceRecord
--       @VotingAnchor
--       (idEncoder getVotingAnchorId)
--       votingAnchorEncoder
-- @
replaceRecord ::
  forall a.
  (DbInfo a) =>
  HsqlE.Params (Key a) -> -- Key encoder
  HsqlE.Params a -> -- Record encoder
  HsqlS.Statement (Key a, a) () -- Returns a statement to replace a record
replaceRecord keyEnc recordEnc =
  HsqlS.Statement sql encoder HsqlD.noResult True
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
-- queryTxOutUnspentCount :: MonadIO m => TxOutTableType -> DbAction m Word64
-- queryTxOutUnspentCount txOutTableType =
--   case txOutTableType of
--     TxOutCore ->
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
  HsqlS.Statement () Word64
countWhere colName condition =
  HsqlS.Statement sql HsqlE.noParams decoder True
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
  HsqlS.Statement p Word64
parameterisedCountWhere colName condition encoder =
  HsqlS.Statement sql encoder decoder True
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
  HsqlS.Statement () Word64
countAll =
  HsqlS.Statement sql HsqlE.noParams decoder True
  where
    table = tableName (Proxy @a)
    decoder = HsqlD.singleRow (HsqlD.column $ HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COUNT(*)::bigint"
          , " FROM " <> table
          ]
