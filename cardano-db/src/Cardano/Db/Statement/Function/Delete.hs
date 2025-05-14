{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.Function.Delete where

import Cardano.Prelude (Int64, Proxy (..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Statement as HsqlS

import Cardano.Db.Statement.Types (DbInfo (..), validateColumn)

-- | Creates a statement to delete rows that match a condition on a column
--
-- === Example
-- @
-- deleteInvalidRecords :: MonadIO m => DbAction m ()
-- deleteInvalidRecords =
--   runDbSession (mkCallInfo "deleteInvalidRecords") $
--     HsqlSes.statement () (deleteWhere @Record "status" "= 'INVALID'")
-- @
deleteWhere ::
  forall a.
  (DbInfo a) =>
  -- | Column name to filter on
  Text.Text ->
  -- | SQL condition to apply (e.g., "IS NULL", ">= $1", "= 'INVALID'")
  Text.Text ->
  -- | Returns a statement that deletes matching rows
  HsqlS.Statement () ()
deleteWhere colName condition =
  HsqlS.Statement sql HsqlE.noParams HsqlD.noResult True
  where
    -- Validate the column name
    validCol = validateColumn @a colName

    -- SQL statement to delete rows matching the condition
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "DELETE FROM " <> tableName (Proxy @a)
          , " WHERE " <> validCol <> " " <> condition
          ]

-- | Helper function for parameterized DELETE queries
parameterisedDeleteWhere ::
  forall a p.
  (DbInfo a) =>
  -- | Column name
  Text.Text ->
  -- | Condition with placeholder
  Text.Text ->
  -- | Parameter encoder
  HsqlE.Params p ->
  HsqlS.Statement p ()
parameterisedDeleteWhere colName condition encoder =
  HsqlS.Statement sql encoder HsqlD.noResult True
  where
    validCol = validateColumn @a colName
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "DELETE FROM " <> tableName (Proxy @a)
          , " WHERE " <> validCol <> " " <> condition
          ]

-- | Creates a statement to delete rows and return the count of deleted rows
--
-- === Example
-- @
-- deleteTxOutRecords :: MonadIO m => DbAction m Int64
-- deleteTxOutRecords =
--   runDbSession (mkCallInfo "deleteTxOutRecords") $
--     HsqlSes.statement () (deleteWhereCount @TxOutCore "id" ">=" HsqlE.noParams)
-- @
deleteWhereCount ::
  forall a b.
  (DbInfo a) =>
  -- | Column name to filter on
  Text.Text ->
  -- | SQL condition to apply (e.g., "IS NULL", ">=", "=")
  Text.Text ->
  -- | Parameter encoder
  HsqlE.Params b ->
  -- | Returns a statement that deletes matching rows and returns count
  HsqlS.Statement b Int64
deleteWhereCount colName condition encoder =
  HsqlS.Statement sql encoder decoder True
  where
    -- Validate the column name
    validCol = validateColumn @a colName
    decoder = HsqlD.singleRow (HsqlD.column $ HsqlD.nonNullable HsqlD.int8)

    -- Condition with parameter placeholder if needed
    conditionWithParam =
      if "NULL" `Text.isInfixOf` condition || "'" `Text.isInfixOf` condition
        then condition -- For "IS NULL" or literal values like "= 'INVALID'"
        else condition <> " $1" -- For parameter-based conditions like ">="

    -- SQL statement with RETURNING count
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "WITH deleted AS ("
          , "  DELETE FROM " <> tableName (Proxy @a)
          , "  WHERE " <> validCol <> " " <> conditionWithParam
          , "  RETURNING *"
          , ")"
          , "SELECT COUNT(*)::bigint FROM deleted"
          ]

-- | Creates a statement to delete all rows in a table
--
-- === Example
-- @
-- truncateTable :: MonadIO m => DbAction m ()
-- truncateTable =
--   runDbSession (mkCallInfo "truncateTable") $
--     HsqlSes.statement () (deleteAll @MyTable)
-- @
deleteAll ::
  forall a.
  (DbInfo a) =>
  HsqlS.Statement () ()
deleteAll =
  HsqlS.Statement sql HsqlE.noParams HsqlD.noResult True
  where
    table = tableName (Proxy @a)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          ["DELETE FROM " <> table]

-- | Creates a statement to delete all rows in a table and return the count
--
-- === Example
-- @
-- truncateAndCount :: MonadIO m => DbAction m Int64
-- truncateAndCount =
--   runDbSession (mkCallInfo "truncateAndCount") $
--     HsqlSes.statement () (deleteAllCount @MyTable)
-- @
deleteAllCount ::
  forall a.
  (DbInfo a) =>
  HsqlS.Statement () Int64
deleteAllCount =
  HsqlS.Statement sql HsqlE.noParams decoder True
  where
    table = tableName (Proxy @a)
    decoder = HsqlD.singleRow (HsqlD.column $ HsqlD.nonNullable HsqlD.int8)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "WITH deleted AS ("
          , "  DELETE FROM " <> table
          , "  RETURNING *"
          , ")"
          , "SELECT COUNT(*)::bigint FROM deleted"
          ]
