{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.Function.Insert
  (insert,
    insertCheckUnique,
    bulkInsertNoReturn,
    bulkInsertReturnIds,
  )
where

import qualified Data.Text as Text
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Statement as HsqlS
import qualified Hasql.Transaction as HsqlT
import qualified Data.Text.Encoding as TextEnc
import qualified Data.List.NonEmpty as NE

import Cardano.Prelude (Proxy(..))
import Cardano.Db.Statement.Types (DbInfo (..))
import Cardano.Db.Statement.Function.Core (ResultType(..), ResultTypeBulk (..))

-- | Inserts a record into a table, with option of returning the generated ID.
--
-- ==== Parameters
-- * @encoder@: The encoder for the record.
-- * @resultType@: Whether to return a result (usually it's newly generated id) and decoder.
-- * @record@: The record to insert.
insert
  :: forall a c r. (DbInfo a)
  => HsqlE.Params a             -- Encoder
  -> ResultType c r             -- Whether to return a result and decoder
  -> a                          -- Record
  -> HsqlT.Transaction r
insert encoder resultType record =
  HsqlT.statement record $ HsqlS.Statement sql encoder decoder True
  where
    (decoder, shouldReturntype) = case resultType of
      NoResult -> (HsqlD.noResult, "")
      WithResult dec  -> (dec, "RETURNING id")

    table = tableName (Proxy @a)
    -- columns drop the ID column
    colsNoId = NE.fromList $ NE.drop 1 (columnNames (Proxy @a))

    values = Text.intercalate ", " $ map (\i -> "$" <> Text.pack (show i)) [1..length colsNoId]

    sql = TextEnc.encodeUtf8 $ Text.concat
      [ "INSERT INTO " <> table
      , " (" <> Text.intercalate ", " (NE.toList colsNoId) <> ")"
      , " VALUES (" <> values <> ")"
      , shouldReturntype
      ]

-- | Inserts a record into a table, checking for a unique constraint violation.
--
-- If the `DbInfoConstraints` instance does not match any table type records, this function will throw an error.
--
-- ==== Parameters
-- * @encoder@: The encoder for the record.
-- * @resultType@: Whether to return a result (usually it's newly generated id) and decoder.
-- * @record@: The record to insert.
insertCheckUnique
  :: forall a c r. (DbInfo a)
  => HsqlE.Params a              -- Encoder
  -> ResultType c r             -- Whether to return a result and decoder
  -> a                          -- Record
  -> HsqlT.Transaction r
insertCheckUnique encoder resultType record =
  case validateUniqueConstraints (Proxy @a) of
    Left err -> error err
    Right _ -> HsqlT.statement record $ HsqlS.Statement sql encoder decoder True
  where

    (decoder, returnClause) = case resultType of
      NoResult -> (HsqlD.noResult, "")
      WithResult dec  -> (dec, "RETURNING id")

    table = tableName (Proxy @a)
    cols = columnNames (Proxy @a)
    uniqueCols = uniqueFields (Proxy @a)

    -- Drop the ID column for value placeholders
    colsNoId = NE.fromList $ NE.drop 1 cols
    dummyUpdateField = NE.head cols
    placeholders = Text.intercalate ", " $ map (\i -> "$" <> Text.pack (show i)) [1..length colsNoId]

    sql = TextEnc.encodeUtf8 $ Text.concat
      [ "INSERT INTO " <> table
      , " (" <> Text.intercalate ", " (NE.toList cols) <> ")"
      , " VALUES (" <> placeholders <> ")"
      , " ON CONFLICT (" <> Text.intercalate ", " uniqueCols <> ")"
      , " DO UPDATE SET " <> dummyUpdateField <> " = EXCLUDED." <> dummyUpdateField
      , returnClause
      ]

-- | Inserts multiple records into a table in a single transaction using UNNEST and discards the generated IDs.
bulkInsertNoReturn
  :: forall a b. (DbInfo a)
  => ([a] -> b)                 -- Field extractor (e.g., to tuple)
  -> HsqlE.Params b             -- Bulk encoder
  -> [a]                        -- Records
  -> HsqlT.Transaction ()
bulkInsertNoReturn extract enc = bulkInsert extract enc NoResultBulk

-- | Inserts multiple records into a table in a single transaction using UNNEST and returns the generated IDs.
bulkInsertReturnIds
  :: forall a b c. (DbInfo a)
  => ([a] -> b)                 -- Field extractor (e.g., to tuple)
  -> HsqlE.Params b             -- Bulk Encoder
  -> HsqlD.Result [c]           -- Bulk decoder
  -> [a]                        -- Records
  -> HsqlT.Transaction [c]
bulkInsertReturnIds extract enc dec = bulkInsert extract enc (WithResultBulk dec)

-- insertManyUnique
--   :: forall a b. (DbInfo a)
--   => ([a] -> b)                -- Field extractor (e.g., to tuple)
--   -> HsqlE.Params b            -- Bulk Encoder
--   -> [a]                       -- Records
--   -> HsqlT.Transaction ()
-- insertManyUnique extract enc = bulkInsert extract enc NoResultBulk

-- | Inserts multiple records into a table in a single transaction using UNNEST.
--
-- This function performs a bulk insert into a specified table, using PostgreSQL’s
-- `UNNEST` to expand arrays of field values into rows. It’s designed for efficiency,
-- executing all inserts in one SQL statement, and can return the generated IDs.
-- This will automatically handle unique constraints, if they are present.
bulkInsert
  :: forall a b c r. (DbInfo a)
  => ([a] -> b)                 -- Field extractor (e.g., to tuple)
  -> HsqlE.Params b             -- Encoder
  -> ResultTypeBulk c r         -- Whether to return a result and decoder
  -> [a]                        -- Records
  -> HsqlT.Transaction r
bulkInsert extract enc returnIds xs =
    case validateUniqueConstraints (Proxy @a) of
    Left err -> error err
    Right uniques ->
      HsqlT.statement params $ HsqlS.Statement sql enc decoder True
      where
        params = extract xs
        table = tableName (Proxy @a)
        cols = NE.toList $ columnNames (Proxy @a)
        colsNoId = drop 1 cols

        unnestVals = Text.intercalate ", " $ map (\i -> "$" <> Text.pack (show i)) [1..length colsNoId]

        conflictClause :: [Text.Text] -> Text.Text
        conflictClause [] = ""
        conflictClause uniqueConstraints = " ON CONFLICT (" <> Text.intercalate ", " uniqueConstraints <> ") DO NOTHING"

        (decoder, shouldReturnId) = case returnIds of
          NoResultBulk -> (HsqlD.noResult, "")
          WithResultBulk dec  -> (dec, "RETURNING id")

        sql = TextEnc.encodeUtf8 $ Text.concat
          ["INSERT INTO " <> table
          , " (" <> Text.intercalate ", " colsNoId <> ") "
          , " SELECT * FROM UNNEST ("
          , unnestVals <> " ) "
          , conflictClause uniques
          , shouldReturnId
          ]

-- | Validates that the unique constraints are valid columns in the table.
-- If there are no unique constraints, this function will return successfully with [].
validateUniqueConstraints :: (DbInfo a) => Proxy a -> Either String [Text.Text]
validateUniqueConstraints p =
  let colNames = NE.toList $ columnNames p
      constraints = uniqueFields p
      invalidConstraints = filter (`notElem` colNames) constraints
  in if null invalidConstraints
      then Right constraints
      else Left $ "Invalid unique constraint columns: " ++ show invalidConstraints
