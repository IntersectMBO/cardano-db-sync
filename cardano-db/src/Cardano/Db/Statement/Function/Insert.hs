{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.Function.Insert (
  insert,
  insertCheckUnique,
  insertIfUnique,
  insertBulk,
)
where

import qualified Data.Text as Text
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Statement as HsqlS

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Encoding as TextEnc

import Cardano.Db.Statement.Function.Core (ResultType (..), ResultTypeBulk (..))
import Cardano.Db.Statement.Types (DbInfo (..), Entity)
import Cardano.Prelude (Proxy (..))
import Data.Functor.Contravariant (contramap)

-- | Inserts a record into a table, with option of returning the generated ID.
--
-- ==== Parameters
-- * @encoder@: The encoder for the record.
-- * @resultType@: Whether to return a result (usually it's newly generated id) and decoder.
-- * @record@: The record to insert.
insert ::
  forall a c r.
  (DbInfo a) =>
  HsqlE.Params a -> -- Encoder for record (without ID)
  ResultType (Entity c) r -> -- Whether to return Entity and decoder
  HsqlS.Statement a r -- Returns the prepared statement
insert encoder resultType =
  HsqlS.Statement sql encoder decoder True
  where
    (decoder, returnClause) = case resultType of
      NoResult -> (HsqlD.noResult, "")
      WithResult dec -> (dec, "RETURNING id")

    table = tableName (Proxy @a)
    colNames = columnNames (Proxy @a)
    columns = Text.intercalate ", " (NE.toList colNames)

    values = Text.intercalate ", " $ map (\i -> "$" <> Text.pack (show i)) [1 .. length colNames]

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "INSERT INTO " <> table
          , " (" <> columns <> ")"
          , " VALUES (" <> values <> ")"
          , returnClause
          ]

-- | Inserts a record into a table, checking for a unique constraint violation.
--
-- If the `DbInfoConstraints` instance does not match any table type records, this function will throw an error.
--
-- ==== Parameters
-- * @encoder@: The encoder for the record.
-- * @resultType@: Whether to return a result (usually it's newly generated id) and decoder.
-- * @record@: The record to insert.
insertCheckUnique ::
  forall a c r.
  (DbInfo a) =>
  HsqlE.Params a -> -- Encoder
  ResultType (Entity c) r -> -- Whether to return a result and decoder
  HsqlS.Statement a r -- Returns the prepared statement
insertCheckUnique encoder resultType =
  case validateUniqueConstraints (Proxy @a) of
    Left err -> error err
    Right _ -> HsqlS.Statement sql encoder decoder True
  where
    (decoder, returnClause) = case resultType of
      NoResult -> (HsqlD.noResult, "")
      WithResult dec -> (dec, "RETURNING id")

    table = tableName (Proxy @a)
    colNames = columnNames (Proxy @a)
    uniqueCols = uniqueFields (Proxy @a)

    -- Drop the ID column for value placeholders
    dummyUpdateField = NE.head colNames
    placeholders = Text.intercalate ", " $ map (\i -> "$" <> Text.pack (show i)) [1 .. length colNames]

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "INSERT INTO " <> table
          , " (" <> Text.intercalate ", " (NE.toList colNames) <> ")"
          , " VALUES (" <> placeholders <> ")"
          , " ON CONFLICT (" <> Text.intercalate ", " uniqueCols <> ")"
          , " DO UPDATE SET " <> dummyUpdateField <> " = EXCLUDED." <> dummyUpdateField
          , returnClause
          ]

-- | Inserts a record into a table, only if it doesn't violate a unique constraint.
-- Returns Nothing if the record already exists (based on unique constraints).
insertIfUnique ::
  forall a c.
  (DbInfo a) =>
  HsqlE.Params a -> -- Encoder
  HsqlD.Row (Entity c) -> -- Row decoder
  HsqlS.Statement a (Maybe (Entity c)) -- Statement that returns Maybe Entity
insertIfUnique encoder entityDecoder =
  case validateUniqueConstraints (Proxy @a) of
    Left err -> error err
    Right _ -> HsqlS.Statement sql encoder decoder True
  where
    decoder = HsqlD.rowMaybe entityDecoder

    table = tableName (Proxy @a)
    colNames = columnNames (Proxy @a)
    uniqueCols = uniqueFields (Proxy @a)

    placeholders = Text.intercalate ", " $ map (\i -> "$" <> Text.pack (show i)) [1 .. length (NE.toList colNames)]

    -- This SQL will try to insert, but on conflict will do nothing
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "WITH ins AS ("
          , "  INSERT INTO " <> table
          , "  (" <> Text.intercalate ", " (NE.toList colNames) <> ")"
          , "  VALUES (" <> placeholders <> ")"
          , "  ON CONFLICT (" <> Text.intercalate ", " uniqueCols <> ") DO NOTHING"
          , "  RETURNING *"
          , ")"
          , "SELECT * FROM ins"
          ]

-- | Inserts multiple records into a table in a single transaction using UNNEST.
--
-- This function performs a bulk insert into a specified table, using PostgreSQL’s
-- `UNNEST` to expand arrays of field values into rows. It’s designed for efficiency,
-- executing all inserts in one SQL statement, and can return the generated IDs.
-- This will automatically handle unique constraints, if they are present.
insertBulk ::
  forall a b r.
  (DbInfo a) =>
  ([a] -> b) -> -- Field extractor
  HsqlE.Params b -> -- Encoder
  ResultTypeBulk r -> -- Result type
  HsqlS.Statement [a] r -- Returns a Statement
insertBulk extract enc returnIds =
  case validateUniqueConstraints (Proxy @a) of
    Left err -> error err
    Right uniques ->
      HsqlS.Statement sql (contramap extract enc) decoder True
      where
        table = tableName (Proxy @a)
        colNames = NE.toList $ columnNames (Proxy @a)

        unnestVals = Text.intercalate ", " $ map (\i -> "$" <> Text.pack (show i)) [1 .. length colNames]

        conflictClause :: [Text.Text] -> Text.Text
        conflictClause [] = ""
        conflictClause uniqueConstraints = " ON CONFLICT (" <> Text.intercalate ", " uniqueConstraints <> ") DO NOTHING"

        (decoder, shouldReturnId) = case returnIds of
          NoResultBulk -> (HsqlD.noResult, "")
          WithResultBulk dec -> (dec, "RETURNING id")

        sql =
          TextEnc.encodeUtf8 $
            Text.concat
              [ "INSERT INTO " <> table
              , " (" <> Text.intercalate ", " colNames <> ") "
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
