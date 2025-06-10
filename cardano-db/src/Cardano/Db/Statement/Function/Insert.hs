{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.Function.Insert (
  insert,
  insertJsonb,
  insertCheckUnique,
  insertCheckUniqueJsonb,
  insertIfUnique,
  insertIfUniqueJsonb,
  insertBulk,
  insertBulkJsonb,
)
where

import qualified Data.Text as Text
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Statement as HsqlS

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Encoding as TextEnc

import Cardano.Db.Statement.Function.Core (ResultType (..), ResultTypeBulk (..))
import Cardano.Db.Statement.Types (DbInfo (..))
import Cardano.Prelude (Proxy (..), typeRep)
import Data.Functor.Contravariant (contramap)

-- | Inserts a record into a table, with option of returning the generated ID.
--
-- ==== Parameters
-- * @encoder@: The encoder for the record.
-- * @resultType@: Whether to return a result (usually it's newly generated id) and decoder.
-- * @statement@: The prepared statement that can be executed.
insert ::
  forall a r.
  DbInfo a =>
  HsqlE.Params a -> -- Encoder for record (without ID)
  ResultType r r -> -- Whether to return result and decoder
  HsqlS.Statement a r -- Returns the prepared statement
insert = mkInsert False

-- | Same as `insert` but having access to the global dbEnvRemoveJsonb.
--
-- ==== Parameters
-- * @encoder@: The encoder for the record.
-- * @resultType@: Whether to return a result (usually it's newly generated id) and decoder.
-- * @statement@: The prepared statement that can be executed, wrapped in DbAction due to needing access to the `dbEnvRemoveJsonb` environment.
insertJsonb ::
  forall a r.
  DbInfo a =>
  Bool -> -- Whether jsonb casting is present in current schema
  HsqlE.Params a -> -- Encoder for record (without ID)
  ResultType r r -> -- Whether to return result and decoder
  HsqlS.Statement a r -- Returns the prepared statement
insertJsonb removeJsonb encoder resultType = do
  mkInsert removeJsonb encoder resultType

-- | Helper function to create an insert statement.
mkInsert ::
  forall a r.
  DbInfo a =>
  Bool -> -- Whether jsonb casting is present in current schema
  HsqlE.Params a -> -- Encoder for record (without ID)
  ResultType r r -> -- Whether to return result and decoder
  HsqlS.Statement a r -- Returns the prepared statement
mkInsert removeJsonb encoder resultType =
  HsqlS.Statement sql encoder decoder True
  where
    (decoder, returnClause) = case resultType of
      NoResult -> (HsqlD.noResult, "")
      WithResult dec -> (dec, " RETURNING id")

    table = tableName (Proxy @a)
    colNames = columnNames (Proxy @a)
    columns = Text.intercalate ", " (NE.toList colNames)
    castParams = buildCastParameters removeJsonb (Proxy @a)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "INSERT INTO " <> table
          , " (" <> columns <> ")"
          , " VALUES (" <> castParams <> ")"
          , returnClause
          ]

-----------------------------------------------------------------------------------------------------------------------------------

-- | Inserts a record into a table, checking for a unique constraint violation.
--
-- If the `DbInfoConstraints` instance does not match any table type records, this function will throw an error.
--
-- ==== Parameters
-- * @encoder@: The encoder for the record.
-- * @resultType@: Whether to return a result (usually it's newly generated id) and decoder.
-- * @statement@: The prepared statement that can be executed.
insertCheckUnique ::
  forall a r.
  DbInfo a =>
  HsqlE.Params a -> -- Encoder for record (without ID)
  ResultType r r -> -- Whether to return a result and decoder
  HsqlS.Statement a r -- Returns the prepared statement
insertCheckUnique = mkInsertCheckUnique False

-- | Same as `insertCheckUnique` but having access to the global dbEnvRemoveJsonb.
--
-- ==== Parameters
-- * @encoder@: The encoder for the record.
-- * @resultType@: Whether to return a result (usually it's newly generated id) and decoder.
-- * @statement@: The prepared statement that can be executed, wrapped in DbAction due to needing access to the `dbEnvRemoveJsonb` environment.
insertCheckUniqueJsonb ::
  forall a r.
  DbInfo a =>
  Bool -> -- Whether jsonb casting is present in current schema
  HsqlE.Params a -> -- Encoder for record (without ID)
  ResultType r r -> -- Whether to return result and decoder
  HsqlS.Statement a r -- Returns the prepared statement
insertCheckUniqueJsonb removeJsonb encoder resultType = do
  mkInsertCheckUnique removeJsonb encoder resultType

-- | Helper function to create an insert statement that checks for unique constraints.
mkInsertCheckUnique ::
  forall a r.
  (DbInfo a) =>
  Bool -> -- Whether jsonb casting is present in current schema
  HsqlE.Params a -> -- Encoder
  ResultType r r -> -- Whether to return a result and decoder
  HsqlS.Statement a r -- Returns the prepared statement
mkInsertCheckUnique removeJsonb encoder resultType =
  case validateUniqueConstraints (Proxy @a) of
    Left err -> error err
    Right [] -> error $ "insertCheckUnique: No unique constraints defined for " <> show (typeRep (Proxy @a))
    Right uniqueCols@(dummyUpdateField : _) -> HsqlS.Statement sql encoder decoder True
      where
        (decoder, returnClause) = case resultType of
          NoResult -> (HsqlD.noResult, "")
          WithResult dec -> (dec, " RETURNING id")

        table = tableName (Proxy @a)
        colNames = columnNames (Proxy @a)
        castParams = buildCastParameters removeJsonb (Proxy @a)

        sql =
          TextEnc.encodeUtf8 $
            Text.concat
              [ "INSERT INTO " <> table
              , " (" <> Text.intercalate ", " (NE.toList colNames) <> ")"
              , " VALUES (" <> castParams <> ")"
              , " ON CONFLICT (" <> Text.intercalate ", " uniqueCols <> ")"
              , " DO UPDATE SET " <> dummyUpdateField <> " = EXCLUDED." <> dummyUpdateField
              , returnClause
              ]

-----------------------------------------------------------------------------------------------------------------------------------

-- | Inserts a record into a table, only if it doesn't violate a unique constraint.
--
-- Returns Nothing if the record already exists (based on unique constraints).
-- === Parameters
-- * @encoder@: The encoder for the record (without ID).
-- * @decoder@: The row decoder for the result.
-- * @statement@: The prepared statement that can be executed, returning Maybe Entity.
insertIfUnique ::
  forall a c.
  (DbInfo a) =>
  HsqlE.Params a -> -- Encoder for record (without ID)
  HsqlD.Row c -> -- Row decoder
  HsqlS.Statement a (Maybe c) -- Statement that returns Maybe Entity
insertIfUnique = mkInsertIfUnique False

-- | Same as `insertCheckUniqueIfUnique` but having access to the global dbEnvRemoveJsonb.
--
-- ==== Parameters
-- * @encoder@: The encoder for the record (without ID).
-- * @decoder@: The row decoder for the result.
-- * @statement@: The prepared statement that can be executed, wrapped in DbAction due to needing access to the `dbEnvRemoveJsonb` environment.
insertIfUniqueJsonb ::
  forall a c.
  DbInfo a =>
  Bool -> -- Whether jsonb casting is present in current schema
  HsqlE.Params a -> -- Encoder for record (without ID)
  HsqlD.Row c -> -- Row decoder
  HsqlS.Statement a (Maybe c) -- Statement that returns Maybe Entity
insertIfUniqueJsonb removeJsonb = do
  mkInsertIfUnique removeJsonb

mkInsertIfUnique ::
  forall a c.
  (DbInfo a) =>
  Bool -> -- Whether jsonb casting is present in current schema
  HsqlE.Params a -> -- Encoder
  HsqlD.Row c -> -- Row decoder
  HsqlS.Statement a (Maybe c) -- Statement that returns Maybe Entity
mkInsertIfUnique removeJsonb encoder decoder =
  case validateUniqueConstraints (Proxy @a) of
    Left err -> error err
    Right _ -> HsqlS.Statement sql encoder (HsqlD.rowMaybe decoder) True
  where
    table = tableName (Proxy @a)
    colNames = NE.toList $ columnNames (Proxy @a)
    uniqueCols = uniqueFields (Proxy @a)
    castParams = buildCastParameters removeJsonb (Proxy @a)

    -- This SQL will try to insert, but on conflict will do nothing
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "WITH ins AS ("
          , "  INSERT INTO " <> table
          , "  (" <> Text.intercalate ", " colNames <> ")"
          , "  VALUES (" <> castParams <> ")"
          , "  ON CONFLICT (" <> Text.intercalate ", " uniqueCols <> ") DO NOTHING"
          , "  RETURNING *"
          , ")"
          , "SELECT * FROM ins"
          ]

-----------------------------------------------------------------------------------------------------------------------------------

-- | Inserts multiple records into a table in a single transaction using UNNEST.
--
-- This function performs a bulk insert into a specified table, using PostgreSQL’s
-- `UNNEST` to expand arrays of field values into rows. It’s designed for efficiency,
-- executing all inserts in one SQL statement, and can return the generated IDs.
-- This will automatically handle unique constraints, if they are present.
--
-- ==== Parameters
-- * @extract@: Function to extract fields from a list of records.
-- * @encoder@: Encoder for the extracted fields.
-- * @returnIds@: Result type indicating whether to return IDs or not.
-- * @statement@: The prepared statement that can be executed.
insertBulk ::
  forall a b r.
  (DbInfo a) =>
  ([a] -> b) -> -- field extractor
  HsqlE.Params b -> -- encoder
  ResultTypeBulk r -> -- result type
  HsqlS.Statement [a] r -- returns a statement
insertBulk = mkInsertBulk False

-- | Same as `insertBulk` but having access to the global dbEnvRemoveJsonb.
--
-- ==== Parameters
-- * @extract@: Function to extract fields from a list of records.
-- * @encoder@: Encoder for the extracted fields.
-- * @returnIds@: Result type indicating whether to return IDs or not.
-- * @statement@: The prepared statement that can be executed, wrapped in DbAction due to needing access to the `dbEnvRemoveJsonb` environment.
insertBulkJsonb ::
  forall a b r.
  DbInfo a =>
  Bool -> -- Whether jsonb casting is present in current schema
  ([a] -> b) -> -- field extractor
  HsqlE.Params b -> -- encoder
  ResultTypeBulk r -> -- result type
  HsqlS.Statement [a] r -- returns a statement
insertBulkJsonb removeJsonb extract enc returnIds = do
  mkInsertBulk removeJsonb extract enc returnIds

mkInsertBulk ::
  forall a b r.
  (DbInfo a) =>
  Bool -> -- Whether jsonb casting is present in current schema
  ([a] -> b) -> -- Field extractor
  HsqlE.Params b -> -- Encoder
  ResultTypeBulk r -> -- Result type
  HsqlS.Statement [a] r -- Returns a Statement
mkInsertBulk removeJsonb extract enc returnIds = do
  case validateUniqueConstraints (Proxy @a) of
    Left err -> error err
    Right uniques ->
      HsqlS.Statement sql (contramap extract enc) decoder True
      where
        table = tableName (Proxy @a)
        colNames = NE.toList $ columnNames (Proxy @a)
        jsonFields = jsonbFields (Proxy @a)
        enumFields' = enumFields (Proxy @a) -- Add this line

        -- Simple parameter list without casting
        unnestParams = Text.intercalate ", " $ map (\i -> "$" <> Text.pack (show (i :: Int))) [1 .. length colNames]

        -- Build column list with both jsonb and enum casting after UNNEST
        selectColumns =
          Text.intercalate ", " $
            map
              ( \col ->
                  case lookup col enumFields' of
                    Just enumType -> col <> "::" <> enumType -- Cast to enum first
                    Nothing ->
                      if removeJsonb || col `notElem` jsonFields
                        then col
                        else col <> "::jsonb"
              )
              colNames -- Update this section
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
              , " SELECT " <> selectColumns <> " FROM UNNEST ("
              , unnestParams <> ") AS t(" <> Text.intercalate ", " colNames <> ") "
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

-- | Add ::jsonb casting for jsonb fields when jsonb is present in the schema
-- | Add ::enum_type casting for enum fields
buildCastParameters :: forall a. (DbInfo a) => Bool -> Proxy a -> Text.Text
buildCastParameters removeJsonb proxy =
  let colNames = NE.toList $ columnNames proxy
      jsonFields = jsonbFields proxy
      enumFields' = enumFields proxy
   in Text.intercalate ", " $
        zipWith
          ( \i col ->
              let param = "$" <> Text.pack (show (i :: Int))
               in case lookup col enumFields' of
                    Just enumType -> param <> "::" <> enumType -- Cast to enum
                    Nothing ->
                      if removeJsonb || col `notElem` jsonFields
                        then param
                        else param <> "::jsonb"
          )
          [1 ..]
          colNames
