{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.Function.Insert (
  insert,
  insertJsonb,
  insertReplace,
  insertCheckUnique,
  insertCheckUniqueJsonb,
  insertIfUnique,
  insertIfUniqueJsonb,
)
where

import qualified Data.Text as Text
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Statement as HsqlS

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Encoding as TextEnc

import Cardano.Db.Statement.Function.Core (ResultType (..))
import Cardano.Db.Statement.Types (DbInfo (..))
import Cardano.Prelude (Proxy (..), typeRep)

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
insertJsonb = mkInsert

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

-- | Inserts a record into a table or replaces all fields on unique constraint conflict.
-- This is equivalent to Persistent's `insertReplace` function.
--
-- ==== Parameters
-- * @encoder@: The encoder for the record.
-- * @resultType@: Whether to return a result (usually it's newly generated id) and decoder.
-- * @statement@: The prepared statement that can be executed.
insertReplace ::
  forall a r.
  DbInfo a =>
  HsqlE.Params a -> -- Encoder for record (without ID)
  ResultType r r -> -- Whether to return result and decoder
  HsqlS.Statement a r -- Returns the prepared statement
insertReplace encoder resultType =
  case validateUniqueConstraints (Proxy @a) of
    Left err -> error err
    Right [] -> error $ "insertReplace: No unique constraints defined for " <> show (typeRep (Proxy @a))
    Right uniqueCols -> HsqlS.Statement sql encoder decoder True
      where
        (decoder, returnClause) = case resultType of
          NoResult -> (HsqlD.noResult, "")
          WithResult dec -> (dec, " RETURNING id")

        table = tableName (Proxy @a)
        allColNames = NE.toList $ columnNames (Proxy @a)
        genFields = generatedFields (Proxy @a)
        colNames = filter (`notElem` genFields) allColNames
        columns = Text.intercalate ", " colNames
        castParams = buildCastParameters False (Proxy @a) -- Always use False since removeJsonb not needed

        -- Create SET clause for all non-generated columns
        updateAllFields =
          Text.intercalate ", " $
            map (\col -> col <> " = EXCLUDED." <> col) colNames

        sql =
          TextEnc.encodeUtf8 $
            Text.concat
              [ "INSERT INTO " <> table
              , " (" <> columns <> ")"
              , " VALUES (" <> castParams <> ")"
              , " ON CONFLICT (" <> Text.intercalate ", " uniqueCols <> ")"
              , " DO UPDATE SET " <> updateAllFields
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
  DbInfo a =>
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
  DbInfo a =>
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
  DbInfo a =>
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
    allColNames = NE.toList $ columnNames (Proxy @a)
    genFields = generatedFields (Proxy @a)
    colNames = filter (`notElem` genFields) allColNames
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

-- | Add ::jsonb casting for jsonb fields when jsonb is present in the schema
-- | Add ::enum_type casting for enum fields
buildCastParameters :: forall a. DbInfo a => Bool -> Proxy a -> Text.Text
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
