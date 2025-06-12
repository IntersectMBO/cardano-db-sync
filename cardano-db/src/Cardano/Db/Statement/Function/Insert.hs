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
  -- insertBulk,
  -- insertBulkJsonb,
  -- insertBulkIgnore,
  -- insertBulkReplace,
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

-----------------------------------------------------------------------------------------------------------------------------------

-- -- | Inserts multiple records into a table in a single transaction using UNNEST.
-- --
-- -- This function performs a bulk insert into a specified table, using PostgreSQL’s
-- -- `UNNEST` to expand arrays of field values into rows. It’s designed for efficiency,
-- -- executing all inserts in one SQL statement, and can return the generated IDs.
-- -- This will automatically handle unique constraints, if they are present.
-- --
-- -- ==== Parameters
-- -- * @extract@: Function to extract fields from a list of records.
-- -- * @encoder@: Encoder for the extracted fields.
-- -- * @returnIds@: Result type indicating whether to return IDs or not.
-- -- * @statement@: The prepared statement that can be executed.
-- insertBulk ::
--   forall a b r.
--   (DbInfo a) =>
--   ([a] -> b) -> -- field extractor
--   HsqlE.Params b -> -- encoder
--   ResultTypeBulk r -> -- result type
--   HsqlS.Statement [a] r -- returns a statement
-- insertBulk = mkInsertBulk False

-- -- | Same as `insertBulk` but having access to the global dbEnvRemoveJsonb.
-- --
-- -- ==== Parameters
-- -- * @extract@: Function to extract fields from a list of records.
-- -- * @encoder@: Encoder for the extracted fields.
-- -- * @returnIds@: Result type indicating whether to return IDs or not.
-- -- * @statement@: The prepared statement that can be executed, wrapped in DbAction due to needing access to the `dbEnvRemoveJsonb` environment.
-- insertBulkJsonb ::
--   forall a b r.
--   DbInfo a =>
--   Bool -> -- Whether jsonb casting is present in current schema
--   ([a] -> b) -> -- field extractor
--   HsqlE.Params b -> -- encoder
--   ResultTypeBulk r -> -- result type
--   HsqlS.Statement [a] r -- returns a statement
-- insertBulkJsonb = mkInsertBulk

-- mkInsertBulk ::
--   forall a b r.
--   (DbInfo a) =>
--   Bool -> -- Whether jsonb casting is present in current schema
--   ([a] -> b) -> -- Field extractor
--   HsqlE.Params b -> -- Encoder
--   ResultTypeBulk r -> -- Result type
--   HsqlS.Statement [a] r -- Returns a Statement
-- mkInsertBulk removeJsonb extract enc returnIds = do
--   case validateUniqueConstraints (Proxy @a) of
--     Left err -> error err
--     Right uniques ->
--       case validateGeneratedFields (Proxy @a) of
--         Left err -> error err
--         Right () -> HsqlS.Statement sql (contramap extract enc) decoder True
--           where
--             table = tableName (Proxy @a)
--             allColNames = NE.toList $ columnNames (Proxy @a)
--             genFields = generatedFields (Proxy @a)
--             colNames = filter (`notElem` genFields) allColNames
--             jsonFields = jsonbFields (Proxy @a)
--             enumFields' = enumFields (Proxy @a)
--             paramTypes = unnestParamTypes (Proxy @a)

--             -- Simple parameter list without casting
--             unnestParams = Text.intercalate ", " $ zipWith (\i col -> "$" <> Text.pack (show (i :: Int)) <> getArrayType col) [1..] colNames
--               where
--                 getArrayType col = case lookup col paramTypes of
--                   Just pgType -> "::" <> pgType
--                   Nothing -> ""

--             -- Build column list with both jsonb and enum casting after UNNEST
--             selectColumns =
--               Text.intercalate ", " $
--                 map
--                   ( \col ->
--                       case lookup col enumFields' of
--                         Just enumType -> col <> "::" <> enumType -- Cast to enum first
--                         Nothing ->
--                           if removeJsonb || col `notElem` jsonFields
--                             then col
--                             else col <> "::jsonb"
--                   )
--                   colNames -- Update this section
--             conflictClause :: [Text.Text] -> Text.Text
--             conflictClause [] = ""
--             conflictClause uniqueConstraints = " ON CONFLICT (" <> Text.intercalate ", " uniqueConstraints <> ") DO NOTHING"

--             (decoder, shouldReturnId) = case returnIds of
--               NoResultBulk -> (HsqlD.noResult, "")
--               WithResultBulk dec -> (dec, "RETURNING id")

--             sql =
--               TextEnc.encodeUtf8 $
--                 Text.concat
--                   [ "INSERT INTO " <> table
--                   , " (" <> Text.intercalate ", " colNames <> ") "
--                   , " SELECT " <> selectColumns <> " FROM UNNEST ("
--                   , unnestParams <> ") AS t(" <> Text.intercalate ", " colNames <> ") "
--                   , conflictClause uniques
--                   , shouldReturnId
--                   ]

-- -----------------------------------------------------------------------------------------------------------------------------------

-- -- | Inserts multiple records, ignoring conflicts on unique constraints.
-- -- This is equivalent to the old `insertManyWithManualUnique` with DO NOTHING.
-- insertBulkIgnore ::
--  forall a b r.
--  (DbInfo a) =>
--  ([a] -> b) -> -- field extractor
--  HsqlE.Params b -> -- encoder
--  ResultTypeBulk r -> -- result type
--  HsqlS.Statement [a] r -- returns a statement
-- insertBulkIgnore extract enc returnIds =
--  case validateUniqueConstraints (Proxy @a) of
--    Left err -> error err
--    Right autoConstraints ->
--      let bulkConstraints = bulkUniqueFields (Proxy @a)
--          allConstraints = if null autoConstraints then bulkConstraints else autoConstraints
--      in if null allConstraints
--         then mkInsertBulk False extract enc returnIds -- No constraints, use regular insert
--         else case validateGeneratedFields (Proxy @a) of
--           Left err -> error err
--           Right () -> HsqlS.Statement sql (contramap extract enc) decoder True
--             where
--               table = tableName (Proxy @a)
--               allColNames = NE.toList $ columnNames (Proxy @a)
--               genFields = generatedFields (Proxy @a)
--               colNames = filter (`notElem` genFields) allColNames
--               jsonFields = jsonbFields (Proxy @a)
--               enumFields' = enumFields (Proxy @a)
--               paramTypes = unnestParamTypes (Proxy @a)

--               -- Validate that bulk constraints exist in column names
--               invalidConstraints = filter (`notElem` allColNames) allConstraints
--               validatedConstraints = if null invalidConstraints
--                 then allConstraints
--                 else error $ "Invalid bulk constraint columns: " <> show invalidConstraints

--               unnestParams = Text.intercalate ", " $ zipWith (\i col -> "$" <> Text.pack (show (i :: Int)) <> getArrayType col) [1..] colNames
--                 where
--                   getArrayType col = case lookup col paramTypes of
--                     Just pgType -> "::" <> pgType
--                     Nothing -> ""

--               selectColumns =
--                 Text.intercalate ", " $
--                   map
--                     ( \col ->
--                         case lookup col enumFields' of
--                           Just enumType -> col <> "::" <> enumType
--                           Nothing ->
--                             if col `notElem` jsonFields
--                               then col
--                               else col <> "::jsonb"
--                     )
--                     colNames

--               conflictClause = " ON CONFLICT (" <> Text.intercalate ", " validatedConstraints <> ") DO NOTHING"

--               (decoder, shouldReturnId) = case returnIds of
--                 NoResultBulk -> (HsqlD.noResult, "")
--                 WithResultBulk dec -> (dec, " RETURNING id")

--               sql =
--                 TextEnc.encodeUtf8 $
--                   Text.concat
--                     [ "INSERT INTO " <> table
--                     , " (" <> Text.intercalate ", " colNames <> ") "
--                     , " SELECT " <> selectColumns <> " FROM UNNEST ("
--                     , unnestParams <> ") AS t(" <> Text.intercalate ", " colNames <> ") "
--                     , conflictClause
--                     , shouldReturnId
--                     ]

-- -----------------------------------------------------------------------------------------------------------------------------------

-- -- | Inserts multiple records into a table or replaces all fields on unique constraint conflict.
-- -- This is equivalent to bulk `insertReplace` functionality.
-- insertBulkReplace ::
--  forall a b r.
--  (DbInfo a) =>
--  ([a] -> b) -> -- field extractor
--  HsqlE.Params b -> -- encoder
--  ResultTypeBulk r -> -- result type
--  HsqlS.Statement [a] r -- returns a statement
-- insertBulkReplace extract enc returnIds =
--  case validateUniqueConstraints (Proxy @a) of
--    Left err -> error err
--    Right autoConstraints ->
--      let bulkConstraints = bulkUniqueFields (Proxy @a)
--          allConstraints = if null autoConstraints then bulkConstraints else autoConstraints
--      in if null allConstraints
--         then error $ "insertBulkReplace: No unique constraints defined for " <> show (typeRep (Proxy @a))
--         else case validateGeneratedFields (Proxy @a) of
--           Left err -> error err
--           Right () -> HsqlS.Statement sql (contramap extract enc) decoder True
--             where
--               table = tableName (Proxy @a)
--               allColNames = NE.toList $ columnNames (Proxy @a)
--               genFields = generatedFields (Proxy @a)
--               colNames = filter (`notElem` genFields) allColNames
--               jsonFields = jsonbFields (Proxy @a)
--               enumFields' = enumFields (Proxy @a)
--               paramTypes = unnestParamTypes (Proxy @a)

--               -- Validate that bulk constraints exist in column names
--               invalidConstraints = filter (`notElem` allColNames) allConstraints
--               validatedConstraints = if null invalidConstraints
--                 then allConstraints
--                 else error $ "Invalid bulk constraint columns: " <> show invalidConstraints

--               unnestParams = Text.intercalate ", " $ zipWith (\i col -> "$" <> Text.pack (show (i :: Int)) <> getArrayType col) [1..] colNames
--                 where
--                   getArrayType col = case lookup col paramTypes of
--                     Just pgType -> "::" <> pgType
--                     Nothing -> ""

--               selectColumns =
--                 Text.intercalate ", " $
--                   map
--                     ( \col ->
--                         case lookup col enumFields' of
--                           Just enumType -> col <> "::" <> enumType
--                           Nothing ->
--                             if col `notElem` jsonFields
--                               then col
--                               else col <> "::jsonb"
--                     )
--                     colNames

--               -- Create SET clause for all non-generated columns
--               updateAllFields = Text.intercalate ", " $
--                 map (\col -> col <> " = EXCLUDED." <> col) colNames

--               conflictClause = " ON CONFLICT (" <> Text.intercalate ", " validatedConstraints <> ") DO UPDATE SET " <> updateAllFields

--               (decoder, shouldReturnId) = case returnIds of
--                 NoResultBulk -> (HsqlD.noResult, "")
--                 WithResultBulk dec -> (dec, " RETURNING id")

--               sql =
--                 TextEnc.encodeUtf8 $
--                   Text.concat
--                     [ "INSERT INTO " <> table
--                     , " (" <> Text.intercalate ", " colNames <> ") "
--                     , " SELECT " <> selectColumns <> " FROM UNNEST ("
--                     , unnestParams <> ") AS t(" <> Text.intercalate ", " colNames <> ") "
--                     , conflictClause
--                     , shouldReturnId
--                     ]
-----------------------------------------------------------------------------------------------------------------------------------

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
