{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.Function.InsertBulk (
  -- * Core Functions
  insertBulkWith,
  ConflictStrategy (..),

  -- * Convenience Functions
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

import Cardano.Db.Statement.Function.Core (ResultTypeBulk (..))
import Cardano.Db.Statement.Types (DbInfo (..))
import Cardano.Prelude (Proxy (..))
import Data.Functor.Contravariant (contramap)

-- | Conflict handling strategies for bulk operations
data ConflictStrategy
  = NoConflict -- No conflict handling (fastest)
  | IgnoreWithColumns [Text.Text] -- ON CONFLICT (columns) DO NOTHING
  | IgnoreWithConstraint Text.Text -- ON CONFLICT ON CONSTRAINT name DO NOTHING
  | ReplaceWithColumns [Text.Text] -- ON CONFLICT (columns) DO UPDATE SET
  | ReplaceWithConstraint Text.Text -- ON CONFLICT ON CONSTRAINT name DO UPDATE SET

-- | Core bulk insert function with configurable conflict handling using UNNEST.
--
-- This is the foundation function that all other bulk insert operations use.
-- Uses PostgreSQL's `UNNEST` to expand arrays into rows for efficient bulk insertion.
-- Supports various conflict strategies including ignore and replace operations.
--
-- ==== Parameters
-- * @conflictStrategy@: How to handle unique constraint violations.
-- * @removeJsonb@: Whether JSONB casting is present in current schema.
-- * @extract@: Function to extract fields from a list of records.
-- * @encoder@: Encoder for the extracted fields.
-- * @returnIds@: Result type indicating whether to return generated IDs.
-- * @statement@: The prepared statement that can be executed.
insertBulkWith ::
  forall a b r.
  DbInfo a =>
  ConflictStrategy -> -- How to handle conflicts
  Bool -> -- Whether jsonb casting is present in current schema
  ([a] -> b) -> -- field extractor
  HsqlE.Params b -> -- encoder
  ResultTypeBulk r -> -- result type
  HsqlS.Statement [a] r -- returns a statement
insertBulkWith conflictStrategy removeJsonb extract enc returnIds =
  case validateGeneratedFields (Proxy @a) of
    Left err -> error err
    Right () -> HsqlS.Statement sql (contramap extract enc) decoder True
      where
        table = tableName (Proxy @a)
        allColNames = NE.toList $ columnNames (Proxy @a)
        genFields = generatedFields (Proxy @a)
        colNames = filter (`notElem` genFields) allColNames
        jsonFields = jsonbFields (Proxy @a)
        enumFields' = enumFields (Proxy @a)
        paramTypes = unnestParamTypes (Proxy @a)

        unnestParams = Text.intercalate ", " $ zipWith mkParam [1 ..] colNames
          where
            mkParam i col = "$" <> Text.pack (show (i :: Int)) <> getArrayType col
            getArrayType col = case lookup col paramTypes of
              Just pgType -> "::" <> pgType
              Nothing -> ""

        selectColumns =
          Text.intercalate ", " $
            map mkSelectColumn colNames
          where
            mkSelectColumn col = case lookup col enumFields' of
              Just enumType -> col <> "::" <> enumType
              Nothing ->
                if removeJsonb || col `notElem` jsonFields
                  then col
                  else col <> "::jsonb"

        conflictClause = case conflictStrategy of
          NoConflict -> ""
          IgnoreWithColumns cols ->
            " ON CONFLICT (" <> Text.intercalate ", " cols <> ") DO NOTHING"
          IgnoreWithConstraint name ->
            " ON CONFLICT ON CONSTRAINT " <> name <> " DO NOTHING"
          ReplaceWithColumns cols ->
            let updateFields =
                  Text.intercalate ", " $
                    map (\col -> col <> " = EXCLUDED." <> col) colNames
             in " ON CONFLICT (" <> Text.intercalate ", " cols <> ") DO UPDATE SET " <> updateFields
          ReplaceWithConstraint name ->
            let updateFields =
                  Text.intercalate ", " $
                    map (\col -> col <> " = EXCLUDED." <> col) colNames
             in " ON CONFLICT ON CONSTRAINT " <> name <> " DO UPDATE SET " <> updateFields

        (decoder, shouldReturnId) = case returnIds of
          NoResultBulk -> (HsqlD.noResult, "")
          WithResultBulk dec -> (dec, " RETURNING id")

        sql =
          TextEnc.encodeUtf8 $
            Text.concat
              [ "INSERT INTO " <> table
              , " (" <> Text.intercalate ", " colNames <> ") "
              , " SELECT " <> selectColumns <> " FROM UNNEST ("
              , unnestParams <> ") AS t(" <> Text.intercalate ", " colNames <> ") "
              , conflictClause
              , shouldReturnId
              ]

-----------------------------------------------------------------------------------------------------------------------------------
-- CONVENIENCE FUNCTIONS
-----------------------------------------------------------------------------------------------------------------------------------

-- | Simple bulk insert without conflict handling - fastest option.
--
-- Performs bulk insertion using PostgreSQL's `UNNEST` function without any
-- conflict resolution. This is the fastest bulk insert option when you're
-- certain no unique constraint violations will occur.
--
-- ==== Parameters
-- * @extract@: Function to extract fields from a list of records.
-- * @encoder@: Encoder for the extracted fields.
-- * @returnIds@: Result type indicating whether to return generated IDs.
-- * @statement@: The prepared statement that can be executed.
insertBulk ::
  forall a b r.
  DbInfo a =>
  ([a] -> b) ->
  HsqlE.Params b ->
  ResultTypeBulk r ->
  HsqlS.Statement [a] r
insertBulk = insertBulkWith NoConflict False

-- | Bulk insert with JSONB type support using UNNEST.
--
-- Similar to `insertBulk` but provides control over JSONB field casting.
-- Use this when your table contains JSONB columns and you need to handle
-- schema variations across different database versions.
--
-- ==== Parameters
-- * @removeJsonb@: Whether to skip JSONB casting (for older schemas).
-- * @extract@: Function to extract fields from a list of records.
-- * @encoder@: Encoder for the extracted fields.
-- * @returnIds@: Result type indicating whether to return generated IDs.
-- * @statement@: The prepared statement that can be executed.
insertBulkJsonb ::
  forall a b r.
  DbInfo a =>
  Bool -> -- removeJsonb flag
  ([a] -> b) ->
  HsqlE.Params b ->
  ResultTypeBulk r ->
  HsqlS.Statement [a] r
insertBulkJsonb = insertBulkWith NoConflict
