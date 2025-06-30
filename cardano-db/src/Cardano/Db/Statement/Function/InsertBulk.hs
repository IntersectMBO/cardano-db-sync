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
  insertBulkIgnore,
  insertBulkReplace,
  insertBulkMaybeIgnore,
  insertBulkMaybeIgnoreWithConstraint,
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
import Cardano.Prelude (Proxy (..), typeRep)
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

-- | Bulk insert with automatic conflict detection and ignore strategy.
--
-- Automatically detects unique constraints from the table definition and
-- generates appropriate `ON CONFLICT DO NOTHING` clauses. Falls back to
-- simple insert if no constraints are defined.
--
-- ==== Parameters
-- * @extract@: Function to extract fields from a list of records.
-- * @encoder@: Encoder for the extracted fields.
-- * @returnIds@: Result type indicating whether to return generated IDs.
-- * @statement@: The prepared statement that can be executed.
insertBulkIgnore ::
  forall a b r.
  DbInfo a =>
  ([a] -> b) ->
  HsqlE.Params b ->
  ResultTypeBulk r ->
  HsqlS.Statement [a] r
insertBulkIgnore extract enc returnIds =
  case getConflictStrategy (Proxy @a) of
    NoConflict -> insertBulkWith NoConflict False extract enc returnIds
    strategy -> insertBulkWith strategy False extract enc returnIds
  where
    getConflictStrategy :: Proxy a -> ConflictStrategy
    getConflictStrategy p =
      case validateUniqueConstraints p of
        Left _ -> NoConflict
        Right autoConstraints ->
          let bulkConstraints = bulkUniqueFields p
              allConstraints = if null autoConstraints then bulkConstraints else autoConstraints
           in if null allConstraints
                then NoConflict
                else IgnoreWithColumns allConstraints

-- | Bulk insert with automatic conflict detection and replace strategy.
--
-- Automatically detects unique constraints and generates `ON CONFLICT DO UPDATE`
-- clauses to replace existing records. Requires at least one unique constraint
-- to be defined in the table schema.
--
-- ==== Parameters
-- * @extract@: Function to extract fields from a list of records.
-- * @encoder@: Encoder for the extracted fields.
-- * @returnIds@: Result type indicating whether to return generated IDs.
-- * @statement@: The prepared statement that can be executed.
insertBulkReplace ::
  forall a b r.
  DbInfo a =>
  ([a] -> b) ->
  HsqlE.Params b ->
  ResultTypeBulk r ->
  HsqlS.Statement [a] r
insertBulkReplace extract enc returnIds =
  case getConflictStrategy (Proxy @a) of
    NoConflict -> error $ "insertBulkReplace: No unique constraints defined for " <> show (typeRep (Proxy @a))
    IgnoreWithColumns cols -> insertBulkWith (ReplaceWithColumns cols) False extract enc returnIds
    IgnoreWithConstraint name -> insertBulkWith (ReplaceWithConstraint name) False extract enc returnIds
    _ -> error "Invalid conflict strategy for replace"
  where
    getConflictStrategy :: Proxy a -> ConflictStrategy
    getConflictStrategy p =
      case validateUniqueConstraints p of
        Left _ -> NoConflict
        Right autoConstraints ->
          let bulkConstraints = bulkUniqueFields p
              allConstraints = if null autoConstraints then bulkConstraints else autoConstraints
           in if null allConstraints
                then NoConflict
                else IgnoreWithColumns allConstraints

-----------------------------------------------------------------------------------------------------------------------------------
-- PERFORMANCE-OPTIMIZED FUNCTIONS FOR ManualDbConstraints PATTERN
-----------------------------------------------------------------------------------------------------------------------------------

-- | High-performance bulk insert with conditional conflict handling.
--
-- Optimized for the ManualDbConstraints pattern where constraint existence
-- is determined at runtime. Uses fastest simple insert when constraints don't
-- exist, switches to conflict handling only when needed.
--
-- ==== Parameters
-- * @constraintExists@: Runtime flag indicating if constraints are present.
-- * @extract@: Function to extract fields from a list of records.
-- * @encoder@: Encoder for the extracted fields.
-- * @returnIds@: Result type indicating whether to return generated IDs.
-- * @statement@: The prepared statement that can be executed.
insertBulkMaybeIgnore ::
  forall a b r.
  DbInfo a =>
  Bool -> -- Whether constraint exists (from ManualDbConstraints)
  ([a] -> b) ->
  HsqlE.Params b ->
  ResultTypeBulk r ->
  HsqlS.Statement [a] r
insertBulkMaybeIgnore constraintExists extract enc returnIds =
  if constraintExists
    then insertBulkWith conflictStrategy False extract enc returnIds
    else insertBulk extract enc returnIds -- Fastest when no constraint exists
  where
    conflictStrategy = case uniqueFields (Proxy @a) of
      [] -> IgnoreWithConstraint (autoConstraintName (Proxy @a)) -- For generated columns
      cols -> IgnoreWithColumns cols -- For normal columns

-- | Conditional bulk insert with custom constraint name specification.
--
-- Similar to `insertBulkMaybeIgnore` but allows specifying a custom constraint
-- name for special cases where the auto-derived constraint name doesn't match
-- the actual database constraint.
--
-- ==== Parameters
-- * @constraintExists@: Runtime flag indicating if constraints are present.
-- * @constraintName@: Custom name of the constraint to handle conflicts on.
-- * @extract@: Function to extract fields from a list of records.
-- * @encoder@: Encoder for the extracted fields.
-- * @returnIds@: Result type indicating whether to return generated IDs.
-- * @statement@: The prepared statement that can be executed.
insertBulkMaybeIgnoreWithConstraint ::
  forall a b r.
  DbInfo a =>
  Bool -> -- Whether constraint exists
  Text.Text -> -- Custom constraint name
  ([a] -> b) ->
  HsqlE.Params b ->
  ResultTypeBulk r ->
  HsqlS.Statement [a] r
insertBulkMaybeIgnoreWithConstraint constraintExists constraintName extract enc returnIds =
  if constraintExists
    then insertBulkWith (IgnoreWithConstraint constraintName) False extract enc returnIds
    else insertBulk extract enc returnIds

-----------------------------------------------------------------------------------------------------------------------------------
-- HELPER FUNCTIONS
-----------------------------------------------------------------------------------------------------------------------------------

-- | Auto-derives PostgreSQL constraint names following standard conventions.
--
-- Generates constraint names in the format "unique_{table_name}" which matches
-- PostgreSQL's default naming convention for unique constraints. Used internally
-- by bulk insert functions when constraint names need to be inferred.
--
-- ==== Parameters
-- * @proxy@: Type proxy for the table type.
-- * @constraintName@: Generated constraint name following PostgreSQL conventions.
autoConstraintName :: DbInfo a => Proxy a -> Text.Text
autoConstraintName p = "unique_" <> tableName p
