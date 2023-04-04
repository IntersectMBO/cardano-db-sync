{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Cardano.Db.AlterTable (
  AlterTable (..),
  DbAlterTableException (..),
  alterTable,
) where

import Control.Exception.Lifted (Exception, handle, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.Text as Text
import Database.Persist.EntityDef.Internal (entityDB)
import Database.Persist.Postgresql (ConstraintNameDB (..), EntityDef, EntityNameDB (..), FieldNameDB (..), Single (..), SqlBackend, fieldDB, getEntityFields, rawExecute, rawSql)
import Database.PostgreSQL.Simple (ExecStatus (..), SqlError (..))

-- The ability to `ALTER TABLE` currently dealing with `CONSTRAINT` but can be extended
data AlterTable
  = AddUniqueConstraint ConstraintNameDB [FieldNameDB]
  | DropUniqueConstraint ConstraintNameDB
  deriving (Show)

data DbAlterTableException
  = DbAlterTableException String SqlError
  deriving (Show)

instance Exception DbAlterTableException

alterTable ::
  forall m.
  ( MonadBaseControl IO m
  , MonadIO m
  ) =>
  EntityDef ->
  AlterTable ->
  ReaderT SqlBackend m ()
alterTable entity (AddUniqueConstraint cname cols) = do
  if checkAllFieldsValid entity cols
    then do
      constraintRes <- queryHasConstraint cname
      if constraintRes
        then -- the constraint already exists so do nothing
          pure ()
        else -- if it doesn't exist then add a new constraint
          handle alterTableExceptHandler (rawExecute queryAddConstraint [])
    else throwErr "Some of the unique values which that are being added to the constraint don't correlate with what exists"
  where
    queryAddConstraint :: Text.Text
    queryAddConstraint =
      Text.concat
        [ "ALTER TABLE "
        , unEntityNameDB (entityDB entity)
        , " ADD CONSTRAINT "
        , unConstraintNameDB cname
        , " UNIQUE("
        , Text.intercalate "," $ map escapeDBName' cols
        , ")"
        ]

    escapeDBName' :: FieldNameDB -> Text.Text
    escapeDBName' = unFieldNameDB

    throwErr :: forall m'. MonadIO m' => [Char] -> ReaderT SqlBackend m' ()
    throwErr e = liftIO $ throwIO (DbAlterTableException e sqlError)
alterTable entity (DropUniqueConstraint cname) =
  handle alterTableExceptHandler (rawExecute query [])
  where
    query :: Text.Text
    query =
      Text.concat
        [ "ALTER TABLE "
        , unEntityNameDB (entityDB entity)
        , " DROP CONSTRAINT IF EXISTS "
        , unConstraintNameDB cname
        ]

-- check if a constraint is already present
queryHasConstraint ::
  MonadIO m =>
  ConstraintNameDB ->
  ReaderT SqlBackend m Bool
queryHasConstraint cname = do
  constraintRes :: [Single Int] <- rawSql queryCheckConstraint []
  if constraintRes == [Single 1]
    then pure True
    else pure False
  where
    queryCheckConstraint :: Text.Text
    queryCheckConstraint =
      Text.concat
        [ "SELECT COUNT(*) FROM pg_constraint WHERE conname ='"
        , unConstraintNameDB cname
        , "'"
        ]

-- check to see that the field inputs exist
checkAllFieldsValid :: Foldable t => EntityDef -> t FieldNameDB -> Bool
checkAllFieldsValid entity cols = do
  let fieldDef = getEntityFields entity
      fieldDbs = map fieldDB fieldDef
  all (`elem` fieldDbs) cols

alterTableExceptHandler ::
  forall m a.
  MonadIO m =>
  SqlError ->
  ReaderT SqlBackend m a
alterTableExceptHandler e = liftIO $ throwIO (DbAlterTableException "" e)

sqlError :: SqlError
sqlError =
  SqlError
    { sqlState = ""
    , sqlExecStatus = FatalError
    , sqlErrorMsg = ""
    , sqlErrorDetail = ""
    , sqlErrorHint = ""
    }
