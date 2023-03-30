{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Cardano.Db.AlterTable (
  AlterTable(..),
  DbAlterTableException(..),
  alterTable,
) where

import Control.Exception.Lifted (Exception, handle, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.Text as T
import Database.Persist.Postgresql (ConstraintNameDB (..), EntityNameDB (..), FieldNameDB (..), SqlBackend, rawExecute, EntityDef, getEntityFields, fieldDB)
import Database.PostgreSQL.Simple (SqlError (..), ExecStatus (..))
import Database.Persist.EntityDef.Internal (entityDB)

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
  -- TODO: might not be worth doing the check as the query would return an
  -- error anyways if the fields aresn't present
  -- would be nice to handle it back in applyAndInsertBlockMaybe
  if checkAllFieldsValid entity cols
    then handle alterTableExceptHandler (rawExecute query [])
    else liftIO $ throwIO (DbAlterTableException "invalid field" sqlError)
  where
    query :: T.Text
    query =
      T.concat
        [ "ALTER TABLE "
        , unEntityNameDB (entityDB entity)
        , " ADD CONSTRAINT "
        , unConstraintNameDB cname
        , " UNIQUE("
        , T.intercalate "," $ map escapeDBName' cols
        , ")"
        ]
    escapeDBName' :: FieldNameDB -> T.Text
    escapeDBName' name = unFieldNameDB name
alterTable entity (DropUniqueConstraint cname) =
  handle alterTableExceptHandler (rawExecute query [])
  where
    query :: T.Text
    query =
      T.concat
        [ "ALTER TABLE "
        , unEntityNameDB (entityDB entity)
        , " DROP CONSTRAINT "
        , unConstraintNameDB cname
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
sqlError = SqlError
  { sqlState = ""
  , sqlExecStatus = FatalError
  , sqlErrorMsg = ""
  , sqlErrorDetail = ""
  , sqlErrorHint = ""
  }
