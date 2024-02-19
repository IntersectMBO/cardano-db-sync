{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Db.Migration.Extra.JsonbMigrationQueries where

import Cardano.Db.Error (LookupFail (..))
import Control.Exception.Lifted (handle, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT)

import Database.Esqueleto.Experimental
import Database.PostgreSQL.Simple (SqlError)

resetJsonbMigration ::
  forall m.
  ( MonadBaseControl IO m
  , MonadIO m
  ) =>
  ReaderT SqlBackend m ()
resetJsonbMigration = do
  handle exceptHandler $
    rawExecute
      "ALTER TABLE tx_metadata ALTER COLUMN json TYPE jsonb USING json::jsonb"
      []
  handle exceptHandler $
    rawExecute
      "ALTER TABLE script ALTER COLUMN json TYPE jsonb USING json::jsonb"
      []
  handle exceptHandler $
    rawExecute
      "ALTER TABLE datum ALTER COLUMN value TYPE jsonb USING value::jsonb"
      []
  handle exceptHandler $
    rawExecute
      "ALTER TABLE redeemer_data ALTER COLUMN value TYPE jsonb USING value::jsonb"
      []
  handle exceptHandler $
    rawExecute
      "ALTER TABLE cost_model ALTER COLUMN cost TYPE jsonb USING value::jsonb"
      []
  handle exceptHandler $
    rawExecute
      "ALTER TABLE off_chain_vote_data ALTER COLUMN json TYPE jsonb USING value::jsonb"
      []

queryJsonbTypeExists ::
  (MonadIO m) =>
  ReaderT SqlBackend m Bool
queryJsonbTypeExists = do
  isjsonb <- rawSql query []
  pure $ case isjsonb of
    [Single (1 :: Int)] -> True
    _ -> False
  where
    tableName = "'tx_metadata'"
    columnName = "'json'"
    -- check if the column is of type jsonb
    query =
      mconcat
        [ "SELECT COUNT(*) FROM information_schema.columns "
        , "WHERE table_name ="
        , tableName
        , "AND column_name ="
        , columnName
        , "AND data_type = 'jsonb';"
        ]

exceptHandler ::
  forall m a.
  MonadIO m =>
  SqlError ->
  ReaderT SqlBackend m a
exceptHandler e =
  liftIO $ throwIO (DBPruneConsumed $ show e)
