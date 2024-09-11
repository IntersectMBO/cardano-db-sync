{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Db.Operations.Other.JsonbQuery where

import Cardano.Db.Error (LookupFail (..))
import Control.Exception.Lifted (handle, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT)

import Database.Esqueleto.Experimental
import Database.PostgreSQL.Simple (SqlError)

enableJsonbInSchema ::
  forall m.
  ( MonadBaseControl IO m
  , MonadIO m
  ) =>
  ReaderT SqlBackend m ()
enableJsonbInSchema = do
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
      "ALTER TABLE cost_model ALTER COLUMN costs TYPE jsonb USING costs::jsonb"
      []
  handle exceptHandler $
    rawExecute
      "ALTER TABLE gov_action_proposal ALTER COLUMN description TYPE jsonb USING description::jsonb"
      []
  handle exceptHandler $
    rawExecute
      "ALTER TABLE off_chain_pool_data ALTER COLUMN json TYPE jsonb USING json::jsonb"
      []
  handle exceptHandler $
    rawExecute
      "ALTER TABLE off_chain_vote_data ALTER COLUMN json TYPE jsonb USING json::jsonb"
      []

disableJsonbInSchema ::
  forall m.
  ( MonadBaseControl IO m
  , MonadIO m
  ) =>
  ReaderT SqlBackend m ()
disableJsonbInSchema = do
  handle exceptHandler $
    rawExecute
      "ALTER TABLE tx_metadata ALTER COLUMN json TYPE VARCHAR"
      []
  handle exceptHandler $
    rawExecute
      "ALTER TABLE script ALTER COLUMN json TYPE VARCHAR"
      []
  handle exceptHandler $
    rawExecute
      "ALTER TABLE datum ALTER COLUMN value TYPE VARCHAR"
      []
  handle exceptHandler $
    rawExecute
      "ALTER TABLE redeemer_data ALTER COLUMN value TYPE VARCHAR"
      []
  handle exceptHandler $
    rawExecute
      "ALTER TABLE cost_model ALTER COLUMN costs TYPE VARCHAR"
      []
  handle exceptHandler $
    rawExecute
      "ALTER TABLE gov_action_proposal ALTER COLUMN description TYPE VARCHAR"
      []
  handle exceptHandler $
    rawExecute
      "ALTER TABLE off_chain_pool_data ALTER COLUMN json TYPE VARCHAR"
      []
  handle exceptHandler $
    rawExecute
      "ALTER TABLE off_chain_vote_data ALTER COLUMN json TYPE VARCHAR"
      []

queryJsonbInSchemaExists ::
  (MonadIO m) =>
  ReaderT SqlBackend m Bool
queryJsonbInSchemaExists = do
  isjsonb <- rawSql query []
  pure $ case isjsonb of
    [Single (1 :: Int)] -> True
    _other -> False
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
  liftIO $ throwIO (DBRJsonbInSchema $ show e)
