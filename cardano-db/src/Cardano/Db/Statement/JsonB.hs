{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Db.Statement.JsonB where

import Cardano.Prelude (ExceptT, MonadError (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import qualified Hasql.Connection as HsqlC
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

import Cardano.Db.Error (DbError (..))
import Cardano.Db.Statement.Function.Core (mkCallInfo, mkCallSite, runDbSession)
import Cardano.Db.Types (DbAction)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc

--------------------------------------------------------------------------------
-- Enable JSONB for specific fields in the schema
--------------------------------------------------------------------------------
enableJsonbInSchemaStmt :: HsqlStmt.Statement () ()
enableJsonbInSchemaStmt = do
  HsqlStmt.Statement
    ( mconcat $
        zipWith
          ( \s i ->
              (if i > (0 :: Integer) then "; " else "")
                <> "ALTER TABLE "
                <> fst s
                <> " ALTER COLUMN "
                <> snd s
                <> " TYPE jsonb USING "
                <> snd s
                <> "::jsonb"
          )
          jsonbColumns
          [0 ..]
    )
    HsqlE.noParams
    HsqlD.noResult
    True
  where
    jsonbColumns :: [(ByteString, ByteString)]
    jsonbColumns =
      [ ("tx_metadata", "json")
      , ("script", "json")
      , ("datum", "value")
      , ("redeemer_data", "value")
      , ("cost_model", "costs")
      , ("gov_action_proposal", "description")
      , ("off_chain_pool_data", "json")
      , ("off_chain_vote_data", "json")
      ]

enableJsonbInSchema :: MonadIO m => DbAction m ()
enableJsonbInSchema =
  runDbSession (mkCallInfo "enableJsonbInSchema") $
    HsqlSes.statement () enableJsonbInSchemaStmt

--------------------------------------------------------------------------------
-- Disable JSONB for specific fields in the schema
--------------------------------------------------------------------------------
disableJsonbInSchemaStmt :: HsqlStmt.Statement () ()
disableJsonbInSchemaStmt =
  HsqlStmt.Statement
    ( mconcat $
        zipWith
          ( \columnDef i ->
              (if i > (0 :: Integer) then "; " else "")
                <> "ALTER TABLE "
                <> fst columnDef
                <> " ALTER COLUMN "
                <> snd columnDef
                <> " TYPE VARCHAR"
          )
          jsonColumnsToRevert
          [0 ..]
    )
    HsqlE.noParams
    HsqlD.noResult
    True
  where
    -- List of table and column pairs to convert back from JSONB
    jsonColumnsToRevert :: [(ByteString, ByteString)]
    jsonColumnsToRevert =
      [ ("tx_metadata", "json")
      , ("script", "json")
      , ("datum", "value")
      , ("redeemer_data", "value")
      , ("cost_model", "costs")
      , ("gov_action_proposal", "description")
      , ("off_chain_pool_data", "json")
      , ("off_chain_vote_data", "json")
      ]

disableJsonbInSchema :: MonadIO m => DbAction m ()
disableJsonbInSchema =
  runDbSession (mkCallInfo "disableJsonbInSchema") $
    HsqlSes.statement () disableJsonbInSchemaStmt

-- | Check if the JSONB column exists in the schema
jsonbSchemaStatement :: HsqlStmt.Statement () Int64
jsonbSchemaStatement =
  HsqlStmt.Statement
    query
    HsqlE.noParams
    decoder
    True
  where
    query =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COUNT(*)"
          , " FROM information_schema.columns"
          , " WHERE table_name = 'tx_metadata'"
          , " AND column_name = 'json'"
          , " AND data_type = 'jsonb'"
          ]

    decoder :: HsqlD.Result Int64
    decoder =
      HsqlD.singleRow $
        HsqlD.column $
          HsqlD.nonNullable HsqlD.int8

-- Original function for direct connection use
queryJsonbInSchemaExists :: HsqlC.Connection -> ExceptT DbError IO Bool
queryJsonbInSchemaExists conn = do
  result <- liftIO $ HsqlSes.run (HsqlSes.statement () jsonbSchemaStatement) conn
  case result of
    Left err -> throwError $ DbError mkCallSite "queryJsonbInSchemaExists" $ Just err
    Right countRes -> pure $ countRes == 1

-- Test function using DbAction monad
queryJsonbInSchemaExistsTest :: MonadIO m => DbAction m Bool
queryJsonbInSchemaExistsTest = do
  result <-
    runDbSession (mkCallInfo "queryJsonbInSchemaExists") $
      HsqlSes.statement () jsonbSchemaStatement
  pure $ result == 1
