{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Db.Operations.Other.JsonbQuery where

import Cardano.Prelude (ExceptT, MonadError (..))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import qualified Hasql.Connection as HsqlC
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlS
import qualified Hasql.Statement as HsqlS

import Cardano.Db.Error (DbError (..))
import Cardano.Db.Statement.Function.Core (mkCallSite)

enableJsonbInSchema :: HsqlS.Statement () ()
enableJsonbInSchema = do
  HsqlS.Statement
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

disableJsonbInSchema :: HsqlS.Statement () ()
disableJsonbInSchema =
  HsqlS.Statement
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

queryJsonbInSchemaExists :: HsqlC.Connection -> ExceptT DbError IO Bool
queryJsonbInSchemaExists conn = do
  result <- liftIO $ HsqlS.run (HsqlS.statement () jsonbSchemaStatement) conn
  case result of
    Left err -> throwError $ DbError mkCallSite "queryJsonbInSchemaExists" $ Just err
    Right countRes -> pure $ countRes == 1
  where
    jsonbSchemaStatement :: HsqlS.Statement () Int64
    jsonbSchemaStatement =
      HsqlS.Statement
        query
        HsqlE.noParams -- No parameters needed
        decoder
        True -- Prepared statement
    query =
      "SELECT COUNT(*) \
      \FROM information_schema.columns \
      \WHERE table_name = 'tx_metadata' \
      \AND column_name = 'json' \
      \AND data_type = 'jsonb';"

    decoder :: HsqlD.Result Int64
    decoder =
      HsqlD.singleRow $
        HsqlD.column $
          HsqlD.nonNullable HsqlD.int8
