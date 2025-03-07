{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Db.Operations.Other.JsonbQuery where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import qualified Hasql.Connection as HsqlC
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlS
import qualified Hasql.Statement as HsqlS
import qualified Hasql.Transaction as HsqlT

import Cardano.Db.Error (DbError (..), AsDbError (..))
import Cardano.Db.Statement.Function.Core (mkCallSite)
import Cardano.Prelude (ExceptT, MonadError (..), forM_)

enableJsonbInSchema :: HsqlT.Transaction ()
enableJsonbInSchema = do
  forM_ stmts $ \stmt -> HsqlT.statement () (enableJsonbInSchemaStmt stmt)
  where
    enableJsonbInSchemaStmt ::  (ByteString, ByteString) -> HsqlS.Statement () ()
    enableJsonbInSchemaStmt (t, c) =
      HsqlS.Statement
        ("ALTER TABLE " <> t <> " ALTER COLUMN " <> c <> " TYPE jsonb USING " <> c <> "::jsonb")
        HsqlE.noParams
        HsqlD.noResult
        True

    stmts :: [(ByteString, ByteString)]
    stmts = [ ("tx_metadata", "json")
            , ("script", "json")
            , ("datum", "value")
            , ("redeemer_data", "value")
            , ("cost_model", "costs")
            , ("gov_action_proposal", "description")
            , ("off_chain_pool_data", "json")
            , ("off_chain_vote_data", "json")
            ]

disableJsonbInSchema :: HsqlT.Transaction ()
disableJsonbInSchema = do
  forM_ stmts $ \(t, c) -> HsqlT.statement () (disableJsonbInSchemaStmt t c)
  where
    disableJsonbInSchemaStmt t c = HsqlS.Statement
      ("ALTER TABLE " <> t <> " ALTER COLUMN " <> c <> " TYPE VARCHAR")
      HsqlE.noParams
      HsqlD.noResult
      True

    stmts :: [(ByteString, ByteString)]
    stmts = [ ("tx_metadata", "json")
            , ("script", "json")
            , ("datum", "value")
            , ("redeemer_data", "value")
            , ("cost_model", "costs")
            , ("gov_action_proposal", "description")
            , ("off_chain_pool_data", "json")
            , ("off_chain_vote_data", "json")
            ]

queryJsonbInSchemaExists :: AsDbError e => HsqlC.Connection -> ExceptT e IO Bool
queryJsonbInSchemaExists conn = do
  result <- liftIO $ HsqlS.run (HsqlS.statement () jsonbSchemaStatement) conn
  case result of
    Left err -> throwError $ toDbError $ QueryError "queryJsonbInSchemaExists" mkCallSite err
    Right countRes -> pure $ countRes == 1
  where
    jsonbSchemaStatement :: HsqlS.Statement () Int64
    jsonbSchemaStatement =
      HsqlS.Statement
        query
        HsqlE.noParams  -- No parameters needed
        decoder
        True  -- Prepared statement

    query =
      "SELECT COUNT(*) \
      \FROM information_schema.columns \
      \WHERE table_name = 'tx_metadata' \
      \AND column_name = 'json' \
      \AND data_type = 'jsonb';"

    decoder :: HsqlD.Result Int64
    decoder = HsqlD.singleRow $
      HsqlD.column $
        HsqlD.nonNullable HsqlD.int8
