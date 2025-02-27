{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Cardano.Db.Statement.Function.Query where

import qualified Data.Text as Text
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Statement as HsqlS
import qualified Hasql.Transaction as HsqlT
import qualified Data.Text.Encoding as TextEnc

import Cardano.Db.Statement.Function.Core (ResultType (..))
import Cardano.Db.Statement.Types (DbInfo (..))
import Cardano.Prelude (Proxy(..))

-- | Checks if a record with a specific ID exists in a table.
--
-- This function performs an efficient EXISTS check on a given table, using the record's ID.
--
--
-- === Example
-- @
-- queryVotingAnchorIdExists :: MonadIO m => VotingAnchorId -> DbAction m Bool
-- queryVotingAnchorIdExists votingAnchorId = runDbT ReadOnly $ mkDbTransaction "queryVotingAnchorIdExists" $
--   queryIdExists \@VotingAnchor
--     (idEncoder getVotingAnchorId)
--     (WithResult (HsqlD.singleRow $ HsqlD.column (HsqlD.nonNullable HsqlD.bool)))
--     votingAnchorId
-- @
queryIdExists
  :: forall a b r. (DbInfo a)
  => HsqlE.Params b   -- Encoder for the ID value
  -> ResultType Bool r -- Decoder for the boolean result
  -> b                -- ID value to check
  -> HsqlT.Transaction r
queryIdExists encoder resultType idVal =
  HsqlT.statement idVal $ HsqlS.Statement sql encoder decoder True
  where
    decoder = case resultType of
      NoResult -> HsqlD.noResult
      WithResult dec -> dec

    table = tableName (Proxy @a)

    sql = TextEnc.encodeUtf8 $ Text.concat
      [ "SELECT EXISTS (SELECT 1 FROM " <> table
      , " WHERE id = $1)"
      ]
