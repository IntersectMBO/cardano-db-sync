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
import Cardano.Db.Statement.Types (DbInfo (..), Key)
import Cardano.Prelude (Proxy(..))
import qualified Data.List.NonEmpty as NE
import Data.Functor.Contravariant (Contravariant (..))

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

replace
  :: forall a. (DbInfo a)
  => Key a                       -- ^ Key for the record to replace
  -> HsqlE.Params (Key a)        -- ^ Key encoder
  -> HsqlE.Params a              -- ^ Record encoder
  -> a                           -- ^ New record value
  -> HsqlT.Transaction ()        -- Changed return type to ()
replace key keyEnc recordEnc record =
  HsqlT.statement (key, record) $ HsqlS.Statement sql encoder HsqlD.noResult True
  where
    table = tableName (Proxy @a)
    colsNames = NE.toList $ columnNames (Proxy @a)

    setClause = Text.intercalate ", " $
      zipWith (\col idx -> col <> " = $" <> Text.pack (show idx))
              colsNames
              [2..(length colsNames + 1)]

    -- Fix: create a combined encoder for the (key, record) tuple
    encoder = contramap fst keyEnc <> contramap snd recordEnc

    sql = TextEnc.encodeUtf8 $ Text.concat
      [ "UPDATE " <> table
      , " SET " <> setClause
      , " WHERE id = $1"
      ]
