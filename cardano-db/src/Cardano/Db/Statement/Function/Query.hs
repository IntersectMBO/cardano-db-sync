{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Db.Statement.Function.Query where

import qualified Data.Text as Text
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Statement as HsqlS

import qualified Data.Text.Encoding as TextEnc

import Cardano.Db.Statement.Function.Core (ResultType (..))
import Cardano.Db.Statement.Types (DbInfo (..), Entity, Key)
import Cardano.Prelude (Proxy (..))
import Data.Functor.Contravariant (Contravariant (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)

replace ::
  forall a.
  (DbInfo a) =>
  HsqlE.Params (Key a) -> -- ID encoder
  HsqlE.Params a -> -- Record encoder
  HsqlS.Statement (Key a, a) ()
replace keyEncoder recordEncoder =
  HsqlS.Statement sql encoder HsqlD.noResult True
  where
    table = tableName (Proxy @a)
    colNames = NE.toList $ columnNames (Proxy @a)

    setClause =
      Text.intercalate ", " $
        zipWith
          (\col i -> col <> " = $" <> Text.pack (show (i + (1 :: Integer))))
          colNames
          [1 ..]

    encoder = contramap fst keyEncoder <> contramap snd recordEncoder

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "UPDATE " <> table
          , " SET " <> setClause
          , " WHERE id = $1"
          ]

selectByField ::
  forall a b.
  (DbInfo a) =>
  Text -> -- Field name
  HsqlE.Params b -> -- Parameter encoder (not Value)
  HsqlD.Row (Entity a) -> -- Entity decoder
  HsqlS.Statement b (Maybe (Entity a))
selectByField fieldName paramEncoder entityDecoder =
  HsqlS.Statement
    ( TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT * FROM " <> tableName (Proxy @a)
          , " WHERE " <> fieldName <> " = $1"
          ]
    )
    paramEncoder -- Direct use of paramEncoder
    (HsqlD.rowMaybe entityDecoder)
    True

-- | Checks if a record with a specific ID exists in a table.
--
-- This function performs an EXISTS check on a given table, using the record's ID.
--
-- === Example
-- @
-- queryVotingAnchorIdStmt :: HsqlS.Statement Id.VotingAnchorId Bool
-- queryVotingAnchorIdStmt = existsById @VotingAnchor
--   (Id.idEncoder Id.getVotingAnchorId)
--   (WithResult (HsqlD.singleRow $ HsqlD.column (HsqlD.nonNullable HsqlD.bool)))
-- @
existsById ::
  forall a r.
  (DbInfo a, Key a ~ Key a) =>
  HsqlE.Params (Key a) -> -- Key encoder
  ResultType Bool r -> -- Whether to return Entity and decoder
  HsqlS.Statement (Key a) r
existsById encoder resultType =
  HsqlS.Statement sql encoder decoder True
  where
    decoder = case resultType of
      NoResult -> HsqlD.noResult
      WithResult dec -> dec

    table = tableName (Proxy @a)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT EXISTS (SELECT 1 FROM " <> table
          , " WHERE id = $1)"
          ]

-- | Creates a statement to replace a record with a new value
--
-- === Example
-- @
-- replaceVotingAnchor :: MonadIO m => VotingAnchorId -> VotingAnchor -> DbAction m ()
-- replaceVotingAnchor key record =
--   runDbSession (mkCallInfo "replaceVotingAnchor") $
--     HsqlS.statement (key, record) $ replaceRecord
--       @VotingAnchor
--       (idEncoder getVotingAnchorId)
--       votingAnchorEncoder
-- @
replaceRecord ::
  forall a.
  (DbInfo a) =>
  HsqlE.Params (Key a) -> -- Key encoder
  HsqlE.Params a -> -- Record encoder
  HsqlS.Statement (Key a, a) () -- Returns a statement to replace a record
replaceRecord keyEnc recordEnc =
  HsqlS.Statement sql encoder HsqlD.noResult True
  where
    table = tableName (Proxy @a)
    colsNames = NE.toList $ columnNames (Proxy @a)

    setClause =
      Text.intercalate ", " $
        zipWith
          (\col idx -> col <> " = $" <> Text.pack (show idx))
          colsNames
          [2 .. (length colsNames + 1)]

    -- Combined encoder for the (key, record) tuple
    encoder = contramap fst keyEnc <> contramap snd recordEnc

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "UPDATE " <> table
          , " SET " <> setClause
          , " WHERE id = $1"
          ]
