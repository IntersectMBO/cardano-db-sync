{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.MinId where

import Cardano.Db.Schema
import Cardano.Db.Text
import Cardano.Prelude
import qualified Data.Text as Text
import Database.Persist.Sql (SqlBackend, ToBackendKey, fromSqlKey, toSqlKey)

data MinIds = MinIds
  { minTxInId :: Maybe TxInId
  , minTxOutId :: Maybe TxOutId
  , minMaTxOutId :: Maybe MaTxOutId
  }

instance Monoid MinIds where
  mempty = MinIds Nothing Nothing Nothing

instance Semigroup MinIds where
  mn1 <> mn2 =
    MinIds
      { minTxInId = minJust (minTxInId mn1) (minTxInId mn2)
      , minTxOutId = minJust (minTxOutId mn1) (minTxOutId mn2)
      , minMaTxOutId = minJust (minMaTxOutId mn1) (minMaTxOutId mn2)
      }

textToMinId :: Text -> Maybe MinIds
textToMinId txt =
  case Text.split (== ':') txt of
    [tminTxInId, tminTxOutId, tminMaTxOutId] ->
      Just $
        MinIds
          { minTxInId = toSqlKey <$> readKey tminTxInId
          , minTxOutId = toSqlKey <$> readKey tminTxOutId
          , minMaTxOutId = toSqlKey <$> readKey tminMaTxOutId
          }
    _ -> Nothing
  where
    readKey :: Text -> Maybe Int64
    readKey "" = Nothing
    readKey str = readMaybe (Text.unpack str)

minIdsToText :: MinIds -> Text
minIdsToText minIds =
  Text.intercalate
    ":"
    [ fromKey $ minTxInId minIds
    , fromKey $ minTxOutId minIds
    , fromKey $ minMaTxOutId minIds
    ]
  where
    fromKey :: ToBackendKey SqlBackend record => Maybe (Key record) -> Text
    fromKey Nothing = ""
    fromKey (Just k) = textShow $ fromSqlKey k

minJust :: Ord a => Maybe a -> Maybe a -> Maybe a
minJust (Just a) (Just b) = Just $ min a b
minJust (Just a) _ = Just a
minJust _ x = x
