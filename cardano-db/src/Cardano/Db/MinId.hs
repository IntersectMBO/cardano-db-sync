{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardano.Db.MinId where

import           Cardano.Prelude
import           Cardano.Db.Schema
import           Cardano.Db.Text
import qualified Data.Text as Text
import           Database.Persist.Sql (SqlBackend, ToBackendKey, fromSqlKey, toSqlKey)
import           Prelude (error)

data MinIds = MinIds
  { minTxId :: Maybe TxId
  , minTxInId :: Maybe TxInId
  , minTxOutId :: Maybe TxOutId
  , minMaTxOutId :: Maybe MaTxOutId
  }

instance Monoid MinIds where
  mempty = MinIds Nothing Nothing Nothing Nothing

instance Semigroup MinIds where
  mn1 <> mn2 =
    MinIds
        { minTxId = minJust (minTxId mn1) (minTxId mn2)
        , minTxInId = minJust (minTxInId mn1) (minTxInId mn2)
        , minTxOutId = minJust (minTxOutId mn1) (minTxOutId mn2)
        , minMaTxOutId = minJust (minMaTxOutId mn1) (minMaTxOutId mn2)
        }

textToMinId :: Text -> MinIds
textToMinId txt =
    case Text.split (== ':') txt of
      [tminTxId, tminTxInId, tminTxOutId, tminMaTxOutId] ->
        MinIds
          { minTxId = toSqlKey <$> readKey tminTxId
          , minTxInId = toSqlKey <$> readKey tminTxInId
          , minTxOutId = toSqlKey <$> readKey tminTxOutId
          , minMaTxOutId = toSqlKey <$> readKey tminMaTxOutId
          }
      _ -> error $ "Failed to parse MinIds: " <> Text.unpack txt
  where
    readKey :: Text -> Maybe Int64
    readKey "" = Nothing
    readKey str = readMaybe (Text.unpack str)

minIdsToText :: MinIds -> Text
minIdsToText minIds =
    Text.intercalate ":"
      [ fromKey $ minTxId minIds
      , fromKey $ minTxInId minIds
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
