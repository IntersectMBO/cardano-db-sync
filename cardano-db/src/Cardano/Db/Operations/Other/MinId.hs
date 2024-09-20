{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.Db.Operations.Other.MinId where

import Cardano.Db.Operations.Query (queryMinRefId)
import Cardano.Db.Operations.Types (MaTxOutFields (..), TxOutFields (..), TxOutTableType (..))
import Cardano.Db.Schema.BaseSchema
import qualified Cardano.Db.Schema.Core.TxOut as C
import qualified Cardano.Db.Schema.Variant.TxOut as V
import Cardano.Prelude
import qualified Data.Text as Text
import Database.Persist.Sql (PersistEntity, PersistField, SqlBackend, fromSqlKey, toSqlKey)

data MinIds (a :: TxOutTableType) = MinIds
  { minTxInId :: Maybe TxInId
  , minTxOutId :: Maybe (TxOutIdFor a)
  , minMaTxOutId :: Maybe (MaTxOutIdFor a)
  }

instance (TxOutFields a, MaTxOutFields a, Ord (TxOutIdFor a), Ord (MaTxOutIdFor a)) => Monoid (MinIds a) where
  mempty = MinIds Nothing Nothing Nothing

instance (TxOutFields a, MaTxOutFields a, Ord (TxOutIdFor a), Ord (MaTxOutIdFor a)) => Semigroup (MinIds a) where
  mn1 <> mn2 =
    MinIds
      { minTxInId = minJust (minTxInId mn1) (minTxInId mn2)
      , minTxOutId = minJust (minTxOutId mn1) (minTxOutId mn2)
      , minMaTxOutId = minJust (minMaTxOutId mn1) (minMaTxOutId mn2)
      }

data MinIdsWrapper
  = CMinIdsWrapper (MinIds 'TxOutCore)
  | VMinIdsWrapper (MinIds 'TxOutVariantAddress)

instance Monoid MinIdsWrapper where
  mempty = CMinIdsWrapper mempty -- or VMinIdsWrapper mempty, depending on your preference

instance Semigroup MinIdsWrapper where
  (CMinIdsWrapper a) <> (CMinIdsWrapper b) = CMinIdsWrapper (a <> b)
  (VMinIdsWrapper a) <> (VMinIdsWrapper b) = VMinIdsWrapper (a <> b)
  _ <> b = b -- If types don't match, return the second argument which is a no-op

minIdsToText :: MinIdsWrapper -> Text
minIdsToText (CMinIdsWrapper minIds) = minIdsCoreToText minIds
minIdsToText (VMinIdsWrapper minIds) = minIdsVariantToText minIds

textToMinIds :: TxOutTableType -> Text -> Maybe MinIdsWrapper
textToMinIds txOutTableType txt =
  case txOutTableType of
    TxOutCore -> CMinIdsWrapper <$> textToMinIdsCore txt
    TxOutVariantAddress -> VMinIdsWrapper <$> textToMinIdsVariant txt

minIdsCoreToText :: MinIds 'TxOutCore -> Text
minIdsCoreToText minIds =
  Text.intercalate
    ":"
    [ maybe "" (Text.pack . show . fromSqlKey) $ minTxInId minIds
    , maybe "" (Text.pack . show . fromSqlKey) $ minTxOutId minIds
    , maybe "" (Text.pack . show . fromSqlKey) $ minMaTxOutId minIds
    ]

minIdsVariantToText :: MinIds 'TxOutVariantAddress -> Text
minIdsVariantToText minIds =
  Text.intercalate
    ":"
    [ maybe "" (Text.pack . show . fromSqlKey) $ minTxInId minIds
    , maybe "" (Text.pack . show) $ minTxOutId minIds
    , maybe "" (Text.pack . show . fromSqlKey) $ minMaTxOutId minIds
    ]

textToMinIdsCore :: Text -> Maybe (MinIds 'TxOutCore)
textToMinIdsCore txt =
  case Text.split (== ':') txt of
    [tminTxInId, tminTxOutId, tminMaTxOutId] ->
      Just $
        MinIds
          { minTxInId = toSqlKey <$> readMaybe (Text.unpack tminTxInId)
          , minTxOutId = toSqlKey <$> readMaybe (Text.unpack tminTxOutId)
          , minMaTxOutId = toSqlKey <$> readMaybe (Text.unpack tminMaTxOutId)
          }
    _otherwise -> Nothing

textToMinIdsVariant :: Text -> Maybe (MinIds 'TxOutVariantAddress)
textToMinIdsVariant txt =
  case Text.split (== ':') txt of
    [tminTxInId, tminTxOutId, tminMaTxOutId] ->
      Just $
        MinIds
          { minTxInId = toSqlKey <$> readMaybe (Text.unpack tminTxInId)
          , minTxOutId = readMaybe (Text.unpack tminTxOutId)
          , minMaTxOutId = toSqlKey <$> readMaybe (Text.unpack tminMaTxOutId)
          }
    _otherwise -> Nothing

minJust :: (Ord a) => Maybe a -> Maybe a -> Maybe a
minJust Nothing y = y
minJust x Nothing = x
minJust (Just x) (Just y) = Just (min x y)

--------------------------------------------------------------------------------
-- CompleteMinId
--------------------------------------------------------------------------------
completeMinId ::
  (MonadIO m) =>
  Maybe TxId ->
  MinIdsWrapper ->
  ReaderT SqlBackend m MinIdsWrapper
completeMinId mTxId mIdW = case mIdW of
  CMinIdsWrapper minIds -> CMinIdsWrapper <$> completeMinIdCore mTxId minIds
  VMinIdsWrapper minIds -> VMinIdsWrapper <$> completeMinIdVariant mTxId minIds

completeMinIdCore :: MonadIO m => Maybe TxId -> MinIds 'TxOutCore -> ReaderT SqlBackend m (MinIds 'TxOutCore)
completeMinIdCore mTxId minIds = do
  case mTxId of
    Nothing -> pure mempty
    Just txId -> do
      mTxInId <- whenNothingQueryMinRefId (minTxInId minIds) TxInTxInId txId
      mTxOutId <- whenNothingQueryMinRefId (minTxOutId minIds) C.TxOutTxId txId
      mMaTxOutId <- case mTxOutId of
        Nothing -> pure Nothing
        Just txOutId -> whenNothingQueryMinRefId (minMaTxOutId minIds) C.MaTxOutTxOutId txOutId
      pure $
        MinIds
          { minTxInId = mTxInId
          , minTxOutId = mTxOutId
          , minMaTxOutId = mMaTxOutId
          }

completeMinIdVariant :: MonadIO m => Maybe TxId -> MinIds 'TxOutVariantAddress -> ReaderT SqlBackend m (MinIds 'TxOutVariantAddress)
completeMinIdVariant mTxId minIds = do
  case mTxId of
    Nothing -> pure mempty
    Just txId -> do
      mTxInId <- whenNothingQueryMinRefId (minTxInId minIds) TxInTxInId txId
      mTxOutId <- whenNothingQueryMinRefId (minTxOutId minIds) V.TxOutTxId txId
      mMaTxOutId <- case mTxOutId of
        Nothing -> pure Nothing
        Just txOutId -> whenNothingQueryMinRefId (minMaTxOutId minIds) V.MaTxOutTxOutId txOutId
      pure $
        MinIds
          { minTxInId = mTxInId
          , minTxOutId = mTxOutId
          , minMaTxOutId = mMaTxOutId
          }

whenNothingQueryMinRefId ::
  forall m record field.
  (MonadIO m, PersistEntity record, PersistField field) =>
  Maybe (Key record) ->
  EntityField record field ->
  field ->
  ReaderT SqlBackend m (Maybe (Key record))
whenNothingQueryMinRefId mKey efield field = do
  case mKey of
    Just k -> pure $ Just k
    Nothing -> queryMinRefId efield field
