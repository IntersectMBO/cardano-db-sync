{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.Db.Schema.MinIds where

import Cardano.Prelude
import qualified Data.Text as Text
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import Text.Read (read)

import Cardano.Db.Schema.Core.Base (TxIn)
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Schema.Variants (MaTxOutIdW (..), TxOutIdW (..), TxOutVariantType (..))
import qualified Cardano.Db.Schema.Variants.TxOutCore as VC
import qualified Cardano.Db.Schema.Variants.TxOutAddress as VA
import Cardano.Db.Statement.Function.Query (queryMinRefId)
import Cardano.Db.Statement.Types (DbInfo, Key)
import Cardano.Db.Types (DbAction)

--------------------------------------------------------------------------------
-- MinIds and MinIdsWrapper
--------------------------------------------------------------------------------
data MinIds = MinIds
  { minTxInId :: !(Maybe Id.TxInId)
  , minTxOutId :: !(Maybe TxOutIdW)
  , minMaTxOutId :: !(Maybe MaTxOutIdW)
  }

instance Monoid MinIds where
  mempty = MinIds Nothing Nothing Nothing

instance Semigroup MinIds where
  mn1 <> mn2 =
    MinIds
      { minTxInId = minJust (minTxInId mn1) (minTxInId mn2)
      , minTxOutId = minJustBy compareTxOutIds (minTxOutId mn1) (minTxOutId mn2)
      , minMaTxOutId = minJustBy compareMaTxOutIds (minMaTxOutId mn1) (minMaTxOutId mn2)
      }

data MinIdsWrapper
  = CMinIdsWrapper !MinIds
  | VMinIdsWrapper !MinIds

instance Monoid MinIdsWrapper where
  mempty = CMinIdsWrapper mempty

instance Semigroup MinIdsWrapper where
  (CMinIdsWrapper a) <> (CMinIdsWrapper b) = CMinIdsWrapper (a <> b)
  (VMinIdsWrapper a) <> (VMinIdsWrapper b) = VMinIdsWrapper (a <> b)
  _ <> b = b -- If types don't match, return the second argument

--------------------------------------------------------------------------------
-- Helper functions for MinIds
--------------------------------------------------------------------------------
compareTxOutIds :: TxOutIdW -> TxOutIdW -> Ordering
compareTxOutIds (VCTxOutIdW a) (VCTxOutIdW b) = compare (Id.getTxOutCoreId a) (Id.getTxOutCoreId b)
compareTxOutIds (VATxOutIdW a) (VATxOutIdW b) = compare (Id.getTxOutAddressId a) (Id.getTxOutAddressId b)
compareTxOutIds _ _ = EQ -- Different types can't be compared meaningfully

compareMaTxOutIds :: MaTxOutIdW -> MaTxOutIdW -> Ordering
compareMaTxOutIds (CMaTxOutIdW a) (CMaTxOutIdW b) = compare (Id.getMaTxOutCoreId a) (Id.getMaTxOutCoreId b)
compareMaTxOutIds (VMaTxOutIdW a) (VMaTxOutIdW b) = compare (Id.getMaTxOutAddressId a) (Id.getMaTxOutAddressId b)
compareMaTxOutIds _ _ = EQ

minJustBy :: (a -> a -> Ordering) -> Maybe a -> Maybe a -> Maybe a
minJustBy _ Nothing y = y
minJustBy _ x Nothing = x
minJustBy cmp (Just x) (Just y) = Just (if cmp x y == LT then x else y)

minJust :: Ord a => Maybe a -> Maybe a -> Maybe a
minJust Nothing y = y
minJust x Nothing = x
minJust (Just x) (Just y) = Just (min x y)

extractCoreTxOutId :: Maybe TxOutIdW -> Maybe Id.TxOutCoreId
extractCoreTxOutId =
  ( >>=
      \case
        VCTxOutIdW id -> Just id
        _otherwise -> Nothing
  )

extractVariantTxOutId :: Maybe TxOutIdW -> Maybe Id.TxOutAddressId
extractVariantTxOutId =
  ( >>=
      \case
        VATxOutIdW id -> Just id
        _otherwise -> Nothing
  )

extractCoreMaTxOutId :: Maybe MaTxOutIdW -> Maybe Id.MaTxOutCoreId
extractCoreMaTxOutId =
  ( >>=
      \case
        CMaTxOutIdW id -> Just id
        _otherwise -> Nothing
  )

extractVariantMaTxOutId :: Maybe MaTxOutIdW -> Maybe Id.MaTxOutAddressId
extractVariantMaTxOutId =
  ( >>=
      \case
        VMaTxOutIdW id -> Just id
        _otherwise -> Nothing
  )

--------------------------------------------------------------------------------
-- Text serialization for MinIds
--------------------------------------------------------------------------------
minIdsCoreToText :: MinIds -> Text
minIdsCoreToText minIds =
  Text.intercalate
    ":"
    [ maybe "" (Text.pack . show . Id.getTxInId) $ minTxInId minIds
    , maybe "" txOutIdCoreToText $ minTxOutId minIds
    , maybe "" maTxOutIdCoreToText $ minMaTxOutId minIds
    ]
  where
    txOutIdCoreToText :: TxOutIdW -> Text
    txOutIdCoreToText (VCTxOutIdW txOutId) = Text.pack . show $ Id.getTxOutCoreId txOutId
    txOutIdCoreToText _ = ""  -- Skip non-core IDs

    maTxOutIdCoreToText :: MaTxOutIdW -> Text
    maTxOutIdCoreToText (CMaTxOutIdW maTxOutId) = Text.pack . show $ Id.getMaTxOutCoreId maTxOutId
    maTxOutIdCoreToText _ = ""  -- Skip non-core IDs

minIdsAddressToText :: MinIds -> Text
minIdsAddressToText minIds =
  Text.intercalate
    ":"
    [ maybe "" (Text.pack . show . Id.getTxInId) $ minTxInId minIds
    , maybe "" txOutIdAddressToText $ minTxOutId minIds
    , maybe "" maTxOutIdAddressToText $ minMaTxOutId minIds
    ]
  where
    txOutIdAddressToText :: TxOutIdW -> Text
    txOutIdAddressToText (VATxOutIdW txOutId) = Text.pack . show $ Id.getTxOutAddressId txOutId
    txOutIdAddressToText _ = ""  -- Skip non-variant IDs

    maTxOutIdAddressToText :: MaTxOutIdW -> Text
    maTxOutIdAddressToText (VMaTxOutIdW maTxOutId) = Text.pack . show $ Id.getMaTxOutAddressId maTxOutId
    maTxOutIdAddressToText _ = ""  -- Skip non-variant IDs

--------------------------------------------------------------------------------
minIdsToText :: MinIdsWrapper -> Text
minIdsToText (CMinIdsWrapper minIds) = minIdsToTextHelper minIds "C"
minIdsToText (VMinIdsWrapper minIds) = minIdsToTextHelper minIds "V"

minIdsToTextHelper :: MinIds -> Text -> Text
minIdsToTextHelper minIds prefix =
  Text.intercalate
    ":"
    [ txInIdText
    , txOutIdText
    , maTxOutIdText
    , prefix -- Add type identifier
    ]
  where
    txInIdText = maybe "" (Text.pack . show . Id.getTxInId) $ minTxInId minIds

    txOutIdText = case minTxOutId minIds of
      Nothing -> ""
      Just (VCTxOutIdW id) -> "C" <> Text.pack (show (Id.getTxOutCoreId id))
      Just (VATxOutIdW id) -> "V" <> Text.pack (show (Id.getTxOutAddressId id))

    maTxOutIdText = case minMaTxOutId minIds of
      Nothing -> ""
      Just (CMaTxOutIdW id) -> "C" <> Text.pack (show (Id.getMaTxOutCoreId id))
      Just (VMaTxOutIdW id) -> "V" <> Text.pack (show (Id.getMaTxOutAddressId id))

--------------------------------------------------------------------------------
textToMinIds :: TxOutVariantType -> Text -> Maybe MinIdsWrapper
textToMinIds txOutVariantType txt =
  case Text.split (== ':') txt of
    [tminTxInId, tminTxOutId, tminMaTxOutId, typeId] ->
      let
        mTxInId =
          if Text.null tminTxInId
            then Nothing
            else Just $ Id.TxInId $ read $ Text.unpack tminTxInId

        mTxOutId =
          if Text.null tminTxOutId
            then Nothing
            else case Text.head tminTxOutId of
              'C' ->
                Just $
                  VCTxOutIdW $
                    Id.TxOutCoreId $
                      read $
                        Text.unpack $
                          Text.tail tminTxOutId
              'V' ->
                Just $
                  VATxOutIdW $
                    Id.TxOutAddressId $
                      read $
                        Text.unpack $
                          Text.tail tminTxOutId
              _ -> Nothing

        mMaTxOutId =
          if Text.null tminMaTxOutId
            then Nothing
            else case Text.head tminMaTxOutId of
              'C' ->
                Just $
                  CMaTxOutIdW $
                    Id.MaTxOutCoreId $
                      read $
                        Text.unpack $
                          Text.tail tminMaTxOutId
              'V' ->
                Just $
                  VMaTxOutIdW $
                    Id.MaTxOutAddressId $
                      read $
                        Text.unpack $
                          Text.tail tminMaTxOutId
              _ -> Nothing

        minIds = MinIds mTxInId mTxOutId mMaTxOutId
       in
        case (txOutVariantType, typeId) of
          (TxOutVariantCore, "C") -> Just $ CMinIdsWrapper minIds
          (TxOutVariantAddress, "V") -> Just $ VMinIdsWrapper minIds
          _otherwise -> Nothing
    _otherwise -> Nothing

--------------------------------------------------------------------------------
-- CompleteMinId
--------------------------------------------------------------------------------
completeMinId ::
  (MonadIO m) =>
  Maybe Id.TxId ->
  MinIdsWrapper ->
  DbAction m MinIdsWrapper
completeMinId mTxId mIdW = case mIdW of
  CMinIdsWrapper minIds -> CMinIdsWrapper <$> completeMinIdCore mTxId minIds
  VMinIdsWrapper minIds -> VMinIdsWrapper <$> completeMinIdVariant mTxId minIds

completeMinIdCore :: MonadIO m => Maybe Id.TxId -> MinIds -> DbAction m MinIds
completeMinIdCore mTxId minIds = do
  case mTxId of
    Nothing -> pure mempty
    Just txId -> do
      mTxInId <-
        whenNothingQueryMinRefId @TxIn
          (minTxInId minIds)
          "tx_in_id"
          txId
          (Id.idEncoder Id.getTxId)
          (Id.idDecoder Id.TxInId)

      mTxOutId <-
        whenNothingQueryMinRefId @VC.TxOutCore
          (extractCoreTxOutId $ minTxOutId minIds)
          "tx_id"
          txId
          (Id.idEncoder Id.getTxId)
          (Id.idDecoder Id.TxOutCoreId)

      mMaTxOutId <- case mTxOutId of
        Nothing -> pure Nothing
        Just txOutId ->
          whenNothingQueryMinRefId @VC.MaTxOutCore
            (extractCoreMaTxOutId $ minMaTxOutId minIds)
            "tx_out_id"
            txOutId
            (Id.idEncoder Id.getTxOutCoreId)
            (Id.idDecoder Id.MaTxOutCoreId)

      pure $
        MinIds
          { minTxInId = mTxInId
          , minTxOutId = VCTxOutIdW <$> mTxOutId
          , minMaTxOutId = CMaTxOutIdW <$> mMaTxOutId
          }

completeMinIdVariant :: MonadIO m => Maybe Id.TxId -> MinIds -> DbAction m MinIds
completeMinIdVariant mTxId minIds = do
  case mTxId of
    Nothing -> pure mempty
    Just txId -> do
      mTxInId <-
        whenNothingQueryMinRefId @TxIn
          (minTxInId minIds)
          "tx_in_id"
          txId
          (Id.idEncoder Id.getTxId)
          (Id.idDecoder Id.TxInId)

      mTxOutId <-
        whenNothingQueryMinRefId @VA.TxOutAddress
          (extractVariantTxOutId $ minTxOutId minIds)
          "tx_id"
          txId
          (Id.idEncoder Id.getTxId)
          (Id.idDecoder Id.TxOutAddressId)

      mMaTxOutId <- case mTxOutId of
        Nothing -> pure Nothing
        Just txOutId ->
          whenNothingQueryMinRefId @VA.MaTxOutAddress
            (extractVariantMaTxOutId $ minMaTxOutId minIds)
            "tx_out_id"
            txOutId
            (Id.idEncoder Id.getTxOutAddressId)
            (Id.idDecoder Id.MaTxOutAddressId)

      pure $
        MinIds
          { minTxInId = mTxInId
          , minTxOutId = VATxOutIdW <$> mTxOutId
          , minMaTxOutId = VMaTxOutIdW <$> mMaTxOutId
          }

whenNothingQueryMinRefId ::
  forall a b m.
  (DbInfo a, MonadIO m) =>
  Maybe (Key a) -> -- Existing key value
  Text -> -- Field name
  b -> -- Value to compare
  HsqlE.Params b -> -- Encoder for value
  HsqlD.Row (Key a) -> -- Decoder for key
  DbAction m (Maybe (Key a))
whenNothingQueryMinRefId mKey fieldName value encoder keyDecoder =
  case mKey of
    Just k -> pure $ Just k
    Nothing -> queryMinRefId fieldName value encoder keyDecoder
