{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Insert.Grouped (
  BlockGroupedData (..),
  MissingMaTxOut (..),
  ExtendedTxIn (..),
  ExtendedTxOut (..),
  insertBlockGroupedData,
  insertReverseIndex,
  resolveTxInputs,
  resolveScriptHash,
) where

import Cardano.BM.Trace (Trace, logWarning)
import Cardano.Db (DbLovelace (..), minIdsToText, textShow)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (SyncEnv)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Shelley.Query
import Cardano.DbSync.Era.Util
import Cardano.DbSync.Error
import Cardano.Prelude
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.List as List
import qualified Data.Text as Text
import Database.Persist.Sql (SqlBackend)

-- | Group data within the same block, to insert them together in batches
--
-- important NOTE: Any queries (usually found in 'Cardano.DbSync.Era.Shelley.Query')
-- that touch these 5 tables (tx_out, tx_in, ma_tx_out, tx_metadata, ma_tx_mint) need to
-- have a fallback using this in memory structure. This is because
-- these tables are inserted in the db with a delay. 'resolveTxInputs' and
-- 'resolveScriptHash' are examples that fallback to this structure.
--
-- important NOTE: 'MaTxOut' is the only table referencing 'TxOut'. If any
-- other table references it in the future it has to be added here and delay its
-- insertion.
data BlockGroupedData = BlockGroupedData
  { groupedTxIn :: ![ExtendedTxIn]
  , groupedTxOut :: ![(ExtendedTxOut, [MissingMaTxOut])]
  , groupedTxMetadata :: ![DB.TxMetadata]
  , groupedTxMint :: ![DB.MaTxMint]
  , groupedTxFees :: !Word64
  , groupedTxOutSum :: !Word64
  }

-- | While we collect data, we don't have access yet to the 'TxOutId', since
-- it's inserted to the db later. So it's missing fields compared to DB.MaTxOut.
data MissingMaTxOut = MissingMaTxOut
  { mmtoIdent :: !DB.MultiAssetId
  , mmtoQuantity :: !DB.DbWord64
  }

-- | 'TxOut' with its TxHash. The hash is used to resolve inputs which
-- reference outputs that are not inserted to the db yet.
data ExtendedTxOut = ExtendedTxOut
  { etoTxHash :: !ByteString
  , etoTxOut :: !DB.TxOut
  }

data ExtendedTxIn = ExtendedTxIn
  { etiTxIn :: !DB.TxIn
  , etiTxOutId :: !(Either Generic.TxIn DB.TxOutId)
  }
  deriving (Show)

instance Monoid BlockGroupedData where
  mempty = BlockGroupedData [] [] [] [] 0 0

instance Semigroup BlockGroupedData where
  tgd1 <> tgd2 =
    BlockGroupedData
      (groupedTxIn tgd1 <> groupedTxIn tgd2)
      (groupedTxOut tgd1 <> groupedTxOut tgd2)
      (groupedTxMetadata tgd1 <> groupedTxMetadata tgd2)
      (groupedTxMint tgd1 <> groupedTxMint tgd2)
      (groupedTxFees tgd1 + groupedTxFees tgd2)
      (groupedTxOutSum tgd1 + groupedTxOutSum tgd2)

insertBlockGroupedData ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  BlockGroupedData ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) DB.MinIds
insertBlockGroupedData syncEnv grouped = do
  hasConsumed <- liftIO $ getHasConsumedOrPruneTxOut syncEnv
  txOutIds <- lift . DB.insertManyTxOutPlex hasConsumed $ etoTxOut . fst <$> groupedTxOut grouped
  let maTxOuts = concatMap mkmaTxOuts $ zip txOutIds (snd <$> groupedTxOut grouped)
  maTxOutIds <- lift $ DB.insertManyMaTxOut maTxOuts
  txInIds <- lift . DB.insertManyTxIn $ etiTxIn <$> groupedTxIn grouped
  whenConsumeOrPruneTxOut syncEnv $ do
    etis <- resolveRemainingInputs (groupedTxIn grouped) $ zip txOutIds (fst <$> groupedTxOut grouped)
    updateTuples <- lift $ mapM (prepareUpdates tracer) (zip txInIds etis)
    lift $ DB.updateListTxOutConsumedByTxInId $ catMaybes updateTuples
  void . lift . DB.insertManyTxMetadata $ groupedTxMetadata grouped
  void . lift . DB.insertManyTxMint $ groupedTxMint grouped
  pure $ DB.MinIds (minimumMaybe txInIds) (minimumMaybe txOutIds) (minimumMaybe maTxOutIds)
  where
    tracer = getTrace syncEnv

    mkmaTxOuts :: (DB.TxOutId, [MissingMaTxOut]) -> [DB.MaTxOut]
    mkmaTxOuts (txOutId, mmtos) = mkmaTxOut txOutId <$> mmtos

    mkmaTxOut :: DB.TxOutId -> MissingMaTxOut -> DB.MaTxOut
    mkmaTxOut txOutId missingMaTx =
      DB.MaTxOut
        { DB.maTxOutIdent = mmtoIdent missingMaTx
        , DB.maTxOutQuantity = mmtoQuantity missingMaTx
        , DB.maTxOutTxOutId = txOutId
        }

prepareUpdates ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  (DB.TxInId, ExtendedTxIn) ->
  m (Maybe (DB.TxOutId, DB.TxInId))
prepareUpdates trce (txInId, eti) = case etiTxOutId eti of
  Right txOutId -> pure $ Just (txOutId, txInId)
  Left _ -> do
    liftIO $ logWarning trce $ "Failed to find output for " <> Text.pack (show eti)
    pure Nothing

insertReverseIndex ::
  (MonadBaseControl IO m, MonadIO m) =>
  DB.BlockId ->
  DB.MinIds ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertReverseIndex blockId minIds =
  void . lift . DB.insertReverseIndex $
    DB.ReverseIndex
      { DB.reverseIndexBlockId = blockId
      , DB.reverseIndexMinIds = minIdsToText minIds
      }

-- | If we can't resolve from the db, we fall back to the provided outputs
-- This happens the input consumes an output introduced in the same block.
resolveTxInputs ::
  MonadIO m =>
  Bool ->
  Bool ->
  [ExtendedTxOut] ->
  Generic.TxIn ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) (Generic.TxIn, DB.TxId, Either Generic.TxIn DB.TxOutId, Maybe DbLovelace)
resolveTxInputs hasConsumed needsValue groupedOutputs txIn =
  liftLookupFail ("resolveTxInputs " <> textShow txIn <> " ") $ do
    qres <-
      case (hasConsumed, needsValue) of
        (_, True) -> fmap convertFoundAll <$> resolveInputTxOutIdValue txIn
        (False, _) -> fmap convertnotFound <$> resolveInputTxId txIn
        (True, False) -> fmap convertFoundTxOutId <$> resolveInputTxOutId txIn
    case qres of
      Right ret -> pure $ Right ret
      Left err ->
        case (resolveInMemory txIn groupedOutputs, hasConsumed, needsValue) of
          (Nothing, _, _) -> pure $ Left err
          (Just eutxo, True, True) -> pure $ Right $ convertFoundValue (DB.txOutTxId (etoTxOut eutxo), DB.txOutValue (etoTxOut eutxo))
          (Just eutxo, _, _) -> pure $ Right $ convertnotFound $ DB.txOutTxId (etoTxOut eutxo)
  where
    convertnotFound :: DB.TxId -> (Generic.TxIn, DB.TxId, Either Generic.TxIn DB.TxOutId, Maybe DbLovelace)
    convertnotFound txId = (txIn, txId, Left txIn, Nothing)

    convertFoundTxOutId :: (DB.TxId, DB.TxOutId) -> (Generic.TxIn, DB.TxId, Either Generic.TxIn DB.TxOutId, Maybe DbLovelace)
    convertFoundTxOutId (txId, txOutId) = (txIn, txId, Right txOutId, Nothing)

    convertFoundValue :: (DB.TxId, DbLovelace) -> (Generic.TxIn, DB.TxId, Either Generic.TxIn DB.TxOutId, Maybe DbLovelace)
    convertFoundValue (txId, lovelace) = (txIn, txId, Left txIn, Just lovelace)

    convertFoundAll :: (DB.TxId, DB.TxOutId, DbLovelace) -> (Generic.TxIn, DB.TxId, Either Generic.TxIn DB.TxOutId, Maybe DbLovelace)
    convertFoundAll (txId, txOutId, lovelace) = (txIn, txId, Right txOutId, Just lovelace)

resolveRemainingInputs ::
  MonadIO m =>
  [ExtendedTxIn] ->
  [(DB.TxOutId, ExtendedTxOut)] ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) [ExtendedTxIn]
resolveRemainingInputs etis mp =
  mapM f etis
  where
    f eti = case etiTxOutId eti of
      Right _ -> pure eti
      Left txIn
        | Just txOutId <- fst <$> find (matches txIn . snd) mp ->
            pure eti {etiTxOutId = Right txOutId}
      _ -> pure eti

resolveScriptHash ::
  (MonadBaseControl IO m, MonadIO m) =>
  [ExtendedTxOut] ->
  Generic.TxIn ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) (Maybe ByteString)
resolveScriptHash groupedOutputs txIn =
  liftLookupFail "resolveScriptHash" $ do
    qres <- fmap fst <$> queryResolveInputCredentials txIn
    case qres of
      Right ret -> pure $ Right ret
      Left err ->
        case resolveInMemory txIn groupedOutputs of
          Nothing -> pure $ Left err
          Just eutxo -> pure $ Right $ DB.txOutPaymentCred $ etoTxOut eutxo

resolveInMemory :: Generic.TxIn -> [ExtendedTxOut] -> Maybe ExtendedTxOut
resolveInMemory txIn =
  List.find (matches txIn)

matches :: Generic.TxIn -> ExtendedTxOut -> Bool
matches txIn eutxo =
  Generic.txInHash txIn == etoTxHash eutxo
    && Generic.txInIndex txIn == DB.txOutIndex (etoTxOut eutxo)

minimumMaybe :: (Ord a, Foldable f) => f a -> Maybe a
minimumMaybe xs
  | null xs = Nothing
  | otherwise = Just $ minimum xs
