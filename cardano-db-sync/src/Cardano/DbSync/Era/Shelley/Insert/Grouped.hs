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
  resolveTxInputsValue,
  resolveScriptHash,
  resolveInMemory,
  resolveInMemoryMany,
  mkmaTxOuts,
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
import Cardano.Ledger.Coin (Coin (..))
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
  mempty = BlockGroupedData [] [] [] []

instance Semigroup BlockGroupedData where
  tgd1 <> tgd2 =
    BlockGroupedData
      (groupedTxIn tgd1 <> groupedTxIn tgd2)
      (groupedTxOut tgd1 <> groupedTxOut tgd2)
      (groupedTxMetadata tgd1 <> groupedTxMetadata tgd2)
      (groupedTxMint tgd1 <> groupedTxMint tgd2)

insertBlockGroupedData ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  BlockGroupedData ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) DB.MinIds
insertBlockGroupedData syncEnv grouped = do
  disInOut <- liftIO $ getDisableInOutState syncEnv
  txOutIds <- lift . DB.insertManyTxOutPlex (getHasConsumedOrPruneTxOut syncEnv) disInOut $ etoTxOut . fst <$> groupedTxOut grouped
  let maTxOuts = concatMap mkmaTxOuts $ zip txOutIds (snd <$> groupedTxOut grouped)
  maTxOutIds <- lift $ DB.insertManyMaTxOut maTxOuts
  txInIds <-
    if getSkipTxIn syncEnv
      then pure []
      else lift . DB.insertManyTxIn $ etiTxIn <$> groupedTxIn grouped
  whenConsumeOrPruneTxOut syncEnv $ do
    etis <- resolveRemainingInputs (groupedTxIn grouped) $ zip txOutIds (fst <$> groupedTxOut grouped)
    updateTuples <- lift $ mapM (prepareUpdates tracer) etis
    lift $ DB.updateListTxOutConsumedByTxId $ catMaybes updateTuples
  void . lift . DB.insertManyTxMetadata $ groupedTxMetadata grouped
  void . lift . DB.insertManyTxMint $ groupedTxMint grouped
  pure $ DB.MinIds (minimumMaybe txInIds) (minimumMaybe txOutIds) (minimumMaybe maTxOutIds)
  where
    tracer = getTrace syncEnv

mkmaTxOuts :: (DB.TxOutId, [MissingMaTxOut]) -> [DB.MaTxOut]
mkmaTxOuts (txOutId, mmtos) = mkmaTxOut <$> mmtos
  where
    mkmaTxOut :: MissingMaTxOut -> DB.MaTxOut
    mkmaTxOut missingMaTx =
      DB.MaTxOut
        { DB.maTxOutIdent = mmtoIdent missingMaTx
        , DB.maTxOutQuantity = mmtoQuantity missingMaTx
        , DB.maTxOutTxOutId = txOutId
        }

prepareUpdates ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  ExtendedTxIn ->
  m (Maybe (DB.TxOutId, DB.TxId))
prepareUpdates trce eti = case etiTxOutId eti of
  Right txOutId -> pure $ Just (txOutId, DB.txInTxInId (etiTxIn eti))
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

-- | If we can't resolve from the db, we return nothing.
-- This happens the input consumes an output introduced in the same block.
resolveTxInputs ::
  MonadIO m =>
  Bool ->
  Generic.TxIn ->
  ReaderT SqlBackend m (Generic.TxIn, Maybe DB.TxId, Either Generic.TxIn DB.TxOutId)
resolveTxInputs hasConsumed txIn = do
    qres <-
      if hasConsumed
        then fmap convertFoundTxOutId <$> resolveInputTxOutId txIn
        else fmap convertFoundTxId <$> resolveInputTxId txIn
    case qres of
      Right ret -> pure ret
      Left _ -> pure foundNothing
  where
    convertFoundTxId :: DB.TxId -> (Generic.TxIn, Maybe DB.TxId, Either Generic.TxIn DB.TxOutId)
    convertFoundTxId txId = (txIn, Just txId, Left txIn)

    convertFoundTxOutId :: (DB.TxId, DB.TxOutId) -> (Generic.TxIn, Maybe DB.TxId, Either Generic.TxIn DB.TxOutId)
    convertFoundTxOutId (txId, txOutId) = (txIn, Just txId, Right txOutId)

    foundNothing :: (Generic.TxIn, Maybe DB.TxId, Either Generic.TxIn DB.TxOutId)
    foundNothing = (txIn, Nothing, Left txIn)

-- | If we can't resolve from the db, we fall back to the provided outputs
-- This happens the input consumes an output introduced in the same block.
resolveTxInputsValue ::
  MonadIO m =>
  [[ExtendedTxOut]] ->
  [(ByteString, Generic.TxOut)] ->
  Generic.TxIn ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) (Generic.TxIn, Maybe DB.TxId, Either Generic.TxIn DB.TxOutId, Maybe DbLovelace)
resolveTxInputsValue txOutPrev blockTxOuts txIn =
  liftLookupFail ("resolveTxInputsValue " <> textShow txIn <> " ") $ do
    qres <- fmap convertFoundAll <$> resolveInputTxOutIdValue txIn
    case qres of
      Right ret -> pure $ Right ret
      Left err ->
        case resolveInMemory' txIn blockTxOuts of
          Just txOut -> pure $ Right $ convertFoundValue $ DB.DbLovelace $ fromIntegral $ unCoin $ Generic.txOutAdaValue txOut
          Nothing -> case resolveInMemoryMany txIn txOutPrev of
            Nothing -> pure $ Left err
            Just txOut -> pure $ Right $ convertFoundValue $ DB.txOutValue $ etoTxOut txOut
  where
    convertFoundAll :: (DB.TxId, DB.TxOutId, DbLovelace) -> (Generic.TxIn, Maybe DB.TxId, Either Generic.TxIn DB.TxOutId, Maybe DbLovelace)
    convertFoundAll (txId, txOutId, lovelace) = (txIn, Just txId, Right txOutId, Just lovelace)

    convertFoundValue :: DbLovelace -> (Generic.TxIn, Maybe DB.TxId, Either Generic.TxIn DB.TxOutId, Maybe DbLovelace)
    convertFoundValue lovelace = (txIn, Nothing, Left txIn, Just lovelace)

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
  [[ExtendedTxOut]] ->
  Generic.TxIn ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) (Maybe ByteString)
resolveScriptHash groupedOutputs txIn =
  liftLookupFail "resolveScriptHash" $ do
    qres <- fmap fst <$> queryResolveInputCredentials txIn
    case qres of
      Right ret -> pure $ Right ret
      Left err ->
        case resolveInMemoryMany txIn groupedOutputs of
          Nothing -> pure $ Left err
          Just eutxo -> pure $ Right $ DB.txOutPaymentCred $ etoTxOut eutxo

resolveInMemory :: Generic.TxIn -> [ExtendedTxOut] -> Maybe ExtendedTxOut
resolveInMemory txIn =
  List.find (matches txIn)

matches :: Generic.TxIn -> ExtendedTxOut -> Bool
matches txIn eutxo =
  Generic.txInHash txIn == etoTxHash eutxo
    && Generic.txInIndex txIn == DB.txOutIndex (etoTxOut eutxo)

resolveInMemory' :: Generic.TxIn -> [(ByteString, Generic.TxOut)] -> Maybe Generic.TxOut
resolveInMemory' txIn txOuts =
  snd <$> List.find (matches' txIn) txOuts

matches' :: Generic.TxIn -> (ByteString, Generic.TxOut) -> Bool
matches' txIn (txHash, txOut) =
  Generic.txInHash txIn == txHash
    && Generic.txInIndex txIn == Generic.txOutIndex txOut

resolveInMemoryMany :: Generic.TxIn -> [[ExtendedTxOut]] -> Maybe ExtendedTxOut
resolveInMemoryMany txIn =
  findMapMaybe (resolveInMemory txIn)
  where
    findMapMaybe :: (a -> Maybe b) -> [a] -> Maybe b
    findMapMaybe f = go
      where
        go [] = Nothing
        go (a : as) = case f a of
          Nothing -> go as
          Just b -> Just b

minimumMaybe :: (Ord a, Foldable f) => f a -> Maybe a
minimumMaybe xs
  | null xs = Nothing
  | otherwise = Just $ minimum xs
