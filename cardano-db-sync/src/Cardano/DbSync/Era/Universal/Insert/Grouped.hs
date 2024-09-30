{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Universal.Insert.Grouped (
  BlockGroupedData (..),
  MissingMaTxOut (..),
  ExtendedTxIn (..),
  ExtendedTxOut (..),
  insertBlockGroupedData,
  insertReverseIndex,
  resolveTxInputs,
  resolveScriptHash,
  mkmaTxOuts,
) where

import Cardano.BM.Trace (Trace, logWarning)
import Cardano.Db (DbLovelace (..), MinIds (..), minIdsCoreToText, minIdsVariantToText)
import qualified Cardano.Db as DB
import qualified Cardano.Db.Schema.Core.TxOut as C
import qualified Cardano.Db.Schema.Variant.TxOut as V
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (SyncEnv (..))
import Cardano.DbSync.Cache (queryTxIdWithCache)
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
  , etoTxOut :: !DB.TxOutW
  }

data ExtendedTxIn = ExtendedTxIn
  { etiTxIn :: !DB.TxIn
  , etiTxOutId :: !(Either Generic.TxIn DB.TxOutIdW)
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
  ExceptT SyncNodeError (ReaderT SqlBackend m) DB.MinIdsWrapper
insertBlockGroupedData syncEnv grouped = do
  disInOut <- liftIO $ getDisableInOutState syncEnv
  txOutIds <- lift . DB.insertManyTxOut disInOut $ etoTxOut . fst <$> groupedTxOut grouped
  let maTxOuts = concatMap (mkmaTxOuts txOutTableType) $ zip txOutIds (snd <$> groupedTxOut grouped)
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
  pure $ makeMinId txInIds txOutIds maTxOutIds
  where
    tracer = getTrace syncEnv
    txOutTableType = getTxOutTableType syncEnv

    makeMinId :: [DB.TxInId] -> [DB.TxOutIdW] -> [DB.MaTxOutIdW] -> DB.MinIdsWrapper
    makeMinId txInIds txOutIds maTxOutIds =
      case txOutTableType of
        DB.TxOutCore -> do
          DB.CMinIdsWrapper $
            DB.MinIds
              { minTxInId = listToMaybe txInIds
              , minTxOutId = listToMaybe $ DB.convertTxOutIdCore txOutIds
              , minMaTxOutId = listToMaybe $ DB.convertMaTxOutIdCore maTxOutIds
              }
        DB.TxOutVariantAddress ->
          DB.VMinIdsWrapper $
            DB.MinIds
              { minTxInId = listToMaybe txInIds
              , minTxOutId = listToMaybe $ DB.convertTxOutIdVariant txOutIds
              , minMaTxOutId = listToMaybe $ DB.convertMaTxOutIdVariant maTxOutIds
              }

mkmaTxOuts :: DB.TxOutTableType -> (DB.TxOutIdW, [MissingMaTxOut]) -> [DB.MaTxOutW]
mkmaTxOuts _txOutTableType (txOutId, mmtos) = mkmaTxOut <$> mmtos
  where
    mkmaTxOut :: MissingMaTxOut -> DB.MaTxOutW
    mkmaTxOut missingMaTx =
      case txOutId of
        DB.CTxOutIdW txOutId' ->
          DB.CMaTxOutW $
            C.MaTxOut
              { C.maTxOutIdent = mmtoIdent missingMaTx
              , C.maTxOutQuantity = mmtoQuantity missingMaTx
              , C.maTxOutTxOutId = txOutId'
              }
        DB.VTxOutIdW txOutId' ->
          DB.VMaTxOutW
            V.MaTxOut
              { V.maTxOutIdent = mmtoIdent missingMaTx
              , V.maTxOutQuantity = mmtoQuantity missingMaTx
              , V.maTxOutTxOutId = txOutId'
              }

prepareUpdates ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  ExtendedTxIn ->
  m (Maybe (DB.TxOutIdW, DB.TxId))
prepareUpdates trce eti = case etiTxOutId eti of
  Right txOutId -> pure $ Just (txOutId, DB.txInTxInId (etiTxIn eti))
  Left _ -> do
    liftIO $ logWarning trce $ "Failed to find output for " <> Text.pack (show eti)
    pure Nothing

insertReverseIndex ::
  (MonadBaseControl IO m, MonadIO m) =>
  DB.BlockId ->
  DB.MinIdsWrapper ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertReverseIndex blockId minIdsWrapper =
  case minIdsWrapper of
    DB.CMinIdsWrapper minIds ->
      void . lift . DB.insertReverseIndex $
        DB.ReverseIndex
          { DB.reverseIndexBlockId = blockId
          , DB.reverseIndexMinIds = minIdsCoreToText minIds
          }
    DB.VMinIdsWrapper minIds ->
      void . lift . DB.insertReverseIndex $
        DB.ReverseIndex
          { DB.reverseIndexBlockId = blockId
          , DB.reverseIndexMinIds = minIdsVariantToText minIds
          }

-- | If we can't resolve from the db, we fall back to the provided outputs
-- This happens the input consumes an output introduced in the same block.
resolveTxInputs ::
  MonadIO m =>
  SyncEnv ->
  Bool ->
  Bool ->
  [ExtendedTxOut] ->
  Generic.TxIn ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) (Generic.TxIn, DB.TxId, Either Generic.TxIn DB.TxOutIdW, Maybe DbLovelace)
resolveTxInputs syncEnv hasConsumed needsValue groupedOutputs txIn =
  liftLookupFail ("resolveTxInputs " <> textShow txIn <> " ") $ do
    qres <-
      case (hasConsumed, needsValue) of
        (_, True) -> fmap convertFoundAll <$> resolveInputTxOutIdValue syncEnv txIn
        (False, _) -> fmap convertnotFoundCache <$> queryTxIdWithCache (envCache syncEnv) (Generic.txInTxId txIn)
        (True, False) -> fmap convertFoundTxOutId <$> resolveInputTxOutId syncEnv txIn
    case qres of
      Right ret -> pure $ Right ret
      Left err ->
        case (resolveInMemory txIn groupedOutputs, hasConsumed, needsValue) of
          (Nothing, _, _) -> pure $ Left err
          (Just eutxo, True, True) -> pure $ Right $ convertFoundValue (etoTxOut eutxo)
          (Just eutxo, _, _) -> pure $ Right $ convertnotFound (etoTxOut eutxo)
  where
    convertnotFoundCache :: DB.TxId -> (Generic.TxIn, DB.TxId, Either Generic.TxIn DB.TxOutIdW, Maybe DbLovelace)
    convertnotFoundCache txId = (txIn, txId, Left txIn, Nothing)

    convertnotFound :: DB.TxOutW -> (Generic.TxIn, DB.TxId, Either Generic.TxIn DB.TxOutIdW, Maybe DbLovelace)
    convertnotFound txOutWrapper = case txOutWrapper of
      DB.CTxOutW cTxOut -> (txIn, C.txOutTxId cTxOut, Left txIn, Nothing)
      DB.VTxOutW vTxOut _ -> (txIn, V.txOutTxId vTxOut, Left txIn, Nothing)

    convertFoundTxOutId :: (DB.TxId, DB.TxOutIdW) -> (Generic.TxIn, DB.TxId, Either Generic.TxIn DB.TxOutIdW, Maybe DbLovelace)
    convertFoundTxOutId (txId, txOutId) = (txIn, txId, Right txOutId, Nothing)

    -- convertFoundValue :: (DB.TxId, DbLovelace) -> (Generic.TxIn, DB.TxId, Either Generic.TxIn DB.TxOutIdW, Maybe DbLovelace)
    convertFoundValue :: DB.TxOutW -> (Generic.TxIn, DB.TxId, Either Generic.TxIn DB.TxOutIdW, Maybe DbLovelace)
    convertFoundValue txOutWrapper = case txOutWrapper of
      DB.CTxOutW cTxOut -> (txIn, C.txOutTxId cTxOut, Left txIn, Just $ C.txOutValue cTxOut)
      DB.VTxOutW vTxOut _ -> (txIn, V.txOutTxId vTxOut, Left txIn, Just $ V.txOutValue vTxOut)
    -- (txIn, txId, Left txIn, Just lovelace)

    convertFoundAll :: (DB.TxId, DB.TxOutIdW, DbLovelace) -> (Generic.TxIn, DB.TxId, Either Generic.TxIn DB.TxOutIdW, Maybe DbLovelace)
    convertFoundAll (txId, txOutId, lovelace) = (txIn, txId, Right txOutId, Just lovelace)

resolveRemainingInputs ::
  MonadIO m =>
  [ExtendedTxIn] ->
  [(DB.TxOutIdW, ExtendedTxOut)] ->
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
  SyncEnv ->
  [ExtendedTxOut] ->
  Generic.TxIn ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) (Maybe ByteString)
resolveScriptHash syncEnv groupedOutputs txIn =
  liftLookupFail "resolveScriptHash" $ do
    qres <- fmap fst <$> queryResolveInputCredentials syncEnv txIn
    case qres of
      Right ret -> pure $ Right ret
      Left err ->
        case resolveInMemory txIn groupedOutputs of
          Nothing -> pure $ Left err
          Just eutxo -> case etoTxOut eutxo of
            DB.CTxOutW cTxOut -> pure $ Right $ C.txOutPaymentCred cTxOut
            DB.VTxOutW _ vAddress -> case vAddress of
              Nothing -> pure $ Left $ DB.DBTxOutVariant "resolveScriptHash: VTxOutW with Nothing address"
              Just vAddr -> pure $ Right $ V.addressPaymentCred vAddr

resolveInMemory :: Generic.TxIn -> [ExtendedTxOut] -> Maybe ExtendedTxOut
resolveInMemory txIn =
  List.find (matches txIn)

matches :: Generic.TxIn -> ExtendedTxOut -> Bool
matches txIn eutxo =
  Generic.toTxHash txIn == etoTxHash eutxo
    && Generic.txInIndex txIn == getTxOutIndex (etoTxOut eutxo)
  where
    getTxOutIndex :: DB.TxOutW -> Word64
    getTxOutIndex txOutWrapper = case txOutWrapper of
      DB.CTxOutW cTxOut -> C.txOutIndex cTxOut
      DB.VTxOutW vTxOut _ -> V.txOutIndex vTxOut
