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

import qualified Data.List as List
import qualified Data.Text as Text

import Cardano.BM.Trace (Trace, logWarning)
import Cardano.Db (DbLovelace (..), MinIds (..))
import qualified Cardano.Db as DB
import qualified Cardano.Db.Schema.Variants.TxOutAddress as VA
import qualified Cardano.Db.Schema.Variants.TxOutCore as VC
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncEnv (..), SyncOptions (..))
import Cardano.DbSync.Cache (queryTxIdWithCacheEither)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Shelley.Query
import Cardano.Prelude

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
  deriving (Show)

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
  MonadIO m =>
  SyncEnv ->
  BlockGroupedData ->
  DB.DbAction m DB.MinIdsWrapper
insertBlockGroupedData syncEnv grouped = do
  disInOut <- liftIO $ getDisableInOutState syncEnv
  txOutIds <- DB.insertBulkTxOut disInOut $ etoTxOut . fst <$> groupedTxOut grouped
  let maTxOuts = concatMap (mkmaTxOuts txOutVariantType) $ zip txOutIds (snd <$> groupedTxOut grouped)
  maTxOutIds <- DB.insertBulkMaTxOut maTxOuts
  txInIds <-
    if getSkipTxIn syncEnv
      then pure []
      else DB.insertBulkTxIn $ etiTxIn <$> groupedTxIn grouped
  whenConsumeOrPruneTxOut syncEnv $ do
    etis <- resolveRemainingInputs (groupedTxIn grouped) $ zip txOutIds (fst <$> groupedTxOut grouped)
    updateTuples <- mapM (prepareUpdates tracer) etis
    DB.updateListTxOutConsumedByTxId $ catMaybes updateTuples
  void . DB.insertBulkTxMetadata removeJsonbFromSchema $ groupedTxMetadata grouped
  void . DB.insertBulkMaTxMint $ groupedTxMint grouped
  pure $ makeMinId txInIds txOutIds maTxOutIds
  where
    tracer = getTrace syncEnv
    txOutVariantType = getTxOutVariantType syncEnv
    removeJsonbFromSchema = ioRemoveJsonbFromSchema $ soptInsertOptions $ envOptions syncEnv

    makeMinId :: [DB.TxInId] -> [DB.TxOutIdW] -> [DB.MaTxOutIdW] -> DB.MinIdsWrapper
    makeMinId txInIds txOutIds maTxOutIds =
      case txOutVariantType of
        DB.TxOutVariantCore -> do
          DB.CMinIdsWrapper $
            DB.MinIds
              { minTxInId = listToMaybe txInIds
              , minTxOutId = listToMaybe txOutIds
              , minMaTxOutId = listToMaybe maTxOutIds
              }
        DB.TxOutVariantAddress ->
          DB.VMinIdsWrapper $
            DB.MinIds
              { minTxInId = listToMaybe txInIds
              , minTxOutId = listToMaybe txOutIds
              , minMaTxOutId = listToMaybe maTxOutIds
              }

mkmaTxOuts :: DB.TxOutVariantType -> (DB.TxOutIdW, [MissingMaTxOut]) -> [DB.MaTxOutW]
mkmaTxOuts _txOutVariantType (txOutId, mmtos) = mkmaTxOut <$> mmtos
  where
    mkmaTxOut :: MissingMaTxOut -> DB.MaTxOutW
    mkmaTxOut missingMaTx =
      case txOutId of
        DB.VCTxOutIdW txOutId' ->
          DB.CMaTxOutW $
            VC.MaTxOutCore
              { VC.maTxOutCoreIdent = mmtoIdent missingMaTx
              , VC.maTxOutCoreQuantity = mmtoQuantity missingMaTx
              , VC.maTxOutCoreTxOutId = txOutId'
              }
        DB.VATxOutIdW txOutId' ->
          DB.VMaTxOutW
            VA.MaTxOutAddress
              { VA.maTxOutAddressIdent = mmtoIdent missingMaTx
              , VA.maTxOutAddressQuantity = mmtoQuantity missingMaTx
              , VA.maTxOutAddressTxOutId = txOutId'
              }

prepareUpdates ::
  MonadIO m =>
  Trace IO Text ->
  ExtendedTxIn ->
  m (Maybe (DB.TxOutIdW, DB.TxId))
prepareUpdates trce eti = case etiTxOutId eti of
  Right txOutId -> pure $ Just (txOutId, DB.txInTxInId (etiTxIn eti))
  Left _ -> do
    liftIO $ logWarning trce $ "Failed to find output for " <> Text.pack (show eti)
    pure Nothing

insertReverseIndex ::
  MonadIO m =>
  DB.BlockId ->
  DB.MinIdsWrapper ->
  DB.DbAction m ()
insertReverseIndex blockId minIdsWrapper =
  case minIdsWrapper of
    DB.CMinIdsWrapper minIds ->
      void $
        DB.insertReverseIndex $
          DB.ReverseIndex
            { DB.reverseIndexBlockId = blockId
            , DB.reverseIndexMinIds = DB.minIdsCoreToText minIds
            }
    DB.VMinIdsWrapper minIds ->
      void $
        DB.insertReverseIndex $
          DB.ReverseIndex
            { DB.reverseIndexBlockId = blockId
            , DB.reverseIndexMinIds = DB.minIdsAddressToText minIds
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
  DB.DbAction m (Generic.TxIn, DB.TxId, Either Generic.TxIn DB.TxOutIdW, Maybe DbLovelace)
resolveTxInputs syncEnv hasConsumed needsValue groupedOutputs txIn = do
  qres <-
    case (hasConsumed, needsValue) of
      (_, True) -> fmap convertFoundAll <$> resolveInputTxOutIdValueEither syncEnv txIn
      (False, _) -> fmap convertnotFoundCache <$> queryTxIdWithCacheEither (envCache syncEnv) (Generic.txInTxId txIn)
      (True, False) -> fmap convertFoundTxOutId <$> resolveInputTxOutIdEither syncEnv txIn
  case qres of
    Right result -> pure result
    Left _dbErr ->
      -- The key insight: Don't throw immediately, try in-memory resolution first
      case (resolveInMemory txIn groupedOutputs, hasConsumed, needsValue) of
        (Nothing, _, _) ->
          -- Only throw if in-memory resolution also fails
          throwError $
            DB.DbError
              (DB.mkDbCallStack "resolveTxInputs")
              ("TxOut not found for TxIn: " <> textShow txIn)
              Nothing
        (Just eutxo, True, True) ->
          pure $ convertFoundValue (etoTxOut eutxo)
        (Just eutxo, _, _) ->
          pure $ convertnotFound (etoTxOut eutxo)
  where
    convertnotFoundCache :: DB.TxId -> (Generic.TxIn, DB.TxId, Either Generic.TxIn DB.TxOutIdW, Maybe DbLovelace)
    convertnotFoundCache txId = (txIn, txId, Left txIn, Nothing)

    convertFoundTxOutId :: (DB.TxId, DB.TxOutIdW) -> (Generic.TxIn, DB.TxId, Either Generic.TxIn DB.TxOutIdW, Maybe DbLovelace)
    convertFoundTxOutId (txId, txOutId) = (txIn, txId, Right txOutId, Nothing)

    convertFoundValue :: DB.TxOutW -> (Generic.TxIn, DB.TxId, Either Generic.TxIn DB.TxOutIdW, Maybe DbLovelace)
    convertFoundValue txOutWrapper = case txOutWrapper of
      DB.VCTxOutW cTxOut -> (txIn, VC.txOutCoreTxId cTxOut, Left txIn, Just $ VC.txOutCoreValue cTxOut)
      DB.VATxOutW vTxOut _ -> (txIn, VA.txOutAddressTxId vTxOut, Left txIn, Just $ VA.txOutAddressValue vTxOut)

    convertFoundAll :: (DB.TxId, DB.TxOutIdW, DbLovelace) -> (Generic.TxIn, DB.TxId, Either Generic.TxIn DB.TxOutIdW, Maybe DbLovelace)
    convertFoundAll (txId, txOutId, lovelace) = (txIn, txId, Right txOutId, Just lovelace)

    convertnotFound :: DB.TxOutW -> (Generic.TxIn, DB.TxId, Either Generic.TxIn DB.TxOutIdW, Maybe DbLovelace)
    convertnotFound txOutWrapper = case txOutWrapper of
      DB.VCTxOutW cTxOut -> (txIn, VC.txOutCoreTxId cTxOut, Left txIn, Nothing)
      DB.VATxOutW vTxOut _ -> (txIn, VA.txOutAddressTxId vTxOut, Left txIn, Nothing)

resolveRemainingInputs ::
  MonadIO m =>
  [ExtendedTxIn] ->
  [(DB.TxOutIdW, ExtendedTxOut)] ->
  DB.DbAction m [ExtendedTxIn]
resolveRemainingInputs etis mp =
  mapM f etis
  where
    f eti = case etiTxOutId eti of
      Right _ -> pure eti
      Left txIn
        | Just txOutId <- fst <$> find (matches txIn . snd) mp ->
            pure eti {etiTxOutId = Right txOutId}
      _otherwise -> pure eti

resolveScriptHash ::
  MonadIO m =>
  SyncEnv ->
  [ExtendedTxOut] ->
  Generic.TxIn ->
  DB.DbAction m (Maybe ByteString)
resolveScriptHash syncEnv groupedOutputs txIn = do
  qres <- queryResolveInputCredentials syncEnv txIn
  case qres of
    Just ret -> pure $ Just ret
    Nothing ->
      case resolveInMemory txIn groupedOutputs of
        Nothing -> throwError $ DB.DbError (DB.mkDbCallStack "resolveScriptHash") "resolveInMemory: VATxOutW with Nothing address" Nothing
        Just eutxo -> case etoTxOut eutxo of
          DB.VCTxOutW cTxOut -> pure $ VC.txOutCorePaymentCred cTxOut
          DB.VATxOutW _ vAddress -> case vAddress of
            Nothing -> throwError $ DB.DbError (DB.mkDbCallStack "resolveScriptHash") "VATxOutW with Nothing address" Nothing
            Just vAddr -> pure $ VA.addressPaymentCred vAddr

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
      DB.VCTxOutW cTxOut -> VC.txOutCoreIndex cTxOut
      DB.VATxOutW vTxOut _ -> VA.txOutAddressIndex vTxOut
