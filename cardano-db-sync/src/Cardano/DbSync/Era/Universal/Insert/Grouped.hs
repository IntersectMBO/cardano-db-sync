{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
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

import Cardano.BM.Trace (logWarning)
import Cardano.Db (DbLovelace (..), MinIds (..))
import qualified Cardano.Db as DB
import qualified Cardano.Db.Schema.Variants.TxOutAddress as VA
import qualified Cardano.Db.Schema.Variants.TxOutCore as VC
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncEnv (..), SyncOptions (..))
import Cardano.DbSync.Cache (queryTxIdWithCache)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Shelley.Generic.Util (unTxHash)
import Cardano.DbSync.Era.Shelley.Query
import Cardano.DbSync.Error (SyncNodeError (..), mkSyncNodeCallStack)
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

-- | Parallel implementation with single connection coordination
insertBlockGroupedData ::
  SyncEnv ->
  BlockGroupedData ->
  ExceptT SyncNodeError DB.DbM DB.MinIdsWrapper
insertBlockGroupedData syncEnv grouped = do
  disInOut <- liftIO $ getDisableInOutState syncEnv

  -- Parallel preparation of independent data
  (preparedTxIn, preparedMetadata, preparedMint, txOutChunks) <- liftIO $ do
    a1 <- async $ pure $ prepareTxInProcessing syncEnv grouped
    a2 <- async $ pure $ prepareMetadataProcessing syncEnv grouped
    a3 <- async $ pure $ prepareMintProcessing syncEnv grouped
    a4 <- async $ do
      let txOutData = etoTxOut . fst <$> groupedTxOut grouped
          bulkSize = DB.getTxOutBulkSize (getTxOutVariantType syncEnv)
      pure $ DB.chunkForBulkQueryWith bulkSize txOutData

    r1 <- wait a1
    r2 <- wait a2
    r3 <- wait a3
    r4 <- wait a4
    pure (r1, r2, r3, r4)
  -- Sequential TxOut processing (generates required IDs)
  txOutIds <- concat <$> mapM (lift . DB.insertBulkTxOut disInOut) txOutChunks
  -- Execute independent operations (TxIn, Metadata, Mint) in parallel
  txInIds <- executePreparedTxInPiped preparedTxIn
  -- TxOut-dependent operations (MaTxOut + UTxO consumption)
  maTxOutIds <- processMaTxOuts syncEnv txOutIds grouped
  executePreparedMetadataPiped preparedMetadata
  executePreparedMintPiped preparedMint

  -- Process UTxO consumption (depends on txOutIds)
  processUtxoConsumption syncEnv grouped txOutIds

  pure $ makeMinId syncEnv txInIds txOutIds maTxOutIds

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

insertReverseIndex ::
  DB.BlockId ->
  DB.MinIdsWrapper ->
  ExceptT SyncNodeError DB.DbM ()
insertReverseIndex blockId minIdsWrapper =
  case minIdsWrapper of
    DB.CMinIdsWrapper minIds ->
      void $
        lift $
          DB.insertReverseIndex $
            DB.ReverseIndex
              { DB.reverseIndexBlockId = blockId
              , DB.reverseIndexMinIds = DB.minIdsCoreToText minIds
              }
    DB.VMinIdsWrapper minIds ->
      void $
        lift $
          DB.insertReverseIndex $
            DB.ReverseIndex
              { DB.reverseIndexBlockId = blockId
              , DB.reverseIndexMinIds = DB.minIdsAddressToText minIds
              }

-- | If we can't resolve from the db, we fall back to the provided outputs
-- This happens the input consumes an output introduced in the same block.
resolveTxInputs ::
  SyncEnv ->
  Bool ->
  Bool ->
  [ExtendedTxOut] ->
  Generic.TxIn ->
  ExceptT SyncNodeError DB.DbM (Generic.TxIn, DB.TxId, Either Generic.TxIn DB.TxOutIdW, Maybe DbLovelace)
resolveTxInputs syncEnv hasConsumed needsValue groupedOutputs txIn = do
  qres <- case (hasConsumed, needsValue) of
    -- No cache (complex query)
    (_, True) -> fmap convertFoundAll <$> resolveInputTxOutIdValue syncEnv txIn
    -- Direct query (simple case)
    (False, _) -> do
      mTxId <- lift $ DB.queryTxId (Generic.unTxHash $ Generic.txInTxId txIn)
      case mTxId of
        Just txId -> pure $ Right $ convertnotFoundCache txId
        Nothing ->
          throwError $
            SNErrDefault
              mkSyncNodeCallStack
              ("TxId not found for hash: " <> show (Generic.unTxHash $ Generic.txInTxId txIn))
    (True, False) -> do
      -- Consumed mode use cache
      eTxId <- queryTxIdWithCache syncEnv (Generic.txInTxId txIn)
      case eTxId of
        Right txId -> do
          -- Now get the TxOutId separately
          let txOutVariantType = getTxOutVariantType syncEnv
          eTxOutId <- lift $ DB.resolveInputTxOutIdFromTxId txOutVariantType txId (Generic.txInIndex txIn)
          case eTxOutId of
            Right txOutId -> pure $ Right $ convertFoundTxOutId (txId, txOutId)
            Left err -> pure $ Left err
        Left err -> pure $ Left err
  case qres of
    Right result -> pure result
    Left _dbErr ->
      -- Don't throw immediately, try in-memory resolution first
      case (resolveInMemory txIn groupedOutputs, hasConsumed, needsValue) of
        (Nothing, _, _) ->
          -- Only throw if in-memory resolution also fails
          throwError $
            SNErrDefault
              mkSyncNodeCallStack
              ("TxIn not found in memory: " <> textShow txIn)
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
  [ExtendedTxIn] ->
  [(DB.TxOutIdW, ExtendedTxOut)] ->
  ExceptT SyncNodeError DB.DbM [ExtendedTxIn]
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
  SyncEnv ->
  [ExtendedTxOut] ->
  Generic.TxIn ->
  ExceptT SyncNodeError DB.DbM (Maybe ByteString)
resolveScriptHash syncEnv groupedOutputs txIn = do
  qres <- queryResolveInputCredentials syncEnv txIn
  case qres of
    Just ret -> pure $ Just ret
    Nothing ->
      case resolveInMemory txIn groupedOutputs of
        Nothing -> throwError $ SNErrDefault mkSyncNodeCallStack "resolveInMemory: VATxOutW with Nothing address"
        Just eutxo -> case etoTxOut eutxo of
          DB.VCTxOutW cTxOut -> pure $ VC.txOutCorePaymentCred cTxOut
          DB.VATxOutW _ vAddress -> case vAddress of
            Nothing -> throwError $ SNErrDefault mkSyncNodeCallStack "VATxOutW with Nothing address"
            Just vAddr -> pure $ VA.addressPaymentCred vAddr

resolveInMemory :: Generic.TxIn -> [ExtendedTxOut] -> Maybe ExtendedTxOut
resolveInMemory txIn =
  List.find (matches txIn)

matches :: Generic.TxIn -> ExtendedTxOut -> Bool
matches txIn eutxo =
  Generic.toTxHash txIn
    == etoTxHash eutxo
    && Generic.txInIndex txIn
      == getTxOutIndex (etoTxOut eutxo)
  where
    getTxOutIndex :: DB.TxOutW -> Word64
    getTxOutIndex txOutWrapper = case txOutWrapper of
      DB.VCTxOutW cTxOut -> VC.txOutCoreIndex cTxOut
      DB.VATxOutW vTxOut _ -> VA.txOutAddressIndex vTxOut

-----------------------------------------------------------------------------------------------------------------------------------
-- PARALLEL PROCESSING HELPER FUNCTIONS
-----------------------------------------------------------------------------------------------------------------------------------

-- | Prepared TxIn data for async execution
data PreparedTxIn = PreparedTxIn
  { ptiChunks :: ![[DB.TxIn]]
  , ptiSkip :: !Bool
  }

-- | Prepared Metadata data for async execution
data PreparedMetadata = PreparedMetadata
  { pmChunks :: ![[DB.TxMetadata]]
  , pmRemoveJsonb :: !Bool
  }

-- | Prepared Mint data for async execution
data PreparedMint = PreparedMint
  { pmtChunks :: ![[DB.MaTxMint]]
  }

-- | Prepare TxIn processing (can run in parallel with TxOut)
prepareTxInProcessing :: SyncEnv -> BlockGroupedData -> PreparedTxIn
prepareTxInProcessing syncEnv grouped =
  PreparedTxIn
    { ptiChunks = DB.chunkForBulkQuery (Proxy @DB.TxIn) Nothing $ etiTxIn <$> groupedTxIn grouped
    , ptiSkip = getSkipTxIn syncEnv
    }

-- | Prepare Metadata processing (fully independent)
prepareMetadataProcessing :: SyncEnv -> BlockGroupedData -> PreparedMetadata
prepareMetadataProcessing syncEnv grouped =
  PreparedMetadata
    { pmChunks = DB.chunkForBulkQuery (Proxy @DB.TxMetadata) (Just $ envIsJsonbInSchema syncEnv) $ groupedTxMetadata grouped
    , pmRemoveJsonb = ioRemoveJsonbFromSchema $ soptInsertOptions $ envOptions syncEnv
    }

-- | Prepare Mint processing (fully independent)
prepareMintProcessing :: SyncEnv -> BlockGroupedData -> PreparedMint
prepareMintProcessing _syncEnv grouped =
  PreparedMint
    { pmtChunks = DB.chunkForBulkQuery (Proxy @DB.MaTxMint) Nothing $ groupedTxMint grouped
    }

-- | Execute prepared TxIn operations (using pipeline)
executePreparedTxInPiped :: PreparedTxIn -> ExceptT SyncNodeError DB.DbM [DB.TxInId]
executePreparedTxInPiped prepared =
  if ptiSkip prepared
    then pure []
    else lift $ DB.insertBulkTxInPiped (ptiChunks prepared)

-- | Execute prepared Metadata operations (using pipeline)
executePreparedMetadataPiped :: PreparedMetadata -> ExceptT SyncNodeError DB.DbM ()
executePreparedMetadataPiped prepared =
  void $ lift $ DB.insertBulkTxMetadataPiped (pmRemoveJsonb prepared) (pmChunks prepared)

-- | Execute prepared Mint operations (using pipeline)
executePreparedMintPiped :: PreparedMint -> ExceptT SyncNodeError DB.DbM ()
executePreparedMintPiped prepared =
  void $ lift $ DB.insertBulkMaTxMintPiped (pmtChunks prepared)

-- | Process MaTxOut operations (depends on TxOut IDs)
processMaTxOuts :: SyncEnv -> [DB.TxOutIdW] -> BlockGroupedData -> ExceptT SyncNodeError DB.DbM [DB.MaTxOutIdW]
processMaTxOuts syncEnv txOutIds grouped = do
  let txOutVariantType = getTxOutVariantType syncEnv
      maTxOuts =
        concatMap (mkmaTxOuts txOutVariantType) $
          zip txOutIds (snd <$> groupedTxOut grouped)
      maTxOutChunks = DB.chunkForBulkQueryWith (DB.getMaTxOutBulkSize txOutVariantType) maTxOuts
  lift $ DB.insertBulkMaTxOutPiped maTxOutChunks

-- | Process UTxO consumption updates (depends on TxOut IDs)
processUtxoConsumption :: SyncEnv -> BlockGroupedData -> [DB.TxOutIdW] -> ExceptT SyncNodeError DB.DbM ()
processUtxoConsumption syncEnv grouped txOutIds = do
  let tracer = getTrace syncEnv
      txOutVariantType = getTxOutVariantType syncEnv

  whenConsumeOrPruneTxOut syncEnv $ do
    -- Resolve remaining inputs
    etis <- resolveRemainingInputs (groupedTxIn grouped) $ zip txOutIds (fst <$> groupedTxOut grouped)
    -- Categorise resolved inputs for bulk vs individual processing
    let (hashBasedUpdates, idBasedUpdates, failedInputs) = categorizeResolvedInputs etis
        hashUpdateChunks = DB.chunkForBulkQuery (Proxy @DB.TxIn) Nothing hashBasedUpdates
        idUpdateChunks = DB.chunkForBulkQuery (Proxy @DB.TxIn) Nothing idBasedUpdates

    -- Bulk process hash-based updates
    unless (null hashBasedUpdates) $
      void $
        lift $
          DB.updateConsumedByTxHashPiped txOutVariantType hashUpdateChunks
    -- Individual process ID-based updates
    unless (null idBasedUpdates) $
      void $
        lift $
          DB.updateListTxOutConsumedByTxIdBP idUpdateChunks
    -- Log failures
    mapM_ (liftIO . logWarning tracer . ("Failed to find output for " <>) . Text.pack . show) failedInputs

-----------------------------------------------------------------------------------------------------------------------------------
-- PARALLEL PROCESSING HELPER FUNCTIONS (NO PIPELINES)
-----------------------------------------------------------------------------------------------------------------------------------

-- | Helper function to create MinIds result
makeMinId :: SyncEnv -> [DB.TxInId] -> [DB.TxOutIdW] -> [DB.MaTxOutIdW] -> DB.MinIdsWrapper
makeMinId syncEnv txInIds txOutIds maTxOutIds =
  case getTxOutVariantType syncEnv of
    DB.TxOutVariantCore ->
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

-- | Helper function to categorize resolved inputs for parallel processing
-- Note: Inputs with Left (unresolved to TxOutId) are treated as hash-based updates
-- and also tracked as potentially failed for logging purposes.
categorizeResolvedInputs :: [ExtendedTxIn] -> ([DB.BulkConsumedByHash], [(DB.TxOutIdW, DB.TxId)], [ExtendedTxIn])
categorizeResolvedInputs =
  foldr categorizeOne ([], [], [])
  where
    categorizeOne eti@ExtendedTxIn {..} (hAcc, iAcc, fAcc) =
      case etiTxOutId of
        Right txOutId ->
          -- Successfully resolved to a TxOutId
          (hAcc, (txOutId, DB.txInTxInId etiTxIn) : iAcc, fAcc)
        Left genericTxIn ->
          -- Try to resolve by hash, but also track as potentially failed
          let bulkData =
                DB.BulkConsumedByHash
                  { bchTxHash = unTxHash (Generic.txInTxId genericTxIn)
                  , bchOutputIndex = Generic.txInIndex genericTxIn
                  , bchConsumingTxId = DB.txInTxInId etiTxIn
                  }
           in (bulkData : hAcc, iAcc, eti : fAcc)
