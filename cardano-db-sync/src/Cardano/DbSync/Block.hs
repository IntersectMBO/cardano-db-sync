{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}

module Cardano.DbSync.Block (
  insertListBlocks,
) where

import Cardano.BM.Trace (logError, logInfo)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Ledger
import Cardano.DbSync.Api.Types (ConsistentLevel (..), InsertOptions (..), LedgerEnv (..), SyncEnv (..))
import Cardano.DbSync.Era.Byron.Insert (insertByronBlock)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Universal.Block (insertBlockUniversal, prepareBlock)
import Cardano.DbSync.Era.Universal.Epoch (hasEpochStartEvent) -- , hasNewEpochEvent)
import Cardano.DbSync.Era.Universal.Insert.LedgerEvent (insertNewEpochLedgerEvents)
import Cardano.DbSync.Error
import Cardano.DbSync.Ledger.State (applyBlockAndSnapshot, defaultApplyResult)
import Cardano.DbSync.Ledger.Types (ApplyResult (..))
import Cardano.DbSync.LocalStateQuery
import Cardano.DbSync.Rollback
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.DbSync.Util.Constraint (addConstraintsIfNotExist)
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import Cardano.Prelude
import Cardano.Slotting.Slot (EpochNo (..))
import Control.Monad.Logger (LoggingT)
import qualified Data.ByteString.Short as SBS
import qualified Data.Strict.Maybe as Strict
import Database.Persist.SqlBackend.Internal
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import Ouroboros.Network.Block (BlockNo, blockHash, blockNo, getHeaderFields, headerFieldBlockNo, unBlockNo)
import Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))
import Control.Concurrent.Async
import Cardano.DbSync.Era.Universal.Insert.Tx
import Cardano.DbSync.Era.Universal.Insert.Grouped (insertBlockGroupedData)
import Cardano.DbSync.Cache (queryPrevBlockWithCache)
import Control.Monad.Extra (whenJust)
import Database.Persist.Sql

insertListBlocks ::
  SyncEnv ->
  [CardanoBlock] ->
  ExceptT SyncNodeError IO ()
insertListBlocks syncEnv blocks = do
  bl <- liftIO $ isConsistent syncEnv
  if bl then
    applyAndInsertBlocks syncEnv False blocks
  else do
    mrestBlocks <- applyAndInsertBlocksMaybe syncEnv blocks
    whenJust mrestBlocks $ applyAndInsertBlocks syncEnv True

applyAndInsertBlocksMaybe ::
  SyncEnv ->
  [CardanoBlock] ->
  ExceptT SyncNodeError IO (Maybe [CardanoBlock])
applyAndInsertBlocksMaybe syncEnv = go
  where
    go [] = pure Nothing
    go ls@(cblk: rest) = do
      eiBlockInDbAlreadyId <- lift $ DB.runDbLogging (envBackend syncEnv) tracer $ DB.queryBlockId (cardanoBlockHash cblk)
      case eiBlockInDbAlreadyId of
        Left _ -> do
          liftIO
            . logInfo tracer
            $ mconcat
              [ "Received block which is not in the db with "
              , textShow (getHeaderFields cblk)
              , ". Time to restore consistency."
              ]
          ExceptT $ DB.runDbIohkLogging (envBackend syncEnv) tracer $ runExceptT $ rollbackFromBlockNo syncEnv (blockNo cblk)
          liftIO $ setConsistentLevel syncEnv Consistent
          pure $ Just ls
        Right _ -> do
          (applyRes, _) <- liftIO (mkApplyResult syncEnv cblk False)
          whenJust (getNewEpoch applyRes) $ \epochNo ->
            liftIO $ logInfo tracer $ "Reached " <> textShow epochNo
          go rest

    getNewEpoch :: ApplyResult -> Maybe EpochNo
    getNewEpoch appRes =
      Generic.neEpoch <$> maybeFromStrict (apNewEpoch appRes)

    tracer = getTrace syncEnv

applyAndInsertBlocks ::
  SyncEnv ->
  Bool ->
  [CardanoBlock] ->
  ExceptT SyncNodeError IO ()
applyAndInsertBlocks syncEnv firstAfterRollback = go
  where
    go [] = pure ()
    go ls@(blk : rest) = do
      prevBlockId <- DB.runDbLoggingExceptT backend tracer $ queryPrevBlockWithCache "applyAndInsertBlocks" (envCache syncEnv) (cardanoBlockHash blk)
      let newBlockId = 1 + DB.unBlockKey prevBlockId
      let flagList = firstAfterRollback : replicate (length rest) False
      let zippedArgs = zip (DB.BlockKey <$> [newBlockId..]) flagList
      let (byronBlocks, blocks) = takeWhileByron $ zip zippedArgs (blk : rest)
      DB.runDbIohkLoggingExceptT backend tracer $ mapM_ (applyAndInsertByronBlock syncEnv) byronBlocks
      DB.runDbIohkLoggingExceptT backend tracer $ mapM_ (applyAndInsertBlock syncEnv) blocks -- we can use this split to parallelise even further within

    backend = envBackend syncEnv
    tracer = getTrace syncEnv

applyAndInsertByronBlock ::
  SyncEnv ->
  ((DB.BlockId, Bool), ByronBlock) ->
  ExceptT SyncNodeError (ReaderT SqlBackend (LoggingT IO)) ()
applyAndInsertByronBlock syncEnv ((_blockId, firstAfterRollback), blk) = do
  (applyResult, tookSnapshot) <- liftIO (mkApplyResult syncEnv (BlockByron blk) True)
  let isStartEventOrRollback = hasEpochStartEvent (apEvents applyResult) || firstAfterRollback
  let details = apSlotDetails applyResult
  insertNewEpochLedgerEvents syncEnv (sdEpochNo (apSlotDetails applyResult)) (apEvents applyResult)
  ExceptT $ insertByronBlock syncEnv isStartEventOrRollback blk details
  insertBlockRest syncEnv blkNo applyResult tookSnapshot
  where
    cblk :: CardanoBlock = BlockByron blk
    blkNo = headerFieldBlockNo $ getHeaderFields cblk

applyAndInsertBlock ::
  SyncEnv ->
  ((DB.BlockId, Bool), CardanoBlock) ->
  ExceptT SyncNodeError (ReaderT SqlBackend (LoggingT IO)) ()
applyAndInsertBlock syncEnv ((blockId, firstAfterRollback), cblock) = do
  (applyResult, tookSnapshot) <- liftIO (mkApplyResult syncEnv cblock True)
  insertNewEpochLedgerEvents syncEnv (sdEpochNo (apSlotDetails applyResult)) (apEvents applyResult)
  whenGeneric $ \blk ->
    insertBlock syncEnv (blockId, blk) applyResult firstAfterRollback tookSnapshot
  where
    tracer = getTrace syncEnv
    iopts = getInsertOptions syncEnv
    whenGeneric action =
       maybe (liftIO $ logError tracer "Found Byron Block after Shelley") action (toGenericBlock iopts cblock)

insertBlock ::
  SyncEnv ->
  (DB.BlockId, Generic.Block) ->
  ApplyResult ->
  Bool ->
  Bool ->
  ExceptT SyncNodeError (ReaderT SqlBackend (LoggingT IO)) ()
insertBlock syncEnv (blockId, blk) applyResult firstAfterRollback tookSnapshot = do
  (blockDB, preparedTxs) <-
    liftIO $ concurrently
      (runOrThrowIO $ runExceptT $ DB.runDbLoggingExceptT backend tracer $ prepareBlock syncEnv blk)
      (mapConcurrently prepareTxWithPool (Generic.blkTxs blk))

  _minIds <- insertBlockGroupedData syncEnv $ mconcat (snd <$> preparedTxs)
  mapM_ (uncurry3 $ insertTxRest syncEnv blockId epochNo slotNo applyResult) (fst <$> preparedTxs)
  insertBlockUniversal
    syncEnv
    blockId
    blk
    blockDB
    applyResult
    isStartEventOrRollback
  insertBlockRest syncEnv blkNo applyResult tookSnapshot
  where
    details = apSlotDetails applyResult
    isStartEventOrRollback = hasEpochStartEvent (apEvents applyResult) || firstAfterRollback
    epochNo = sdEpochNo details
    slotNo = sdSlotNo details
    blkNo = Generic.blkBlockNo blk
    backend = envBackend syncEnv
    tracer = getTrace syncEnv

    prepareTxWithPool tx = runOrThrowIO $ runSqlPoolNoTransaction (prepTx tx) (envPool syncEnv) Nothing
    prepTx = runExceptT . prepareTxGrouped syncEnv [] blockId

insertBlockRest ::
  SyncEnv ->
  BlockNo ->
  ApplyResult ->
  -- has snapshot been taken
  Bool ->
  ExceptT SyncNodeError (ReaderT SqlBackend (LoggingT IO)) ()
insertBlockRest syncEnv blkNo applyResult tookSnapshot = do

  -- TODO update the epoch
  -- updateEpoch
  whenPruneTxOut syncEnv $
    when (unBlockNo blkNo `mod` getPruneInterval syncEnv == 0) $
      do
        lift $ DB.deleteConsumedTxOut tracer txOutTableType (getSafeBlockNoDiff syncEnv)
  commitOrIndexes
  where
    details = apSlotDetails applyResult

    {-}
    isNewEpochEvent = hasNewEpochEvent (apEvents applyResult)
    updateEpoch =
      -- if have --dissable-epoch && --dissable-cache then no need to run this function
      when (soptEpochAndCacheEnabled $ envOptions syncEnv)
        . newExceptT
        $ epochHandler
          syncEnv
          tracer
          (envCache syncEnv)
          isNewEpochEvent
          (BlockDetails cblk details)
    -}
    _getPrices :: Maybe Ledger.Prices
    _getPrices = case apPrices applyResult of
      Strict.Just pr -> Just pr
      Strict.Nothing | hasLedgerState syncEnv -> Just $ Ledger.Prices minBound minBound
      Strict.Nothing -> Nothing

    commitOrIndexes :: ExceptT SyncNodeError (ReaderT SqlBackend (LoggingT IO)) ()
    commitOrIndexes = do
      commited <-
        if withinTwoMin || tookSnapshot
          then do
            lift DB.transactionCommit
            pure True
          else pure False
      when withinHalfHour $ do
        bootStrapMaybe syncEnv
        ranIndexes <- liftIO $ getRanIndexes syncEnv
        lift $ addConstraintsIfNotExist syncEnv tracer
        unless ranIndexes $ do
          lift $ unless commited DB.transactionCommit
          liftIO $ runIndexMigrations syncEnv

    withinTwoMin = isSyncedWithinSeconds details 120 == SyncFollowing
    withinHalfHour = isSyncedWithinSeconds details 1800 == SyncFollowing

    tracer = getTrace syncEnv
    txOutTableType = getTxOutTableType syncEnv

mkApplyResult :: SyncEnv -> CardanoBlock -> Bool -> IO (ApplyResult, Bool)
mkApplyResult syncEnv cblk isCons = do
  (applyRes, tookSnapshot) <- case envLedgerEnv syncEnv of
    HasLedger hle -> applyBlockAndSnapshot hle cblk isCons
    NoLedger nle -> do
      slotDetails <- getSlotDetailsNode nle (cardanoBlockSlotNo cblk)
      pure (defaultApplyResult slotDetails, False)
  let details = apSlotDetails applyRes
  epochEvents <- liftIO $ atomically $ generateNewEpochEvents syncEnv details
  pure (applyRes {apEvents = sort $ epochEvents <> apEvents applyRes}, tookSnapshot)

takeWhileByron :: [(a, CardanoBlock)] -> ([(a, ByronBlock)], [(a, CardanoBlock)])
takeWhileByron = go []
  where
    go accByron [] = (reverse accByron, [])
    go accByron ls@(t : rest) = case toByron (snd t) of
      Nothing -> (reverse accByron, ls)
      Just byronBlock -> go ((fst t, byronBlock) : accByron) rest

    toByron :: CardanoBlock -> Maybe ByronBlock
    toByron = \case
      BlockByron blk -> Just blk
      _ -> Nothing

toGenericBlock :: InsertOptions -> CardanoBlock -> Maybe Generic.Block
toGenericBlock iopts = \case
  BlockByron _ -> Nothing
  BlockShelley blk -> Just $ Generic.fromShelleyBlock blk
  BlockAllegra blk -> Just $ Generic.fromAllegraBlock blk
  BlockMary blk -> Just $ Generic.fromMaryBlock blk
  BlockAlonzo blk -> Just $ Generic.fromAlonzoBlock (ioPlutusExtra iopts) Nothing blk
  BlockBabbage blk -> Just $ Generic.fromBabbageBlock (ioPlutusExtra iopts) Nothing blk
  BlockConway blk -> Just $ Generic.fromConwayBlock (ioPlutusExtra iopts) Nothing blk

cardanoBlockHash :: CardanoBlock -> ByteString
cardanoBlockHash = SBS.fromShort . Consensus.getOneEraHash . blockHash
