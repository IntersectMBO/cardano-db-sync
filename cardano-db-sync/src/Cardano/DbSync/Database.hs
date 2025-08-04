{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Database (
  runDbThread,
) where

import Cardano.BM.Trace (logDebug, logError, logInfo)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (ConsistentLevel (..), SyncEnv (..))
import Cardano.DbSync.DbEvent
import Cardano.DbSync.Default
import Cardano.DbSync.Error
import Cardano.DbSync.Ledger.State
import Cardano.DbSync.Metrics
import Cardano.DbSync.Rollback
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.Prelude hiding (atomically)
import Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Extra (whenJust)
import Ouroboros.Network.Block (BlockNo (..), Point (..))
import Ouroboros.Network.Point (blockPointHash, blockPointSlot)

data NextState
  = Continue
  | Done
  deriving (Eq)

runDbThread ::
  SyncEnv ->
  ThreadChannels ->
  IO ()
runDbThread syncEnv queue = do
  logInfo tracer "Starting DB thread"
  logException tracer "runDbThread: " processQueue
  logInfo tracer "Shutting down DB thread"
  where
    tracer = getTrace syncEnv

    -- Main loop to process the queue
    processQueue :: IO ()
    processQueue = do
      actions <- blockingFlushDbEventQueue queue

      -- Log the number of blocks being processed if there are multiple
      when (length actions > 1) $ do
        logDebug tracer $ "Processing " <> textShow (length actions) <> " blocks"

      -- Handle the case where the syncing thread has restarted
      case hasRestart actions of
        Just resultVar -> handleRestart resultVar
        Nothing -> processActions actions

    -- Process a list of actions
    processActions :: [DbEvent] -> IO ()
    processActions actions = do
      -- runActions is where we start inserting information we recieve from the node.
      result <- runExceptT $ runActions syncEnv actions

      -- Update metrics with the latest block information
      updateBlockMetrics

      -- Handle the result of running the actions
      case result of
        Left err -> logError tracer $ show err
        Right Continue -> processQueue -- Continue processing
        Right Done -> pure () -- Stop processing

    -- Handle the case where the syncing thread has restarted
    handleRestart :: StrictTMVar IO ([(CardanoPoint, Bool)], WithOrigin BlockNo) -> IO ()
    handleRestart resultVar = do
      logInfo tracer "Chain Sync client thread has restarted"
      latestPoints <- getLatestPoints syncEnv
      currentTip <- getCurrentTipBlockNo syncEnv
      logDbState syncEnv
      atomically $ putTMVar resultVar (latestPoints, currentTip)
      processQueue -- Continue processing
    updateBlockMetrics :: IO ()
    updateBlockMetrics = do
      let metricsSetters = envMetricSetters syncEnv
      void $ async $ DB.runDbDirectLogged (fromMaybe mempty $ DB.dbTracer $ envDbEnv syncEnv) (envDbEnv syncEnv) $ do
        mBlock <- DB.queryLatestBlock
        liftIO $ whenJust mBlock $ \block -> do
          let blockNo = BlockNo $ fromMaybe 0 $ DB.blockBlockNo block
              slotNo = SlotNo $ fromMaybe 0 $ DB.blockSlotNo block
          setDbBlockHeight metricsSetters blockNo
          setDbSlotHeight metricsSetters slotNo

-- | Run the list of 'DbEvent's. Block are applied in a single set (as a transaction)
-- and other operations are applied one-by-one.
runActions ::
  SyncEnv ->
  [DbEvent] ->
  ExceptT SyncNodeError IO NextState
runActions syncEnv actions = do
  dbEvent Continue actions
  where
    dbEvent :: NextState -> [DbEvent] -> ExceptT SyncNodeError IO NextState
    dbEvent next [] = pure next
    dbEvent Done _ = pure Done
    dbEvent Continue xs =
      case spanDbApply xs of
        ([], DbFinish : _) -> do
          pure Done
        ([], DbRollBackToPoint chainSyncPoint serverTip resultVar : ys) -> do
          deletedAllBlocks <- ExceptT $ prepareRollback syncEnv chainSyncPoint serverTip
          points <- lift $ rollbackLedger syncEnv chainSyncPoint

          case (deletedAllBlocks, points) of
            (True, Nothing) -> do
              liftIO $ setConsistentLevel syncEnv Consistent
              liftIO $ validateConsistentLevel syncEnv chainSyncPoint
            (False, Nothing) -> do
              liftIO $ setConsistentLevel syncEnv DBAheadOfLedger
              liftIO $ validateConsistentLevel syncEnv chainSyncPoint
            _anyOtherOption -> do
              liftIO $ setConsistentLevel syncEnv DBAheadOfLedger
          blockNo <- lift $ getDbTipBlockNo syncEnv
          lift $ atomically $ putTMVar resultVar (points, blockNo)
          dbEvent Continue ys
        (ys, zs) -> do
          ExceptT $ insertListBlocks syncEnv ys
          if null zs
            then pure Continue
            else dbEvent Continue zs

-- | This not only checks that the ledger and ChainSync points are equal, but also that the
-- 'Consistent' Level is correct based on the db tip.
validateConsistentLevel :: SyncEnv -> CardanoPoint -> IO ()
validateConsistentLevel syncEnv stPoint = do
  dbTipInfo <- getDbLatestBlockInfo (envDbEnv syncEnv)
  cLevel <- getConsistentLevel syncEnv
  compareTips stPoint dbTipInfo cLevel
  where
    compareTips _ dbTip Unchecked =
      logAndThrowIO tracer $
        SNErrDatabaseValConstLevel $
          "Found Unchecked Consistent Level. " <> showContext dbTip Unchecked
    compareTips (Point Origin) Nothing Consistent = pure ()
    compareTips (Point Origin) _ DBAheadOfLedger = pure ()
    compareTips (Point (At blk)) (Just tip) Consistent
      | getHeaderHash (blockPointHash blk) == bHash tip
          && blockPointSlot blk == bSlotNo tip =
          pure ()
    compareTips (Point (At blk)) (Just tip) DBAheadOfLedger
      | blockPointSlot blk <= bSlotNo tip = pure ()
    compareTips _ dbTip cLevel =
      logAndThrowIO tracer $
        SNErrDatabaseValConstLevel $
          "Unexpected Consistent Level. " <> showContext dbTip cLevel

    tracer = getTrace syncEnv
    showContext dbTip cLevel =
      mconcat
        [ "Ledger state point is "
        , show stPoint
        , ". DB Tip is "
        , show dbTip
        , ". Consistent Level is "
        , show cLevel
        ]

-- | Split the DbEvent list into a prefix containing blocks to apply and a postfix.
spanDbApply :: [DbEvent] -> ([CardanoBlock], [DbEvent])
spanDbApply lst =
  case lst of
    (DbApplyBlock bt : xs) -> let (ys, zs) = spanDbApply xs in (bt : ys, zs)
    xs -> ([], xs)

hasRestart :: [DbEvent] -> Maybe (StrictTMVar IO ([(CardanoPoint, Bool)], WithOrigin BlockNo))
hasRestart = go
  where
    go [] = Nothing
    go (DbRestartState mvar : _) = Just mvar
    go (_ : rest) = go rest
