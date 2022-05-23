{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Cardano.DbSync.Default
  ( insertListBlocks
  , rollbackToPoint
  ) where


import           Cardano.Prelude

import           Cardano.BM.Trace (logInfo)

import qualified Cardano.Db as DB

import           Cardano.DbSync.Api
import           Cardano.DbSync.Cache
import           Cardano.DbSync.Epoch
import           Cardano.DbSync.Era.Byron.Insert (insertByronBlock)
import           Cardano.DbSync.Era.Cardano.Insert (insertEpochSyncTime)
import           Cardano.DbSync.Era.Shelley.Adjust (adjustEpochRewards)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import           Cardano.DbSync.Era.Shelley.Insert (insertShelleyBlock)
import           Cardano.DbSync.Era.Shelley.Insert.Epoch (insertPoolDepositRefunds, insertRewards)
import           Cardano.DbSync.Era.Shelley.Validate (validateEpochRewards)
import           Cardano.DbSync.Error
import           Cardano.DbSync.LedgerState (LedgerEvent (..), ApplyResult (..), applyBlockAndSnapshot)
import           Cardano.DbSync.Rollback (rollbackToPoint)
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import qualified Cardano.Ledger.Alonzo.Scripts as Ledger

import           Cardano.Slotting.Slot (EpochNo (..))

import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Except.Extra (newExceptT)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict

import           Database.Persist.SqlBackend.Internal.StatementCache
import           Database.Persist.SqlBackend.Internal

import           Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))


insertListBlocks
    :: SyncEnv -> [CardanoBlock]
    -> IO (Either SyncNodeError ())
insertListBlocks env blocks = do
    DB.runDbIohkLogging backend tracer .
      runExceptT $ do
        traverse_ (applyAndInsert env) blocks
  where
    tracer = getTrace env
    backend = envBackend env

applyAndInsert
    :: SyncEnv -> CardanoBlock -> ExceptT SyncNodeError (ReaderT SqlBackend (LoggingT IO)) ()
applyAndInsert env cblk = do
    !applyResult <- liftIO $ applyBlockAndSnapshot (envLedger env) cblk
    let !details = apSlotDetails applyResult
    insertLedgerEvents env (sdEpochNo details) (apEvents applyResult)
    insertEpoch details
    let firstBlockOfEpoch = hasEpochStartEvent (apEvents applyResult)
    let isMember = \poolId -> Set.member poolId (apPoolsRegistered applyResult)
    case cblk of
      BlockByron blk ->
        newExceptT $ insertByronBlock env firstBlockOfEpoch blk details
      BlockShelley blk -> newExceptT $
        insertShelleyBlock env firstBlockOfEpoch (Generic.fromShelleyBlock blk)
          details isMember (apNewEpoch applyResult) (apStakeSlice applyResult)
      BlockAllegra blk -> newExceptT $
        insertShelleyBlock env firstBlockOfEpoch (Generic.fromAllegraBlock blk)
          details isMember (apNewEpoch applyResult) (apStakeSlice applyResult)
      BlockMary blk -> newExceptT $
        insertShelleyBlock env firstBlockOfEpoch (Generic.fromMaryBlock blk)
          details isMember (apNewEpoch applyResult) (apStakeSlice applyResult)
      BlockAlonzo blk -> newExceptT $
        insertShelleyBlock env firstBlockOfEpoch (Generic.fromAlonzoBlock (getPrices applyResult) blk)
          details isMember (apNewEpoch applyResult) (apStakeSlice applyResult)
      BlockBabbage blk -> newExceptT $
        insertShelleyBlock env firstBlockOfEpoch (Generic.fromBabbageBlock (getPrices applyResult) blk)
          details isMember (apNewEpoch applyResult) (apStakeSlice applyResult)
  where
    tracer = getTrace env

    insertEpoch details = when (soptExtended $ envOptions env) .
      newExceptT $ epochInsert tracer (BlockDetails cblk details)

    getPrices :: ApplyResult -> Ledger.Prices
    getPrices applyResult = case apPrices applyResult of
      Strict.Just pr -> pr
      Strict.Nothing -> Ledger.Prices minBound minBound

-- -------------------------------------------------------------------------------------------------

insertLedgerEvents
    :: (MonadBaseControl IO m, MonadIO m)
    => SyncEnv -> EpochNo -> [LedgerEvent]
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertLedgerEvents env currentEpochNo@(EpochNo curEpoch) =
    mapM_ handler
  where
    tracer = getTrace env
    lenv = envLedger env
    cache = envCache env
    ntw = leNetwork lenv

    subFromCurrentEpoch :: Word64 -> EpochNo
    subFromCurrentEpoch m =
      if unEpochNo currentEpochNo >= m then EpochNo $ unEpochNo currentEpochNo - m
      else EpochNo 0

    toSyncState :: SyncState -> DB.SyncState
    toSyncState SyncLagging = DB.SyncLagging
    toSyncState SyncFollowing = DB.SyncFollowing

    handler
        :: (MonadBaseControl IO m, MonadIO m)
        => LedgerEvent -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    handler ev =
      case ev of
        LedgerNewEpoch en ss -> do
          lift $ do
            insertEpochSyncTime en (toSyncState ss) (leEpochSyncTime lenv)
          sqlBackend <- lift ask
          persistantCacheSize <- liftIO $ statementCacheSize $ connStmtMap sqlBackend
          liftIO . logInfo tracer $ "Persistant SQL Statement Cache size is " <> textShow persistantCacheSize
          stats <- liftIO $ textShowStats cache
          liftIO . logInfo tracer $ stats
          liftIO . logInfo tracer $ "Starting epoch " <> textShow (unEpochNo en)
        LedgerStartAtEpoch en ->
          -- This is different from the previous case in that the db-sync started
          -- in this epoch, for example after a restart, instead of after an epoch boundary.
          liftIO . logInfo tracer $ "Starting at epoch " <> textShow (unEpochNo en)
        LedgerDeltaRewards _e rwd -> do
          let rewards = Map.toList $ Generic.rwdRewards rwd
          insertRewards ntw (subFromCurrentEpoch 2) currentEpochNo cache (Map.toList $ Generic.rwdRewards rwd)
          -- This event is only created when it's not empty, so we don't need to check for null here.
          liftIO . logInfo tracer $ "Inserted " <> show (length rewards) <> " Delta rewards"
        LedgerIncrementalRewards _ rwd -> do
          let rewards = Map.toList $ Generic.rwdRewards rwd
          insertRewards ntw (subFromCurrentEpoch 1) (EpochNo $ curEpoch + 1) cache rewards
        LedgerRestrainedRewards e rwd creds -> do
          lift $ adjustEpochRewards tracer ntw cache e rwd creds
        LedgerTotalRewards _e rwd -> do
          lift $ validateEpochRewards tracer ntw (subFromCurrentEpoch 2) currentEpochNo rwd
        LedgerMirDist rwd -> do
          unless (Map.null rwd) $ do
            let rewards = Map.toList rwd
            insertRewards ntw (subFromCurrentEpoch 1) currentEpochNo cache rewards
            liftIO . logInfo tracer $ "Inserted " <> show (length rewards) <> " Mir rewards"
        LedgerPoolReap en drs -> do
          unless (Map.null $ Generic.rwdRewards drs) $ do
            insertPoolDepositRefunds env en drs

hasEpochStartEvent :: [LedgerEvent] -> Bool
hasEpochStartEvent = any isNewEpoch
  where
    isNewEpoch :: LedgerEvent -> Bool
    isNewEpoch le =
      case le of
        LedgerNewEpoch {} -> True
        LedgerStartAtEpoch {} -> True
        _otherwise -> False
