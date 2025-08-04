{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Rollback (
  prepareRollback,
  rollbackFromBlockNo,
  rollbackLedger,
  unsafeRollback,
  handlePostRollbackSnapshots,
) where

import Cardano.Prelude
import qualified Data.ByteString.Short as SBS
import qualified Data.Strict.Maybe as Strict
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (getOneEraHash)
import Ouroboros.Consensus.HeaderValidation hiding (TipInfo)
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Network.Block
import Ouroboros.Network.Point

import Cardano.BM.Trace (Trace, logInfo, logWarning)
import Control.Monad.Extra (whenJust)

import qualified Cardano.Db as DB
import Cardano.DbSync.Api (getLatestPoints, getPruneConsume, getTrace, getTxOutVariantType, verifySnapshotPoint)
import Cardano.DbSync.Api.Types (LedgerEnv (..), SyncEnv (..))
import Cardano.DbSync.Cache
import Cardano.DbSync.DbEvent (liftFail)
import Cardano.DbSync.Error (SyncNodeError (..), logAndThrowIO, mkSyncNodeCallStack)
import Cardano.DbSync.Ledger.State (listKnownSnapshots, loadLedgerAtPoint, saveCleanupState, writeLedgerState)
import Cardano.DbSync.Ledger.Types (CardanoLedgerState (..), SnapshotPoint (..))
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.DbSync.Util.Constraint (addConstraintsIfNotExist)

rollbackFromBlockNo ::
  SyncEnv ->
  BlockNo ->
  ExceptT SyncNodeError DB.DbM ()
rollbackFromBlockNo syncEnv blkNo = do
  nBlocks <- lift $ DB.queryBlockCountAfterBlockNo (unBlockNo blkNo) True
  mres <- lift $ DB.queryBlockNoAndEpoch (unBlockNo blkNo)
  -- Use whenJust like the original - silently skip if block not found
  whenJust mres $ \(blockId, epochNo) -> do
    liftIO . logInfo trce $
      mconcat
        [ "Deleting "
        , textShow nBlocks
        , " numbered equal to or greater than "
        , textShow blkNo
        ]

    deletedBlockCount <- lift $ DB.deleteBlocksBlockId trce txOutVariantType blockId epochNo (DB.pcmConsumedTxOut $ getPruneConsume syncEnv)
    when (deletedBlockCount > 0) $ do
      -- We use custom constraints to improve input speeds when syncing.
      -- If they don't already exists we add them here as once a rollback has happened
      -- we always need the constraints.
      addConstraintsIfNotExist syncEnv trce

    rollbackCache cache blockId
    liftIO . logInfo trce $ "Blocks deleted"
  where
    trce = getTrace syncEnv
    cache = envCache syncEnv
    txOutVariantType = getTxOutVariantType syncEnv

prepareRollback :: SyncEnv -> CardanoPoint -> Tip CardanoBlock -> IO (Either SyncNodeError Bool)
prepareRollback syncEnv point serverTip = do
  DB.runDbDirectSilent (envDbEnv syncEnv) $ runExceptT action
  where
    trce = getTrace syncEnv

    action :: ExceptT SyncNodeError DB.DbM Bool
    action = do
      case getPoint point of
        Origin -> do
          nBlocks <- lift DB.queryCountSlotNo
          if nBlocks == 0
            then do
              liftIO . logInfo trce $ "Starting from Genesis"
              pure True
            else do
              liftIO . logInfo trce $
                mconcat
                  [ "Delaying delete of "
                  , textShow nBlocks
                  , " while rolling back to genesis."
                  , " Applying blocks until a new block is found."
                  , " The node is currently at "
                  , textShow serverTip
                  ]
              pure False
        At blk -> do
          nBlocks <- lift $ DB.queryCountSlotNosGreaterThan (unSlotNo $ blockPointSlot blk)
          mBlockNo <- liftFail (mkSyncNodeCallStack "prepareRollback") $ DB.queryBlockHashBlockNo (SBS.fromShort . getOneEraHash $ blockPointHash blk)
          case mBlockNo of
            Nothing -> throwError $ SNErrRollback "Rollback.prepareRollback: queryBlockHashBlockNo: Block hash not found"
            Just blockN -> do
              liftIO . logInfo trce $
                mconcat
                  [ "Delaying delete of "
                  , textShow nBlocks
                  , " blocks after "
                  , textShow blockN
                  , " while rolling back to ("
                  , renderPoint point
                  , "). Applying blocks until a new block is found. The node is currently at "
                  , textShow serverTip
                  ]
              pure False

rollbackLedger :: SyncEnv -> CardanoPoint -> IO (Maybe [CardanoPoint])
rollbackLedger syncEnv point =
  case envLedgerEnv syncEnv of
    HasLedger hle -> do
      mst <- loadLedgerAtPoint hle point
      case mst of
        Right st -> do
          let statePoint = headerStatePoint $ headerState $ clsState st
          -- This is an extra validation that should always succeed.
          unless (point == statePoint) $
            logAndThrowIO (getTrace syncEnv) $
              SNErrDatabaseRollBackLedger $
                mconcat
                  [ "Ledger "
                  , show statePoint
                  , " and ChainSync "
                  , show point
                  , " don't match."
                  ]
          pure Nothing
        Left lsfs ->
          Just . fmap fst <$> verifySnapshotPoint syncEnv (OnDisk <$> lsfs)
    NoLedger _ -> pure Nothing

-- For testing and debugging.
-- Enhanced rollback that logs more info and handles the rollback more carefully
unsafeRollback :: Trace IO Text -> DB.TxOutVariantType -> DB.PGConfig -> SlotNo -> IO (Either SyncNodeError ())
unsafeRollback trce txOutVariantType config slotNo = do
  logWarning trce $ "Starting a forced rollback to slot: " <> textShow (unSlotNo slotNo)

  -- Perform rollback with improved diagnostics
  Right
    <$> DB.runDbStandaloneDirectSilent
      (DB.PGPassCached config)
      ( do
          -- Get latest points before rollback for reference
          latestPointsBefore <- DB.queryLatestPoints
          liftIO $ logInfo trce $ "Latest points before rollback: " <> textShow (length latestPointsBefore) <> " points"

          -- Perform the actual rollback
          void $ DB.deleteBlocksSlotNo trce txOutVariantType slotNo True

          -- Query state after rollback
          latestPointsAfter <- DB.queryLatestPoints
          liftIO $ logInfo trce $ "Latest points after rollback: " <> textShow (length latestPointsAfter) <> " points"
          case latestPointsAfter of
            [] -> liftIO $ logWarning trce "No blocks remain in database - sync will start from genesis"
            ((mSlot, _) : _) -> liftIO $ logInfo trce $ "New database tip at slot: " <> textShow mSlot

          liftIO $ logInfo trce "Database rollback completed successfully"
      )

-- Handle ledger snapshots after a rollback to ensure they're consistent with the database
-- This should be called after SyncEnv is created but before sync starts
handlePostRollbackSnapshots :: SyncEnv -> Maybe SlotNo -> IO ()
handlePostRollbackSnapshots syncEnv mRollbackSlot = do
  case (mRollbackSlot, envLedgerEnv syncEnv) of
    (Just rollbackSlot, HasLedger hle) -> do
      let trce = getTrace syncEnv
      logInfo trce $ "Checking ledger snapshots after rollback to slot " <> textShow (unSlotNo rollbackSlot)

      -- Get the current database state after rollback
      latestPoints <- getLatestPoints syncEnv
      let dbTip = case latestPoints of
            [] -> Nothing
            ((point, _) : _) -> Just point

      case dbTip of
        Nothing -> do
          logWarning trce "No blocks in database after rollback - clearing all ledger snapshots"
          -- Clear all in-memory ledger state since we'll start from genesis
          writeLedgerState hle Strict.Nothing
        Just dbTipPoint -> do
          logInfo trce $ "Database tip after rollback: " <> renderPoint dbTipPoint

          -- Check if we have any valid snapshots for this state
          snapshotPoints <- listKnownSnapshots hle
          validSnapshots <- verifySnapshotPoint syncEnv snapshotPoints

          case validSnapshots of
            [] -> do
              logWarning trce "No valid ledger snapshots found for current database state"
              logInfo trce "Loading ledger state at database tip to create new snapshot"

              -- Try to load ledger state at the database tip
              eitherLedgerState <- loadLedgerAtPoint hle dbTipPoint
              case eitherLedgerState of
                Right loadedState -> do
                  logInfo trce $ "Successfully loaded ledger state at " <> renderPoint dbTipPoint
                  logInfo trce "Creating new snapshot at database tip after rollback"
                  saveCleanupState hle loadedState Nothing
                  logInfo trce "Snapshot created successfully"
                Left lsFiles -> do
                  logWarning trce $ "Failed to load ledger state at database tip. Missing snapshot files: " <> textShow (length lsFiles)
                  logInfo trce "Clearing in-memory ledger state to force reload from disk snapshots"
                  writeLedgerState hle Strict.Nothing
            validPoints -> do
              let bestSnapshot = minimumBy (flip compare) (fst <$> validPoints)
              logInfo trce $ "Found valid snapshot at: " <> renderPoint bestSnapshot
              -- Keep the current state if we have valid snapshots
              pure ()
    (Just _, NoLedger _) -> do
      logInfo (getTrace syncEnv) "No ledger state to handle after rollback (NoLedger mode)"
    (Nothing, _) -> pure () -- No rollback happened
