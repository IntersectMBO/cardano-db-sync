{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Snapshot operations for db-sync, using consensus snapshot format.
--
-- This module replaces the old custom .lstate snapshot format with
-- consensus's directory-based format (<slot>[_suffix]/ containing
-- state, meta, utxoSize files).
module Cardano.DbSync.Ledger.Snapshot
  ( -- * Migration
    migrateOldSnapshots
    -- * Save
  , saveCurrentLedgerState
  , saveCleanupState
  , snapshotWriteLoop
  , runLedgerStateWriteThread
    -- * Load
  , loadSnapshotFromDisk
  , findStateFromSnapshot
    -- * List / Cleanup
  , listDiskSnapshots
  , deleteNewerSnapshots
    -- * Snapshot points
  , listKnownSnapshots
  , getSlotNoSnapshot
  ) where

import Cardano.BM.Trace (Trace, logInfo, logWarning)
import Cardano.DbSync.Api.Types (LedgerEnv (..))
import Cardano.DbSync.Config.Types (LedgerStateDir (..))
import Cardano.DbSync.Ledger.Types
import Cardano.DbSync.Types (CardanoPoint)
import Cardano.Prelude hiding (atomically)
import Cardano.Slotting.Slot (EpochNo (..), WithOrigin (..))
import Control.Concurrent.Class.MonadSTM.Strict (atomically, readTVar, writeTVar)
import Control.Concurrent.STM.TBQueue (readTBQueue, writeTBQueue)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.List as List
import qualified Data.Strict.Maybe as Strict
import qualified Database.LSMTree as LSMTree
import qualified Ouroboros.Consensus.Ledger.Abstract as Consensus
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq as Consensus (StateRef (..))
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Network.Block
import qualified Ouroboros.Network.Point as Point
import System.Directory (listDirectory, removeFile, doesDirectoryExist)
import System.FilePath (takeExtension, (</>))

-- | Remove old .lstate files from a previous db-sync version.
-- This is a one-time migration on first startup with the new format.
migrateOldSnapshots :: LedgerStateDir -> Trace IO Text -> IO ()
migrateOldSnapshots (LedgerStateDir stateDir) tracer = do
  exists <- doesDirectoryExist stateDir
  when exists $ do
    files <- listDirectory stateDir
    let oldFiles = filter (\f -> takeExtension f == ".lstate") files
    unless (null oldFiles) $ do
      logInfo tracer $ "Migrating: removing " <> textShow (length oldFiles) <> " old .lstate snapshots"
      forM_ oldFiles $ \f ->
        handle (\(_ :: IOException) -> pure ()) $ removeFile (stateDir </> f)

-- | Queue a snapshot for async writing.
saveCurrentLedgerState :: HasLedgerEnv -> DbSyncStateRef -> Maybe EpochNo -> IO ()
saveCurrentLedgerState env lState _mEpochNo = do
  -- Don't store genesis
  case Consensus.ledgerTipSlot $ ledgerState (clsState (srState lState)) of
    Origin -> pure ()
    At _ -> do
      atomically $ writeTVar (srCanClose lState) False
      atomically $ writeTBQueue (leSnapshotQueue env) lState

-- | Save a snapshot and clean up old ones.
saveCleanupState :: HasLedgerEnv -> DbSyncStateRef -> Maybe EpochNo -> IO ()
saveCleanupState env ledger mEpochNo =
  saveCurrentLedgerState env ledger mEpochNo

-- | The write thread that takes snapshots from the queue.
runLedgerStateWriteThread :: Trace IO Text -> LedgerEnv -> IO ()
runLedgerStateWriteThread tracer lenv =
  case lenv of
    HasLedger le -> snapshotWriteLoop tracer le
    NoLedger _ -> forever $ threadDelay 600000000

-- | Write loop: read from queue, take snapshot via consensus SnapshotManager.
snapshotWriteLoop :: Trace IO Text -> HasLedgerEnv -> IO ()
snapshotWriteLoop tracer env = loop
  where
    loop :: IO ()
    loop = do
      ledger <- atomically $ readTBQueue (leSnapshotQueue env)
      startTime <- getCurrentTime
      let cRef = toConsensusStateRef ledger
      mResult <- takeSnapshot (leSnapshotManager env) Nothing cRef
      atomically $ writeTVar (srCanClose ledger) True
      endTime <- getCurrentTime
      case mResult of
        Nothing -> pure ()
        Just (ds, _pt) -> do
          logInfo tracer $
            mconcat
              [ "Wrote snapshot "
              , textShow (snapshotToDirName ds)
              , " in "
              , textShow (diffUTCTime endTime startTime)
              ]
          -- Trim old snapshots after writing the new one
          let policy = SnapshotPolicy
                { onDiskNumSnapshots = 3
                , onDiskShouldTakeSnapshot = \_ _ -> True
                }
          deleted <- trimSnapshots (leSnapshotManager env) policy
          unless (null deleted) $
            logInfo tracer $ "Trimmed " <> textShow (length deleted) <> " old snapshots"
      loop

-- | Load a snapshot from disk using consensus APIs.
-- Returns a DbSyncStateRef with clsEpochBlockNo = 0
-- (will be corrected after first epoch boundary).
loadSnapshotFromDisk ::
  HasLedgerEnv ->
  DiskSnapshot ->
  IO (Either Text DbSyncStateRef)
loadSnapshotFromDisk env ds = do
  startTime <- getCurrentTime
  eResult <- handle (\(err :: LSMTree.SnapshotDoesNotExistError) -> pure $ Left $ textShow err) $
    leLoadSnapshot env ds
  endTime <- getCurrentTime
  case eResult of
    Left err -> pure $ Left $ "Failed to load snapshot " <> textShow (snapshotToDirName ds)
      <> ": " <> err
    Right cRef -> do
      logInfo (leTrace env) $
        mconcat
          [ "Loaded snapshot "
          , textShow (snapshotToDirName ds)
          , " in "
          , textShow (diffUTCTime endTime startTime)
          ]
      let Consensus.StateRef st _ = cRef
      Right <$> fromConsensusStateRef (deriveEpochBlockNo st) cRef

-- | Try to find a snapshot matching the given point, or fall back to genesis.
findStateFromSnapshot ::
  HasLedgerEnv ->
  CardanoPoint ->
  IO (Either [DiskSnapshot] DbSyncStateRef)
findStateFromSnapshot env point = do
  snapshots <- listSnapshots (leSnapshotManager env)
  case getPoint point of
    Origin -> do
      -- Delete all snapshots and start from genesis
      forM_ snapshots $ safeDeleteSnapshot (leSnapshotManager env)
      Right <$> initCardanoLedgerState env
    At blk -> do
      let targetSlot = Point.blockPointSlot blk
      -- Delete snapshots newer than the target
      let (newer, rest) = List.span (\ds -> SlotNo (dsNumber ds) > targetSlot) snapshots
      forM_ newer $ \ds -> do
        logInfo (leTrace env) $ "Deleting newer snapshot: " <> textShow (snapshotToDirName ds)
        safeDeleteSnapshot (leSnapshotManager env) ds
      -- Try to find a matching snapshot
      case List.find (\ds -> SlotNo (dsNumber ds) == targetSlot) rest of
        Just ds -> do
          result <- loadSnapshotFromDisk env ds
          case result of
            Right sr -> pure $ Right sr
            Left err -> do
              logWarning (leTrace env) $ "Failed to load snapshot: " <> err <> ". Trying older."
              safeDeleteSnapshot (leSnapshotManager env) ds
              let older = List.filter (\d -> SlotNo (dsNumber d) < targetSlot) rest
              pure $ Left older
        Nothing -> do
          let older = List.filter (\ds -> SlotNo (dsNumber ds) < targetSlot) rest
          case older of
            [] -> pure $ Left []
            _ -> pure $ Left older

-- | List snapshots using the consensus snapshot manager.
listDiskSnapshots :: HasLedgerEnv -> IO [DiskSnapshot]
listDiskSnapshots env = listSnapshots (leSnapshotManager env)

-- | Safely delete a snapshot, ignoring missing LSM snapshot errors.
-- Works around a consensus bug where 'deleteSnapshotIfTemporary' calls
-- 'LSM.deleteSnapshot' unconditionally, which throws 'SnapshotDoesNotExistError'
-- if the LSM snapshot is missing (e.g. after a crash during snapshot write).
safeDeleteSnapshot :: SnapshotManager IO IO blk st -> DiskSnapshot -> IO ()
safeDeleteSnapshot sm ds =
  handle (\(_ :: LSMTree.SnapshotDoesNotExistError) -> pure ()) $
    deleteSnapshotIfTemporary sm ds

-- | Delete snapshot directories newer than the given point.
deleteNewerSnapshots :: HasLedgerEnv -> CardanoPoint -> IO ()
deleteNewerSnapshots env point = do
  snapshots <- listDiskSnapshots env
  case getPoint point of
    Origin ->
      forM_ snapshots $ safeDeleteSnapshot (leSnapshotManager env)
    At blk -> do
      let targetSlot = Point.blockPointSlot blk
          newer = filter (\ds -> SlotNo (dsNumber ds) > targetSlot) snapshots
      forM_ newer $ safeDeleteSnapshot (leSnapshotManager env)

-- | List known snapshot points (both in-memory and on-disk).
listKnownSnapshots :: HasLedgerEnv -> IO [SnapshotPoint]
listKnownSnapshots env = do
  inMem <- fmap InMemory <$> listMemorySnapshots env
  onDisk <- fmap OnDisk <$> listDiskSnapshots env
  pure $ List.sortOn (Down . getSlotNoSnapshot) $ inMem <> onDisk

getSlotNoSnapshot :: SnapshotPoint -> WithOrigin SlotNo
getSlotNoSnapshot (OnDisk ds) = At $ SlotNo (dsNumber ds)
getSlotNoSnapshot (InMemory cp) = pointSlot cp

listMemorySnapshots :: HasLedgerEnv -> IO [CardanoPoint]
listMemorySnapshots env = do
  mState <- atomically $ readTVar $ leStateVar env
  case mState of
    Strict.Nothing -> pure []
    Strict.Just ledgerDB ->
      pure $
        filter
          notGenesis
          (castPoint . Consensus.getTip . clsState . srState <$> getEdgePoints ledgerDB)
  where
    getEdgePoints ldb =
      case toList $ ledgerDbCheckpoints ldb of
        [] -> []
        [a] -> [a]
        (h : ls) -> catMaybes [Just h, lastMay ls]
    notGenesis GenesisPoint = False
    notGenesis (BlockPoint _ _) = True
