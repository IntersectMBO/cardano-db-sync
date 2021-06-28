{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Cardano.DbSync.Plugin.Default
  ( defDbSyncNodePlugin
  , insertDefaultBlock
  , rollbackToPoint
  ) where


import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logDebug, logInfo)

import qualified Cardano.Db as DB

import           Cardano.DbSync.Era.Byron.Insert (insertByronBlock)
import           Cardano.DbSync.Era.Cardano.Insert (insertEpochSyncTime)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import           Cardano.DbSync.Era.Shelley.Insert (insertShelleyBlock)
import           Cardano.DbSync.Era.Shelley.Insert.Epoch
import           Cardano.DbSync.Rollback (rollbackToPoint)

import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..))

import           Cardano.DbSync.Era

import           Cardano.Sync.Api
import           Cardano.Sync.Error
import           Cardano.Sync.LedgerState
import           Cardano.Sync.Plugin
import           Cardano.Sync.Types
import           Cardano.Sync.Util

import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Except.Extra (newExceptT)

import           Data.IORef (IORef, newIORef, readIORef, writeIORef)

import           Database.Persist.Sql (SqlBackend)

import           Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))

import           System.IO.Unsafe (unsafePerformIO)

-- | The default SyncNodePlugin.
-- Does exactly what the cardano-db-sync node did before the plugin system was added.
-- The non-default node takes this structure and extends the lists.
defDbSyncNodePlugin :: SqlBackend -> SyncNodePlugin
defDbSyncNodePlugin backend =
  SyncNodePlugin
    { plugOnStartup = []
    , plugInsertBlock = [insertDefaultBlock backend]
    , plugRollbackBlock = [rollbackToPoint backend]
    }

-- -------------------------------------------------------------------------------------------------

insertDefaultBlock
    :: SqlBackend -> Trace IO Text -> SyncEnv -> [BlockDetails]
    -> IO (Either SyncNodeError ())
insertDefaultBlock backend tracer env blockDetails = do
    thisIsAnUglyHack tracer (envLedger env)
    DB.runDbIohkLogging backend tracer $
      runExceptT (traverse_ insert blockDetails)
  where
    insert
        :: (MonadBaseControl IO m, MonadIO m)
        => BlockDetails -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    insert (BlockDetails cblk details) = do
      -- Calculate the new ledger state to pass to the DB insert functions but do not yet
      -- update ledgerStateVar.
      let lenv = envLedger env
      lStateSnap <- liftIO $ applyBlock (envLedger env) cblk details
      mkSnapshotMaybe lStateSnap (isSyncedWithinSeconds details 60)
      handleLedgerEvents tracer (envLedger env) (lssEvents lStateSnap)
      case cblk of
        BlockByron blk ->
          newExceptT $ insertByronBlock tracer blk details
        BlockShelley blk ->
          newExceptT $ insertShelleyBlock tracer lenv (Generic.fromShelleyBlock blk) lStateSnap details
        BlockAllegra blk ->
          newExceptT $ insertShelleyBlock tracer lenv (Generic.fromAllegraBlock blk) lStateSnap details
        BlockMary blk ->
          newExceptT $ insertShelleyBlock tracer lenv (Generic.fromMaryBlock blk) lStateSnap details
        BlockAlonzo blk ->
          newExceptT $ insertShelleyBlock tracer lenv (Generic.fromAlonzoBlock blk) lStateSnap details

    mkSnapshotMaybe
        :: (MonadBaseControl IO m, MonadIO m)
        => LedgerStateSnapshot -> DB.SyncState
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    mkSnapshotMaybe snapshot syncState = whenJust (lssNewEpoch snapshot) $ \newEpoch -> do
      liftIO $ logDebug (leTrace $ envLedger env) "Preparing for a snapshot"
      let newEpochNo = Generic.neEpoch newEpoch
      -- flush all volatile data
      flushBulkOperation (envLedger env)
      -- commit everything in the db
      lift DB.transactionCommit
      liftIO $ logDebug (leTrace $ envLedger env) "Taking a ledger a snapshot"
      -- finally take a ledger snapshot
      -- TODO: Instead of newEpochNo - 1, is there any way to get the epochNo from 'lssOldState'?
      liftIO $ saveCleanupState (envLedger env) (lssOldState snapshot) syncState  (Just $ newEpochNo - 1)
      liftIO $ logInfo (leTrace $ envLedger env) "Took a ledger snapshot"

-- -------------------------------------------------------------------------------------------------
-- This horrible hack is only need because of the split between `cardano-sync` and `cardano-db-sync`.

{-# NOINLINE offlineThreadStarted #-}
offlineThreadStarted :: IORef Bool
offlineThreadStarted = unsafePerformIO $ newIORef False

thisIsAnUglyHack :: Trace IO Text -> LedgerEnv -> IO ()
thisIsAnUglyHack tracer lenv = do
  started <- readIORef offlineThreadStarted
  unless started $ do
    -- This is horrible!
    writeIORef offlineThreadStarted True
    void . async $ runOfflineFetchThread tracer lenv
    logInfo tracer "thisIsAnUglyHack: Main thead"

-- -------------------------------------------------------------------------------------------------

handleLedgerEvents
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> LedgerEnv -> [LedgerEvent]
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
handleLedgerEvents tracer lenv =
    mapM_ printer
  where
    printer
        :: (MonadBaseControl IO m, MonadIO m)
        => LedgerEvent -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    printer ev =
      case ev of
        LedgerNewEpoch en ss -> do
          lift $ insertEpochSyncTime en ss (leEpochSyncTime lenv)
          liftIO . logInfo tracer $ "Starting epoch " <> textShow (unEpochNo en)
        LedgerRewards details rwds -> do
          let progress = calcEpochProgress 4 details
          when (progress > 0.6) $
            liftIO . logInfo tracer $ mconcat [ "LedgerRewards: ", textShow progress ]
          postEpochRewards lenv rwds
        LedgerStakeDist sdist ->
          postEpochStake lenv sdist

calcEpochProgress :: Int -> SlotDetails -> Double
calcEpochProgress digits sd =
  let factor = 10 ^ digits
      dval = fromIntegral (unEpochSlot $ sdEpochSlot sd) / fromIntegral (unEpochSize $ sdEpochSize sd)
  in fromIntegral (floor (dval * factor) :: Int) / factor
