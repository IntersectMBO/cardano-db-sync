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

import           Cardano.DbSync.Era

import           Cardano.DbSync.Era.Byron.Insert (insertByronBlock)
import           Cardano.DbSync.Era.Cardano.Insert (insertEpochSyncTime)
import           Cardano.DbSync.Era.Shelley.Adjust (adjustEpochRewards)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import           Cardano.DbSync.Era.Shelley.Insert (insertShelleyBlock)
import           Cardano.DbSync.Era.Shelley.Insert.Epoch
import           Cardano.DbSync.Era.Shelley.Validate
import           Cardano.DbSync.Rollback (rollbackToPoint)

import           Cardano.Ledger.Coin (Coin (..))
import           Cardano.Ledger.Credential (StakeCredential)
import           Cardano.Ledger.Crypto (StandardCrypto)

import           Cardano.Slotting.Slot (EpochNo (..))

import           Cardano.Sync.Api
import           Cardano.Sync.Error
import           Cardano.Sync.LedgerState
import           Cardano.Sync.Plugin
import           Cardano.Sync.Types
import           Cardano.Sync.Util

import           Control.Monad.Class.MonadSTM.Strict (putTMVar, tryTakeTMVar)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Except.Extra (newExceptT)

import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map

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
      handleLedgerEvents tracer (envLedger env) (lssPoint lStateSnap) (lssEvents lStateSnap)
      case cblk of
        BlockByron blk ->
          newExceptT $ insertByronBlock tracer blk details
        BlockShelley blk ->
          newExceptT $ insertShelleyBlock tracer lenv (Generic.fromShelleyBlock blk) lStateSnap details
        BlockAllegra blk ->
          newExceptT $ insertShelleyBlock tracer lenv (Generic.fromAllegraBlock blk) lStateSnap details
        BlockMary blk ->
          newExceptT $ insertShelleyBlock tracer lenv (Generic.fromMaryBlock blk) lStateSnap details
        BlockAlonzo blk -> do
          let pp = getAlonzoPParams $ lssState lStateSnap
          newExceptT $ insertShelleyBlock tracer lenv (Generic.fromAlonzoBlock pp blk) lStateSnap details

    mkSnapshotMaybe
        :: (MonadBaseControl IO m, MonadIO m)
        => LedgerStateSnapshot -> DB.SyncState
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    mkSnapshotMaybe snapshot syncState =
      whenJust (lssNewEpoch snapshot) $ \newEpoch -> do
        liftIO $ logDebug (leTrace $ envLedger env) "Preparing for a snapshot"
        let newEpochNo = Generic.neEpoch newEpoch
        -- flush all volatile data
        finalizeEpochBulkOps (envLedger env)
        liftIO $ logDebug (leTrace $ envLedger env) "Taking a ledger a snapshot"
        -- finally take a ledger snapshot
        -- TODO: Instead of newEpochNo - 1, is there any way to get the epochNo from 'lssOldState'?
        liftIO $ saveCleanupState (envLedger env) (lssOldState snapshot) syncState  (Just $ newEpochNo - 1)

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
    => Trace IO Text -> LedgerEnv -> CardanoPoint -> [LedgerEvent]
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
handleLedgerEvents tracer lenv point =
    mapM_ handler
  where
    handler
        :: (MonadBaseControl IO m, MonadIO m)
        => LedgerEvent -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    handler ev =
      case ev of
        LedgerNewEpoch en ss -> do
          lift $ do
            insertEpochSyncTime en ss (leEpochSyncTime lenv)
            adjustEpochRewards tracer (en - 2)
          finalizeEpochBulkOps lenv
          -- Commit everything in the db *AFTER* the epoch rewards have been inserted, the orphaned
          -- rewards removed and the bulk operations finalized.
          lift DB.transactionCommit
          liftIO . logInfo tracer $ "Starting epoch " <> textShow (unEpochNo en)
        LedgerStartAtEpoch en ->
          -- This is different from the previous case in that the db-sync started
          -- in this epoch, for example after a restart, instead of after an epoch boundary.
          liftIO . logInfo tracer $ "Starting at epoch " <> textShow (unEpochNo en)
        LedgerRewards _details rwds -> do
          liftIO . logInfo tracer $ mconcat
            [ "Handling ", show (Map.size (Generic.rwdRewards rwds)), " rewards for epoch "
            , show (unEpochNo $ Generic.rwdEpoch rwds), " ", renderPoint point
            ]
          postEpochRewards lenv rwds point
        LedgerStakeDist sdist -> do
          liftIO . logInfo tracer $ mconcat
            [ "Handling ", show (Map.size (Generic.sdistStakeMap sdist)), " stakes for epoch "
            , show (unEpochNo $ Generic.sdistEpochNo sdist), " ", renderPoint point
            ]
          postEpochStake lenv sdist point
        LedgerRewardDist en rd ->
          lift $ stashPoolRewards tracer lenv en rd
        LedgerMirDist md ->
          lift $ stashMirRewards tracer lenv md

-- These two functions must handle being called in either order.
stashPoolRewards
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> LedgerEnv -> EpochNo -> Map (StakeCredential StandardCrypto) Coin
    -> ReaderT SqlBackend m ()
stashPoolRewards tracer lenv epoch rmap = do
  mMirRwd <- liftIO . atomically $ tryTakeTMVar (leMirRewards lenv)
  case mMirRwd of
    Nothing ->
      liftIO . atomically $ putTMVar (lePoolRewards lenv) (epoch, rmap)
    Just mirMap ->
      validateEpochRewards tracer (leNetwork lenv) (epoch - 2) (Map.unionWith plusCoin rmap mirMap)

stashMirRewards
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> LedgerEnv -> Map (StakeCredential StandardCrypto) Coin
    -> ReaderT SqlBackend m ()
stashMirRewards tracer lenv mirMap = do
    mRwds <- liftIO . atomically $ tryTakeTMVar (lePoolRewards lenv)
    case mRwds of
      Nothing ->
        liftIO . atomically $ putTMVar (leMirRewards lenv) mirMap
      Just (epoch, rmap) ->
        validateEpochRewards tracer (leNetwork lenv) (epoch - 2) (Map.unionWith plusCoin rmap mirMap)
