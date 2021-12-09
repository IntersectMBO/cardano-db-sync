{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Cardano.DbSync.Default
  ( insertDefaultBlock
  , rollbackToPoint
  ) where


import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logDebug, logInfo)

import qualified Cardano.Db as DB

import           Cardano.DbSync.Api
import           Cardano.DbSync.Epoch
import           Cardano.DbSync.Era.Byron.Insert (insertByronBlock)
import           Cardano.DbSync.Era.Cardano.Insert (insertEpochSyncTime)
import           Cardano.DbSync.Era.Shelley.Adjust (adjustEpochRewards)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import           Cardano.DbSync.Era.Shelley.Insert (insertShelleyBlock)
import           Cardano.DbSync.Era.Shelley.Insert.Epoch (finalizeEpochBulkOps, forceInsertRewards,
                   insertPoolDepositRefunds, isEmptyEpochBulkOps, postEpochRewards, postEpochStake)
import           Cardano.DbSync.Era.Shelley.Validate (validateEpochRewards)
import           Cardano.DbSync.Error
import           Cardano.DbSync.LedgerState (LedgerEvent (..), LedgerStateSnapshot (..), applyBlock,
                   getAlonzoPParams, saveCleanupState)
import           Cardano.DbSync.Rollback (rollbackToPoint)
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import           Cardano.Ledger.BaseTypes (Network)
import           Cardano.Ledger.Coin (Coin (..))
import           Cardano.Ledger.Credential (StakeCredential)
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))

import           Cardano.Slotting.Block (BlockNo (..))
import           Cardano.Slotting.Slot (EpochNo (..))


import           Control.Monad.Class.MonadSTM.Strict (putTMVar, tryTakeTMVar)
import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Except.Extra (newExceptT)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Database.Persist.Sql (SqlBackend)

import           Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))
import           Ouroboros.Network.Block (blockNo)


insertDefaultBlock
    :: SyncEnv -> [CardanoBlock]
    -> IO (Either SyncNodeError ())
insertDefaultBlock env blocks =
    DB.runDbIohkLogging backend tracer .
      runExceptT $ do
        traverse_ insertDetails blocks
  where
    tracer = getTrace env
    backend = envBackend env

    insertDetails
        :: CardanoBlock -> ExceptT SyncNodeError (ReaderT SqlBackend (LoggingT IO)) ()
    insertDetails cblk = do
      -- Calculate the new ledger state to pass to the DB insert functions but do not yet
      -- update ledgerStateVar.
      let lenv = envLedger env
      lStateSnap <- liftIO $ applyBlock (envLedger env) cblk
      let !details = lssSlotDetails lStateSnap
      mkSnapshotMaybe env lStateSnap (blockNo cblk) (isSyncedWithinSeconds details 600)
      handleLedgerEvents tracer (envLedger env) (lssPoint lStateSnap) (lssEvents lStateSnap)
      let firstBlockOfEpoch = hasEpochStartEvent (lssEvents lStateSnap)
      case cblk of
        BlockByron blk ->
          newExceptT $ insertByronBlock tracer firstBlockOfEpoch blk details
        BlockShelley blk ->
          newExceptT $ insertShelleyBlock tracer lenv firstBlockOfEpoch (Generic.fromShelleyBlock blk) lStateSnap details
        BlockAllegra blk ->
          newExceptT $ insertShelleyBlock tracer lenv firstBlockOfEpoch (Generic.fromAllegraBlock blk) lStateSnap details
        BlockMary blk ->
          newExceptT $ insertShelleyBlock tracer lenv firstBlockOfEpoch (Generic.fromMaryBlock blk) lStateSnap details
        BlockAlonzo blk -> do
          let pp = getAlonzoPParams $ lssState lStateSnap
          newExceptT $ insertShelleyBlock tracer lenv firstBlockOfEpoch (Generic.fromAlonzoBlock pp blk) lStateSnap details
      when (soptExtended $ envOptions env) $
        ExceptT $ epochInsert tracer $ BlockDetails cblk details

mkSnapshotMaybe
        :: (MonadBaseControl IO m, MonadIO m)
        => SyncEnv -> LedgerStateSnapshot -> BlockNo -> DB.SyncState
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
mkSnapshotMaybe env snapshot blkNo syncState =
    case maybeFromStrict (lssNewEpoch snapshot) of
      Just newEpoch -> do
        liftIO $ logDebug (leTrace $ envLedger env) "Preparing for a snapshot"
        let newEpochNo = Generic.neEpoch newEpoch
        -- flush all volatile data
        finalizeEpochBulkOps (envLedger env)
        liftIO $ logDebug (leTrace $ envLedger env) "Taking a ledger a snapshot"
        -- finally take a ledger snapshot
        -- TODO: Instead of newEpochNo - 1, is there any way to get the epochNo from 'lssOldState'?
        liftIO $ saveCleanupState (envLedger env) (lssOldState snapshot) (Just $ newEpochNo - 1)
      Nothing ->
        when (timeToSnapshot syncState blkNo) .
          whenM (isEmptyEpochBulkOps $ envLedger env) .
            liftIO $ saveCleanupState (envLedger env) (lssOldState snapshot) Nothing

  where
    timeToSnapshot :: DB.SyncState -> BlockNo -> Bool
    timeToSnapshot syncSt bNo =
      case (syncSt, unBlockNo bNo) of
        (DB.SyncFollowing, bno) -> bno `mod` (snapshotEveryFollowing (envOptions env)) == 0
        (DB.SyncLagging, bno) -> bno `mod` (snapshotEveryLagging (envOptions env)) == 0

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
            adjustEpochRewards tracer en
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
        LedgerRewardDist rwd ->
          lift $ stashPoolRewards tracer lenv rwd
        LedgerMirDist md ->
          lift $ stashMirRewards tracer lenv md
        LedgerPoolReap en drs ->
          insertPoolDepositRefunds lenv (Generic.Rewards en $ convertPoolDepositReunds (leNetwork lenv) drs)

convertPoolDepositReunds
    :: Network -> Map (StakeCredential StandardCrypto) (Map (KeyHash 'StakePool StandardCrypto) Coin)
    -> Map Generic.StakeCred (Set Generic.Reward)
convertPoolDepositReunds nw =
    mapBimap (Generic.toStakeCred nw) (Set.fromList . map convert . Map.toList)
  where
    convert :: (KeyHash 'StakePool StandardCrypto, Coin) -> Generic.Reward
    convert (kh, coin) =
      Generic.Reward
        { Generic.rewardSource = DB.RwdDepositRefund
        , Generic.rewardPool = Just (Generic.toStakePoolKeyHash kh)
        , Generic.rewardAmount = coin
        }

mapBimap :: Ord k2 => (k1 -> k2) -> (a1 -> a2) -> Map k1 a1 -> Map k2 a2
mapBimap fk fa = Map.fromAscList . map (bimap fk fa) . Map.toAscList


hasEpochStartEvent :: [LedgerEvent] -> Bool
hasEpochStartEvent = any isNewEpoch
  where
    isNewEpoch :: LedgerEvent -> Bool
    isNewEpoch le =
      case le of
        LedgerNewEpoch {} -> True
        LedgerStartAtEpoch {} -> True
        _otherwise -> False

-- -------------------------------------------------------------------------------------------------
-- These two functions must handle being called in either order.
stashPoolRewards
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> LedgerEnv -> Generic.Rewards
    -> ReaderT SqlBackend m ()
stashPoolRewards tracer lenv rmap = do
  mMirRwd <- liftIO . atomically $ tryTakeTMVar (leMirRewards lenv)
  case mMirRwd of
    Nothing ->
      liftIO . atomically $ putTMVar (lePoolRewards lenv) rmap
    Just mirMap -> do
      let totalRwds = Generic.mergeRewards rmap mirMap
      forceInsertRewards tracer lenv totalRwds
      validateEpochRewards tracer (Generic.mergeRewards rmap mirMap)

stashMirRewards
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> LedgerEnv -> Generic.Rewards
    -> ReaderT SqlBackend m ()
stashMirRewards tracer lenv mirMap = do
  mRwds <- liftIO . atomically $ tryTakeTMVar (lePoolRewards lenv)
  case mRwds of
    Nothing ->
      liftIO . atomically $ putTMVar (leMirRewards lenv) mirMap
    Just rmap -> do
      let totalRwds = Generic.mergeRewards rmap mirMap
      forceInsertRewards tracer lenv totalRwds
      validateEpochRewards tracer totalRwds
