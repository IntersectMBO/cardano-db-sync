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

import           Cardano.BM.Trace (logDebug, logInfo)

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
import           Cardano.DbSync.LedgerState (LedgerEvent (..), LedgerStateSnapshot (..), applyBlock,
                   getAlonzoPParams, getBabbagePParams, saveCleanupState)
import           Cardano.DbSync.Rollback (rollbackToPoint)
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import qualified Cardano.Ledger.Babbage.PParams as Babbage
import           Cardano.Ledger.BaseTypes (Network)
import           Cardano.Ledger.Coin (Coin (..))
import           Cardano.Ledger.Credential (StakeCredential)
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))

import           Cardano.Slotting.Block (BlockNo (..))
import           Cardano.Slotting.Slot (EpochNo (..))


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
      lStateSnap <- liftIO $ applyBlock (envLedger env) cblk
      let !details = lssSlotDetails lStateSnap
      mkSnapshotMaybe env lStateSnap (blockNo cblk) (isSyncedWithinSeconds details 600)
      handleLedgerEvents env (sdEpochNo details) (lssEvents lStateSnap)
      let firstBlockOfEpoch = hasEpochStartEvent (lssEvents lStateSnap)
      case cblk of
        BlockByron blk ->
          newExceptT $ insertByronBlock env firstBlockOfEpoch blk details
        BlockShelley blk ->
          newExceptT $ insertShelleyBlock env firstBlockOfEpoch (Generic.fromShelleyBlock blk) lStateSnap details
        BlockAllegra blk ->
          newExceptT $ insertShelleyBlock env firstBlockOfEpoch (Generic.fromAllegraBlock blk) lStateSnap details
        BlockMary blk ->
          newExceptT $ insertShelleyBlock env firstBlockOfEpoch (Generic.fromMaryBlock blk) lStateSnap details
        BlockAlonzo blk -> do
          let prices = Alonzo._prices $ getAlonzoPParams $ lssState lStateSnap
          newExceptT $ insertShelleyBlock env firstBlockOfEpoch (Generic.fromAlonzoBlock prices blk) lStateSnap details
        BlockBabbage blk -> do
          let prices = Babbage._prices $ getBabbagePParams $ lssState lStateSnap
          newExceptT $ insertShelleyBlock env firstBlockOfEpoch (Generic.fromBabbageBlock prices blk) lStateSnap details
      when (soptExtended $ envOptions env) .
        newExceptT $ epochInsert tracer (BlockDetails cblk details)

mkSnapshotMaybe
        :: (MonadBaseControl IO m, MonadIO m)
        => SyncEnv -> LedgerStateSnapshot -> BlockNo -> DB.SyncState
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
mkSnapshotMaybe env snapshot blkNo syncState =
    case maybeFromStrict (lssNewEpoch snapshot) of
      Just newEpoch -> do
        liftIO $ logDebug (leTrace $ envLedger env) "Preparing for a snapshot"
        let newEpochNo = Generic.neEpoch newEpoch
        liftIO $ logDebug (leTrace $ envLedger env) "Taking a ledger a snapshot"
        -- finally take a ledger snapshot
        -- TODO: Instead of newEpochNo - 1, is there any way to get the epochNo from 'lssOldState'?
        liftIO $ saveCleanupState (envLedger env) (lssOldState snapshot) (Just $ newEpochNo - 1)
      Nothing ->
        when (timeToSnapshot syncState blkNo) $
          liftIO $ saveCleanupState (envLedger env) (lssOldState snapshot) Nothing

  where
    timeToSnapshot :: DB.SyncState -> BlockNo -> Bool
    timeToSnapshot syncSt bNo =
      case (syncSt, unBlockNo bNo) of
        (DB.SyncFollowing, bno) -> bno `mod` snapshotEveryFollowing (envOptions env) == 0
        (DB.SyncLagging, bno) -> bno `mod` snapshotEveryLagging (envOptions env) == 0

-- -------------------------------------------------------------------------------------------------

handleLedgerEvents
    :: (MonadBaseControl IO m, MonadIO m)
    => SyncEnv -> EpochNo -> [LedgerEvent]
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
handleLedgerEvents env currentEpochNo@(EpochNo curEpoch) =
    mapM_ handler
  where
    tracer = getTrace env
    lenv = envLedger env
    cache = envCache env

    subFromCurrentEpoch :: Word64 -> EpochNo
    subFromCurrentEpoch m =
      if unEpochNo currentEpochNo >= m then EpochNo $ unEpochNo currentEpochNo - m
      else EpochNo 0

    handler
        :: (MonadBaseControl IO m, MonadIO m)
        => LedgerEvent -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    handler ev =
      case ev of
        LedgerNewEpoch en ss -> do
          lift $ do
            insertEpochSyncTime en ss (leEpochSyncTime lenv)
          stats <- liftIO $ textShowStats cache
          liftIO . logInfo tracer $ stats
          liftIO . logInfo tracer $ "Starting epoch " <> textShow (unEpochNo en)
        LedgerStartAtEpoch en ->
          -- This is different from the previous case in that the db-sync started
          -- in this epoch, for example after a restart, instead of after an epoch boundary.
          liftIO . logInfo tracer $ "Starting at epoch " <> textShow (unEpochNo en)
        LedgerDeltaRewards rwd -> do
          let rewards = Map.toList $ Generic.rwdRewards rwd
          insertRewards (subFromCurrentEpoch 2) currentEpochNo cache (Map.toList $ Generic.rwdRewards rwd)
          -- This event is only created when it's not empty, so we don't need to check for null here.
          liftIO . logInfo tracer $ "Inserted " <> show (length rewards) <> " Delta rewards"
        LedgerIncrementalRewards rwd -> do
          let rewards = Map.toList $ Generic.rwdRewards rwd
          insertRewards (subFromCurrentEpoch 1) (EpochNo $ curEpoch + 1) cache rewards
        LedgerRestrainedRewards e rwd creds -> do
          lift $ adjustEpochRewards tracer cache e rwd creds
        LedgerTotalRewards rwd -> do
          lift $ validateEpochRewards tracer (subFromCurrentEpoch 2) rwd
        LedgerMirDist rwd -> do
          let rewards = Map.toList rwd
          insertRewards (subFromCurrentEpoch 1) currentEpochNo cache rewards
          unless (null rewards) $
            liftIO . logInfo tracer $ "Inserted " <> show (length rewards) <> " Mir rewards"
        LedgerPoolReap en drs ->
          insertPoolDepositRefunds env (Generic.Rewards en $ convertPoolDepositReunds (leNetwork lenv) drs)

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
