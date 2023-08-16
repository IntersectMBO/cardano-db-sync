{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Default (
  insertListBlocks,
) where

import Cardano.BM.Trace (logInfo)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (ConsistentLevel (..), InsertOptions (..), LedgerEnv (..), SyncEnv (..), SyncOptions (..))
import Cardano.DbSync.Cache.Types (textShowStats)
import Cardano.DbSync.Epoch (epochHandler)
import Cardano.DbSync.Era.Byron.Insert (insertByronBlock)
import Cardano.DbSync.Era.Cardano.Insert (insertEpochSyncTime)
import Cardano.DbSync.Era.Shelley.Adjust (adjustEpochRewards)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Shelley.Insert (insertShelleyBlock, mkAdaPots)
import Cardano.DbSync.Era.Shelley.Insert.Epoch (insertPoolDepositRefunds, insertRewards)
import Cardano.DbSync.Era.Shelley.Validate (validateEpochRewards)
import Cardano.DbSync.Error
import Cardano.DbSync.Fix.EpochStake
import Cardano.DbSync.Ledger.Event (LedgerEvent (..))
import Cardano.DbSync.Ledger.State (applyBlockAndSnapshot, defaultApplyResult)
import Cardano.DbSync.Ledger.Types (ApplyResult (..))
import Cardano.DbSync.LocalStateQuery
import Cardano.DbSync.Rollback
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import Cardano.Ledger.Shelley.AdaPots as Shelley
import Cardano.Prelude
import Cardano.Slotting.Slot (EpochNo (..), SlotNo)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except.Extra (newExceptT)
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import Database.Persist.SqlBackend.Internal
import Database.Persist.SqlBackend.Internal.StatementCache
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import Ouroboros.Network.Block (blockHash, blockNo, getHeaderFields, headerFieldBlockNo, unBlockNo)

insertListBlocks ::
  SyncEnv ->
  [CardanoBlock] ->
  IO (Either SyncNodeError ())
insertListBlocks synEnv blocks = do
  DB.runDbIohkLogging (envBackend synEnv) tracer
    . runExceptT
    $ do
      traverse_ (applyAndInsertBlockMaybe synEnv) blocks
  where
    tracer = getTrace synEnv

applyAndInsertBlockMaybe ::
  SyncEnv ->
  CardanoBlock ->
  ExceptT SyncNodeError (ReaderT SqlBackend (LoggingT IO)) ()
applyAndInsertBlockMaybe syncEnv cblk = do
  bl <- liftIO $ isConsistent syncEnv
  (!applyRes, !tookSnapshot) <- liftIO (mkApplyResult bl)
  if bl
    then -- In the usual case it will be consistent so we don't need to do any queries. Just insert the block
      insertBlock syncEnv cblk applyRes False tookSnapshot
    else do
      eiBlockInDbAlreadyId <- lift (DB.queryBlockId (SBS.fromShort . Consensus.getOneEraHash $ blockHash cblk))
      -- If the block is already in db, do nothing. If not, delete all blocks with greater 'BlockNo' or
      -- equal, insert the block and restore consistency between ledger and db.
      case eiBlockInDbAlreadyId of
        Left _ -> do
          liftIO . logInfo tracer $
            mconcat
              [ "Received block which is not in the db with "
              , textShow (getHeaderFields cblk)
              , ". Time to restore consistency."
              ]
          rollbackFromBlockNo syncEnv (blockNo cblk)
          void $ migrateStakeDistr syncEnv (apOldLedger applyRes)
          insertBlock syncEnv cblk applyRes True tookSnapshot
          liftIO $ setConsistentLevel syncEnv Consistent
        Right blockId | Just (adaPots, slotNo, epochNo) <- getAdaPots applyRes -> do
          replaced <- lift $ DB.replaceAdaPots blockId $ mkAdaPots blockId slotNo epochNo adaPots
          if replaced
            then liftIO $ logInfo tracer $ "Fixed AdaPots for " <> textShow epochNo
            else liftIO $ logInfo tracer $ "Reached " <> textShow epochNo
        Right _
          | Just epochNo <- getNewEpoch applyRes ->
              liftIO $ logInfo tracer $ "Reached " <> textShow epochNo
        _ -> pure ()
  where
    tracer = getTrace syncEnv

    mkApplyResult :: Bool -> IO (ApplyResult, Bool)
    mkApplyResult isCons = do
      case envLedgerEnv syncEnv of
        HasLedger hle -> applyBlockAndSnapshot hle cblk isCons
        NoLedger nle -> do
          slotDetails <- getSlotDetailsNode nle (cardanoBlockSlotNo cblk)
          pure (defaultApplyResult slotDetails, False)

    getAdaPots :: ApplyResult -> Maybe (Shelley.AdaPots, SlotNo, EpochNo)
    getAdaPots appRes = do
      newEpoch <- maybeFromStrict $ apNewEpoch appRes
      adaPots <- maybeFromStrict $ Generic.neAdaPots newEpoch
      pure (adaPots, sdSlotNo $ apSlotDetails appRes, sdEpochNo $ apSlotDetails appRes)

    getNewEpoch :: ApplyResult -> Maybe EpochNo
    getNewEpoch appRes =
      Generic.neEpoch <$> maybeFromStrict (apNewEpoch appRes)

insertBlock ::
  SyncEnv ->
  CardanoBlock ->
  ApplyResult ->
  -- is first Block after rollback
  Bool ->
  -- has snapshot been taken
  Bool ->
  ExceptT SyncNodeError (ReaderT SqlBackend (LoggingT IO)) ()
insertBlock syncEnv cblk applyRes firstAfterRollback tookSnapshot = do
  !epochEvents <- liftIO $ atomically $ generateNewEpochEvents syncEnv (apSlotDetails applyRes)
  let !applyResult = applyRes {apEvents = sort $ epochEvents <> apEvents applyRes}
  let !details = apSlotDetails applyResult
  let !withinTwoMin = isWithinTwoMin details
  let !withinHalfHour = isWithinHalfHour details
  insertLedgerEvents syncEnv (sdEpochNo details) (apEvents applyResult)
  let isNewEpochEvent = hasNewEpochEvent (apEvents applyResult)
  let isStartEventOrRollback = hasEpochStartEvent (apEvents applyResult) || firstAfterRollback
  let isMember poolId = Set.member poolId (apPoolsRegistered applyResult)
  let insertShelley blk =
        insertShelleyBlock
          syncEnv
          isStartEventOrRollback
          withinTwoMin
          withinHalfHour
          blk
          details
          isMember
          applyResult

  -- Here we insert the block and it's txs, but in adition we also cache some values which we later
  -- use when updating the Epoch, thus saving us having to recalulating them later.
  case cblk of
    BlockByron blk ->
      newExceptT $
        insertByronBlock syncEnv isStartEventOrRollback blk details
    BlockShelley blk ->
      newExceptT $
        insertShelley $
          Generic.fromShelleyBlock blk
    BlockAllegra blk ->
      newExceptT $
        insertShelley $
          Generic.fromAllegraBlock blk
    BlockMary blk ->
      newExceptT $
        insertShelley $
          Generic.fromMaryBlock blk
    BlockAlonzo blk ->
      newExceptT $
        insertShelley $
          Generic.fromAlonzoBlock (ioPlutusExtra iopts) (getPrices applyResult) blk
    BlockBabbage blk ->
      newExceptT $
        insertShelley $
          Generic.fromBabbageBlock (ioPlutusExtra iopts) (getPrices applyResult) blk
    BlockConway blk ->
      newExceptT $
        insertShelley $
          Generic.fromConwayBlock (ioPlutusExtra iopts) (getPrices applyResult) blk
  -- update the epoch
  updateEpoch details isNewEpochEvent
  whenPruneTxOut syncEnv $
    when (unBlockNo blkNo `mod` getPruneInterval syncEnv == 0) $ do
      lift $ DB.deleteConsumedTxOut tracer (getSafeBlockNoDiff syncEnv)
  lift $ commitOrIndexes withinTwoMin withinHalfHour
  where
    tracer = getTrace syncEnv
    iopts = getInsertOptions syncEnv

    updateEpoch details isNewEpochEvent =
      -- if have --dissable-epoch && --dissable-cache then no need to run this function
      when (soptEpochAndCacheEnabled $ envOptions syncEnv)
        . newExceptT
        $ epochHandler
          syncEnv
          tracer
          (envCache syncEnv)
          isNewEpochEvent
          (BlockDetails cblk details)

    getPrices :: ApplyResult -> Maybe Ledger.Prices
    getPrices applyResult = case apPrices applyResult of
      Strict.Just pr -> Just pr
      Strict.Nothing | hasLedgerState syncEnv -> Just $ Ledger.Prices minBound minBound
      Strict.Nothing -> Nothing

    commitOrIndexes :: Bool -> Bool -> ReaderT SqlBackend (LoggingT IO) ()
    commitOrIndexes withinTwoMin withinHalfHour = do
      commited <-
        if withinTwoMin || tookSnapshot
          then do
            DB.transactionCommit
            pure True
          else pure False
      when withinHalfHour $ do
        ranIndexes <- liftIO $ getRanIndexes syncEnv
        unless ranIndexes $ do
          unless commited DB.transactionCommit
          liftIO $ runIndexMigrations syncEnv

    isWithinTwoMin :: SlotDetails -> Bool
    isWithinTwoMin sd = isSyncedWithinSeconds sd 120 == SyncFollowing

    isWithinHalfHour :: SlotDetails -> Bool
    isWithinHalfHour sd = isSyncedWithinSeconds sd 1800 == SyncFollowing

    blkNo = headerFieldBlockNo $ getHeaderFields cblk

-- -------------------------------------------------------------------------------------------------

insertLedgerEvents ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  EpochNo ->
  [LedgerEvent] ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertLedgerEvents syncEnv currentEpochNo@(EpochNo curEpoch) =
  mapM_ handler
  where
    tracer = getTrace syncEnv
    cache = envCache syncEnv
    ntw = getNetwork syncEnv

    subFromCurrentEpoch :: Word64 -> EpochNo
    subFromCurrentEpoch m =
      if unEpochNo currentEpochNo >= m
        then EpochNo $ unEpochNo currentEpochNo - m
        else EpochNo 0

    toSyncState :: SyncState -> DB.SyncState
    toSyncState SyncLagging = DB.SyncLagging
    toSyncState SyncFollowing = DB.SyncFollowing

    handler ::
      (MonadBaseControl IO m, MonadIO m) =>
      LedgerEvent ->
      ExceptT SyncNodeError (ReaderT SqlBackend m) ()
    handler ev =
      case ev of
        LedgerNewEpoch en ss -> do
          lift $ do
            insertEpochSyncTime en (toSyncState ss) (envEpochSyncTime syncEnv)
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
          let rewards = Map.toList $ Generic.unRewards rwd
          insertRewards ntw (subFromCurrentEpoch 2) currentEpochNo cache (Map.toList $ Generic.unRewards rwd)
          -- This event is only created when it's not empty, so we don't need to check for null here.
          liftIO . logInfo tracer $ "Inserted " <> show (length rewards) <> " Delta rewards"
        LedgerIncrementalRewards _ rwd -> do
          let rewards = Map.toList $ Generic.unRewards rwd
          insertRewards ntw (subFromCurrentEpoch 1) (EpochNo $ curEpoch + 1) cache rewards
        LedgerRestrainedRewards e rwd creds -> do
          lift $ adjustEpochRewards tracer ntw cache e rwd creds
        LedgerTotalRewards _e rwd -> do
          lift $ validateEpochRewards tracer ntw (subFromCurrentEpoch 2) currentEpochNo rwd
        LedgerAdaPots _ ->
          pure () -- These are handled separately by insertBlock
        LedgerMirDist rwd -> do
          unless (Map.null rwd) $ do
            let rewards = Map.toList rwd
            insertRewards ntw (subFromCurrentEpoch 1) currentEpochNo cache rewards
            liftIO . logInfo tracer $ "Inserted " <> show (length rewards) <> " Mir rewards"
        LedgerPoolReap en drs -> do
          unless (Map.null $ Generic.unRewards drs) $ do
            insertPoolDepositRefunds syncEnv en drs
        LedgerDeposits {} -> pure ()

hasEpochStartEvent :: [LedgerEvent] -> Bool
hasEpochStartEvent = any isNewEpoch
  where
    isNewEpoch :: LedgerEvent -> Bool
    isNewEpoch le =
      case le of
        LedgerNewEpoch {} -> True
        LedgerStartAtEpoch {} -> True
        _otherwise -> False

hasNewEpochEvent :: [LedgerEvent] -> Bool
hasNewEpochEvent = any isNewEpoch
  where
    isNewEpoch :: LedgerEvent -> Bool
    isNewEpoch le =
      case le of
        LedgerNewEpoch {} -> True
        _otherwise -> False
