{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Threads.Database (
  DbAction (..),
  ThreadChannels,
  lengthDbActionQueue,
  mkDbApply,
  runDbThread,
) where

import Cardano.BM.Trace (logError, logInfo)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (ConsistentLevel (..), LedgerEnv (..), SyncEnv (..))
import Cardano.DbSync.Block
import Cardano.DbSync.DbAction
import Cardano.DbSync.Error
import Cardano.DbSync.Ledger.State
import Cardano.DbSync.Ledger.Types (CardanoLedgerState (..), LedgerStateFile (..), SnapshotPoint (..))
import Cardano.DbSync.Metrics
import Cardano.DbSync.Rollback
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.Prelude hiding (atomically)
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..), WithOrigin (..))
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Extra (whenJust)
import Control.Monad.Trans.Except.Extra (newExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Database.Persist.Sql (SqlBackend)
import Ouroboros.Consensus.Block.Abstract (HeaderHash, Point (..), fromRawHash)
import Ouroboros.Consensus.HeaderValidation hiding (TipInfo)
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Network.Block (BlockNo (..), Point (..))
import Ouroboros.Network.Point (blockPointHash, blockPointSlot)
import qualified Ouroboros.Network.Point as Point

data NextState
  = Continue
  | Done
  deriving (Eq)

runDbThread ::
  SyncEnv ->
  MetricSetters ->
  ThreadChannels ->
  IO ()
runDbThread syncEnv metricsSetters queue = do
  logInfo trce "Running DB thread"
  logException trce "runDBThread: " loop
  logInfo trce "Shutting down DB thread"
  where
    trce = getTrace syncEnv
    loop = do
      xs <- blockingFlushDbActionQueue queue

      case hasRestart xs of
        Nothing -> do
          eNextState <- runExceptT $ runActions syncEnv xs

          mBlock <- getDbLatestBlockInfo (envBackend syncEnv)
          whenJust mBlock $ \block -> do
            setDbBlockHeight metricsSetters $ bBlockNo block
            setDbSlotHeight metricsSetters $ bSlotNo block

          case eNextState of
            Left err -> logError trce $ show err
            Right Continue -> loop
            Right Done -> pure ()
        Just resultVar -> do
          -- In this case the syncing thread has restarted, so ignore all blocks that are not
          -- inserted yet.
          logInfo trce "Chain Sync client thread has restarted"
          latestPoints <- getLatestPoints syncEnv
          currentTip <- getCurrentTipBlockNo syncEnv
          logDbState syncEnv
          atomically $ putTMVar resultVar (latestPoints, currentTip)
          loop

-- | Run the list of 'DbAction's. Block are applied in a single set (as a transaction)
-- and other operations are applied one-by-one.
runActions ::
  SyncEnv ->
  [DbAction] ->
  ExceptT SyncNodeError IO NextState
runActions syncEnv actions = do
  dbAction Continue actions
  where
    dbAction :: NextState -> [DbAction] -> ExceptT SyncNodeError IO NextState
    dbAction next [] = pure next
    dbAction Done _ = pure Done
    dbAction Continue xs =
      case spanDbApply xs of
        ([], DbFinish : _) -> do
          pure Done
        ([], DbRollBackToPoint chainSyncPoint serverTip resultVar : ys) -> do
          deletedAllBlocks <- newExceptT $ prepareRollback syncEnv chainSyncPoint serverTip
          points <- lift $ rollbackLedger syncEnv chainSyncPoint

          -- Ledger state always rollbacks at least back to the 'point' given by the Node.
          -- It needs to rollback even further, if 'points' is not 'Nothing'.
          -- The db may not rollback to the Node point.
          case (deletedAllBlocks, points) of
            (True, Nothing) -> do
              liftIO $ setConsistentLevel syncEnv Consistent
              liftIO $ validateConsistentLevel syncEnv chainSyncPoint
            (False, Nothing) -> do
              liftIO $ setConsistentLevel syncEnv DBAheadOfLedger
              liftIO $ validateConsistentLevel syncEnv chainSyncPoint
            _anyOtherOption ->
              -- No need to validate here
              liftIO $ setConsistentLevel syncEnv DBAheadOfLedger
          blockNo <- lift $ getDbTipBlockNo syncEnv
          lift $ atomically $ putTMVar resultVar (points, blockNo)
          dbAction Continue ys
        (ys, zs) -> do
          insertListBlocks syncEnv ys
          if null zs
            then pure Continue
            else dbAction Continue zs

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

-- | This not only checks that the ledger and ChainSync points are equal, but also that the
-- 'Consistent' Level is correct based on the db tip.
validateConsistentLevel :: SyncEnv -> CardanoPoint -> IO ()
validateConsistentLevel syncEnv stPoint = do
  dbTipInfo <- getDbLatestBlockInfo (envBackend syncEnv)
  cLevel <- getConsistentLevel syncEnv
  compareTips stPoint dbTipInfo cLevel
  where
    compareTips _ dbTip Unchecked =
      logAndThrowIO trce $
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
      logAndThrowIO trce $
        SNErrDatabaseValConstLevel $
          "Unexpected Consistent Level. " <> showContext dbTip cLevel

    trce = getTrace syncEnv
    showContext dbTip cLevel =
      mconcat
        [ "Ledger state point is "
        , show stPoint
        , ". DB Tip is "
        , show dbTip
        , ". Consistent Level is "
        , show cLevel
        ]

-- | Split the DbAction list into a prefix containing blocks to apply and a postfix.
spanDbApply :: [DbAction] -> ([CardanoBlock], [DbAction])
spanDbApply lst =
  case lst of
    (DbApplyBlock bt : xs) -> let (ys, zs) = spanDbApply xs in (bt : ys, zs)
    xs -> ([], xs)

hasRestart :: [DbAction] -> Maybe (StrictTMVar IO ([(CardanoPoint, Bool)], WithOrigin BlockNo))
hasRestart = go
  where
    go [] = Nothing
    go (DbRestartState mvar : _) = Just mvar
    go (_ : rest) = go rest

-- | 'True' is for in memory points and 'False' for on disk
getLatestPoints :: SyncEnv -> IO [(CardanoPoint, Bool)]
getLatestPoints env = do
  case envLedgerEnv env of
    HasLedger hasLedgerEnv -> do
      snapshotPoints <- listKnownSnapshots hasLedgerEnv
      verifySnapshotPoint env snapshotPoints
    NoLedger _ -> do
      -- Brings the 5 latest.
      lastPoints <- DB.runDbIohkNoLogging (envBackend env) DB.queryLatestPoints
      pure $ mapMaybe convert lastPoints
  where
    convert (Nothing, _) = Nothing
    convert (Just slot, bs) = convertToDiskPoint (SlotNo slot) bs

verifySnapshotPoint :: SyncEnv -> [SnapshotPoint] -> IO [(CardanoPoint, Bool)]
verifySnapshotPoint env snapPoints =
  catMaybes <$> mapM validLedgerFileToPoint snapPoints
  where
    validLedgerFileToPoint :: SnapshotPoint -> IO (Maybe (CardanoPoint, Bool))
    validLedgerFileToPoint (OnDisk lsf) = do
      hashes <- getSlotHash (envBackend env) (lsfSlotNo lsf)
      let valid = find (\(_, h) -> lsfHash lsf == hashToAnnotation h) hashes
      case valid of
        Just (slot, hash) | slot == lsfSlotNo lsf -> pure $ convertToDiskPoint slot hash
        _ -> pure Nothing
    validLedgerFileToPoint (InMemory pnt) = do
      case pnt of
        GenesisPoint -> pure Nothing
        BlockPoint slotNo hsh -> do
          hashes <- getSlotHash (envBackend env) slotNo
          let valid = find (\(_, dbHash) -> getHeaderHash hsh == dbHash) hashes
          case valid of
            Just (dbSlotNo, _) | slotNo == dbSlotNo -> pure $ Just (pnt, True)
            _ -> pure Nothing

convertToDiskPoint :: SlotNo -> ByteString -> Maybe (CardanoPoint, Bool)
convertToDiskPoint slot hashBlob = (,False) <$> convertToPoint slot hashBlob

convertToPoint :: SlotNo -> ByteString -> Maybe CardanoPoint
convertToPoint slot hashBlob =
  Point . Point.block slot <$> convertHashBlob hashBlob
  where
    convertHashBlob :: ByteString -> Maybe (HeaderHash CardanoBlock)
    convertHashBlob = Just . fromRawHash (Proxy @CardanoBlock)

getSlotHash :: SqlBackend -> SlotNo -> IO [(SlotNo, ByteString)]
getSlotHash backend = DB.runDbIohkNoLogging backend . DB.querySlotHash

getDbLatestBlockInfo :: SqlBackend -> IO (Maybe TipInfo)
getDbLatestBlockInfo backend = do
  runMaybeT $ do
    block <- MaybeT $ DB.runDbIohkNoLogging backend DB.queryLatestBlock
    -- The EpochNo, SlotNo and BlockNo can only be zero for the Byron
    -- era, but we need to make the types match, hence `fromMaybe`.
    pure $
      TipInfo
        { bHash = DB.blockHash block
        , bEpochNo = EpochNo . fromMaybe 0 $ DB.blockEpochNo block
        , bSlotNo = SlotNo . fromMaybe 0 $ DB.blockSlotNo block
        , bBlockNo = BlockNo . fromMaybe 0 $ DB.blockBlockNo block
        }

getDbTipBlockNo :: SyncEnv -> IO (Point.WithOrigin BlockNo)
getDbTipBlockNo env = do
  mblk <- getDbLatestBlockInfo (envBackend env)
  pure $ maybe Point.Origin (Point.At . bBlockNo) mblk

logDbState :: SyncEnv -> IO ()
logDbState env = do
  mblk <- getDbLatestBlockInfo (envBackend env)
  case mblk of
    Nothing -> logInfo trce "Database is empty"
    Just tip -> logInfo trce $ mconcat ["Database tip is at ", showTip tip]
  where
    showTip :: TipInfo -> Text
    showTip tipInfo =
      mconcat
        [ "slot "
        , textShow (unSlotNo $ bSlotNo tipInfo)
        , ", block "
        , textShow (unBlockNo $ bBlockNo tipInfo)
        ]

    trce = getTrace env

getCurrentTipBlockNo :: SyncEnv -> IO (WithOrigin BlockNo)
getCurrentTipBlockNo env = do
  maybeTip <- getDbLatestBlockInfo (envBackend env)
  case maybeTip of
    Just tip -> pure $ At (bBlockNo tip)
    Nothing -> pure Origin
