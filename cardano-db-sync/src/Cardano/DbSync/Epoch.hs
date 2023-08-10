{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Epoch (
  epochHandler,
) where

import Cardano.BM.Trace (Trace, logError, logInfo)
import qualified Cardano.Chain.Block as Byron
import qualified Cardano.Db as DB
import Cardano.DbSync.Api (getTrace)
import Cardano.DbSync.Api.Types (SyncEnv)
import Cardano.DbSync.Cache.Epoch (readEpochBlockDiffFromCache, readLastMapEpochFromCache, writeToMapEpochCache)
import Cardano.DbSync.Cache.Types (Cache (..), EpochBlockDiff (..))
import Cardano.DbSync.Error
import Cardano.DbSync.Types (
  BlockDetails (BlockDetails),
  SlotDetails (..),
  SyncState (SyncFollowing),
 )
import Cardano.DbSync.Util
import Cardano.Prelude hiding (from, on, replace)
import Cardano.Slotting.Slot (unEpochNo)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Database.Esqueleto.Experimental (SqlBackend, replace)
import Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))

-- Populating the Epoch table has two mode:
--  * SyncLagging: when the node is far behind the chain tip and is just updating the DB. In this
--    mode, the row for an epoch is only calculated and inserted when at the end of the epoch.
--  * Following: When the node is at or close to the chain tip, the row for a given epoch is
--    updated on each new block.

epochHandler ::
  SyncEnv ->
  Trace IO Text ->
  Cache ->
  Bool ->
  BlockDetails ->
  ReaderT SqlBackend (LoggingT IO) (Either SyncNodeError ())
epochHandler syncEnv trce cache isNewEpochEvent (BlockDetails cblk details) =
  case cblk of
    BlockByron bblk ->
      case byronBlockRaw bblk of
        Byron.ABOBBoundary {} -> do
          -- For the OBFT era there are no boundary blocks so we ignore them even in
          -- the Ouroboros Classic era but count
          updateEpochStart syncEnv cache details isNewEpochEvent True
        Byron.ABOBBlock _blk ->
          updateEpochStart syncEnv cache details isNewEpochEvent False
    BlockShelley {} -> epochSlotTimecheck
    BlockAllegra {} -> epochSlotTimecheck
    BlockMary {} -> epochSlotTimecheck
    BlockAlonzo {} -> epochSlotTimecheck
    BlockBabbage {} -> epochSlotTimecheck
    BlockConway {} -> epochSlotTimecheck
  where
    -- What we do here is completely independent of Shelley/Allegra/Mary eras.
    epochSlotTimecheck :: ReaderT SqlBackend (LoggingT IO) (Either SyncNodeError ())
    epochSlotTimecheck = do
      when (sdSlotTime details > sdCurrentTime details) $
        liftIO . logError trce $
          mconcat
            ["Slot time '", textShow (sdSlotTime details), "' is in the future"]
      updateEpochStart syncEnv cache details isNewEpochEvent False

updateEpochStart ::
  SyncEnv ->
  Cache ->
  SlotDetails ->
  Bool ->
  Bool ->
  ReaderT SqlBackend (LoggingT IO) (Either SyncNodeError ())
updateEpochStart syncEnv cache slotDetails isNewEpochEvent isBoundaryBlock = do
  mLastMapEpochFromCache <- liftIO $ readLastMapEpochFromCache cache
  mEpochBlockDiff <- liftIO $ readEpochBlockDiffFromCache cache
  let curEpochNo = unEpochNo $ sdEpochNo slotDetails

  if
      -- The tip has been reached so now replace/update the epoch every block.
      | getSyncStatus slotDetails == SyncFollowing ->
          handleEpochWhenFollowing syncEnv cache mLastMapEpochFromCache mEpochBlockDiff curEpochNo
      -- When syncing we check if current block is the first block in an epoch.
      -- If so then it's time to put the previous epoch into the DB.
      | isNewEpochEvent ->
          updateEpochWhenSyncing syncEnv cache mEpochBlockDiff mLastMapEpochFromCache curEpochNo isBoundaryBlock
      -- we're syncing and the epochNo are the same so we just update the cache until above check passes.
      | otherwise ->
          handleEpochCachingWhenSyncing
            syncEnv
            cache
            mLastMapEpochFromCache
            mEpochBlockDiff

-----------------------------------------------------------------------------------------------------
-- When Following
-----------------------------------------------------------------------------------------------------

-- When updating an epoch whilst following we have the opertunity to try and use the cacheEpoch values
-- to calculate our epoch rather than querying the db which is expensive.
handleEpochWhenFollowing ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Cache ->
  Maybe DB.Epoch ->
  Maybe EpochBlockDiff ->
  Word64 ->
  ReaderT SqlBackend m (Either SyncNodeError ())
handleEpochWhenFollowing syncEnv cache newestEpochFromMap epochBlockDiffCache epochNo = do
  case newestEpochFromMap of
    Just newestEpochFromMapache -> do
      case epochBlockDiffCache of
        Nothing -> noCacheUseDB
        Just currentEpCache -> makeEpochWithCacheWhenFollowing syncEnv cache newestEpochFromMapache currentEpCache epochNo

    -- If there isn't an epoch in cache, let's see if we can get one from the db. Otherwise
    -- calculate the epoch using the expensive db query.
    Nothing -> do
      mNewestEpochFromDb <- DB.queryLatestEpoch
      case mNewestEpochFromDb of
        Nothing -> noCacheUseDB
        Just newestEpochFromDb -> do
          -- is the epoch from db different to current epochNo then we need to make expensive query
          if DB.epochNo newestEpochFromDb /= epochNo
            then makeEpochWithDBQuery syncEnv cache Nothing epochNo "handleEpochWhenFollowing"
            else case epochBlockDiffCache of
              Nothing -> noCacheUseDB
              -- Let's use both values aquired to calculate our new epoch.
              Just currentEpCache -> makeEpochWithCacheWhenFollowing syncEnv cache newestEpochFromDb currentEpCache epochNo
  where
    -- this is used if we have no epoch in cache or --disable-cache flag is on
    noCacheUseDB = makeEpochWithDBQuery syncEnv cache Nothing epochNo "handleEpochWhenFollowing no Cache"

-- Update the epoch in cache and db, which could be either an update or insert
-- dependent on if epoch already exists.
makeEpochWithCacheWhenFollowing ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Cache ->
  DB.Epoch ->
  EpochBlockDiff ->
  Word64 ->
  ReaderT SqlBackend m (Either SyncNodeError ())
makeEpochWithCacheWhenFollowing syncEnv cache newestEpochFromMapache currentEpCache epochNo = do
  let calculatedEpoch = calculateNewEpoch newestEpochFromMapache currentEpCache
  -- if the epoch already exists then we update it otherwise create new entry.
  mEpochID <- DB.queryForEpochId epochNo
  case mEpochID of
    Nothing -> do
      _ <- writeToMapEpochCache syncEnv cache calculatedEpoch
      (\_ -> Right ()) <$> DB.insertEpoch calculatedEpoch
    Just epochId -> do
      _ <- writeToMapEpochCache syncEnv cache calculatedEpoch
      Right <$> replace epochId calculatedEpoch

-----------------------------------------------------------------------------------------------------
-- When Syncing
-----------------------------------------------------------------------------------------------------

-- This function is called once every epoch on the very first block of the new epoch.
-- At that point we can get the previously accumilated data from previous epoch and insert/update it into the db.
-- Whilst at the same time store the current block data into epoch cache.
updateEpochWhenSyncing ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Cache ->
  Maybe EpochBlockDiff ->
  Maybe DB.Epoch ->
  Word64 ->
  Bool ->
  ReaderT SqlBackend m (Either SyncNodeError ())
updateEpochWhenSyncing syncEnv cache mEpochBlockDiff mLastMapEpochFromCache epochNo isBoundaryBlock = do
  let trce = getTrace syncEnv
      isFirstEpoch = epochNo == 0
      -- count boundary block in the first epoch
      additionalBlockCount = if isBoundaryBlock && isFirstEpoch then 1 else 0

  case mEpochBlockDiff of
    -- if the flag --disable-cache is active then we won't have an EpochBlockDiff and instead want to
    -- use expensive query to make the epoch.
    Nothing -> do
      newEpoch <- DB.queryCalcEpochEntry epochNo
      writeToMapEpochCache syncEnv cache newEpoch
    Just epochBlockDiffCache ->
      case mLastMapEpochFromCache of
        -- if there is no Map Epoch in cache here, then the server must have restarted and failed on the last
        -- block of the previous epoch and the current block is the first in new Epoch.
        -- We must now use a db query to calculate and insert previous epoch as the cache for the epoch was lost.
        Nothing -> do
          let calculatedEpoch = initCalculateNewEpoch epochBlockDiffCache additionalBlockCount
          _ <- makeEpochWithDBQuery syncEnv cache (Just calculatedEpoch) epochNo "updateEpochWhenSyncing"
          pure $ Right ()
        -- simply use cache
        Just lastMapEpochFromCache -> do
          let calculatedEpoch = initCalculateNewEpoch epochBlockDiffCache additionalBlockCount
          void $ writeToMapEpochCache syncEnv cache calculatedEpoch
          mEpochID <- DB.queryForEpochId epochNo
          case mEpochID of
            Nothing -> do
              liftIO . logInfo trce $ epochSucessMsg "Inserted" "updateEpochWhenSyncing" "Cache" lastMapEpochFromCache
              _ <- DB.insertEpoch lastMapEpochFromCache
              pure $ Right ()
            Just epochId -> do
              liftIO . logInfo trce $ epochSucessMsg "Replaced" "updateEpochWhenSyncing" "Cache" calculatedEpoch
              Right <$> replace epochId calculatedEpoch

-- When syncing, on every block we update the Map epoch in cache. Making sure to handle restarts
handleEpochCachingWhenSyncing ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Cache ->
  Maybe DB.Epoch ->
  Maybe EpochBlockDiff ->
  ReaderT SqlBackend m (Either SyncNodeError ())
handleEpochCachingWhenSyncing syncEnv cache newestEpochFromMap epochBlockDiffCache = do
  case (newestEpochFromMap, epochBlockDiffCache) of
    (Just newestEpMap, Just currentEpC) -> do
      let calculatedEpoch = calculateNewEpoch newestEpMap currentEpC
      writeToMapEpochCache syncEnv cache calculatedEpoch
    -- when we don't have a newestEpochFromMap the server must have been restarted.
    -- so we need to replenish the cache using expensive db query.
    (Nothing, Just currentEpC) -> do
      newEpoch <- DB.queryCalcEpochEntry $ ebdEpochNo currentEpC
      writeToMapEpochCache syncEnv cache newEpoch
    -- There will always be a EpochBlockDiff at this point in time
    (_, _) -> pure $ Left $ SNErrDefault "handleEpochCachingWhenSyncing: No caches available to update cache"

-----------------------------------------------------------------------------------------------------
-- Helper functions
-----------------------------------------------------------------------------------------------------

-- This is an expensive DB query so we minimise it's use to
-- server restarts when syncing or folloing and rollbacks
makeEpochWithDBQuery ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Cache ->
  Maybe DB.Epoch ->
  Word64 ->
  Text ->
  ReaderT SqlBackend m (Either SyncNodeError ())
makeEpochWithDBQuery syncEnv cache mInitEpoch epochNo callSiteMsg = do
  let trce = getTrace syncEnv
  calcEpoch <- DB.queryCalcEpochEntry epochNo
  mEpochID <- DB.queryForEpochId epochNo
  let epochInitOrCalc = fromMaybe calcEpoch mInitEpoch
  case mEpochID of
    Nothing -> do
      _ <- writeToMapEpochCache syncEnv cache epochInitOrCalc
      _ <- DB.insertEpoch calcEpoch
      liftIO . logInfo trce $ epochSucessMsg "Inserted " callSiteMsg "DB query" calcEpoch
      pure $ Right ()
    Just epochId -> do
      -- write the newly calculated epoch to cache.
      _ <- writeToMapEpochCache syncEnv cache epochInitOrCalc
      liftIO . logInfo trce $ epochSucessMsg "Replaced " callSiteMsg "DB query" calcEpoch
      Right <$> replace epochId calcEpoch

-- Because we store a Map of epochs, at every iteration we take the newest epoch and it's values
-- We then add those to the data we kept when inserting the txs & block inside the EpochBlockDiff cache.
calculateNewEpoch ::
  DB.Epoch ->
  EpochBlockDiff ->
  DB.Epoch
calculateNewEpoch newestEpochMapCache epochBlockDiffCache =
  -- if the bellow doesn't equal, then it must be a the first block in new epoch thus we initiate the values.
  -- rather than adding them to the newest epoch we have in the Map Epoch.
  if DB.epochNo newestEpochMapCache == ebdEpochNo epochBlockDiffCache
    then do
      let newBlkCount = DB.epochBlkCount newestEpochMapCache + 1
          newOutSum = DB.epochOutSum newestEpochMapCache + ebdOutSum epochBlockDiffCache
          newFees = DB.unDbLovelace (DB.epochFees newestEpochMapCache) + ebdFees epochBlockDiffCache
          newTxCount = fromIntegral (DB.epochTxCount newestEpochMapCache) + ebdTxCount epochBlockDiffCache
          newEpochNo = ebdEpochNo epochBlockDiffCache
          newStartTime = DB.epochStartTime newestEpochMapCache
          newEndTime = ebdTime epochBlockDiffCache
      DB.Epoch
        { DB.epochOutSum = newOutSum
        , DB.epochFees = DB.DbLovelace newFees
        , DB.epochTxCount = fromIntegral newTxCount
        , DB.epochBlkCount = fromIntegral newBlkCount
        , DB.epochNo = newEpochNo
        , DB.epochStartTime = newStartTime
        , DB.epochEndTime = newEndTime
        }
    else initCalculateNewEpoch epochBlockDiffCache 0

initCalculateNewEpoch :: EpochBlockDiff -> Word64 -> DB.Epoch
initCalculateNewEpoch epochBlockDiffCache boundaryBlockcount =
  DB.Epoch
    { DB.epochOutSum = ebdOutSum epochBlockDiffCache
    , DB.epochFees = DB.DbLovelace $ ebdFees epochBlockDiffCache
    , DB.epochTxCount = ebdTxCount epochBlockDiffCache
    , DB.epochBlkCount = 1 + boundaryBlockcount
    , DB.epochNo = ebdEpochNo epochBlockDiffCache
    , -- as this is the first block in epoch the end time and start time are the same
      DB.epochStartTime = ebdTime epochBlockDiffCache
    , DB.epochEndTime = ebdTime epochBlockDiffCache
    }

epochSucessMsg :: Text -> Text -> Text -> DB.Epoch -> Text
epochSucessMsg insertOrReplace callSite cacheOrDB newEpoch =
  mconcat
    [ "\n"
    , insertOrReplace
    , " epoch "
    , DB.textShow $ DB.epochNo newEpoch
    , " from "
    , callSite
    , " with "
    , cacheOrDB
    , ". \n epoch: "
    , DB.textShow newEpoch
    ]
