{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Epoch (
  epochHandler,
) where

import Cardano.BM.Trace (logError, logInfo)
import qualified Cardano.Chain.Block as Byron
import qualified Cardano.Db as DB
import Cardano.DbSync.AppT (App, MonadAppDB (..), askTrace)
import Cardano.DbSync.Cache.Epoch (readEpochBlockDiffFromCache, readLastMapEpochFromCache, writeToMapEpochCache)
import Cardano.DbSync.Cache.Types (CacheStatus (..), EpochBlockDiff (..))
import Cardano.DbSync.Error.Types (SyncNodeError (..))
import Cardano.DbSync.Types (
  BlockDetails (BlockDetails),
  SlotDetails (..),
  SyncState (SyncFollowing),
 )
import Cardano.DbSync.Util
import Cardano.Prelude hiding (from, on, replace)
import Cardano.Slotting.Slot (unEpochNo)
import Database.Esqueleto.Experimental (replace)
import Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))

-- Populating the Epoch table has two mode:
--  * SyncLagging: when the node is far behind the chain tip and is just updating the DB. In this
--    mode, the row for an epoch is only calculated and inserted when at the end of the epoch.
--  * Following: When the node is at or close to the chain tip, the row for a given epoch is
--    updated on each new block.

epochHandler ::
  CacheStatus ->
  Bool ->
  BlockDetails ->
  App (Either SyncNodeError ())
epochHandler cache isNewEpochEvent (BlockDetails cblk details) =
  case cblk of
    BlockByron bblk ->
      case byronBlockRaw bblk of
        Byron.ABOBBoundary {} -> do
          -- For the OBFT era there are no boundary blocks so we ignore them even in
          -- the Ouroboros Classic era but count
          updateEpochStart cache details isNewEpochEvent True
        Byron.ABOBBlock _blk ->
          updateEpochStart cache details isNewEpochEvent False
    BlockShelley {} -> epochSlotTimecheck
    BlockAllegra {} -> epochSlotTimecheck
    BlockMary {} -> epochSlotTimecheck
    BlockAlonzo {} -> epochSlotTimecheck
    BlockBabbage {} -> epochSlotTimecheck
    BlockConway {} -> epochSlotTimecheck
  where
    -- What we do here is completely independent of Shelley/Allegra/Mary eras.
    epochSlotTimecheck :: App (Either SyncNodeError ())
    epochSlotTimecheck = do
      trce <- askTrace
      when (sdSlotTime details > sdCurrentTime details)
        $ liftIO
          . logError trce
        $ mconcat
          ["Slot time '", textShow (sdSlotTime details), "' is in the future"]
      updateEpochStart cache details isNewEpochEvent False

updateEpochStart ::
  CacheStatus ->
  SlotDetails ->
  Bool ->
  Bool ->
  App (Either SyncNodeError ())
updateEpochStart cache slotDetails isNewEpochEvent isBoundaryBlock = do
  mLastMapEpochFromCache <- readLastMapEpochFromCache cache
  mEpochBlockDiff <- readEpochBlockDiffFromCache cache
  let curEpochNo = unEpochNo $ sdEpochNo slotDetails

  if
      -- The tip has been reached so now replace/update the epoch every block.
      | getSyncStatus slotDetails == SyncFollowing ->
          handleEpochWhenFollowing cache mLastMapEpochFromCache mEpochBlockDiff curEpochNo
      -- When syncing we check if current block is the first block in an epoch.
      -- If so then it's time to put the previous epoch into the DB.
      | isNewEpochEvent ->
          updateEpochWhenSyncing cache mEpochBlockDiff mLastMapEpochFromCache curEpochNo isBoundaryBlock
      -- we're syncing and the epochNo are the same so we just update the cache until above check passes.
      | otherwise ->
          handleEpochCachingWhenSyncing
            cache
            mLastMapEpochFromCache
            mEpochBlockDiff

-----------------------------------------------------------------------------------------------------
-- When Following
-----------------------------------------------------------------------------------------------------

-- When updating an epoch whilst following we have the opertunity to try and use the cacheEpoch values
-- to calculate our epoch rather than querying the db which is expensive.
handleEpochWhenFollowing ::
  CacheStatus ->
  Maybe DB.Epoch ->
  Maybe EpochBlockDiff ->
  Word64 ->
  App (Either SyncNodeError ())
handleEpochWhenFollowing cache newestEpochFromMap epochBlockDiffCache epochNo = do
  case newestEpochFromMap of
    Just newestEpochFromMapache -> do
      case epochBlockDiffCache of
        Nothing -> noCacheUseDB
        Just currentEpCache -> makeEpochWithCacheWhenFollowing cache newestEpochFromMapache currentEpCache epochNo

    -- If there isn't an epoch in cache, let's see if we can get one from the db. Otherwise
    -- calculate the epoch using the expensive db query.
    Nothing -> do
      mNewestEpochFromDb <- dbQueryToApp DB.queryLatestEpoch
      case mNewestEpochFromDb of
        Nothing -> noCacheUseDB
        Just newestEpochFromDb -> do
          -- is the epoch from db different to current epochNo then we need to make expensive query
          if DB.epochNo newestEpochFromDb /= epochNo
            then makeEpochWithDBQuery cache Nothing epochNo "handleEpochWhenFollowing"
            else case epochBlockDiffCache of
              Nothing -> noCacheUseDB
              -- Let's use both values aquired to calculate our new epoch.
              Just currentEpCache -> makeEpochWithCacheWhenFollowing cache newestEpochFromDb currentEpCache epochNo
  where
    -- this is used if we have no epoch in cache or --disable-cache flag is on
    noCacheUseDB = makeEpochWithDBQuery cache Nothing epochNo "handleEpochWhenFollowing no Cache"

-- Update the epoch in cache and db, which could be either an update or insert
-- dependent on if epoch already exists.
makeEpochWithCacheWhenFollowing ::
  CacheStatus ->
  DB.Epoch ->
  EpochBlockDiff ->
  Word64 ->
  App (Either SyncNodeError ())
makeEpochWithCacheWhenFollowing cache newestEpochFromMapache currentEpCache epochNo = do
  let calculatedEpoch = calculateNewEpoch newestEpochFromMapache currentEpCache
  -- if the epoch already exists then we update it otherwise create new entry.
  mEpochID <- dbQueryToApp $ DB.queryForEpochId epochNo
  case mEpochID of
    Nothing -> do
      _ <- writeToMapEpochCache cache calculatedEpoch
      (\_ -> Right ()) <$> dbQueryToApp (DB.insertEpoch calculatedEpoch)
    Just epochId -> do
      _ <- writeToMapEpochCache cache calculatedEpoch
      res <- dbQueryToApp $ replace epochId calculatedEpoch
      pure $ Right res

-----------------------------------------------------------------------------------------------------
-- When Syncing
-----------------------------------------------------------------------------------------------------

-- This function is called once every epoch on the very first block of the new epoch.
-- At that point we can get the previously accumilated data from previous epoch and insert/update it into the db.
-- Whilst at the same time store the current block data into epoch cache.
updateEpochWhenSyncing ::
  CacheStatus ->
  Maybe EpochBlockDiff ->
  Maybe DB.Epoch ->
  Word64 ->
  Bool ->
  App (Either SyncNodeError ())
updateEpochWhenSyncing cache mEpochBlockDiff mLastMapEpochFromCache epochNo isBoundaryBlock = do
  trce <- askTrace
  let isFirstEpoch = epochNo == 0
      -- count boundary block in the first epoch
      additionalBlockCount = if isBoundaryBlock && isFirstEpoch then 1 else 0

  case mEpochBlockDiff of
    -- if the flag --disable-cache is active then we won't have an EpochBlockDiff and instead want to
    -- use expensive query to make the epoch.
    Nothing -> do
      newEpoch <- dbQueryToApp $ DB.queryCalcEpochEntry epochNo
      writeToMapEpochCache cache newEpoch
    Just epochBlockDiffCache ->
      case mLastMapEpochFromCache of
        -- if there is no Map Epoch in cache here, then the server must have restarted and failed on the last
        -- block of the previous epoch and the current block is the first in new Epoch.
        -- We must now use a db query to calculate and insert previous epoch as the cache for the epoch was lost.
        Nothing -> do
          let calculatedEpoch = initCalculateNewEpoch epochBlockDiffCache additionalBlockCount
          _ <- makeEpochWithDBQuery cache (Just calculatedEpoch) epochNo "updateEpochWhenSyncing"
          pure $ Right ()
        -- simply use cache
        Just lastMapEpochFromCache -> do
          let calculatedEpoch = initCalculateNewEpoch epochBlockDiffCache additionalBlockCount
          void $ writeToMapEpochCache cache calculatedEpoch
          mEpochID <- dbQueryToApp $ DB.queryForEpochId epochNo
          case mEpochID of
            Nothing -> do
              liftIO . logInfo trce $ epochSucessMsg "Inserted" "updateEpochWhenSyncing" "Cache" lastMapEpochFromCache
              _ <- dbQueryToApp $ DB.insertEpoch lastMapEpochFromCache
              pure $ Right ()
            Just epochId -> do
              liftIO . logInfo trce $ epochSucessMsg "Replaced" "updateEpochWhenSyncing" "Cache" calculatedEpoch
              res <- dbQueryToApp $ replace epochId calculatedEpoch
              pure $ Right res

-- When syncing, on every block we update the Map epoch in cache. Making sure to handle restarts
handleEpochCachingWhenSyncing ::
  CacheStatus ->
  Maybe DB.Epoch ->
  Maybe EpochBlockDiff ->
  App (Either SyncNodeError ())
handleEpochCachingWhenSyncing cache newestEpochFromMap epochBlockDiffCache = do
  case (newestEpochFromMap, epochBlockDiffCache) of
    (Just newestEpMap, Just currentEpC) -> do
      let calculatedEpoch = calculateNewEpoch newestEpMap currentEpC
      writeToMapEpochCache cache calculatedEpoch
    -- when we don't have a newestEpochFromMap the server must have been restarted.
    -- so we need to replenish the cache using expensive db query.
    (Nothing, Just currentEpC) -> do
      newEpoch <- dbQueryToApp $ DB.queryCalcEpochEntry $ ebdEpochNo currentEpC
      writeToMapEpochCache cache newEpoch
    -- There will always be a EpochBlockDiff at this point in time
    (_, _) -> pure $ Left $ SNErrDefault "handleEpochCachingWhenSyncing: No caches available to update cache"

-----------------------------------------------------------------------------------------------------
-- Helper functions
-----------------------------------------------------------------------------------------------------

-- This is an expensive DB query so we minimise it's use to
-- server restarts when syncing or folloing and rollbacks
makeEpochWithDBQuery ::
  CacheStatus ->
  Maybe DB.Epoch ->
  Word64 ->
  Text ->
  App (Either SyncNodeError ())
makeEpochWithDBQuery cache mInitEpoch epochNo callSiteMsg = do
  trce <- askTrace
  calcEpoch <- dbQueryToApp $ DB.queryCalcEpochEntry epochNo
  mEpochID <- dbQueryToApp $ DB.queryForEpochId epochNo
  let epochInitOrCalc = fromMaybe calcEpoch mInitEpoch
  case mEpochID of
    Nothing -> do
      _ <- writeToMapEpochCache cache epochInitOrCalc
      _ <- dbQueryToApp $ DB.insertEpoch calcEpoch
      liftIO . logInfo trce $ epochSucessMsg "Inserted " callSiteMsg "DB query" calcEpoch
      pure $ Right ()
    Just epochId -> do
      -- write the newly calculated epoch to cache.
      _ <- writeToMapEpochCache cache epochInitOrCalc
      liftIO . logInfo trce $ epochSucessMsg "Replaced " callSiteMsg "DB query" calcEpoch
      result <- dbQueryToApp $ replace epochId calcEpoch
      pure $ Right result

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
