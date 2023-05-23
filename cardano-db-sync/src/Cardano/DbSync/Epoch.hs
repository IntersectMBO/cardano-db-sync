{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Epoch (
  epochHandler,
) where

import Cardano.BM.Trace (Trace, logError, logInfo)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api (SyncEnv, getTrace)
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
epochHandler syncEnv trce cache isStartEvent (BlockDetails cblk details) =
  case cblk of
    BlockByron {} -> updateEpochStart syncEnv cache details isStartEvent
    BlockShelley {} -> epochSlotTimecheck
    BlockAllegra {} -> epochSlotTimecheck
    BlockMary {} -> epochSlotTimecheck
    BlockAlonzo {} -> epochSlotTimecheck
    BlockBabbage {} -> epochSlotTimecheck
    BlockConway {} -> panic "TODO: Conway not supported yet"
  where
    -- What we do here is completely independent of Shelley/Allegra/Mary eras.
    epochSlotTimecheck :: ReaderT SqlBackend (LoggingT IO) (Either SyncNodeError ())
    epochSlotTimecheck = do
      when (sdSlotTime details > sdCurrentTime details) $
        liftIO . logError trce $
          mconcat
            ["Slot time '", textShow (sdSlotTime details), "' is in the future"]
      updateEpochStart syncEnv cache details isStartEvent

updateEpochStart ::
  SyncEnv ->
  Cache ->
  SlotDetails ->
  Bool ->
  ReaderT SqlBackend (LoggingT IO) (Either SyncNodeError ())
updateEpochStart syncEnv cache slotDetails isStartEvent = do
  mLastMapEpochFromCache <- liftIO $ readLastMapEpochFromCache cache
  mEpochBlockDiff <- liftIO $ readEpochBlockDiffFromCache cache
  let curEpochNo = unEpochNo $ sdEpochNo slotDetails

  if
      -- The tip has been reached so now replace/update the epoch every block.
      | getSyncStatus slotDetails == SyncFollowing ->
          handleEpochWhenFollowing syncEnv cache mLastMapEpochFromCache mEpochBlockDiff curEpochNo
      -- When syncing we check if current block is the first block in an epoch.
      -- If so then it's time to put the previous epoch into the DB.
      | isStartEvent ->
          handleEpochWhenSyncing syncEnv cache mEpochBlockDiff mLastMapEpochFromCache curEpochNo
      -- we're syncing and the epochNo are the same so we just update the cache until above check passes.
      | otherwise ->
          handleEpochCacheWhenSyncing
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
handleEpochWhenFollowing syncEnv cache newestEpochFromMap currentEpochCache epochNo = do
  case newestEpochFromMap of
    Just lastMapEpCache -> do
      case currentEpochCache of
        Nothing -> pure $ Left $ NEError "replaceEpoch: No currentEpochCache"
        Just currentEpCache -> makeEpochWithCacheWhenFollowing syncEnv cache lastMapEpCache currentEpCache epochNo

    -- If there isn't an epoch in cache, let's see if we can get one from the db. Otherwise
    -- calculate the epoch using the expensive db query.
    Nothing -> do
      mNewestEpochFromDb <- DB.queryLatestEpoch
      case mNewestEpochFromDb of
        -- no latest epoch in db (very unlikely) so lets use expensive db query
        Nothing -> do
          makeEpochWithDBQuery syncEnv cache epochNo
        Just newestEpochFromDb -> do
          case currentEpochCache of
            -- There should never be no EpochBlockDiff in cache at this point in the pipeline but just incase!
            Nothing -> pure $ Left $ NEError "replaceEpoch: No currentEpochCache"
            -- Let's use both values aquired to calculate our new epoch.
            Just currentEpCache -> makeEpochWithCacheWhenFollowing syncEnv cache newestEpochFromDb currentEpCache epochNo

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
makeEpochWithCacheWhenFollowing syncEnv cache lastMapEpCache currentEpCache epochNo = do
  let calculatedEpoch = calculateNewEpoch lastMapEpCache currentEpCache
      trce = getTrace syncEnv
  -- if the epoch already exists then we update it otherwise create new entry.
  mEpochID <- DB.queryForEpochId epochNo
  case mEpochID of
    Nothing -> do
      _ <- writeToMapEpochCache syncEnv cache calculatedEpoch
      liftIO . logInfo trce $ "makeEpochWithCacheWhenFollowing: Insert epoch " <> textShow epochNo
      (\_ -> Right ()) <$> DB.insertEpoch calculatedEpoch
    Just epochId -> do
      _ <- writeToMapEpochCache syncEnv cache calculatedEpoch
      liftIO . logInfo trce $ "makeEpochWithCacheWhenFollowing: Replace epoch " <> textShow epochNo
      Right <$> replace epochId calculatedEpoch

-----------------------------------------------------------------------------------------------------
-- When Syncing
-----------------------------------------------------------------------------------------------------

-- This function is called once every epoch on the very first block of the new epoch.
-- At that point we can get the previously accumilated data from previous epoch and insert/update it into the db.
-- Whilst at the same time store the current block data into epoch cache.
handleEpochWhenSyncing ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Cache ->
  Maybe EpochBlockDiff ->
  Maybe DB.Epoch ->
  Word64 ->
  ReaderT SqlBackend m (Either SyncNodeError ())
handleEpochWhenSyncing syncEnv cache mEpochBlockDiff mLastMapEpochFromCache epochNo = do
  let trce = getTrace syncEnv
  case mEpochBlockDiff of
    -- theres should always be a mEpochBlockDiff at this point
    Nothing -> pure $ Left $ NEError "handleEpochWhenSyncing: No mEpochBlockDiff"
    Just currentEpochCache ->
      case mLastMapEpochFromCache of
        -- if there is no Map Epoch in cache here, then the server must have restarted and failed on the last
        -- block of the previous epoch and the current block is the first in new Epoch.
        -- We must now use a db query to calculate and insert previous epoch as the cache for the epoch was lost.
        Nothing -> do
          let calculatedEpoch = initCalculateNewEpoch currentEpochCache
          _ <- writeToMapEpochCache syncEnv cache calculatedEpoch
          _ <- makeEpochWithDBQuery syncEnv cache $ ebdEpochNo currentEpochCache - 1
          pure $ Right ()
        -- simply use cache
        Just lastMapEpochFromCache -> do
          let calculatedEpoch = initCalculateNewEpoch currentEpochCache
          _ <- writeToMapEpochCache syncEnv cache calculatedEpoch
          mEpochID <- DB.queryForEpochId epochNo
          case mEpochID of
            Nothing -> do
              liftIO . logInfo trce $ epochSucessMsg "Inserted" "Cache" lastMapEpochFromCache
              _ <- DB.insertEpoch lastMapEpochFromCache
              pure $ Right ()
            Just epochId -> do
              liftIO . logInfo trce $ epochSucessMsg "Replaced" "Cache" calculatedEpoch
              Right <$> replace epochId calculatedEpoch

-- When syncing, on every block we update the Map epoch in cache. Making sure to handle restarts
handleEpochCacheWhenSyncing ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Cache ->
  Maybe DB.Epoch ->
  Maybe EpochBlockDiff ->
  ReaderT SqlBackend m (Either SyncNodeError ())
handleEpochCacheWhenSyncing syncEnv cache newestEpochFromMap currentEpochCache = do
  case (newestEpochFromMap, currentEpochCache) of
    (Just lastMapEpC, Just currentEpC) -> do
      let calculatedEpoch = calculateNewEpoch lastMapEpC currentEpC
      writeToMapEpochCache syncEnv cache calculatedEpoch
    -- when we don't have a newestEpochFromMap the server must have been restarted.
    -- so we need to replenish the cache using expensive db query.
    (Nothing, Just currentEpC) -> do
      newEpoch <- DB.queryCalcEpochEntry $ ebdEpochNo currentEpC
      writeToMapEpochCache syncEnv cache newEpoch
    -- There will always be a EpochBlockDiff at this point in time
    (_, _) -> pure $ Left $ NEError "handleEpochCacheWhenSyncing: No caches available to update cache"

-----------------------------------------------------------------------------------------------------
-- Helper functions
-----------------------------------------------------------------------------------------------------

-- This is an expensive DB query so we minimise it's use to
-- server restarts when syncing or folloing and rollbacks
makeEpochWithDBQuery ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Cache ->
  Word64 ->
  ReaderT SqlBackend m (Either SyncNodeError ())
makeEpochWithDBQuery syncEnv cache epochNo = do
  let trce = getTrace syncEnv
  newEpoch <- DB.queryCalcEpochEntry epochNo
  mEpochID <- DB.queryForEpochId epochNo
  case mEpochID of
    Nothing -> do
      _ <- writeToMapEpochCache syncEnv cache newEpoch
      _ <- DB.insertEpoch newEpoch
      liftIO . logInfo trce $ epochSucessMsg "Inserted" "DB query" newEpoch
      pure $ Right ()
    Just epochId -> do
      -- write the newly calculated epoch to cache.
      _ <- writeToMapEpochCache syncEnv cache newEpoch
      liftIO . logInfo trce $ epochSucessMsg "Replaced" "DB query" newEpoch
      Right <$> replace epochId newEpoch

-- Because we store a Map of epochs, at every iteration we take the newest epoch and it's values
-- We then add those to the data we kept when inserting the txs & block inside the EpochBlockDiff cache.
calculateNewEpoch ::
  DB.Epoch ->
  EpochBlockDiff ->
  DB.Epoch
calculateNewEpoch newestEpochMapCache currentEpochCache =
  -- if the bellow doesn't equal, then it must be a the first block in new epoch thus we initiate the values.
  -- rather than adding them to the newest epoch we have in the Map Epoch.
  if DB.epochNo newestEpochMapCache == ebdEpochNo currentEpochCache
    then do
      let newBlkCount = DB.epochBlkCount newestEpochMapCache + 1
          newOutSum = DB.epochOutSum newestEpochMapCache + ebdOutSum currentEpochCache
          newFees = DB.unDbLovelace (DB.epochFees newestEpochMapCache) + ebdFees currentEpochCache
          newTxCount = fromIntegral (DB.epochTxCount newestEpochMapCache) + ebdTxCount currentEpochCache
          newEpochNo = ebdEpochNo currentEpochCache
          newStartTime = DB.epochStartTime newestEpochMapCache
          newEndTime = ebdTime currentEpochCache
      DB.Epoch
        { DB.epochOutSum = newOutSum
        , DB.epochFees = DB.DbLovelace newFees
        , DB.epochTxCount = fromIntegral newTxCount
        , DB.epochBlkCount = fromIntegral newBlkCount
        , DB.epochNo = newEpochNo
        , DB.epochStartTime = newStartTime
        , DB.epochEndTime = newEndTime
        }
    else initCalculateNewEpoch currentEpochCache

initCalculateNewEpoch :: EpochBlockDiff -> DB.Epoch
initCalculateNewEpoch currentEpochCache =
  DB.Epoch
    { DB.epochOutSum = ebdOutSum currentEpochCache
    , DB.epochFees = DB.DbLovelace $ ebdFees currentEpochCache
    , DB.epochTxCount = ebdTxCount currentEpochCache
    , DB.epochBlkCount = 1
    , DB.epochNo = ebdEpochNo currentEpochCache
    , -- as this is the first block in epoch the end time and start time are the same
      DB.epochStartTime = ebdTime currentEpochCache
    , DB.epochEndTime = ebdTime currentEpochCache
    }

epochSucessMsg :: Text -> Text -> DB.Epoch -> Text
epochSucessMsg msg cacheOrDB newEpoch =
  mconcat
    [ "\n"
    , msg
    , " epoch "
    , DB.textShow $ DB.epochNo newEpoch
    , " with "
    , cacheOrDB
    , ". \n epoch: "
    , DB.textShow newEpoch
    ]
