{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Epoch (
  epochHandler,
) where

-- import Cardano.BM.Trace (Trace, logError, logInfo)
import Cardano.BM.Trace (Trace, logError)
import qualified Cardano.Chain.Block as Byron hiding (blockHash)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api (SyncEnv)
import Cardano.DbSync.Cache.Epoch (calculateCurrentEpochNo, readCurrentEpochFromCacheEpoch, readLastMapEpochFromCacheEpoch, writeEpochToCacheMapEpoch)
import Cardano.DbSync.Cache.Types (Cache (..), CurrentEpoch (..))
import Cardano.DbSync.Error
import Cardano.DbSync.Types (
  BlockDetails (BlockDetails),
  SlotDetails (..),
  SyncState (SyncFollowing),
 )
import Cardano.DbSync.Util
import Cardano.Prelude hiding (from, on, replace)
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
  BlockDetails ->
  ReaderT SqlBackend (LoggingT IO) (Either SyncNodeError ())
epochHandler syncEnv trce cache (BlockDetails cblk details) =
  case cblk of
    BlockByron bblk ->
      case byronBlockRaw bblk of
        Byron.ABOBBoundary {} ->
          -- For the OBFT era there are no boundary blocks so we ignore them even in
          -- the Ouroboros Classic era.
          pure $ Right ()
        Byron.ABOBBlock _blk ->
          updateEpochStart syncEnv cache details
    BlockShelley {} -> epochSlotTimecheck
    BlockAllegra {} -> epochSlotTimecheck
    BlockMary {} -> epochSlotTimecheck
    BlockAlonzo {} -> epochSlotTimecheck
    BlockBabbage {} -> epochSlotTimecheck
  where
    -- What we do here is completely independent of Shelley/Allegra/Mary eras.
    epochSlotTimecheck :: ReaderT SqlBackend (LoggingT IO) (Either SyncNodeError ())
    epochSlotTimecheck = do
      when (sdSlotTime details > sdCurrentTime details) $
        liftIO . logError trce $
          mconcat
            ["Slot time '", textShow (sdSlotTime details), "' is in the future"]
      updateEpochStart syncEnv cache details

-----------------------------------------------------------------------------------------------------
updateEpochStart ::
  SyncEnv ->
  Cache ->
  SlotDetails ->
  ReaderT SqlBackend (LoggingT IO) (Either SyncNodeError ())
updateEpochStart syncEnv cache slotDetails = do
  mLastMapEpochFromCache <- liftIO $ readLastMapEpochFromCacheEpoch cache
  mCurrentEpochCache <- liftIO $ readCurrentEpochFromCacheEpoch cache
  curEpochNo <- calculateCurrentEpochNo mCurrentEpochCache
  let prevEpochNo = maybe 0 epPreviousEpochNo mCurrentEpochCache

  if
      -- The tip has been reached and are now going to replace/update the epoch every block
      | getSyncStatus slotDetails == SyncFollowing ->
        updateEpochWhenFollowing syncEnv cache mLastMapEpochFromCache mCurrentEpochCache curEpochNo
      -- When syncing we check if equality we store when inserting the block as this means we hit the first new block in an epoch.
      -- This is when we store the epoch onto the db.
      | prevEpochNo /= curEpochNo ->
        updateEpochWhenSyncing syncEnv cache mCurrentEpochCache mLastMapEpochFromCache
      -- we're syncing and the epochNo are the same so we just update the cache until above check passes.
      | otherwise ->
        updateEpochCacheWhenSyncing syncEnv cache mLastMapEpochFromCache mCurrentEpochCache

-----------------------------------------------------------------------------------------------------
-- This function is called once every epoch on the very first block of the new epoch.
-- At that point we can get the previously accumilated data from previous epoch and insert/update it into the db.
updateEpochWhenSyncing ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Cache ->
  Maybe CurrentEpoch ->
  Maybe DB.Epoch ->
  ReaderT SqlBackend m (Either SyncNodeError ())
updateEpochWhenSyncing syncEnv cache mCurrentEpochCache mLastMapEpochFromCache = do
  case mCurrentEpochCache of
    -- theres should always be a mCurrentEpochCache at this point
    Nothing -> pure $ Left $ NEError "updateEpochWhenSyncing: No mCurrentEpochCache"
    Just currentEpochCache ->
      case mLastMapEpochFromCache of
        -- if there is no Map Epoch in cache at this point, then the server must have restarted and failed on the last
        -- block of the previous epoch and the current block is the first in new Epoch.
        -- We must now use a db query to calculate and insert previous epoch.
        Nothing -> do
          let calculatedEpoch = initCalculateNewEpoch currentEpochCache
          _ <- writeEpochToCacheMapEpoch syncEnv cache calculatedEpoch
          _ <- updateEpochWithDBQuery syncEnv cache $ epPreviousEpochNo currentEpochCache
          pure $ Right ()
        -- just use the cache to
        Just lastMapEpochFromCache -> do
          let calculatedEpoch = initCalculateNewEpoch currentEpochCache
          _ <- writeEpochToCacheMapEpoch syncEnv cache calculatedEpoch
          _ <- DB.insertEpoch lastMapEpochFromCache
          pure $ Right ()

-----------------------------------------------------------------------------------------------------

-- When updating an epoch we have the opertunity to try and use the cacheEpoch values
-- to calculate our epoch rather than querying the db which is expensive.
updateEpochWhenFollowing ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Cache ->
  Maybe DB.Epoch ->
  Maybe CurrentEpoch ->
  Word64 ->
  ReaderT SqlBackend m (Either SyncNodeError ())
updateEpochWhenFollowing syncEnv cache newestEpochFromMap currentEpochCache epochNo = do
  case newestEpochFromMap of
    Just lastMapEpCache -> do
      case currentEpochCache of
        Nothing -> pure $ Left $ NEError "replaceEpoch: No currentEpochCache"
        Just currentEpCache -> updateEpochWithCacheWhenFollowing syncEnv cache lastMapEpCache currentEpCache epochNo

    -- If there isn't an epoch in cache, let's see if we can get one from the db. Otherwise
    -- calculate the epoch using the expensive db query.
    Nothing -> do
      mNewestEpochFromDb <- DB.queryLatestEpoch
      case mNewestEpochFromDb of
        -- no latest epoch in db (very unlikely) so lets use expensive db query
        Nothing -> do
          updateEpochWithDBQuery syncEnv cache epochNo
        Just newestEpochFromDb -> do
          case currentEpochCache of
            -- There should never be no CurrentEpoch in cache at this point in the pipeline but just incase!
            Nothing -> pure $ Left $ NEError "replaceEpoch: No currentEpochCache"
            -- Let's use both values aquired to calculate our new epoch.
            Just currentEpCache -> updateEpochWithCacheWhenFollowing syncEnv cache newestEpochFromDb currentEpCache epochNo

-- Update the epoch in cache and db, which could be either an update or insert
-- dependent on if epoch already exists.
updateEpochWithCacheWhenFollowing ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Cache ->
  DB.Epoch ->
  CurrentEpoch ->
  Word64 ->
  ReaderT SqlBackend m (Either SyncNodeError ())
updateEpochWithCacheWhenFollowing syncEnv cache lastMapEpCache currentEpCache epochNo = do
  let calculatedEpoch = calculateNewEpoch lastMapEpCache currentEpCache
  -- if the epoch already exists then we update it otherwise create new entry.
  mEpochID <- DB.queryForEpochId epochNo
  case mEpochID of
    Nothing -> do
      _ <- writeEpochToCacheMapEpoch syncEnv cache calculatedEpoch
      (\_ -> Right ()) <$> DB.insertEpoch calculatedEpoch
    Just epochId -> do
      _ <- writeEpochToCacheMapEpoch syncEnv cache calculatedEpoch
      Right <$> replace epochId calculatedEpoch

-- This is an expensive DB query so we minimise it's use to
-- server restarts when syncing or folloing and rollbacks
updateEpochWithDBQuery ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Cache ->
  Word64 ->
  ReaderT SqlBackend m (Either SyncNodeError ())
updateEpochWithDBQuery syncEnv cache epochNo = do
  newEpoch <- DB.queryCalcEpochEntry epochNo
  mEpochID <- DB.queryForEpochId epochNo
  case mEpochID of
    Nothing -> do
      _ <- writeEpochToCacheMapEpoch syncEnv cache newEpoch
      _ <- DB.insertEpoch newEpoch
      pure $ Right ()
    Just epochId -> do
      -- write the newly calculated epoch to cache.
      _ <- writeEpochToCacheMapEpoch syncEnv cache newEpoch
      Right <$> replace epochId newEpoch

-----------------------------------------------------------------------------------------------------

-- When syncing, on every block we update the Map epoch in cache. Making sure to handle restarts
updateEpochCacheWhenSyncing ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Cache ->
  Maybe DB.Epoch ->
  Maybe CurrentEpoch ->
  ReaderT SqlBackend m (Either SyncNodeError ())
updateEpochCacheWhenSyncing syncEnv cache newestEpochFromMap currentEpochCache = do
  case (newestEpochFromMap, currentEpochCache) of
    (Just lastMapEpC, Just currentEpC) -> do
      let calculatedEpoch = calculateNewEpoch lastMapEpC currentEpC
      writeEpochToCacheMapEpoch syncEnv cache calculatedEpoch
    -- when we don't have a newestEpochFromMap the server must have been restarted.
    -- so we need to replenish the cache using expensive db query.
    (Nothing, Just currentEpC) -> do
      newEpoch <- DB.queryCalcEpochEntry $ epCurrentEpochNo currentEpC
      writeEpochToCacheMapEpoch syncEnv cache newEpoch
    -- There will always be a CurrentEpoch at this point in time
    (_, _) -> pure $ Left $ NEError "updateEpochCacheWhenSyncing: No caches available to update cache"

-----------------------------------------------------------------------------------------------------

-- Because we store a Map of epochs, at every iteration we take the newest epoch and it's values
-- We then add to it, the data we kept inside of CurrentEpoch when inserting the current block.
calculateNewEpoch ::
  DB.Epoch ->
  CurrentEpoch ->
  DB.Epoch
calculateNewEpoch newestEpochMapCache currentEpochCache =
  -- if the bellow doesn't equal, then it must be a new epoch as the current block is not in the same epoch.
  if DB.epochNo newestEpochMapCache == epCurrentEpochNo currentEpochCache
    then do
      let newBlkCount = DB.epochBlkCount newestEpochMapCache + 1
          newOutSum = DB.epochOutSum newestEpochMapCache + epCurrentOutSum currentEpochCache
          newFees = DB.unDbLovelace (DB.epochFees newestEpochMapCache) + epCurrentFees currentEpochCache
          newTxCount = fromIntegral (DB.epochTxCount newestEpochMapCache) + epCurrentTxCount currentEpochCache
          newEpochNo = epCurrentEpochNo currentEpochCache
          newStartTime = DB.epochStartTime newestEpochMapCache
          newEndTime = epCurrentBlockTime currentEpochCache
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

initCalculateNewEpoch :: CurrentEpoch -> DB.Epoch
initCalculateNewEpoch currentEpochCache =
  DB.Epoch
    { DB.epochOutSum = epCurrentOutSum currentEpochCache
    , DB.epochFees = DB.DbLovelace $ epCurrentFees currentEpochCache
    , DB.epochTxCount = epCurrentTxCount currentEpochCache
    , DB.epochBlkCount = 1
    , DB.epochNo = epCurrentEpochNo currentEpochCache
    , -- as this is the first block in epoch the end time and start time are the same
      DB.epochStartTime = epCurrentBlockTime currentEpochCache
    , DB.epochEndTime = epCurrentBlockTime currentEpochCache
    }
