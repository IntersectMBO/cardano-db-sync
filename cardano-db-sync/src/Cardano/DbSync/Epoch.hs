{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Epoch (
  epochHandler,
) where

import Cardano.BM.Trace (Trace, logError, logInfo)
import qualified Cardano.Chain.Block as Byron hiding (blockHash)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api (SyncEnv)
import Cardano.DbSync.Cache.Epoch (calculateCurrentEpochNo, readEpochCurrentFromCacheEpoch, readLastMapEpochFromCacheEpoch, writeLatestEpochToCacheEpoch)
import Cardano.DbSync.Cache.Types (Cache (..), EpochCurrent (..))
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

-- import Cardano.Slotting.Slot (unEpochNo)

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
          checkSlotAndEpochNum syncEnv trce cache details
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
      checkSlotAndEpochNum syncEnv trce cache details

-----------------------------------------------------------------------------------------------------
checkSlotAndEpochNum ::
  SyncEnv ->
  Trace IO Text ->
  Cache ->
  SlotDetails ->
  ReaderT SqlBackend (LoggingT IO) (Either SyncNodeError ())
checkSlotAndEpochNum syncEnv trce cache slotDetails = do
  -- read the chached Epoch
  mLastMapEpochFromCache <- liftIO $ readLastMapEpochFromCacheEpoch cache
  mCurrentCacheEpoch <- liftIO $ readEpochCurrentFromCacheEpoch cache
  curEpochNo <- calculateCurrentEpochNo mCurrentCacheEpoch
  let prevEpochNo = maybe 0 epPreviousEpochNo mCurrentCacheEpoch

  if
      -- we've reached the tip and are now going to replace/update the epoch every block
      | getSyncStatus slotDetails == SyncFollowing -> do
        liftIO . logInfo trce $ "\n \n checkSlotAndEpochNum: updateEpochWhenFollowing " <> DB.textShow curEpochNo <> "\n \n"
        updateEpochWhenFollowing trce syncEnv cache mLastMapEpochFromCache mCurrentCacheEpoch curEpochNo

      -- If we're syncing the server turns off and we restart there will be
      -- isNothing mLastMapEpochFromCache -> do

      -- this passes because when server restarts on sync it thinks the prev is one less
      | prevEpochNo /= curEpochNo -> do
        liftIO . logInfo trce $ "\n \n checkSlotAndEpochNum: updateEpochWhenSyncing \n \n" <> DB.textShow curEpochNo <> "\n prev Epoch: " <> DB.textShow prevEpochNo <> "\n cur Epcoh: " <> DB.textShow mCurrentCacheEpoch <> "\n \n"
        updateEpochWhenSyncing syncEnv cache mCurrentCacheEpoch mLastMapEpochFromCache

      -- we're syncing and the epochIds are the same so we just update the cache until we hit a new epoch.
      | otherwise -> do
        liftIO . logInfo trce $ "\n \n checkSlotAndEpochNum: updateEpochCacheWhenSyncing " <> DB.textShow curEpochNo <> "\n" <> DB.textShow mCurrentCacheEpoch <> "\n \n"
        updateEpochCacheWhenSyncing syncEnv cache mLastMapEpochFromCache mCurrentCacheEpoch

-----------------------------------------------------------------------------------------------------
-- when syncing we insert the epoch once every epoch
updateEpochWhenSyncing ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Cache ->
  Maybe EpochCurrent ->
  Maybe DB.Epoch ->
  ReaderT SqlBackend m (Either SyncNodeError ())
updateEpochWhenSyncing syncEnv cache mCurentEpochCache mLastMapEpochFromCache = do
  case mCurentEpochCache of
    -- theres should always be a mCurentEpochCache at this point
    Nothing -> pure $ Left $ NEError "updateEpochWhenSyncing: No mCurentEpochCache"
    Just curentEpochCache ->
      case mLastMapEpochFromCache of
        Nothing -> do
          let newCalculatedEpoch = initCalculateNewEpoch curentEpochCache
          _ <- writeLatestEpochToCacheEpoch syncEnv cache newCalculatedEpoch
          _ <- DB.insertEpoch newCalculatedEpoch
          pure $ Right ()
        Just lastMapEpochFromCache -> do
          let newCalculatedEpoch = calculateNewEpoch lastMapEpochFromCache curentEpochCache
          _ <- writeLatestEpochToCacheEpoch syncEnv cache newCalculatedEpoch
          _ <- DB.insertEpoch lastMapEpochFromCache
          pure $ Right ()

-----------------------------------------------------------------------------------------------------

-- | When replacing an epoch we have the opertunity to try and use the cacheEpoch values
--   to calculate our new epoch all from cache rather than querying the db which is expensive.
updateEpochWhenFollowing ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  SyncEnv ->
  Cache ->
  Maybe DB.Epoch ->
  Maybe EpochCurrent ->
  Word64 ->
  ReaderT SqlBackend m (Either SyncNodeError ())
updateEpochWhenFollowing trce syncEnv cache latestMapEpoch curentEpochCache epochNum = do
  case latestMapEpoch of
    -- There is a latestEpochFromCache so we can use it to work out our new Epoch
    Just lastMapEpCache -> do
      case curentEpochCache of
        Nothing -> pure $ Left $ NEError "replaceEpoch: No curentEpochCache"
        Just currentEpCache -> updateEpochWithCache syncEnv cache lastMapEpCache currentEpCache epochNum

    -- There isn't an epoch in cache so let's see if we can get one from the db,
    -- otherwise we'll calculate the epoch using the expensive db query.
    Nothing -> do
      latestEpochFromDb <- DB.queryLatestEpoch
      case latestEpochFromDb of
        Nothing -> do
          updateEpochWithDBQuery trce syncEnv cache epochNum
        Just latestEpFromDb -> do
          case curentEpochCache of
            -- There should never be no internal cache at this point in the pipeline but just incase!
            Nothing -> pure $ Left $ NEError "replaceEpoch: No curentEpochCache"
            -- Let's use both values aquired to calculate our new epoch.
            Just currentEpCache -> updateEpochWithCache syncEnv cache latestEpFromDb currentEpCache epochNum

-- calculate and replace the epoch
updateEpochWithCache ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Cache ->
  DB.Epoch ->
  EpochCurrent ->
  Word64 ->
  ReaderT SqlBackend m (Either SyncNodeError ())
updateEpochWithCache syncEnv cache lastMapEpCache currentEpCache epochNum = do
  let newCalculatedEpoch = calculateNewEpoch lastMapEpCache currentEpCache
  mEpochID <- DB.queryForEpochId epochNum
  case mEpochID of
    Nothing -> do
      _ <- writeLatestEpochToCacheEpoch syncEnv cache newCalculatedEpoch
      (\_ -> Right ()) <$> DB.insertEpoch newCalculatedEpoch
    Just epochId -> do
      -- replace newly calculated epoch into cache
      void $ writeLatestEpochToCacheEpoch syncEnv cache newCalculatedEpoch
      Right <$> replace epochId newCalculatedEpoch

-- This is an expensive DB query so we minimise it's use to
-- server restarts when syncing or folloing and rollbacks
updateEpochWithDBQuery ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  SyncEnv ->
  Cache ->
  Word64 ->
  ReaderT SqlBackend m (Either SyncNodeError ())
updateEpochWithDBQuery trce syncEnv cache epochNum = do
  newEpoch <- DB.queryCalcEpochEntry epochNum
  mEpochID <- DB.queryForEpochId epochNum
  case mEpochID of
    Nothing -> do
      liftIO . logInfo trce $ "\n \n updateEpochWithDBQuery: No EpochId " <> "\n \n"
      _ <- writeLatestEpochToCacheEpoch syncEnv cache newEpoch
      _ <- DB.insertEpoch newEpoch
      pure $ Right ()
    Just epochId -> do
      liftIO . logInfo trce $ "\n \n updateEpochWithDBQuery: Has EpochId" <> "\n \n"
      -- write the newly calculated epoch to cache.
      _ <- writeLatestEpochToCacheEpoch syncEnv cache newEpoch
      Right <$> replace epochId newEpoch

-----------------------------------------------------------------------------------------------------
updateEpochCacheWhenSyncing ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Cache ->
  Maybe DB.Epoch ->
  Maybe EpochCurrent ->
  ReaderT SqlBackend m (Either SyncNodeError ())
updateEpochCacheWhenSyncing syncEnv cache latestMapEpoch curentEpochCache = do
  case (latestMapEpoch, curentEpochCache) of
    (Just lastMapEpC, Just currentEpC) -> do
      let newCalculatedEpoch = calculateNewEpoch lastMapEpC currentEpC
      writeLatestEpochToCacheEpoch syncEnv cache newCalculatedEpoch
    -- when we don't have a latestMapEpoch the server must have been restarted.
    (Nothing, Just currentEpC) -> do
      newEpoch <- DB.queryCalcEpochEntry $ epCurrentEpochNo currentEpC
      writeLatestEpochToCacheEpoch syncEnv cache newEpoch
    (_, _) -> pure $ Left $ NEError "updateEpochCacheWhenSyncing: No cache available to update"

-----------------------------------------------------------------------------------------------------
calculateNewEpoch ::
  DB.Epoch ->
  EpochCurrent ->
  DB.Epoch
calculateNewEpoch latestEpoch currentCacheEpoch =
  -- if the bellow doesn't equal then it must be a new epoch and we restart our aditions.
  if DB.epochNo latestEpoch == epCurrentEpochNo currentCacheEpoch
    then do
      let newBlkCount = DB.epochBlkCount latestEpoch + 1
          newOutSum = DB.epochOutSum latestEpoch + epCurrentOutSum currentCacheEpoch
          newFees = DB.unDbLovelace (DB.epochFees latestEpoch) + epCurrentFees currentCacheEpoch
          newTxCount = fromIntegral (DB.epochTxCount latestEpoch) + epCurrentTxCount currentCacheEpoch
          newEpochNo = epCurrentEpochNo currentCacheEpoch
          newStartTime = DB.epochStartTime latestEpoch
          newEndTime = epCurrentBlockTime currentCacheEpoch
      DB.Epoch
        { DB.epochOutSum = newOutSum
        , DB.epochFees = DB.DbLovelace newFees
        , DB.epochTxCount = fromIntegral newTxCount
        , DB.epochBlkCount = fromIntegral newBlkCount
        , DB.epochNo = newEpochNo
        , DB.epochStartTime = newStartTime
        , DB.epochEndTime = newEndTime
        }
    else initCalculateNewEpoch currentCacheEpoch

initCalculateNewEpoch :: EpochCurrent -> DB.Epoch
initCalculateNewEpoch currentCacheEpoch =
  DB.Epoch
    { DB.epochOutSum = epCurrentOutSum currentCacheEpoch
    , DB.epochFees = DB.DbLovelace $ epCurrentFees currentCacheEpoch
    , DB.epochTxCount = epCurrentTxCount currentCacheEpoch
    , DB.epochBlkCount = 1
    , DB.epochNo = epCurrentEpochNo currentCacheEpoch
    , -- as this is the first block in epoch the end time and start time are the same
      DB.epochStartTime = epCurrentBlockTime currentCacheEpoch
    , DB.epochEndTime = epCurrentBlockTime currentCacheEpoch
    }
