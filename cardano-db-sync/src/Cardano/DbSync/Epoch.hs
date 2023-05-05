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
import Cardano.DbSync.Cache.Epoch (readEpochInternalFromCacheEpoch, readLastMapEpochFromCacheEpoch, writeLatestEpochToCacheEpoch)
import Cardano.DbSync.Cache.Types (Cache (..), EpochInternal (..))
import Cardano.DbSync.Error
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.Prelude hiding (from, on, replace)
import Cardano.Slotting.Slot (EpochNo (..))
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

-- -------------------------------------------------------------------------------------------------

checkSlotAndEpochNum ::
  SyncEnv ->
  Trace IO Text ->
  Cache ->
  SlotDetails ->
  ReaderT SqlBackend (LoggingT IO) (Either SyncNodeError ())
checkSlotAndEpochNum syncEnv trce cache slotDetails = do
  -- read the chached Epoch
  lastEpochFromCache <- liftIO $ readLastMapEpochFromCacheEpoch cache
  epochInternal <- liftIO $ readEpochInternalFromCacheEpoch cache

  -- if there isn't a cached epoch default it's number to 0 as this is the first block.
  -- let epochNum = maybe 0 (\a -> DB.epochNo a) lastEpochFromCache
  epochNum <- getCurrentEpochNum epochInternal lastEpochFromCache

  let slotEpochNum = unEpochNo $ sdEpochNo slotDetails
  -- liftIO . logInfo trce $ ("\n \n checkSlotAndEpochNum: "
  --                           <> "\n lastEpochFromCache: "
  --                           <> DB.textShow lastEpochFromCache
  --                           <> "\n epochNo from lastEpoch in cache: "
  --                           <> DB.textShow epochNum
  --                           <> "\n slotEpochNum: "
  --                           <> DB.textShow slotEpochNum
  --                           <> "\n isMapEpochCacheNull: "
  --                           <> DB.textShow isMECacheNull
  --                           <> "\n \n")

  -- These cases are listed from the least likey to occur to the most
  -- likley to keep the logic sane.
  if
      | epochNum == 0 -> do
        liftIO . logInfo trce $ "\n \n checkSlotAndEpochNum: epochNum == 0 " <> "\n \n"
        insertOrReplaceEpoch syncEnv cache 0 trce
      | slotEpochNum == epochNum -> do
        liftIO . logInfo trce $ "\n \n checkSlotAndEpochNum: slotEpochNum == epochNum " <> "\n \n"
        insertOrReplaceEpoch syncEnv cache epochNum trce
      | getSyncStatus slotDetails == SyncFollowing -> do
        liftIO . logInfo trce $ "\n \n checkSlotAndEpochNum: getSyncStatus slotDetails == SyncFollowing " <> "\n \n"
        insertOrReplaceEpoch syncEnv cache slotEpochNum trce
      | otherwise -> do
        liftIO . logInfo trce $ "\n \n checkSlotAndEpochNum: otherwise " <> "\n \n"
        pure $ Right ()


getCurrentEpochNum ::
  (MonadBaseControl IO m, MonadIO m) =>
  Maybe EpochInternal ->
  Maybe DB.Epoch ->
  ReaderT SqlBackend m Word64
getCurrentEpochNum mEpochInternal lastEpochFromCache = do
  case mEpochInternal of
    Nothing ->
      case lastEpochFromCache of
        Nothing -> DB.queryLatestEpochNo
        Just lEfromCache -> pure $ DB.epochNo lEfromCache
    Just eInternal -> pure $ epInternalEpochNo eInternal

-- -------------------------------------------------------------------------------------------------

insertOrReplaceEpoch ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Cache ->
  Word64 ->
  Trace IO Text ->
  ReaderT SqlBackend m (Either SyncNodeError ())
insertOrReplaceEpoch syncEnv cache slotEpochNum trce = do
  -- get the epoch id using the epoch number in the slot
  mEpochID <- DB.queryForEpochId slotEpochNum
  -- liftIO . logInfo trce $ "\n \n insertOrReplaceEpoch: \n mEpochId: "
  --                       <> textShow mEpochID
  --                       <> "\n slotEpochNum: "
  --                       <> textShow slotEpochNum
  --                       <> "\n \n"

  -- if the epoch id doesn't exist this means we don't have it yet. Therefore we
  -- calculate and insert a new epoch, else we replace existing epoch.
  maybe
    (insertEpochIntoDB syncEnv trce cache slotEpochNum)
    (replaceEpoch syncEnv trce cache slotEpochNum)
    mEpochID

insertEpochIntoDB ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Trace IO Text ->
  Cache ->
  Word64 ->
  ReaderT SqlBackend m (Either SyncNodeError ())
insertEpochIntoDB syncEnv trce cache slotEpochNum = do
  lastEpochFromCache <- liftIO $ readLastMapEpochFromCacheEpoch cache
  internalEpochCache <- liftIO $ readEpochInternalFromCacheEpoch cache
  -- If we have all the data needed to make a new epoch in cache let's use that instead of
  -- calling the db.
  newCalculatedEpoch <-
    case (lastEpochFromCache, internalEpochCache) of
      (Just lastEpC, Just internalEpC) -> do
        liftIO . logInfo trce $ "\n \n insertEpochIntoDB: using cache"
        pure $ calculateNewEpoch lastEpC internalEpC
      (_, _) -> do
        liftIO . logInfo trce $ "\n \n insertEpochIntoDB: using db query"
        DB.queryCalcEpochEntry slotEpochNum

  -- liftIO . logInfo trce $ "\n newCalculatedEpoch: " <> textShow newCalculatedEpoch <> "\n \n"
  void $ DB.insertEpoch newCalculatedEpoch
  -- put newly inserted epoch into cache
  void $ writeLatestEpochToCacheEpoch syncEnv cache newCalculatedEpoch
  pure $ Right ()

-- | When replacing an epoch we have the opertunity to try and use the cacheEpoch values
--   to calculate our new epoch all from cache rather than querying the db which is expensive.
replaceEpoch ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Trace IO Text ->
  Cache ->
  Word64 ->
  DB.EpochId ->
  ReaderT SqlBackend m (Either SyncNodeError ())
replaceEpoch syncEnv trce cache slotEpochNum epochId = do
  lastEpochFromCache <- liftIO $ readLastMapEpochFromCacheEpoch cache
  internalEpochCache <- liftIO $ readEpochInternalFromCacheEpoch cache
  liftIO . logInfo trce $ "\n \n replaceEpoch:"
  case lastEpochFromCache of
    -- There isn't an epoch in cache so let's see if we can get one from the db,
    -- otherwise we'll calculate the epoch using the expensive db query.
    Nothing -> do
      liftIO . logInfo trce $ "\n no epoch in cache \n \n"
      latestEpochFromDb <- DB.queryLatestEpoch
      case latestEpochFromDb of
        Nothing -> do
          replaceEpochUsingDBQuery syncEnv cache slotEpochNum epochId
        Just latestEpFromDb -> do
          case internalEpochCache of
            -- There should never be no internal cache at this point in the pipeline but just incase!
            Nothing -> pure $ Left $ NEError "replaceEpoch: No internalEpochCache"
            -- Let's use both values aquired to calculate our new epoch.
            Just internalEpCache -> replaceEpochWithValues syncEnv cache epochId latestEpFromDb internalEpCache

    -- There is a latestEpochFromCache so we can use it to work out our new Epoch
    Just lastEpCache -> do
      case internalEpochCache of
        Nothing -> pure $ Left $ NEError "replaceEpoch: No internalEpochCache"
        Just internalEpCache -> replaceEpochWithValues syncEnv cache epochId lastEpCache internalEpCache

-- calculate and replace the epoch
replaceEpochWithValues ::
  MonadIO m =>
  SyncEnv ->
  Cache ->
  DB.EpochId ->
  DB.Epoch ->
  EpochInternal ->
  ReaderT SqlBackend m (Either SyncNodeError ())
replaceEpochWithValues syncEnv cache epochId lastEpCache internalEpCache = do
  let newCalculatedEpoch = calculateNewEpoch lastEpCache internalEpCache
  -- liftIO . logInfo trce $ "\n \n replaceEpochWithValues: \n newCalculatedEpoch: " <> textShow newCalculatedEpoch <> "\n \n"
  -- write newly calculated epoch into cache
  void $ writeLatestEpochToCacheEpoch syncEnv cache newCalculatedEpoch
  Right <$> replace epochId newCalculatedEpoch

-- This is an expensive DB query so we try to minimise it's use.
replaceEpochUsingDBQuery ::
  MonadIO m =>
  SyncEnv ->
  Cache ->
  Word64 ->
  DB.EpochId ->
  ReaderT SqlBackend m (Either SyncNodeError ())
replaceEpochUsingDBQuery syncEnv cache slotEpochNum epochId = do
  newEpoch <- DB.queryCalcEpochEntry slotEpochNum
  -- liftIO . logInfo trce $ "replaceEpochUsingDBQuery: There isn't a latestEpoch in cache or the db, using expensive function replaceEpochUsingDBQuery. "
  -- write the newly calculated epoch to cache.
  void $ writeLatestEpochToCacheEpoch syncEnv cache newEpoch
  Right <$> replace epochId newEpoch

calculateNewEpoch ::
  DB.Epoch ->
  EpochInternal ->
  DB.Epoch
calculateNewEpoch latestEpoch epochInternal =
  -- if the bellow doesn't equal then it must be a new epoch and we restart our aditions.
  if epInternalEpochNo epochInternal == DB.epochNo latestEpoch
    then do
      let newBlkCount = DB.epochBlkCount latestEpoch + 1
          newOutSum = DB.epochOutSum latestEpoch + epInternalOutSum epochInternal
          newFees = DB.unDbLovelace (DB.epochFees latestEpoch) + epInternalFees epochInternal
          newTxCount = fromIntegral (DB.epochTxCount latestEpoch) + epInternalTxCount epochInternal
          newEpochNo = epInternalEpochNo epochInternal
          newStartTime = DB.epochStartTime latestEpoch
          newEndTime = epInternalEndTime epochInternal
      DB.Epoch
        { DB.epochOutSum = newOutSum
        , DB.epochFees = DB.DbLovelace newFees
        , DB.epochTxCount = fromIntegral newTxCount
        , DB.epochBlkCount = fromIntegral newBlkCount
        , DB.epochNo = newEpochNo
        , DB.epochStartTime = newStartTime
        , DB.epochEndTime = newEndTime
        }
    else
      DB.Epoch
        { DB.epochOutSum = epInternalOutSum epochInternal
        , DB.epochFees = DB.DbLovelace $ epInternalFees epochInternal
        , DB.epochTxCount = 0
        , DB.epochBlkCount = 1
        , DB.epochNo = epInternalEpochNo epochInternal
        -- as this is the first block in epoch the end time and start time are the same
        , DB.epochStartTime = epInternalEndTime epochInternal
        , DB.epochEndTime = epInternalEndTime epochInternal
        }
