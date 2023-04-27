{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Cardano.DbSync.Epoch (
  epochHandler,
) where

import Cardano.BM.Trace (Trace, logError, logInfo)
import qualified Cardano.Chain.Block as Byron hiding (blockHash)
import qualified Cardano.Db as DB
import Cardano.DbSync.Cache.Epoch (readEpochInternalFromCacheEpoch, writeLatestEpochToCacheEpoch, getHasMapEpochCache, readLastMapEpochFromCacheEpoch)
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
  Trace IO Text ->
  Cache ->
  BlockDetails ->
  ReaderT SqlBackend (LoggingT IO) (Either SyncNodeError ())
epochHandler trce cache (BlockDetails cblk details) =
  case cblk of
    BlockByron bblk ->
      case byronBlockRaw bblk of
        Byron.ABOBBoundary {} ->
          -- For the OBFT era there are no boundary blocks so we ignore them even in
          -- the Ouroboros Classic era.
          pure $ Right ()
        Byron.ABOBBlock _blk ->
          checkSlotAndEpochNum trce cache details
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
      checkSlotAndEpochNum trce cache details

-- -------------------------------------------------------------------------------------------------

checkSlotAndEpochNum ::
  Trace IO Text ->
  Cache ->
  SlotDetails ->
  ReaderT SqlBackend (LoggingT IO) (Either SyncNodeError ())
checkSlotAndEpochNum trce cache slotDetails = do
  -- read the chached Epoch
  hasMapEpochCache <- liftIO $ getHasMapEpochCache cache
  latestEpochCache <- liftIO $ readLastMapEpochFromCacheEpoch cache

  -- if there isn't a cached epoch default it's number to 0 as this is the first block.
  let epochNum = maybe 0 DB.epochNo latestEpochCache
      slotEpochNum = unEpochNo $ sdEpochNo slotDetails

  -- These cases are listed from the least likey to occur to the most
  -- likley to keep the logic sane.
  if
      | slotEpochNum > 0 && hasMapEpochCache ->
        insertOrReplaceEpoch cache 0 trce
      | slotEpochNum >= epochNum + 2 ->
        insertOrReplaceEpoch cache (epochNum + 1) trce
      | getSyncStatus slotDetails == SyncFollowing ->
        insertOrReplaceEpoch cache slotEpochNum trce
      | otherwise -> pure $ Right ()

-- -------------------------------------------------------------------------------------------------

insertOrReplaceEpoch ::
  (MonadBaseControl IO m, MonadIO m) =>
  Cache ->
  Word64 ->
  Trace IO Text ->
  ReaderT SqlBackend m (Either SyncNodeError ())
insertOrReplaceEpoch cache slotEpochNum trce = do
  -- get the epoch id using a slot number
  mEpochID <- DB.queryForEpochId slotEpochNum
  -- if the epoch id doesn't exist this means we don't have it yet so we
  -- calculate and insert a new epoch otherwise we replace existing epoch.
  maybe
    (insertEpochIntoDB trce cache slotEpochNum)
    (replaceEpoch trce cache slotEpochNum)
    mEpochID

insertEpochIntoDB ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  Cache ->
  Word64 ->
  ReaderT SqlBackend m (Either SyncNodeError ())
insertEpochIntoDB trce cache slotEpochNum = do
  latestEpochCache <- liftIO $ readLastMapEpochFromCacheEpoch cache
  internalEpochCache <- liftIO $ readEpochInternalFromCacheEpoch cache
  -- If we have all the data needed to make a new epoch in cache let's use that instead of
  -- calling the db.
  newCalculatedEpoch <-
    case (latestEpochCache, internalEpochCache) of
      (Just latestEpC, Just internalEpC) -> pure $ calculateNewEpoch latestEpC internalEpC
      (_, _) -> DB.queryCalcEpochEntry slotEpochNum

  liftIO . logInfo trce $ "insertEpochIntoDB: epoch " <> textShow slotEpochNum
  void $ DB.insertEpoch newCalculatedEpoch
  -- put newly inserted epoch into cache
  void $ writeLatestEpochToCacheEpoch cache newCalculatedEpoch
  pure $ Right ()

-- | When replacing an epoch we have the opertunity to try and use the cacheEpoch values
--   to calculate our new epoch all from cache rather than querying the db which is expensive.
replaceEpoch ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  Cache ->
  Word64 ->
  DB.EpochId ->
  ReaderT SqlBackend m (Either SyncNodeError ())
replaceEpoch trce cache slotEpochNum epochId = do
  latestEpochCache <- liftIO $ readLastMapEpochFromCacheEpoch cache
  internalEpochCache <- liftIO $ readEpochInternalFromCacheEpoch cache

  case latestEpochCache of
    -- There is no latest epoch in cache so let's see if we can get it from the db,
    -- otherwise we'll calculate the epoch from an expensive db query.
    Nothing -> do
      latestEpochFromDb <- DB.queryLatestEpoch
      case latestEpochFromDb of
        Nothing -> do
          replaceEpochUsingDBQuery trce cache slotEpochNum epochId
        -- TODO: Vince would a rollback cause us to get the wrong latestEpFromDb?
        Just latestEpFromDb -> do
          case internalEpochCache of
            -- There should never be no internal cache at this point in the process but just incase!
            Nothing -> pure $ Left $ NEError "replaceEpoch: No internalEpochCache"
            Just internalEpCache -> replaceEpochWithValues trce cache epochId latestEpFromDb internalEpCache

    -- There is a latestEpochCache so we can use it to work out our new Epoch
    Just latestEpCache -> do
      case internalEpochCache of
        Nothing -> pure $ Left $ NEError "replaceEpoch: No internalEpochCache"
        Just internalEpCache -> replaceEpochWithValues trce cache epochId latestEpCache internalEpCache

-- calculate and replace the epoch
replaceEpochWithValues ::
  MonadIO m =>
  Trace IO Text ->
  Cache ->
  DB.EpochId ->
  DB.Epoch ->
  EpochInternal ->
  ReaderT SqlBackend m (Either SyncNodeError ())
replaceEpochWithValues trce cache epochId latestEpCache internalEpCache = do
  let newCalculatedEpoch = calculateNewEpoch latestEpCache internalEpCache
  liftIO . logInfo trce $ "replaceEpochWithValues: Calculated epoch using the cache: " <> textShow newCalculatedEpoch
  -- write newly calculated epoch into cache
  void $ writeLatestEpochToCacheEpoch cache newCalculatedEpoch
  Right <$> replace epochId newCalculatedEpoch

-- This is an expensive query so we try to minimise it's use.
replaceEpochUsingDBQuery ::
  MonadIO m =>
  Trace IO Text ->
  Cache ->
  Word64 ->
  DB.EpochId ->
  ReaderT SqlBackend m (Either SyncNodeError ())
replaceEpochUsingDBQuery trce cache slotEpochNum epochId = do
  -- this query is an expensive lookup so should be rarely used
  newEpoch <- DB.queryCalcEpochEntry slotEpochNum
  liftIO . logInfo trce $ "replaceEpochUsingDBQuery: There isn't a latestEpoch in cache or the db, using expensive function replaceEpochUsingDBQuery. "
  -- write the newly calculated epoch to cache.
  void $ writeLatestEpochToCacheEpoch cache newEpoch
  Right <$> replace epochId newEpoch

calculateNewEpoch ::
  DB.Epoch ->
  EpochInternal ->
  DB.Epoch
calculateNewEpoch latestEpoch epochInternal = do
  let newBlkCount = fromIntegral $ DB.epochBlkCount latestEpoch + 1
      newOutSum = fromIntegral (DB.epochOutSum latestEpoch) + epInternalOutSum epochInternal
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
