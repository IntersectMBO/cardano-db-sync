{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Cache.Epoch (
  readCacheEpoch,
  readCurrentEpochFromCacheEpoch,
  readEpochFromCacheEpoch,
  readLastMapEpochFromCacheEpoch,
  rollbackMapEpochInCacheEpoch,
  writeCacheEpoch,
  writeCurrentEpochToCache,
  writeEpochToCacheMapEpoch,
  -- helpers
  isMapEpochCacheNull,
  calculateCurrentEpochNo,
  calculatePreviousEpochNo,
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Api (LedgerEnv (..), SyncEnv (..))
import Cardano.DbSync.Cache.Types (Cache (..), CacheEpoch (..), CacheInternal (..), CurrentEpoch (..))
import Cardano.DbSync.Era.Shelley.Generic.StakeDist (getSecurityParameter)
import Cardano.DbSync.Error (SyncNodeError (..))
import Cardano.DbSync.LedgerState (HasLedgerEnv (..))
import Cardano.DbSync.LocalStateQuery (NoLedgerEnv (..))
import Cardano.Prelude
import Control.Concurrent.Class.MonadSTM.Strict (readTVarIO, writeTVar)
import Data.Map.Strict (deleteMin, insert, lookup, lookupMax, size, split)
import Database.Persist.Postgresql (SqlBackend)

-------------------------------------------------------------------------------------
-- Epoch Cache
-------------------------------------------------------------------------------------
readCacheEpoch :: MonadIO m => Cache -> m (Maybe CacheEpoch)
readCacheEpoch cache =
  case cache of
    UninitiatedCache -> pure Nothing
    Cache ci -> do
      cacheEpoch <- liftIO $ readTVarIO (cEpoch ci)
      pure $ Just cacheEpoch

readCurrentEpochFromCacheEpoch :: MonadIO m => Cache -> m (Maybe CurrentEpoch)
readCurrentEpochFromCacheEpoch cache =
  case cache of
    UninitiatedCache -> pure Nothing
    Cache ci -> do
      cE <- liftIO $ readTVarIO (cEpoch ci)
      case (ceMapEpoch cE, ceCurrentEpoch cE) of
        (_, epochInternal) -> pure epochInternal

readLastMapEpochFromCacheEpoch :: Cache -> IO (Maybe DB.Epoch)
readLastMapEpochFromCacheEpoch cache =
  case cache of
    UninitiatedCache -> pure Nothing
    Cache ci -> do
      cE <- readTVarIO (cEpoch ci)
      let mapEpoch = ceMapEpoch cE
      case lookupMax mapEpoch of
        Nothing -> pure Nothing
        Just (_, ep) -> pure $ Just ep

readEpochFromCacheEpoch :: MonadIO m => Cache -> DB.BlockId -> m (Maybe DB.Epoch)
readEpochFromCacheEpoch cache blockNo =
  case cache of
    UninitiatedCache -> pure Nothing
    Cache ci -> do
      cE <- liftIO $ readTVarIO (cEpoch ci)
      pure $ lookup blockNo (ceMapEpoch cE)

rollbackMapEpochInCacheEpoch :: MonadIO m => CacheInternal -> DB.BlockId -> m (Either SyncNodeError ())
rollbackMapEpochInCacheEpoch cache blockId = do
      cE <- liftIO $ readTVarIO (cEpoch cache)
      -- split the map and delete anything after blockId including it self as new blockId might be
      -- given when inserting the block again when doing rollbacks.
      let (newMapEpoch, _) = split blockId (ceMapEpoch cE)
      writeToCache cache (CacheEpoch newMapEpoch (ceCurrentEpoch cE))

writeCacheEpoch :: MonadIO m => Cache -> CacheEpoch -> m ()
writeCacheEpoch cache cacheEpoch =
  case cache of
    UninitiatedCache -> pure ()
    Cache ci -> liftIO $ atomically $ writeTVar (cEpoch ci) cacheEpoch

writeCurrentEpochToCache ::
  MonadIO m =>
  Cache ->
  CurrentEpoch ->
  ReaderT SqlBackend m (Either SyncNodeError ())
writeCurrentEpochToCache cache epCurrent =
  case cache of
    UninitiatedCache -> pure $ Left $ NEError "writeCurrentEpochToCache: Cache is UninitiatedCache"
    Cache ci -> do
      cE <- liftIO $ readTVarIO (cEpoch ci)
      case (ceMapEpoch cE, ceCurrentEpoch cE) of
        (epochLatest, _) -> writeToCache ci (CacheEpoch epochLatest (Just epCurrent))

-- | Insert an epoch into Map Epoch cache. A blockId is used as the key which was generated when inserting the block
-- | into the db. This is so we have a historic representation of an epoch after every block is inserted.
-- | This becomes usefull when syncing and doing rollbacks and saves on expensive db queries to calculte an epoch.
writeEpochToCacheMapEpoch ::
  MonadIO m =>
  SyncEnv ->
  Cache ->
  DB.Epoch ->
  ReaderT SqlBackend m (Either SyncNodeError ())
writeEpochToCacheMapEpoch syncEnv cache latestEpoch = do
  -- this can also be tought of as max rollback number
  let securityParam =
        case envLedgerEnv syncEnv of
          HasLedger hle -> getSecurityParameter $ leProtocolInfo hle
          NoLedger nle -> getSecurityParameter $ nleProtocolInfo nle
  case cache of
    UninitiatedCache -> pure $ Left $ NEError "writeEpochToCacheMapEpoch: Cache is UninitiatedCache"
    Cache ci -> do
      -- get CurrentEpoch so we can use the BlockId we stored when inserting blocks
      epochInternalCE <- readCurrentEpochFromCacheEpoch cache
      case epochInternalCE of
        Nothing -> pure $ Left $ NEError "writeEpochToCacheMapEpoch: No epochInternalEpochCache"
        Just ei -> do
          cE <- liftIO $ readTVarIO (cEpoch ci)
          let currentBlockId = epCurrentBlockId ei
              mapEpoch = ceMapEpoch cE
              -- To make sure our Map Epoch doesn't get too large so we use something slightly bigger than K value "securityParam"
              -- and once the map gets larger than that number we delete the first inserted item making room for another Epoch.
              scaledMapEpoch =
                if size mapEpoch > fromEnum securityParam
                  then deleteMin mapEpoch
                  else mapEpoch

          let updatedMapEpoch = insert currentBlockId latestEpoch scaledMapEpoch
          writeToCache ci (CacheEpoch updatedMapEpoch (ceCurrentEpoch cE))

------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------

isMapEpochCacheNull :: Cache -> IO Bool
isMapEpochCacheNull cache =
  case cache of
    UninitiatedCache -> pure False
    Cache ci -> do
      cE <- readTVarIO (cEpoch ci)
      pure $ null (ceMapEpoch cE)

writeToCache :: MonadIO m => CacheInternal -> CacheEpoch -> m (Either SyncNodeError ())
writeToCache ci newCacheEpoch = do
  void $ liftIO $ atomically $ writeTVar (cEpoch ci) newCacheEpoch
  pure $ Right ()

-- calculate the current epoch number using db query as backup
-- the query returns 0 if we have not epochs on the db.
calculateCurrentEpochNo ::
  MonadIO m =>
  Maybe CurrentEpoch ->
  ReaderT SqlBackend m Word64
calculateCurrentEpochNo mCurrentEpoch =
  case mCurrentEpoch of
    Nothing -> DB.queryLatestEpochNo
    Just epCurrent -> pure $ epCurrentEpochNo epCurrent

-- Calculate the previous epoch number, using  handling restarts and rollbacks.
calculatePreviousEpochNo ::
  MonadIO m =>
  Cache ->
  ReaderT SqlBackend m Word64
calculatePreviousEpochNo cache = do
  -- we get our CurrentEpoch before updating it as source use of this function
  mCurrentEpoch <- readCurrentEpochFromCacheEpoch cache
  case mCurrentEpoch of
    Just epochCurrent -> pure $ epCurrentEpochNo epochCurrent
    Nothing -> do
      epochNos <- DB.queryEpochNoForLastTwoBlocks
      maybe (pure 0) pure (minimumMay epochNos)
