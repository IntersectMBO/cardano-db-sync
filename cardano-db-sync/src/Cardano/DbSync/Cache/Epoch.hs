{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Cache.Epoch (
  readCacheEpoch,
  readEpochCurrentFromCacheEpoch,
  readEpochFromCacheEpoch,
  readLastMapEpochFromCacheEpoch,
  rollbackMapEpochInCacheEpoch,
  writeCacheEpoch,
  writeEpochCurrentToCache,
  writeLatestEpochToCacheEpoch,
  -- helpers
  isMapEpochCacheNull,
  calculateCurrentEpochNo,
  calculatePreviousEpochNo,
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Api (LedgerEnv (..), SyncEnv (..))
import Cardano.DbSync.Cache.Types (Cache (..), CacheEpoch (..), CacheInternal (..), EpochCurrent (..))
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

readEpochCurrentFromCacheEpoch :: MonadIO m => Cache -> m (Maybe EpochCurrent)
readEpochCurrentFromCacheEpoch cache =
  case cache of
    UninitiatedCache -> pure Nothing
    Cache ci -> do
      cE <- liftIO $ readTVarIO (cEpoch ci)
      case (ceMapEpoch cE, ceEpochCurrent cE) of
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

rollbackMapEpochInCacheEpoch :: MonadIO m => Cache -> DB.BlockId -> m (Either SyncNodeError ())
rollbackMapEpochInCacheEpoch cache blockId = do
  case cache of
    UninitiatedCache -> pure $ Left $ NEError "rollbackMapEpochInCacheEpoch: Cache is UninitiatedCache"
    Cache ci -> do
      cE <- liftIO $ readTVarIO (cEpoch ci)
      -- split the map and delete anything after blockId including it self as new blockId might be
      -- given when inserting the block again when doing rollbacks.
      let (newMapEpoch, _) = split blockId (ceMapEpoch cE)
      writeToCache ci (CacheEpoch newMapEpoch (ceEpochCurrent cE))

writeCacheEpoch :: MonadIO m => Cache -> CacheEpoch -> m ()
writeCacheEpoch cache cacheEpoch =
  case cache of
    UninitiatedCache -> pure ()
    Cache ci -> liftIO $ atomically $ writeTVar (cEpoch ci) cacheEpoch

writeEpochCurrentToCache ::
  MonadIO m =>
  Cache ->
  EpochCurrent ->
  ReaderT SqlBackend m (Either SyncNodeError ())
writeEpochCurrentToCache cache epCurrent =
  case cache of
    UninitiatedCache -> pure $ Left $ NEError "writeEpochCurrentToCache: Cache is UninitiatedCache"
    Cache ci -> do
      cE <- liftIO $ readTVarIO (cEpoch ci)
      case (ceMapEpoch cE, ceEpochCurrent cE) of
        (epochLatest, _) -> writeToCache ci (CacheEpoch epochLatest (Just epCurrent))

-- | put the latest calculated epoch into cache, we use the blockId as it's key
--   which was generated when inserting the block into db and put into EpochCurrent Cache.
writeLatestEpochToCacheEpoch ::
  MonadIO m =>
  SyncEnv ->
  Cache ->
  DB.Epoch ->
  ReaderT SqlBackend m (Either SyncNodeError ())
writeLatestEpochToCacheEpoch syncEnv cache latestEpoch = do
  -- this can also be tought of as max rollback number
  let securityParam =
        case envLedgerEnv syncEnv of
          HasLedger hle -> getSecurityParameter $ leProtocolInfo hle
          NoLedger nle -> getSecurityParameter $ nleProtocolInfo nle
  case cache of
    UninitiatedCache -> pure $ Left $ NEError "writeLatestEpochToCacheEpoch: Cache is UninitiatedCache"
    Cache ci -> do
      -- get EpochCurrent so we can use the BlockId we stored when inserting blocks
      epochInternalCE <- readEpochCurrentFromCacheEpoch cache
      case epochInternalCE of
        Nothing -> pure $ Left $ NEError "writeLatestEpochToCacheEpoch: No epochInternalEpochCache"
        Just ei -> do
          cE <- liftIO $ readTVarIO (cEpoch ci)
          let currentBlockId = epCurrentBlockId ei
              mapEpoch = ceMapEpoch cE
              -- Making sure our mapEpoch doesn't get too large so we use something slightly bigger than K value "securityParam"
              -- and once the map gets above that we delete the last item ready for another to be inserted.
              scaledMapEpoch =
                if size mapEpoch > fromEnum securityParam
                  then deleteMin mapEpoch
                  else mapEpoch

          let updatedMapEpoch = insert currentBlockId latestEpoch scaledMapEpoch
          writeToCache ci (CacheEpoch updatedMapEpoch (ceEpochCurrent cE))

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
  Maybe EpochCurrent ->
  ReaderT SqlBackend m Word64
calculateCurrentEpochNo mEpochPrevious = do
  case mEpochPrevious of
    Nothing -> DB.queryLatestEpochNo
    Just epCurrent -> pure $ epCurrentEpochNo epCurrent

-- Calculate the previous epoch number, handling restarts and rollbacks.
calculatePreviousEpochNo ::
  MonadIO m =>
  Cache ->
  Maybe EpochCurrent ->
  ReaderT SqlBackend m Word64
calculatePreviousEpochNo cache mEpochPrevious =
  case mEpochPrevious of
    Nothing -> do
      latestEpochNo <- DB.queryLatestEpochNo
      mLastMapEpochFromCache <- liftIO $ readLastMapEpochFromCacheEpoch cache
      -- the mapEpoch would be empty when restarting dbsync mid sync
      let lEpoch = maybe latestEpochNo (\_ -> latestEpochNo - 1) mLastMapEpochFromCache
      if latestEpochNo == 0 then pure 0 else pure lEpoch
    Just epochCurrent -> pure $ epCurrentEpochNo epochCurrent
