{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Cache.Epoch (
  readCacheEpoch,
  readEpochInternalFromCacheEpoch,
  readEpochFromCacheEpoch,
  readLastMapEpochFromCacheEpoch,
  rollbackMapEpochInCacheEpoch,
  writeCacheEpoch,
  writeEpochInternalToCache,
  writeLatestEpochToCacheEpoch,
  -- helpers
  getHasMapEpochCache,
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Cache.Types (Cache (..), CacheEpoch (..), CacheInternal (..), EpochInternal (..))
import Cardano.Prelude
import Control.Concurrent.Class.MonadSTM.Strict (readTVarIO, writeTVar)
import Data.Map.Strict (insert, lookup, split, lookupMax, size, deleteMin)
import Database.Persist.Postgresql (SqlBackend)
import Cardano.DbSync.Error (SyncNodeError(..))

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

readEpochInternalFromCacheEpoch :: MonadIO m => Cache -> m (Maybe EpochInternal)
readEpochInternalFromCacheEpoch cache =
  case cache of
    UninitiatedCache -> pure Nothing
    Cache ci -> do
      cE <- liftIO $ readTVarIO (cEpoch ci)
      case (ceMapEpoch cE, ceEpochInternal cE) of
        (_, epochInternal) -> pure epochInternal

readLastMapEpochFromCacheEpoch :: Cache -> IO (Maybe DB.Epoch)
readLastMapEpochFromCacheEpoch cache =
  case cache of
    UninitiatedCache -> pure Nothing
    Cache ci -> do
      cE <- readTVarIO (cEpoch ci)
      let mapEpoch = ceMapEpoch cE
      if null mapEpoch
        then pure Nothing
        else do
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
      writeToCache ci (CacheEpoch newMapEpoch (ceEpochInternal cE))

writeCacheEpoch :: MonadIO m => Cache -> CacheEpoch -> m ()
writeCacheEpoch cache cacheEpoch =
  case cache of
    UninitiatedCache -> pure ()
    Cache ci -> liftIO $ atomically $ writeTVar (cEpoch ci) cacheEpoch

writeEpochInternalToCache ::
  MonadIO m =>
  Cache ->
  EpochInternal ->
  ReaderT SqlBackend m (Either SyncNodeError ())
writeEpochInternalToCache cache epInternal =
  case cache of
    UninitiatedCache -> pure $ Left $ NEError "writeEpochInternalToCache: Cache is UninitiatedCache"
    Cache ci -> do
      cE <- liftIO $ readTVarIO (cEpoch ci)
      case (ceMapEpoch cE, ceEpochInternal cE) of
        (epochLatest, _) -> writeToCache ci (CacheEpoch epochLatest (Just epInternal))

-- | put the latest calculated epoch into cache, we use the blockId as it's key
--   which was generated when inserting the block into db and put into EpochInternal Cache.
writeLatestEpochToCacheEpoch ::
  MonadIO m =>
  Cache ->
  DB.Epoch ->
  ReaderT SqlBackend m (Either SyncNodeError ())
writeLatestEpochToCacheEpoch cache latestEpoch =
  case cache of
    UninitiatedCache -> pure $ Left $ NEError "writeLatestEpochToCacheEpoch: Cache is UninitiatedCache"
    Cache ci -> do
      -- get EpochInternal so we can use the BlockId we stored when inserting blocks
      epochInternalCE <- readEpochInternalFromCacheEpoch cache
      case epochInternalCE of
        Nothing -> pure $ Left $ NEError "writeLatestEpochToCacheEpoch: No epochInternalEpochCache"
        Just ei -> do
          cE <- liftIO $ readTVarIO (cEpoch ci)
          let blockId = epoInternalCurrentBlockId ei
              mapEpoch = ceMapEpoch cE
              -- Making sure our mapEpoch doesn't get too large so we use something slightly bigger than K value "securityParam"
              -- and once the map gets above that we delete the last item ready for another to be inserted.
              scaledMapEpoch =
                if size mapEpoch > 2500
                  then deleteMin mapEpoch
                  else mapEpoch
          let insertedMapEpoch = insert blockId latestEpoch scaledMapEpoch
          writeToCache ci (CacheEpoch insertedMapEpoch (ceEpochInternal cE))

-- Helper --

getHasMapEpochCache :: Cache -> IO Bool
getHasMapEpochCache cache =
  case cache of
    UninitiatedCache -> pure False
    Cache ci -> do
      cE <- readTVarIO (cEpoch ci)
      pure $ null (ceMapEpoch cE)

writeToCache :: MonadIO m => CacheInternal -> CacheEpoch -> m (Either SyncNodeError ())
writeToCache ci val = do
  void $ liftIO $ atomically $ writeTVar (cEpoch ci) val
  pure $ Right ()
