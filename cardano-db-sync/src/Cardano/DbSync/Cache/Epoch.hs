module Cardano.DbSync.Cache.Epoch (
  readCacheEpoch,
  readEpochInternalFromCacheEpoch,
  readLatestEpochFromCacheEpoch,
  writeCacheEpoch,
  writeEpochInternalToCache,
  writeLatestEpochToCacheEpoch,
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Cache.Types (Cache (..), CacheEpoch (..), CacheInternal (..), EpochInternal (..))
import Cardano.Prelude
import Control.Concurrent.Class.MonadSTM.Strict (readTVarIO, writeTVar)
import Database.Persist.Postgresql (SqlBackend)

-------------------------------------------------------------------------------------
-- Epoch Cache
-------------------------------------------------------------------------------------
readCacheEpoch :: Cache -> IO (Maybe CacheEpoch)
readCacheEpoch cache =
  case cache of
    UninitiatedCache -> pure Nothing
    Cache ci -> do
      cacheEpoch <- readTVarIO (cEpoch ci)
      pure $ Just cacheEpoch

readEpochInternalFromCacheEpoch :: Cache -> IO (Maybe EpochInternal)
readEpochInternalFromCacheEpoch cache =
  case cache of
    UninitiatedCache -> pure Nothing
    Cache ci -> do
      cE <- readTVarIO (cEpoch ci)
      case (ceLatestEpoch cE, ceEpochInternal cE) of
        (_, epochInternal) -> pure epochInternal

readLatestEpochFromCacheEpoch :: Cache -> IO (Maybe DB.Epoch)
readLatestEpochFromCacheEpoch cache =
  case cache of
    UninitiatedCache -> pure Nothing
    Cache ci -> do
      cE <- readTVarIO (cEpoch ci)
      case (ceLatestEpoch cE, ceEpochInternal cE) of
        (epochLatest, _) -> pure epochLatest

writeCacheEpoch ::
  MonadIO m =>
  Cache ->
  CacheEpoch ->
  ReaderT SqlBackend m ()
writeCacheEpoch cache cacheEpoch =
  case cache of
    UninitiatedCache -> pure ()
    Cache ci -> liftIO $ atomically $ writeTVar (cEpoch ci) cacheEpoch

writeEpochInternalToCache ::
  MonadIO m =>
  Cache ->
  EpochInternal ->
  ReaderT SqlBackend m ()
writeEpochInternalToCache cache epInternal =
  case cache of
    UninitiatedCache -> pure ()
    Cache ci -> do
      cE <- liftIO $ readTVarIO (cEpoch ci)
      case (ceLatestEpoch cE, ceEpochInternal cE) of
        (epochLatest, _) -> writeToCache ci (CacheEpoch epochLatest (Just epInternal))

writeLatestEpochToCacheEpoch ::
  MonadIO m =>
  Cache ->
  DB.Epoch ->
  ReaderT SqlBackend m ()
writeLatestEpochToCacheEpoch cache latestEpoch =
  case cache of
    UninitiatedCache -> pure ()
    Cache ci -> do
      cE <- liftIO $ readTVarIO (cEpoch ci)
      case (ceLatestEpoch cE, ceEpochInternal cE) of
        (_, epochInternal) -> writeToCache ci (CacheEpoch (Just latestEpoch) epochInternal)

-- Helper --

-- hasLatestEpoch :: Cache -> IO Bool
-- hasLatestEpoch cache =
--   case cache of
--     UninitiatedCache -> pure False
--     Cache ci -> do
--       cE <- readTVarIO (cEpoch ci)
--       pure $ isJust (ceLatestEpoch cE)

-- hasInternalEpoch :: Cache -> IO Bool
-- hasInternalEpoch cache =
--   case cache of
--     UninitiatedCache -> pure False
--     Cache ci -> do
--       cE <- readTVarIO (cEpoch ci)
--       pure $ isJust (ceLatestEpoch cE)

writeToCache :: MonadIO m => CacheInternal -> CacheEpoch -> m ()
writeToCache ci val = liftIO $ atomically $ writeTVar (cEpoch ci) val
