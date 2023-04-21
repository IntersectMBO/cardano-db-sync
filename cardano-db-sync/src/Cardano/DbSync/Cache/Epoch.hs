module Cardano.DbSync.Cache.Epoch (
  readCacheEpoch,
  readEpochFromCacheEpoch,
  writeCacheEpoch,
  writeBlockAndFeeToCacheEpoch,
  writeEpochToCacheEpoch,
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Cache.Types (Cache (..), CacheEpoch (..), CacheInternal (..))
import Cardano.DbSync.Types (CardanoBlock)
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
    Cache ci -> readTVarIO (cEpoch ci)

readEpochFromCacheEpoch :: Cache -> IO (Maybe DB.Epoch)
readEpochFromCacheEpoch cache =
  case cache of
    UninitiatedCache -> pure Nothing
    Cache ci -> do
      cachedEpoch <- readTVarIO (cEpoch ci)
      case cachedEpoch of
        Nothing -> pure Nothing
        Just ce -> pure $ Just =<< ceEpoch ce

writeCacheEpoch ::
  MonadIO m =>
  Cache ->
  CacheEpoch ->
  ReaderT SqlBackend m ()
writeCacheEpoch cache cacheEpoch =
  case cache of
    UninitiatedCache -> pure ()
    Cache ci -> liftIO $ atomically $ writeTVar (cEpoch ci) $ Just cacheEpoch

writeBlockAndFeeToCacheEpoch ::
  MonadIO m =>
  Cache ->
  CardanoBlock ->
  Word64 ->
  ReaderT SqlBackend m ()
writeBlockAndFeeToCacheEpoch cache block fees =
  case cache of
    UninitiatedCache -> pure ()
    Cache ci -> do
      cachedEpoch <- liftIO $ readTVarIO (cEpoch ci)
      case cachedEpoch of
        Nothing -> do
          -- If we don't have an CacheEpoch then this is either the first block
          -- or we're syncing for the first time and we need to continue where we left off
          latestEpochFromDb <- DB.queryLatestEpoch
          liftIO $ atomically $ writeTVar (cEpoch ci) (Just $ CacheEpoch latestEpochFromDb block fees)
        Just cacheE -> liftIO $ atomically $ writeTVar (cEpoch ci) (Just cacheE {ceBlock = block, ceFees = fees})

writeEpochToCacheEpoch ::
  MonadIO m =>
  Cache ->
  DB.Epoch ->
  ReaderT SqlBackend m ()
writeEpochToCacheEpoch cache newEpoch =
  case cache of
    UninitiatedCache -> pure ()
    Cache ci -> do
      cachedEpoch <- liftIO $ readTVarIO (cEpoch ci)
      case cachedEpoch of
        Nothing -> pure ()
        Just cacheE ->
          liftIO $ atomically $ writeTVar (cEpoch ci) (Just $ cacheE {ceEpoch = Just newEpoch})
