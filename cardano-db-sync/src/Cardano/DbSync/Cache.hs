{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Cache (
  Cache,
  CacheNew (..),
  newEmptyCache,
  uninitiatedCache,
  rollbackCache,
  queryPoolKeyWithCache,
  insertPoolKeyWithCache,
  queryRewardAccountWithCache,
  queryRewardAccountWithCacheRetBs,
  queryStakeAddrWithCache,
  queryStakeAddrWithCacheRetBs,
  queryMAWithCache,
  queryPrevBlockWithCache,
  insertBlockAndCache,
  queryDatum,
  insertDatumAndCache,

  -- * CacheStatistics
  CacheStatistics,
  getCacheStatistics,
  textShowStats,
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Cache.LRU (LRUCache)
import qualified Cardano.DbSync.Cache.LRU as LRU
import qualified Cardano.DbSync.Era.Shelley.Generic.Util as Generic
import Cardano.DbSync.Era.Shelley.Query
import Cardano.DbSync.Era.Util
import Cardano.DbSync.Error
import Cardano.DbSync.Types
import qualified Cardano.Ledger.Address as Ledger
import Cardano.Ledger.BaseTypes (Network)
import Cardano.Ledger.Mary.Value
import Cardano.Prelude
import Control.Concurrent.Class.MonadSTM.Strict (
  StrictTVar,
  modifyTVar,
  newTVarIO,
  readTVarIO,
  writeTVar,
 )
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Either.Combinators
import qualified Data.Map.Strict as Map
import Database.Persist.Postgresql (SqlBackend)
import Ouroboros.Consensus.Cardano.Block (StandardCrypto)

type StakeAddrCache = Map StakeCred DB.StakeAddressId

type StakePoolCache = Map PoolKeyHash DB.PoolHashId

-- The 'UninitiatedCache' makes it possible to call functions in this module
-- without having actually initiated the cache yet. It is used by genesis
-- insertions, where the cache has not been initiated yet.
data Cache
  = UninitiatedCache
  | Cache !CacheInternal

data CacheNew
  = CacheNew
  | DontCacheNew
  | EvictAndReturn
  deriving (Eq)

data CacheInternal = CacheInternal
  { cStakeCreds :: !(StrictTVar IO StakeAddrCache)
  , cPools :: !(StrictTVar IO StakePoolCache)
  , cDatum :: !(StrictTVar IO (LRUCache DataHash DB.DatumId))
  , cMultiAssets :: !(StrictTVar IO (LRUCache (PolicyID StandardCrypto, AssetName) DB.MultiAssetId))
  , cPrevBlock :: !(StrictTVar IO (Maybe (DB.BlockId, ByteString)))
  , cStats :: !(StrictTVar IO CacheStatistics)
  }

data CacheStatistics = CacheStatistics
  { credsHits :: !Word64
  , credsQueries :: !Word64
  , poolsHits :: !Word64
  , poolsQueries :: !Word64
  , datumHits :: !Word64
  , datumQueries :: !Word64
  , multiAssetsHits :: !Word64
  , multiAssetsQueries :: !Word64
  , prevBlockHits :: !Word64
  , prevBlockQueries :: !Word64
  }

hitCreds :: StrictTVar IO CacheStatistics -> IO ()
hitCreds ref =
  atomically $ modifyTVar ref (\cs -> cs {credsHits = 1 + credsHits cs, credsQueries = 1 + credsQueries cs})

missCreds :: StrictTVar IO CacheStatistics -> IO ()
missCreds ref =
  atomically $ modifyTVar ref (\cs -> cs {credsQueries = 1 + credsQueries cs})

hitPools :: StrictTVar IO CacheStatistics -> IO ()
hitPools ref =
  atomically $ modifyTVar ref (\cs -> cs {poolsHits = 1 + poolsHits cs, poolsQueries = 1 + poolsQueries cs})

missPools :: StrictTVar IO CacheStatistics -> IO ()
missPools ref =
  atomically $ modifyTVar ref (\cs -> cs {poolsQueries = 1 + poolsQueries cs})

hitDatum :: StrictTVar IO CacheStatistics -> IO ()
hitDatum ref =
  atomically $ modifyTVar ref (\cs -> cs {datumHits = 1 + datumHits cs, datumQueries = 1 + datumQueries cs})

missDatum :: StrictTVar IO CacheStatistics -> IO ()
missDatum ref =
  atomically $ modifyTVar ref (\cs -> cs {datumQueries = 1 + datumQueries cs})

hitMAssets :: StrictTVar IO CacheStatistics -> IO ()
hitMAssets ref =
  atomically $ modifyTVar ref (\cs -> cs {multiAssetsHits = 1 + multiAssetsHits cs, multiAssetsQueries = 1 + multiAssetsQueries cs})

missMAssets :: StrictTVar IO CacheStatistics -> IO ()
missMAssets ref =
  atomically $ modifyTVar ref (\cs -> cs {multiAssetsQueries = 1 + multiAssetsQueries cs})

hitPBlock :: StrictTVar IO CacheStatistics -> IO ()
hitPBlock ref =
  atomically $ modifyTVar ref (\cs -> cs {prevBlockHits = 1 + prevBlockHits cs, prevBlockQueries = 1 + prevBlockQueries cs})

missPrevBlock :: StrictTVar IO CacheStatistics -> IO ()
missPrevBlock ref =
  atomically $ modifyTVar ref (\cs -> cs {prevBlockQueries = 1 + prevBlockQueries cs})

initCacheStatistics :: CacheStatistics
initCacheStatistics = CacheStatistics 0 0 0 0 0 0 0 0 0 0

getCacheStatistics :: Cache -> IO CacheStatistics
getCacheStatistics cs =
  case cs of
    UninitiatedCache -> pure initCacheStatistics
    Cache ci -> readTVarIO (cStats ci)

textShowStats :: Cache -> IO Text
textShowStats UninitiatedCache = pure "UninitiatedCache"
textShowStats (Cache ic) = do
  stats <- readTVarIO $ cStats ic
  creds <- readTVarIO (cStakeCreds ic)
  pools <- readTVarIO (cPools ic)
  datums <- readTVarIO (cDatum ic)
  mAssets <- readTVarIO (cMultiAssets ic)
  pure $
    mconcat
      [ "\nCache Statistics:"
      , "\n  Stake Addresses: "
      , "cache size: "
      , DB.textShow (Map.size creds)
      , if credsQueries stats == 0
          then ""
          else ", hit rate: " <> DB.textShow (100 * credsHits stats `div` credsQueries stats) <> "%"
      , ", hits: "
      , DB.textShow (credsHits stats)
      , ", misses: "
      , DB.textShow (credsQueries stats - credsHits stats)
      , "\n  Pools: "
      , "cache size: "
      , DB.textShow (Map.size pools)
      , if poolsQueries stats == 0
          then ""
          else ", hit rate: " <> DB.textShow (100 * poolsHits stats `div` poolsQueries stats) <> "%"
      , ", hits: "
      , DB.textShow (poolsHits stats)
      , ", misses: "
      , DB.textShow (poolsQueries stats - poolsHits stats)
      , "\n  Datums: "
      , "cache capacity: "
      , DB.textShow (LRU.getCapacity datums)
      , ", cache size: "
      , DB.textShow (LRU.getSize datums)
      , if datumQueries stats == 0
          then ""
          else ", hit rate: " <> DB.textShow (100 * datumHits stats `div` datumQueries stats) <> "%"
      , ", hits: "
      , DB.textShow (datumHits stats)
      , ", misses: "
      , DB.textShow (datumQueries stats - datumHits stats)
      , "\n  Multi Assets: "
      , "cache capacity: "
      , DB.textShow (LRU.getCapacity mAssets)
      , ", cache size: "
      , DB.textShow (LRU.getSize mAssets)
      , if multiAssetsQueries stats == 0
          then ""
          else ", hit rate: " <> DB.textShow (100 * multiAssetsHits stats `div` multiAssetsQueries stats) <> "%"
      , ", hits: "
      , DB.textShow (multiAssetsHits stats)
      , ", misses: "
      , DB.textShow (multiAssetsQueries stats - multiAssetsHits stats)
      , "\n  Previous Block: "
      , if prevBlockQueries stats == 0
          then ""
          else "hit rate: " <> DB.textShow (100 * prevBlockHits stats `div` prevBlockQueries stats) <> "%"
      , ", hits: "
      , DB.textShow (prevBlockHits stats)
      , ", misses: "
      , DB.textShow (prevBlockQueries stats - prevBlockHits stats)
      ]

uninitiatedCache :: Cache
uninitiatedCache = UninitiatedCache

newEmptyCache :: MonadIO m => Word64 -> Word64 -> m Cache
newEmptyCache maCapacity daCapacity =
  liftIO . fmap Cache $
    CacheInternal
      <$> newTVarIO Map.empty
      <*> newTVarIO Map.empty
      <*> newTVarIO (LRU.empty daCapacity)
      <*> newTVarIO (LRU.empty maCapacity)
      <*> newTVarIO Nothing
      <*> newTVarIO initCacheStatistics

-- Rollbacks make everything harder and the same applies to caching.
-- After a rollback db entries are deleted, so we need to clean the same
-- cached entries. Cleaning more cached entries is not an issue. Cleaning less
-- can cause all sorts of issues, since it would give false info about the existance
-- of an entry or even a wrong entry id, if the entry is reinserted on a different
-- id after the rollback.
--
-- IMPORTANT NOTE: we rely here on the fact that 'MultiAsset', 'StakeAddress' and 'PoolHash'
-- tables don't have an ON DELETE reference and as a result are not cleaned up in
-- case of a rollback. If this changes in the future, it is necessary that their
-- cached values are also cleaned up.
--
-- NOTE: BlockId is cleaned up on rollbacks, since it may get reinserted on
-- a different id.
-- NOTE: Other tables are not cleaned up since they are not rollbacked.
rollbackCache :: MonadIO m => Cache -> ReaderT SqlBackend m ()
rollbackCache UninitiatedCache = pure ()
rollbackCache (Cache cache) = do
  liftIO $ do
    atomically $ writeTVar (cPrevBlock cache) Nothing
    atomically $ modifyTVar (cDatum cache) LRU.cleanup

queryRewardAccountWithCache ::
  forall m.
  MonadIO m =>
  Cache ->
  CacheNew ->
  Ledger.RewardAcnt StandardCrypto ->
  ReaderT SqlBackend m (Either DB.LookupFail DB.StakeAddressId)
queryRewardAccountWithCache cache cacheNew rwdAcc =
  mapLeft fst <$> queryRewardAccountWithCacheRetBs cache cacheNew rwdAcc

queryRewardAccountWithCacheRetBs ::
  forall m.
  MonadIO m =>
  Cache ->
  CacheNew ->
  Ledger.RewardAcnt StandardCrypto ->
  ReaderT SqlBackend m (Either (DB.LookupFail, ByteString) DB.StakeAddressId)
queryRewardAccountWithCacheRetBs cache cacheNew rwdAcc =
  queryStakeAddrWithCacheRetBs cache cacheNew (Ledger.getRwdNetwork rwdAcc) (Ledger.getRwdCred rwdAcc)

queryStakeAddrWithCache ::
  forall m.
  MonadIO m =>
  Cache ->
  CacheNew ->
  Network ->
  StakeCred ->
  ReaderT SqlBackend m (Either DB.LookupFail DB.StakeAddressId)
queryStakeAddrWithCache cache cacheNew nw cred =
  mapLeft fst <$> queryStakeAddrWithCacheRetBs cache cacheNew nw cred

queryStakeAddrWithCacheRetBs ::
  forall m.
  MonadIO m =>
  Cache ->
  CacheNew ->
  Network ->
  StakeCred ->
  ReaderT SqlBackend m (Either (DB.LookupFail, ByteString) DB.StakeAddressId)
queryStakeAddrWithCacheRetBs cache cacheNew nw cred = do
  case cache of
    UninitiatedCache -> do
      let !bs = Ledger.serialiseRewardAcnt (Ledger.RewardAcnt nw cred)
      mapLeft (,bs) <$> queryStakeAddress bs
    Cache ci -> do
      mp <- liftIO $ readTVarIO (cStakeCreds ci)
      (mAddrId, mp') <- queryStakeAddrAux cacheNew mp (cStats ci) nw cred
      liftIO $ atomically $ writeTVar (cStakeCreds ci) mp'
      pure mAddrId

queryStakeAddrAux ::
  MonadIO m =>
  CacheNew ->
  StakeAddrCache ->
  StrictTVar IO CacheStatistics ->
  Network ->
  StakeCred ->
  ReaderT SqlBackend m (Either (DB.LookupFail, ByteString) DB.StakeAddressId, StakeAddrCache)
queryStakeAddrAux cacheNew mp sts nw cred =
  case Map.lookup cred mp of
    Just addrId -> do
      liftIO $ hitCreds sts
      case cacheNew of
        EvictAndReturn -> pure (Right addrId, Map.delete cred mp)
        _ -> pure (Right addrId, mp)
    Nothing -> do
      liftIO $ missCreds sts
      let !bs = Ledger.serialiseRewardAcnt (Ledger.RewardAcnt nw cred)
      mAddrId <- mapLeft (,bs) <$> queryStakeAddress bs
      case (mAddrId, cacheNew) of
        (Right addrId, CacheNew) -> pure (Right addrId, Map.insert cred addrId mp)
        (Right addrId, _) -> pure (Right addrId, mp)
        (err, _) -> pure (err, mp)

queryPoolKeyWithCache ::
  MonadIO m =>
  Cache ->
  CacheNew ->
  PoolKeyHash ->
  ReaderT SqlBackend m (Either DB.LookupFail DB.PoolHashId)
queryPoolKeyWithCache cache cacheNew hsh =
  case cache of
    UninitiatedCache -> do
      mPhId <- queryPoolHashId (Generic.unKeyHashRaw hsh)
      case mPhId of
        Nothing -> pure $ Left (DB.DbLookupMessage "PoolKeyHash")
        Just phId -> pure $ Right phId
    Cache ci -> do
      mp <- liftIO $ readTVarIO (cPools ci)
      case Map.lookup hsh mp of
        Just phId -> do
          liftIO $ hitPools (cStats ci)
          -- hit so we can't cache even with 'CacheNew'
          when (cacheNew == EvictAndReturn) $
            liftIO $
              atomically $
                modifyTVar (cPools ci) $
                  Map.delete hsh
          pure $ Right phId
        Nothing -> do
          liftIO $ missPools (cStats ci)
          mPhId <- queryPoolHashId (Generic.unKeyHashRaw hsh)
          case mPhId of
            Nothing -> pure $ Left (DB.DbLookupMessage "PoolKeyHash")
            Just phId -> do
              -- missed so we can't evict even with 'EvictAndReturn'
              when (cacheNew == CacheNew) $
                liftIO $
                  atomically $
                    modifyTVar (cPools ci) $
                      Map.insert hsh phId
              pure $ Right phId

insertPoolKeyWithCache ::
  (MonadBaseControl IO m, MonadIO m) =>
  Cache ->
  CacheNew ->
  PoolKeyHash ->
  ReaderT SqlBackend m DB.PoolHashId
insertPoolKeyWithCache cache cacheNew pHash =
  case cache of
    UninitiatedCache ->
      DB.insertPoolHash $
        DB.PoolHash
          { DB.poolHashHashRaw = Generic.unKeyHashRaw pHash
          , DB.poolHashView = Generic.unKeyHashView pHash
          }
    Cache ci -> do
      mp <- liftIO $ readTVarIO (cPools ci)
      case Map.lookup pHash mp of
        Just phId -> do
          liftIO $ hitPools (cStats ci)
          when (cacheNew == EvictAndReturn) $
            liftIO $
              atomically $
                modifyTVar (cPools ci) $
                  Map.delete pHash
          pure phId
        Nothing -> do
          liftIO $ missPools (cStats ci)
          phId <-
            DB.insertPoolHash $
              DB.PoolHash
                { DB.poolHashHashRaw = Generic.unKeyHashRaw pHash
                , DB.poolHashView = Generic.unKeyHashView pHash
                }
          when (cacheNew == CacheNew) $
            liftIO $
              atomically $
                modifyTVar (cPools ci) $
                  Map.insert pHash phId
          pure phId

queryMAWithCache ::
  MonadIO m =>
  Cache ->
  PolicyID StandardCrypto ->
  AssetName ->
  ReaderT SqlBackend m (Either (ByteString, ByteString) DB.MultiAssetId)
queryMAWithCache cache policyId asset =
  case cache of
    UninitiatedCache -> do
      let !policyBs = Generic.unScriptHash $ policyID policyId
      let !assetNameBs = Generic.unAssetName asset
      maybe (Left (policyBs, assetNameBs)) Right <$> DB.queryMultiAssetId policyBs assetNameBs
    Cache ci -> do
      mp <- liftIO $ readTVarIO (cMultiAssets ci)
      case LRU.lookup (policyId, asset) mp of
        Just (maId, mp') -> do
          liftIO $ hitMAssets (cStats ci)
          liftIO $ atomically $ writeTVar (cMultiAssets ci) mp'
          pure $ Right maId
        Nothing -> do
          liftIO $ missMAssets (cStats ci)
          -- miss. The lookup doesn't change the cache on a miss.
          let !policyBs = Generic.unScriptHash $ policyID policyId
          let !assetNameBs = Generic.unAssetName asset
          maId <- maybe (Left (policyBs, assetNameBs)) Right <$> DB.queryMultiAssetId policyBs assetNameBs
          whenRight maId $
            liftIO . atomically . modifyTVar (cMultiAssets ci) . LRU.insert (policyId, asset)
          pure maId

queryPrevBlockWithCache ::
  MonadIO m =>
  Text ->
  Cache ->
  ByteString ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) DB.BlockId
queryPrevBlockWithCache msg cache hsh =
  case cache of
    UninitiatedCache -> liftLookupFail msg $ DB.queryBlockId hsh
    Cache ci -> do
      mCachedPrev <- liftIO $ readTVarIO (cPrevBlock ci)
      case mCachedPrev of
        -- if the cached block matches the requested hash, we return its db id.
        Just (cachedBlockId, cachedHash) ->
          if cachedHash == hsh
            then do
              liftIO $ hitPBlock (cStats ci)
              pure cachedBlockId
            else queryFromDb ci
        _ -> queryFromDb ci
  where
    queryFromDb ::
      MonadIO m =>
      CacheInternal ->
      ExceptT SyncNodeError (ReaderT SqlBackend m) DB.BlockId
    queryFromDb ci = do
      liftIO $ missPrevBlock (cStats ci)
      liftLookupFail msg $ DB.queryBlockId hsh

insertBlockAndCache ::
  (MonadIO m, MonadBaseControl IO m) =>
  Cache ->
  DB.Block ->
  ReaderT SqlBackend m DB.BlockId
insertBlockAndCache cache block =
  case cache of
    UninitiatedCache -> DB.insertBlock block
    Cache ci -> do
      bid <- DB.insertBlock block
      liftIO $ do
        missPrevBlock (cStats ci)
        atomically $ writeTVar (cPrevBlock ci) $ Just (bid, DB.blockHash block)
      pure bid

queryDatum ::
  MonadIO m =>
  Cache ->
  DataHash ->
  ReaderT SqlBackend m (Maybe DB.DatumId)
queryDatum cache hsh = do
  case cache of
    UninitiatedCache -> DB.queryDatum $ Generic.dataHashToBytes hsh
    Cache ci -> do
      mp <- liftIO $ readTVarIO (cDatum ci)
      case LRU.lookup hsh mp of
        Just (datumId, mp') -> do
          liftIO $ hitDatum (cStats ci)
          liftIO $ atomically $ writeTVar (cDatum ci) mp'
          pure $ Just datumId
        Nothing -> do
          liftIO $ missDatum (cStats ci)
          -- miss. The lookup doesn't change the cache on a miss.
          DB.queryDatum $ Generic.dataHashToBytes hsh

-- This assumes the entry is not cached.
insertDatumAndCache ::
  (MonadIO m, MonadBaseControl IO m) =>
  Cache ->
  DataHash ->
  DB.Datum ->
  ReaderT SqlBackend m DB.DatumId
insertDatumAndCache cache hsh dt = do
  datumId <- DB.insertDatum dt
  case cache of
    UninitiatedCache -> pure datumId
    Cache ci -> do
      liftIO $
        atomically $
          modifyTVar (cDatum ci) $
            LRU.insert hsh datumId
      pure datumId
