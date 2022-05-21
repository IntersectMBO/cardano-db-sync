{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.DbSync.Cache
  ( Cache
  , CacheNew (..)
  , newEmptyCache
  , uninitiatedCache
  , rollbackCache
  , queryPoolKeyWithCache
  , insertPoolKeyWithCache
  , queryStakeAddrWithCache
  , queryMAWithCache
  , queryPrevBlockWithCache
  , insertBlockAndCache

  -- * CacheStatistics
  , CacheStatistics
  , getCacheStatistics
  , textShowStats
  ) where

import           Cardano.Prelude

import           Control.Monad.Class.MonadSTM.Strict (StrictTVar, modifyTVar, newTVarIO, readTVarIO,
                   writeTVar)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import           Cardano.Ledger.Mary.Value

import qualified Cardano.Db as DB

import           Cardano.DbSync.Cache.LRU (LRUCache)
import qualified Cardano.DbSync.Cache.LRU as LRU
import           Cardano.DbSync.Era.Shelley.Generic
import qualified Cardano.DbSync.Era.Shelley.Generic.StakePoolKeyHash as Generic
import qualified Cardano.DbSync.Era.Shelley.Generic.Util as Generic
import           Cardano.DbSync.Era.Shelley.Query
import           Cardano.DbSync.Era.Util
import           Cardano.DbSync.Error

import           Ouroboros.Consensus.Cardano.Block (StandardCrypto)

import           Database.Persist.Postgresql (SqlBackend)

type StakeAddrCache = Map StakeCred DB.StakeAddressId
type StakePoolCache = Map StakePoolKeyHash DB.PoolHashId

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
  deriving Eq

data CacheInternal = CacheInternal
  { cStakeCreds :: !(StrictTVar IO StakeAddrCache)
  , cPools :: !(StrictTVar IO StakePoolCache)
  , cMultiAssets :: !(StrictTVar IO (LRUCache (ByteString, AssetName) DB.MultiAssetId))
  , cPrevBlock :: !(StrictTVar IO (Maybe (DB.BlockId, ByteString)))
  , cStats :: !(StrictTVar IO CacheStatistics)
  }

data CacheStatistics = CacheStatistics
  { credsHits :: !Word64
  , credsQueries :: !Word64
  , poolsHits :: !Word64
  , poolsQueries :: !Word64
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
initCacheStatistics = CacheStatistics 0 0 0 0 0 0 0 0

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
    mAssets <- readTVarIO (cMultiAssets ic)
    pure $ mconcat
      [ "\nCache Statistics:"
      , "\n  Stake Addresses: ", "cache size: ", DB.textShow (Map.size creds)
      , if credsQueries stats == 0
          then ""
          else ", hit rate: " <> DB.textShow (100 * credsHits stats `div` credsQueries stats) <> "%"
      , ", hits: ", DB.textShow (credsHits stats)
      , ", misses: ", DB.textShow (credsQueries stats - credsHits stats)
      , "\n  Pools: ", "cache size: ", DB.textShow (Map.size pools)
      , if poolsQueries stats == 0
          then ""
          else ", hit rate: " <> DB.textShow (100 * poolsHits stats `div` poolsQueries stats) <> "%"
      , ", hits: ", DB.textShow (poolsHits stats)
      , ", misses: ", DB.textShow (poolsQueries stats - poolsHits stats)
      , "\n  Multi Assets: ", "cache capacity: ", DB.textShow (LRU.getCapacity mAssets)
      , ", cache size: ", DB.textShow (LRU.getSize mAssets)
      , if multiAssetsQueries stats == 0
          then ""
          else ", hit rate: " <> DB.textShow (100 * multiAssetsHits stats `div` multiAssetsQueries stats) <> "%"
      , ", hits: ", DB.textShow (multiAssetsHits stats)
      , ", misses: ", DB.textShow (multiAssetsQueries stats - multiAssetsHits stats)
      , "\n  Previous Block: "
      , if prevBlockQueries stats == 0
          then ""
          else "hit rate: " <> DB.textShow (100 * prevBlockHits stats `div` prevBlockQueries stats) <> "%"
      , ", hits: ", DB.textShow (prevBlockHits stats)
      , ", misses: ", DB.textShow (prevBlockQueries stats - prevBlockHits stats)
      ]

uninitiatedCache :: Cache
uninitiatedCache = UninitiatedCache

newEmptyCache :: MonadIO m => Word64 -> m Cache
newEmptyCache maCapacity =
  liftIO . fmap Cache $
    CacheInternal
      <$> newTVarIO Map.empty
      <*> newTVarIO Map.empty
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
-- IMPORTANT NOTE: we rely here on the fact that 'MultiAsset' and 'PoolHash'
-- tables don't have an ON DELETE reference and as a result are not cleaned up in
-- case of a rollback. If this changes in the future, it is necessary that their
-- cached values are also cleaned up.
--
-- NOTE: BlockId is cleaned up on rollbacks, since it may get reinserted on
-- a different id.
-- NOTE: For 'StakeAddresses' we use a mixed approach. If the rollback is long we just drop
-- everything, since it is very rare. If not, we query all the StakeAddressesId of blocks
-- that wil be deleted.
rollbackCache :: MonadIO m => Cache -> Maybe Word64 -> Word64 -> ReaderT SqlBackend m ()
rollbackCache UninitiatedCache _ _ = pure ()
rollbackCache (Cache cache) mBlockNo nBlocks = do
  liftIO $ do
    atomically $ writeTVar (cPools cache) Map.empty
    atomically $ modifyTVar (cMultiAssets cache) LRU.cleanup
    atomically $ writeTVar (cPrevBlock cache) Nothing
  rollbackStakeAddr cache mBlockNo nBlocks

rollbackStakeAddr :: MonadIO m => CacheInternal -> Maybe Word64 -> Word64 -> ReaderT SqlBackend m ()
rollbackStakeAddr ci mBlockNo nBlocks = do
  case mBlockNo of
    Nothing -> liftIO $ atomically $ writeTVar (cStakeCreds ci) Map.empty
    Just blockNo ->
      if nBlocks > 600
        then liftIO $ atomically $ writeTVar (cStakeCreds ci) Map.empty
        else do
          initMp <- liftIO $ readTVarIO (cStakeCreds ci)
          stakeAddrIds <- DB.queryStakeAddressIdsAfter blockNo
          let stakeAddrIdsSet = Set.fromList stakeAddrIds
          let !mp = Map.filter (`Set.member` stakeAddrIdsSet) initMp
          liftIO $ atomically $ writeTVar (cStakeCreds ci) mp

queryStakeAddrWithCache
    :: forall m. MonadIO m => Cache -> CacheNew -> StakeCred
    -> ReaderT SqlBackend m (Either DB.LookupFail DB.StakeAddressId)
queryStakeAddrWithCache cache cacheNew cred = do
  case cache of
    UninitiatedCache -> queryStakeAddress (unStakeCred cred)
    Cache ci -> do
      mp <- liftIO $ readTVarIO (cStakeCreds ci)
      (mAddrId, mp') <- queryStakeAddrAux cacheNew mp (cStats ci) cred
      liftIO $ atomically $ writeTVar (cStakeCreds ci) mp'
      pure mAddrId

queryStakeAddrAux
    :: MonadIO m
    => CacheNew -> StakeAddrCache -> StrictTVar IO CacheStatistics -> StakeCred
    -> ReaderT SqlBackend m (Either DB.LookupFail DB.StakeAddressId, StakeAddrCache)
queryStakeAddrAux cacheNew mp sts hsh =
    case Map.lookup hsh mp of
      Just addrId -> do
        liftIO $ hitCreds sts
        case cacheNew of
          EvictAndReturn -> pure (Right addrId, Map.delete hsh mp)
          _ -> pure (Right addrId, mp)
      Nothing -> do
        liftIO $ missCreds sts
        mAddrId <- queryStakeAddress (unStakeCred hsh)
        case (mAddrId, cacheNew) of
          (Right addrId, CacheNew) -> pure (Right addrId, Map.insert hsh addrId mp)
          (Right addrId, _) -> pure (Right addrId, mp)
          (err, _) -> pure (err, mp)

queryPoolKeyWithCache
    :: MonadIO m
    => Cache -> CacheNew -> StakePoolKeyHash
    -> ReaderT SqlBackend m (Either DB.LookupFail DB.PoolHashId)
queryPoolKeyWithCache cache cacheNew hsh =
  case cache of
    UninitiatedCache -> do
      mPhId <- queryPoolHashId (unStakePoolKeyHash hsh)
      case mPhId of
        Nothing -> pure $ Left (DB.DbLookupMessage "StakePoolKeyHash")
        Just phId -> pure $ Right phId
    Cache ci -> do
      mp <- liftIO $ readTVarIO (cPools ci)
      case Map.lookup hsh mp of
        Just phId -> do
          liftIO $ hitPools (cStats ci)
          -- hit so we can't cache even with 'CacheNew'
          when (cacheNew == EvictAndReturn) $
            liftIO $ atomically $ modifyTVar (cPools ci) $ Map.delete hsh
          pure $ Right phId
        Nothing -> do
          liftIO $ missPools (cStats ci)
          mPhId <- queryPoolHashId (unStakePoolKeyHash hsh)
          case mPhId of
            Nothing -> pure $ Left (DB.DbLookupMessage "StakePoolKeyHash")
            Just phId -> do
              -- missed so we can't evict even with 'EvictAndReturn'
              when (cacheNew == CacheNew) $
                liftIO $ atomically $ modifyTVar (cPools ci) $ Map.insert hsh phId
              pure $ Right phId

insertPoolKeyWithCache
    :: (MonadBaseControl IO m, MonadIO m) => Cache -> CacheNew -> KeyHash 'StakePool StandardCrypto
    -> ReaderT SqlBackend m DB.PoolHashId
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
      let !keyHash = Generic.toStakePoolKeyHash pHash
      case Map.lookup keyHash mp of
        Just phId -> do
          liftIO $ hitPools (cStats ci)
          when (cacheNew == EvictAndReturn) $
            liftIO $ atomically $ modifyTVar (cPools ci) $ Map.delete keyHash
          pure phId
        Nothing -> do
          liftIO $ missPools (cStats ci)
          phId <- DB.insertPoolHash $
            DB.PoolHash
              { DB.poolHashHashRaw = Generic.unKeyHashRaw pHash
              , DB.poolHashView = Generic.unKeyHashView pHash
              }
          when (cacheNew == CacheNew) $
            liftIO $ atomically $ modifyTVar (cPools ci) $ Map.insert keyHash phId
          pure phId

queryMAWithCache :: MonadIO m => Cache -> ByteString -> AssetName
                 -> ReaderT SqlBackend m (Maybe DB.MultiAssetId)
queryMAWithCache cache policyId asset =
  case  cache of
    UninitiatedCache -> DB.queryMultiAssetId policyId (unAssetName asset)
    Cache ci -> do
      mp <- liftIO $ readTVarIO (cMultiAssets ci)
      case LRU.lookup (policyId, asset) mp of
        Just (maId, mp') -> do
          liftIO $ hitMAssets (cStats ci)
          liftIO $ atomically $ writeTVar (cMultiAssets ci) mp'
          pure $ Just maId
        Nothing -> do
          liftIO $ missMAssets (cStats ci)
          -- miss. The lookup doesn't change the cache on a miss.
          maId <- DB.queryMultiAssetId policyId (unAssetName asset)
          case maId of
            Nothing -> do
              pure Nothing
            Just mId -> do
              liftIO $ atomically $ modifyTVar (cMultiAssets ci) $ LRU.insert (policyId, asset) mId
              pure maId

queryPrevBlockWithCache :: MonadIO m => Text -> Cache -> ByteString
                        -> ExceptT SyncNodeError (ReaderT SqlBackend m) DB.BlockId
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
    queryFromDb
        :: MonadIO m
        => CacheInternal -> ExceptT SyncNodeError (ReaderT SqlBackend m) DB.BlockId
    queryFromDb ci = do
      liftIO $ missPrevBlock (cStats ci)
      liftLookupFail msg $ DB.queryBlockId hsh

insertBlockAndCache
    :: (MonadIO m, MonadBaseControl IO m)
    => Cache -> DB.Block -> ReaderT SqlBackend m DB.BlockId
insertBlockAndCache cache block =
  case cache of
    UninitiatedCache -> DB.insertBlock block
    Cache ci -> do
      bid <- DB.insertBlock block
      liftIO $ do
        missPrevBlock (cStats ci)
        atomically $ writeTVar (cPrevBlock ci) $ Just (bid, DB.blockHash block)
      pure bid

-- It is completely ***INSANE*** that we even need to do something like this.
unAssetName :: AssetName -> ByteString
unAssetName (AssetName a) = a
