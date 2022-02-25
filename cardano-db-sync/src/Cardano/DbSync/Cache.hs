{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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

    -- * CacheStats
    , CacheStats
    , getCacheStats
    , textShowStats
    ) where

import           Cardano.Prelude hiding (atomically, (.))

import           Control.Monad.Class.MonadSTM.Strict (StrictTVar, atomically, modifyTVar, newTVarIO,
                   readTVarIO, writeTVar)
import           Control.Monad.Trans.Control
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
data Cache = UninitiatedCache | Cache CacheInternal

data CacheNew = CacheNew | DontCacheNew | EvictAndReturn
  deriving Eq

data CacheInternal = CacheInternal
  { cStakeCreds :: StrictTVar IO StakeAddrCache
  , cPools :: StrictTVar IO StakePoolCache
  , cMultiAssets :: StrictTVar IO (LRUCache (ByteString, AssetName) DB.MultiAssetId)
  , cPrevBlock :: StrictTVar IO (Maybe (DB.BlockId, ByteString))
  , cStats :: StrictTVar IO CacheStats
  }

data CacheStats = CacheStats
  { credsHits :: Word64
  , credsQueries :: Word64
  , poolsHits :: Word64
  , poolsQueries :: Word64
  , multiAssetsHits :: Word64
  , multiAssetsQueries :: Word64
  , prevBlockHits :: Word64
  , prevBlockQueries :: Word64
  }

hitCreds :: StrictTVar IO CacheStats -> IO ()
hitCreds ref = atomically $ modifyTVar ref (\cs -> cs {credsHits = 1 + credsHits cs, credsQueries = 1 + credsQueries cs})
missCreds :: StrictTVar IO CacheStats -> IO ()
missCreds ref = atomically $ modifyTVar ref (\cs -> cs {credsQueries = 1 + credsQueries cs})
hitPools :: StrictTVar IO CacheStats -> IO ()
hitPools ref = atomically $ modifyTVar ref (\cs -> cs {poolsHits = 1 + poolsHits cs, poolsQueries = 1 + poolsQueries cs})
missPools :: StrictTVar IO CacheStats -> IO ()
missPools ref = atomically $ modifyTVar ref (\cs -> cs {poolsQueries = 1 + poolsQueries cs})
hitMAssets :: StrictTVar IO CacheStats -> IO ()
hitMAssets ref = atomically $ modifyTVar ref (\cs -> cs {multiAssetsHits = 1 + multiAssetsHits cs, multiAssetsQueries = 1 + multiAssetsQueries cs})
missMAssets :: StrictTVar IO CacheStats -> IO ()
missMAssets ref = atomically $ modifyTVar ref (\cs -> cs {multiAssetsQueries = 1 + multiAssetsQueries cs})
hitPBlock :: StrictTVar IO CacheStats -> IO ()
hitPBlock ref = atomically $ modifyTVar ref (\cs -> cs {prevBlockHits = 1 + prevBlockHits cs, prevBlockQueries = 1 + prevBlockQueries cs})
missPBlock :: StrictTVar IO CacheStats -> IO ()
missPBlock ref = atomically $ modifyTVar ref (\cs -> cs {prevBlockQueries = 1 + prevBlockQueries cs})

initCacheStats :: CacheStats
initCacheStats = CacheStats 0 0 0 0 0 0 0 0

getCacheStats :: Cache -> IO CacheStats
getCacheStats UninitiatedCache = pure initCacheStats
getCacheStats (Cache CacheInternal {cStats = ref}) =
    readTVarIO ref

textShowStats :: Cache -> IO Text
textShowStats UninitiatedCache = pure "UninitiatedCache"
textShowStats (Cache ic) = do
    stats <- readTVarIO $ cStats ic
    creds <- readTVarIO (cStakeCreds ic)
    pools <- readTVarIO (cPools ic)
    mAssets <- readTVarIO (cMultiAssets ic)
    pure $ mconcat
      [ "Cache Statistics: "
      , "Stake Addresses: ", "cache size: ", DB.textShow (Map.size creds)
      , if credsQueries stats == 0 then "" else ", hit rate: " <> DB.textShow (100 * credsHits stats `div` credsQueries stats) <> "%"
      , ", hits: ", DB.textShow (credsHits stats)
      , ", misses: ", DB.textShow (credsQueries stats - credsHits stats)
      , ", Pools: ", "cache size: ", DB.textShow (Map.size pools)
      , if poolsQueries stats == 0 then "" else ", hit rate: " <> DB.textShow (100 * poolsHits stats `div` poolsQueries stats) <> "%"
      , ", hits: ", DB.textShow (poolsHits stats)
      , ", misses: ", DB.textShow (poolsQueries stats - poolsHits stats)
      , ", Multi Assets: ", "cache capacity: ", DB.textShow (LRU.getCapacity mAssets)
      , ", cache size: ", DB.textShow (LRU.getCapacity mAssets)
      , if multiAssetsQueries stats == 0 then "" else ", hit rate: " <> DB.textShow (100 * multiAssetsHits stats `div` multiAssetsQueries stats) <> "%"
      , ", hits: ", DB.textShow (multiAssetsHits stats)
      , ", misses: ", DB.textShow (multiAssetsQueries stats - multiAssetsHits stats)
      , ", Previous Block: "
      , if prevBlockQueries stats == 0 then "" else ", hit rate: " <> DB.textShow (100 * prevBlockHits stats `div` prevBlockQueries stats) <> "%"
      , ", hits: ", DB.textShow (prevBlockHits stats)
      , ", misses: ", DB.textShow (prevBlockQueries stats - prevBlockHits stats)
      ]

uninitiatedCache :: Cache
uninitiatedCache = UninitiatedCache

newEmptyCache :: MonadIO m => Word64 -> m Cache
newEmptyCache maCapacity = liftIO $ Cache <$>
                             (CacheInternal
                               <$> newTVarIO Map.empty
                               <*> newTVarIO Map.empty
                               <*> newTVarIO (LRU.empty maCapacity)
                               <*> newTVarIO Nothing
                               <*> newTVarIO initCacheStats)

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
-- NOTE: BlockID is cleaned up on rollbacks, since it may get reinserted on
-- a different id.
-- NOTE: For 'StakeAddresses' we use a mixed approach. If the rollback is long we just drop
-- everything, since it is very rare. If not, we query all the StakeAddressesId of blocks
-- that wil be deleted.
rollbackCache :: MonadIO m => Cache -> Maybe Word64 -> Word64 -> ReaderT SqlBackend m ()
rollbackCache UninitiatedCache _ _ = pure ()
rollbackCache (Cache cache) mBlockNo nBlocks = do
  liftIO $ atomically $ writeTVar (cPools cache) Map.empty
  liftIO $ atomically $ modifyTVar (cMultiAssets cache) LRU.cleanup
  liftIO $ atomically $ writeTVar (cPrevBlock cache) Nothing
  rollbackStakeAddr cache mBlockNo nBlocks

rollbackStakeAddr :: MonadIO m => CacheInternal -> Maybe Word64 -> Word64 -> ReaderT SqlBackend m ()
rollbackStakeAddr CacheInternal {cStakeCreds = ref} Nothing _nBlocks =
    liftIO $ atomically $ writeTVar ref Map.empty
rollbackStakeAddr CacheInternal {cStakeCreds = ref} (Just blockNo) nBlocks = do
    if nBlocks > 600 then
      liftIO $ atomically $ writeTVar ref Map.empty
    else do
      initMp <- liftIO $ readTVarIO ref
      stakeAddrIds <- DB.queryStakeAddressIdsAfter blockNo
      let stakeAddrIdsSet = Set.fromList stakeAddrIds
      let !mp = Map.filter (`Set.member` stakeAddrIdsSet) initMp
      liftIO $ atomically $ writeTVar ref mp

queryStakeAddrWithCache :: forall m. MonadIO m => Cache -> CacheNew -> StakeCred
                        -> ReaderT SqlBackend m (Either DB.LookupFail DB.StakeAddressId)
queryStakeAddrWithCache UninitiatedCache _cacheNew cred =
  queryStakeAddress (unStakeCred cred)
queryStakeAddrWithCache (Cache CacheInternal {cStakeCreds = ref, cStats = sts}) cacheNew cred = do
    mp <- liftIO $ readTVarIO ref
    (mAddrId, mp') <- queryStakeAddrAux cacheNew mp sts cred
    liftIO $ atomically $ writeTVar ref mp'
    pure mAddrId

queryStakeAddrAux :: MonadIO m => CacheNew -> StakeAddrCache
                  -> StrictTVar IO CacheStats -> StakeCred
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

queryPoolKeyWithCache :: MonadIO m => Cache -> CacheNew -> StakePoolKeyHash
                      -> ReaderT SqlBackend m (Either DB.LookupFail DB.PoolHashId)
queryPoolKeyWithCache UninitiatedCache _cacheNew hsh = do
    mPhId <- queryPoolHashId (unStakePoolKeyHash hsh)
    case mPhId of
      Nothing -> pure $ Left (DB.DbLookupMessage "StakePoolKeyHash")
      Just phId -> pure $ Right phId
queryPoolKeyWithCache (Cache CacheInternal {cPools = ref, cStats = sts}) cacheNew hsh = do
    mp <- liftIO $ readTVarIO ref
    case Map.lookup hsh mp of
      Just phId -> do
        liftIO $ hitPools sts
        -- hit so we can't cache even with 'CacheNew'
        when (cacheNew == EvictAndReturn) $
          liftIO $ atomically $ modifyTVar ref $ Map.delete hsh
        pure $ Right phId
      Nothing -> do
        liftIO $ missPools sts
        mPhId <- queryPoolHashId (unStakePoolKeyHash hsh)
        case mPhId of
          Nothing -> pure $ Left (DB.DbLookupMessage "StakePoolKeyHash")
          Just phId -> do
            -- missed so we can't evict even with 'EvictAndReturn'
            when (cacheNew == CacheNew) $
              liftIO $ atomically $ modifyTVar ref $ Map.insert hsh phId
            pure $ Right phId

insertPoolKeyWithCache
    :: (MonadBaseControl IO m, MonadIO m) => Cache -> CacheNew -> KeyHash 'StakePool StandardCrypto
    -> ReaderT SqlBackend m DB.PoolHashId
insertPoolKeyWithCache UninitiatedCache _cacheNew pHash =
    DB.insertPoolHash $
      DB.PoolHash
        { DB.poolHashHashRaw = Generic.unKeyHashRaw pHash
        , DB.poolHashView = Generic.unKeyHashView pHash
        }
insertPoolKeyWithCache (Cache CacheInternal {cPools = ref, cStats = sts}) cacheNew pHash = do
    mp <- liftIO $ readTVarIO ref
    let !keyHash = Generic.toStakePoolKeyHash pHash
    case Map.lookup keyHash mp of
      Just phId -> do
        liftIO $ hitPools sts
        when (cacheNew == EvictAndReturn) $
          liftIO $ atomically $ modifyTVar ref $ Map.delete keyHash
        pure phId
      Nothing -> do
        liftIO $ missPools sts
        phId <- DB.insertPoolHash $
          DB.PoolHash
            { DB.poolHashHashRaw = Generic.unKeyHashRaw pHash
            , DB.poolHashView = Generic.unKeyHashView pHash
            }
        when (cacheNew == CacheNew) $
          liftIO $ atomically $ modifyTVar ref $ Map.insert keyHash phId
        pure phId

queryMAWithCache :: MonadIO m => Cache -> ByteString -> AssetName
                 -> ReaderT SqlBackend m (Maybe DB.MultiAssetId)
queryMAWithCache UninitiatedCache policyId (AssetName aName) =
    DB.queryMultiAssetId policyId aName
queryMAWithCache (Cache CacheInternal {cMultiAssets = ref, cStats = sts}) policyId a@(AssetName aName) = do
    mp <- liftIO $ readTVarIO ref
    case LRU.lookup (policyId, a) mp of
      Just (maId, mp') -> do
        liftIO $ hitMAssets sts
        liftIO $ atomically $ writeTVar ref mp'
        pure $ Just maId
      Nothing -> do
        liftIO $ missMAssets sts
        -- miss. The lookup doesn't change the cache on a miss.
        maId <- DB.queryMultiAssetId policyId aName
        case maId of
          Nothing -> do
            pure Nothing
          Just mId -> do
            liftIO $ atomically $ modifyTVar ref $ LRU.insert (policyId, a) mId
            pure maId

queryPrevBlockWithCache :: MonadIO m => Text -> Cache -> ByteString
                        -> ExceptT SyncNodeError (ReaderT SqlBackend m) DB.BlockId
queryPrevBlockWithCache msg UninitiatedCache hsh =
    liftLookupFail msg $ DB.queryBlockId hsh
queryPrevBlockWithCache msg (Cache CacheInternal {cPrevBlock = ref, cStats = sts}) hsh = do
    mCachedPrev <- liftIO $ readTVarIO ref
    case mCachedPrev of
      -- if the cached block matches the requested hash, we return its db id.
      Just (cachedBlockId, cachedHash) | cachedHash == hsh -> do
        liftIO $ hitPBlock sts
        pure cachedBlockId
      -- else we query it from the db.
      _ -> do
        liftIO $ missPBlock sts
        liftLookupFail msg $ DB.queryBlockId hsh

insertBlockAndCache :: (MonadIO m, MonadBaseControl IO m) => Cache -> DB.Block -> ReaderT SqlBackend m DB.BlockId
insertBlockAndCache UninitiatedCache block =
    DB.insertBlock block
insertBlockAndCache (Cache CacheInternal {cPrevBlock = ref, cStats = sts}) block = do
    bid <- DB.insertBlock block
    liftIO $ missPBlock sts
    liftIO $ atomically $ writeTVar ref $ Just (bid, DB.blockHash block)
    pure bid
