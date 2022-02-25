{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Cardano.DbSync.Cache
    ( Cache
    , CacheNew (..)
    , newEmptyCache
    , uninitiatedCache
    , rollbackCache
    , queryPoolKeyWithCache
    , queryStakeAddrWithCache
    , queryStakeAddrListWithCache
    , queryMAWithCache
    , queryPrevBlockWithCache
    , insertBlockAndCache
    ) where

import           Cardano.Prelude hiding ((.))

import           Control.Monad.Trans.Control
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)

import           Cardano.Ledger.Mary.Value

import qualified Cardano.Db as DB

import           Cardano.DbSync.Cache.LRU (LRUCache)
import qualified Cardano.DbSync.Cache.LRU as LRU
import           Cardano.DbSync.Era.Shelley.Generic
import           Cardano.DbSync.Era.Shelley.Query
import           Cardano.DbSync.Error
import           Cardano.DbSync.Era.Util

import           Database.Persist.Postgresql (SqlBackend)

type StakeAddrCache = Map StakeCred DB.StakeAddressId
type StakePoolCache = Map StakePoolKeyHash DB.PoolHashId

-- The 'UninitiatedCache' makes it possible to call functions in this module
-- without having actually initiated the cache yet. It is used by genesis
-- insertions, where the cache has not been initiated yet.
data Cache = UninitiatedCache | Cache CacheInternal

data CacheNew = CacheNew | DontCacheNew | EvictAndReturn

data CacheInternal = CacheInternal
  { cStakeCreds :: IORef StakeAddrCache
  , cPools :: IORef StakePoolCache
  , cMultiAssets :: IORef (LRUCache (ByteString, AssetName) DB.MultiAssetId)
  , prevBlock :: IORef (Maybe (DB.BlockId, ByteString))
  }

uninitiatedCache :: Cache
uninitiatedCache = UninitiatedCache

newEmptyCache :: MonadIO m => Word64 -> m Cache
newEmptyCache maCapacity = liftIO $ Cache <$>
                             (CacheInternal
                               <$> newIORef Map.empty
                               <*> newIORef Map.empty
                               <*> newIORef (LRU.empty maCapacity)
                               <*> newIORef Nothing)

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
  liftIO $ writeIORef (cPools cache) Map.empty
  liftIO $ modifyIORef' (cMultiAssets cache) LRU.cleanup
  liftIO $ writeIORef (prevBlock cache) Nothing
  rollbackStakeAddr cache mBlockNo nBlocks

rollbackStakeAddr :: MonadIO m => CacheInternal -> Maybe Word64 -> Word64 -> ReaderT SqlBackend m ()
rollbackStakeAddr CacheInternal {cStakeCreds = ref} Nothing _nBlocks =
    liftIO $ writeIORef ref Map.empty
rollbackStakeAddr CacheInternal {cStakeCreds = ref} (Just blockNo) nBlocks = do
    if nBlocks > 600 then
      liftIO $ writeIORef ref Map.empty
    else do
      initMp <- liftIO $ readIORef ref
      stakeAddrIds <- DB.queryStakeAddressIdsAfter blockNo
      let stakeAddrIdsSet = Set.fromList stakeAddrIds
      let !mp = Map.filter (`Set.member` stakeAddrIdsSet) initMp
      liftIO $ writeIORef ref mp

queryStakeAddrWithCache :: forall m. MonadIO m => Cache -> CacheNew -> StakeCred 
                        -> ReaderT SqlBackend m (Either DB.LookupFail DB.StakeAddressId)
queryStakeAddrWithCache UninitiatedCache _cacheNew cred = 
  queryStakeAddress (unStakeCred cred)
queryStakeAddrWithCache (Cache CacheInternal {cStakeCreds = ref}) cacheNew cred = do
    mp <- liftIO $ readIORef ref
    (mAddrId, mp') <- queryStakeAddrAux cacheNew mp cred
    liftIO $ writeIORef ref mp'
    pure mAddrId

queryStakeAddrListWithCache :: forall m. MonadIO m => Cache -> [StakeCred]
                            -> ExceptT SyncNodeError (ReaderT SqlBackend m) [(StakeCred, DB.StakeAddressId)]
queryStakeAddrListWithCache cache hashes = do
    initMp <- liftIO $ readCachedState cache
    (mp', ret) <- foldM f (initMp, []) hashes
    liftIO $ writeCachedState cache mp'
    pure $ reverse ret
  where
    f :: (StakeAddrCache, [(StakeCred, DB.StakeAddressId)]) -> StakeCred
      -> ExceptT SyncNodeError (ReaderT SqlBackend m) (StakeAddrCache, [(StakeCred, DB.StakeAddressId)])
    f (mp, acc) stakeCred = do
      (eiAddrId, mp') <- lift $ queryStakeAddrAux CacheNew mp stakeCred
      addrId <- liftLookupFail "StakeAddressId" $ pure eiAddrId
      pure (mp', (stakeCred, addrId) : acc)

    readCachedState :: Cache -> IO StakeAddrCache
    readCachedState UninitiatedCache = pure Map.empty
    readCachedState (Cache CacheInternal {cStakeCreds = ref}) =
      readIORef ref

    writeCachedState :: Cache -> StakeAddrCache -> IO ()
    writeCachedState UninitiatedCache _ = pure ()
    writeCachedState (Cache CacheInternal {cStakeCreds = ref}) mp =
      writeIORef ref mp

queryStakeAddrAux :: MonadIO m => CacheNew -> StakeAddrCache -> StakeCred
                  -> ReaderT SqlBackend m (Either DB.LookupFail DB.StakeAddressId, StakeAddrCache)
queryStakeAddrAux cacheNew mp hsh =
    case Map.lookup hsh mp of
      Just phId -> pure (Right phId, mp)
      Nothing -> do
        mAddrId <- queryStakeAddress (unStakeCred hsh)
        case (mAddrId, cacheNew) of
          (Right addrId, CacheNew) -> pure (Right addrId, Map.insert hsh addrId mp)
          (Right addrId, DontCacheNew) -> pure (Right addrId, mp)
          (Right addrId, EvictAndReturn) -> pure (Right addrId, Map.delete hsh mp)
          (err, _) -> pure (err, mp)

queryPoolKeyWithCache :: MonadIO m => Cache -> CacheNew -> StakePoolKeyHash
                      -> ReaderT SqlBackend m (Either DB.LookupFail DB.PoolHashId)
queryPoolKeyWithCache cache cacheNew hsh = do
    mp <- liftIO $ readCachedState cache
    case Map.lookup hsh mp of
      Just phId -> pure $ Right phId
      Nothing -> do
        mPhId <- queryPoolHashId (unStakePoolKeyHash hsh)
        case mPhId of
          Nothing -> pure $ Left (DB.DbLookupMessage "StakePoolKeyHash")
          Just phId -> do
            case cacheNew of
              CacheNew ->
                liftIO $ modifyCachedState cache $ Map.insert hsh phId
              DontCacheNew -> pure ()
              EvictAndReturn ->
                liftIO $ modifyCachedState cache $ Map.delete hsh
            pure $ Right phId
  where
    readCachedState :: Cache -> IO StakePoolCache
    readCachedState UninitiatedCache = pure Map.empty
    readCachedState (Cache CacheInternal {cPools = ref}) =
      readIORef ref

    modifyCachedState :: Cache -> (StakePoolCache -> StakePoolCache) -> IO ()
    modifyCachedState UninitiatedCache _ = pure ()
    modifyCachedState (Cache CacheInternal {cPools = ref}) f =
      modifyIORef' ref f

queryMAWithCache :: MonadIO m => Cache -> ByteString -> AssetName
                 -> ReaderT SqlBackend m (Maybe DB.MultiAssetId)
queryMAWithCache UninitiatedCache policyId (AssetName aName) =
    DB.queryMultiAssetId policyId aName
queryMAWithCache (Cache CacheInternal {cMultiAssets = ref}) policyId a@(AssetName aName) = do
    mp <- liftIO $ readIORef ref
    case LRU.lookup (policyId, a) mp of
      Just (maId, mp') -> do
        -- hit
        liftIO $ writeIORef ref mp'
        pure $ Just maId
      Nothing -> do
        -- miss. The lookup doesn't change the cache on a miss.
        maId <- DB.queryMultiAssetId policyId aName
        case maId of
          Nothing -> do
            pure Nothing
          Just mId -> do
            liftIO $ modifyIORef' ref $ LRU.insert (policyId, a) mId
            pure maId

queryPrevBlockWithCache :: MonadIO m => Text -> Cache -> ByteString
                        -> ExceptT SyncNodeError (ReaderT SqlBackend m) DB.BlockId
queryPrevBlockWithCache msg UninitiatedCache hsh =
    liftLookupFail msg $ DB.queryBlockId hsh
queryPrevBlockWithCache msg (Cache CacheInternal {prevBlock = ref}) hsh = do
    mCachedPrev <- liftIO $ readIORef ref
    case mCachedPrev of
      -- if the cached block matches the requested hash, we return its db id.
      Just (cachedBlockId, cachedHash) | cachedHash == hsh -> pure cachedBlockId
      -- else we query it from the db.
      _ -> liftLookupFail msg $ DB.queryBlockId hsh

insertBlockAndCache :: (MonadIO m, MonadBaseControl IO m) => Cache -> DB.Block -> ReaderT SqlBackend m DB.BlockId
insertBlockAndCache UninitiatedCache block =
    DB.insertBlock block
insertBlockAndCache (Cache CacheInternal {prevBlock = ref}) block = do
    bid <- DB.insertBlock block
    liftIO $ writeIORef ref $ Just (bid, DB.blockHash block)
    pure bid
