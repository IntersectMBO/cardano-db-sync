{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Cache (
  insertBlockAndCache,
  insertDatumAndCache,
  insertPoolKeyWithCache,
  queryDatum,
  queryMAWithCache,
  queryPoolKeyOrInsert,
  queryPoolKeyWithCache,
  queryPrevBlockWithCache,
  queryOrInsertStakeAddress,
  queryOrInsertRewardAccount,
  insertStakeAddress,
  queryStakeAddrWithCache,
  rollbackCache,

  -- * CacheStatistics
  getCacheStatistics,
) where

import Cardano.BM.Trace
import qualified Cardano.Db as DB
import Cardano.DbSync.Cache.Epoch (rollbackMapEpochInCache)
import qualified Cardano.DbSync.Cache.LRU as LRU
import Cardano.DbSync.Cache.Types (CacheAction (..), CacheInternal (..), CacheStatistics (..), CacheStatus (..), StakeCache (..), initCacheStatistics)
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
  readTVarIO,
  writeTVar,
 )
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Either.Combinators
import qualified Data.Map.Strict as Map
import Database.Persist.Postgresql (SqlBackend)
import Ouroboros.Consensus.Cardano.Block (StandardCrypto)

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
rollbackCache :: MonadIO m => CacheStatus -> DB.BlockId -> ReaderT SqlBackend m ()
rollbackCache NoCache _ = pure ()
rollbackCache (ActiveCache cache) blockId = do
  liftIO $ do
    atomically $ writeTVar (cPrevBlock cache) Nothing
    atomically $ modifyTVar (cDatum cache) LRU.cleanup
    void $ rollbackMapEpochInCache cache blockId

getCacheStatistics :: CacheStatus -> IO CacheStatistics
getCacheStatistics cs =
  case cs of
    NoCache -> pure initCacheStatistics
    ActiveCache ci -> readTVarIO (cStats ci)

queryOrInsertRewardAccount ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  CacheStatus ->
  CacheAction ->
  Ledger.RewardAccount StandardCrypto ->
  ReaderT SqlBackend m DB.StakeAddressId
queryOrInsertRewardAccount trce cache cacheUA rewardAddr = do
  eiAddrId <- queryRewardAccountWithCacheRetBs trce cache cacheUA rewardAddr
  case eiAddrId of
    Left (_err, bs) -> insertStakeAddress rewardAddr (Just bs)
    Right addrId -> pure addrId

queryOrInsertStakeAddress ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  CacheStatus ->
  CacheAction ->
  Network ->
  StakeCred ->
  ReaderT SqlBackend m DB.StakeAddressId
queryOrInsertStakeAddress trce cache cacheUA nw cred =
  queryOrInsertRewardAccount trce cache cacheUA $ Ledger.RewardAccount nw cred

-- If the address already exists in the table, it will not be inserted again (due to
-- the uniqueness constraint) but the function will return the 'StakeAddressId'.
insertStakeAddress ::
  (MonadBaseControl IO m, MonadIO m) =>
  Ledger.RewardAccount StandardCrypto ->
  Maybe ByteString ->
  ReaderT SqlBackend m DB.StakeAddressId
insertStakeAddress rewardAddr stakeCredBs = do
  DB.insertStakeAddress $
    DB.StakeAddress
      { DB.stakeAddressHashRaw = addrBs
      , DB.stakeAddressView = Generic.renderRewardAccount rewardAddr
      , DB.stakeAddressScriptHash = Generic.getCredentialScriptHash $ Ledger.raCredential rewardAddr
      }
  where
    addrBs = fromMaybe (Ledger.serialiseRewardAccount rewardAddr) stakeCredBs

queryRewardAccountWithCacheRetBs ::
  forall m.
  MonadIO m =>
  Trace IO Text ->
  CacheStatus ->
  CacheAction ->
  Ledger.RewardAccount StandardCrypto ->
  ReaderT SqlBackend m (Either (DB.LookupFail, ByteString) DB.StakeAddressId)
queryRewardAccountWithCacheRetBs trce cache cacheUA rwdAcc =
  queryStakeAddrWithCacheRetBs trce cache cacheUA (Ledger.raNetwork rwdAcc) (Ledger.raCredential rwdAcc)

queryStakeAddrWithCache ::
  forall m.
  MonadIO m =>
  Trace IO Text ->
  CacheStatus ->
  CacheAction ->
  Network ->
  StakeCred ->
  ReaderT SqlBackend m (Either DB.LookupFail DB.StakeAddressId)
queryStakeAddrWithCache trce cache cacheUA nw cred =
  mapLeft fst <$> queryStakeAddrWithCacheRetBs trce cache cacheUA nw cred

queryStakeAddrWithCacheRetBs ::
  forall m.
  MonadIO m =>
  Trace IO Text ->
  CacheStatus ->
  CacheAction ->
  Network ->
  StakeCred ->
  ReaderT SqlBackend m (Either (DB.LookupFail, ByteString) DB.StakeAddressId)
queryStakeAddrWithCacheRetBs _trce cache cacheUA nw cred = do
  let bs = Ledger.serialiseRewardAccount (Ledger.RewardAccount nw cred)
  case cache of
    NoCache -> do
      mapLeft (,bs) <$> resolveStakeAddress bs
    ActiveCache ci -> do
      stakeCache <- liftIO $ readTVarIO (cStakeRawHashes ci)
      case queryStakeCache cred stakeCache of
        Just (addrId, stakeCache', _) -> do
          liftIO $ hitCreds (cStats ci)
          case cacheUA of
            EvictAndUpdateCache -> do
              liftIO $ atomically $ writeTVar (cStakeRawHashes ci) $ deleteStakeCache cred stakeCache'
              pure $ Right addrId
            _other -> do
              liftIO $ atomically $ writeTVar (cStakeRawHashes ci) stakeCache'
              pure $ Right addrId
        Nothing -> do
          queryRes <- mapLeft (,bs) <$> resolveStakeAddress bs
          liftIO $ missCreds (cStats ci)
          case queryRes of
            Left _ -> pure queryRes
            Right stakeAddrsId -> do
              let !stakeCache' = case cacheUA of
                    UpdateCache -> stakeCache {scLruCache = LRU.insert cred stakeAddrsId (scLruCache stakeCache)}
                    UpdateCacheStrong -> stakeCache {scStableCache = Map.insert cred stakeAddrsId (scStableCache stakeCache)}
                    _ -> stakeCache
              liftIO $
                atomically $
                  writeTVar (cStakeRawHashes ci) stakeCache'
              pure $ Right stakeAddrsId

-- | True if it was found in LRU
queryStakeCache :: StakeCred -> StakeCache -> Maybe (DB.StakeAddressId, StakeCache, Bool)
queryStakeCache scred scache = case Map.lookup scred (scStableCache scache) of
  Just addrId -> Just (addrId, scache, False)
  Nothing -> case LRU.lookup scred (scLruCache scache) of
    Just (addrId, lru') -> Just (addrId, scache {scLruCache = lru'}, True)
    Nothing -> Nothing

deleteStakeCache :: StakeCred -> StakeCache -> StakeCache
deleteStakeCache scred scache =
  scache {scStableCache = Map.delete scred (scStableCache scache)}

queryPoolKeyWithCache ::
  MonadIO m =>
  CacheStatus ->
  CacheAction ->
  PoolKeyHash ->
  ReaderT SqlBackend m (Either DB.LookupFail DB.PoolHashId)
queryPoolKeyWithCache cache cacheUA hsh =
  case cache of
    NoCache -> do
      mPhId <- DB.queryPoolHashId (Generic.unKeyHashRaw hsh)
      case mPhId of
        Nothing -> pure $ Left (DB.DbLookupMessage "PoolKeyHash")
        Just phId -> pure $ Right phId
    ActiveCache ci -> do
      mp <- liftIO $ readTVarIO (cPools ci)
      case Map.lookup hsh mp of
        Just phId -> do
          liftIO $ hitPools (cStats ci)
          -- hit so we can't cache even with 'CacheNew'
          when (cacheUA == EvictAndUpdateCache) $
            liftIO $
              atomically $
                modifyTVar (cPools ci) $
                  Map.delete hsh
          pure $ Right phId
        Nothing -> do
          liftIO $ missPools (cStats ci)
          mPhId <- DB.queryPoolHashId (Generic.unKeyHashRaw hsh)
          case mPhId of
            Nothing -> pure $ Left (DB.DbLookupMessage "PoolKeyHash")
            Just phId -> do
              -- missed so we can't evict even with 'EvictAndReturn'
              when (cacheUA == UpdateCache) $
                liftIO $
                  atomically $
                    modifyTVar (cPools ci) $
                      Map.insert hsh phId
              pure $ Right phId

insertPoolKeyWithCache ::
  (MonadBaseControl IO m, MonadIO m) =>
  CacheStatus ->
  CacheAction ->
  PoolKeyHash ->
  ReaderT SqlBackend m DB.PoolHashId
insertPoolKeyWithCache cache cacheUA pHash =
  case cache of
    NoCache ->
      DB.insertPoolHash $
        DB.PoolHash
          { DB.poolHashHashRaw = Generic.unKeyHashRaw pHash
          , DB.poolHashView = Generic.unKeyHashView pHash
          }
    ActiveCache ci -> do
      mp <- liftIO $ readTVarIO (cPools ci)
      case Map.lookup pHash mp of
        Just phId -> do
          liftIO $ hitPools (cStats ci)
          when (cacheUA == EvictAndUpdateCache) $
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
          when (cacheUA == UpdateCache) $
            liftIO $
              atomically $
                modifyTVar (cPools ci) $
                  Map.insert pHash phId
          pure phId

queryPoolKeyOrInsert ::
  (MonadBaseControl IO m, MonadIO m) =>
  Text ->
  Trace IO Text ->
  CacheStatus ->
  CacheAction ->
  Bool ->
  PoolKeyHash ->
  ReaderT SqlBackend m DB.PoolHashId
queryPoolKeyOrInsert txt trce cache cacheUA logsWarning hsh = do
  pk <- queryPoolKeyWithCache cache cacheUA hsh
  case pk of
    Right poolHashId -> pure poolHashId
    Left err -> do
      when logsWarning $
        liftIO $
          logWarning trce $
            mconcat
              [ "Failed with "
              , textShow err
              , " while trying to find pool "
              , textShow hsh
              , " for "
              , txt
              , ". We will assume that the pool exists and move on."
              ]
      insertPoolKeyWithCache cache cacheUA hsh

queryMAWithCache ::
  MonadIO m =>
  CacheStatus ->
  PolicyID StandardCrypto ->
  AssetName ->
  ReaderT SqlBackend m (Either (ByteString, ByteString) DB.MultiAssetId)
queryMAWithCache cache policyId asset =
  case cache of
    NoCache -> do
      let !policyBs = Generic.unScriptHash $ policyID policyId
      let !assetNameBs = Generic.unAssetName asset
      maybe (Left (policyBs, assetNameBs)) Right <$> DB.queryMultiAssetId policyBs assetNameBs
    ActiveCache ci -> do
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
  CacheStatus ->
  ByteString ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) DB.BlockId
queryPrevBlockWithCache msg cache hsh =
  case cache of
    NoCache -> liftLookupFail msg $ DB.queryBlockId hsh
    ActiveCache ci -> do
      mCachedPrev <- liftIO $ readTVarIO (cPrevBlock ci)
      case mCachedPrev of
        -- if the cached block matches the requested hash, we return its db id.
        Just (cachedBlockId, cachedHash) ->
          if cachedHash == hsh
            then do
              liftIO $ hitPBlock (cStats ci)
              pure cachedBlockId
            else queryFromDb ci
        Nothing -> queryFromDb ci
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
  CacheStatus ->
  DB.Block ->
  ReaderT SqlBackend m DB.BlockId
insertBlockAndCache cache block =
  case cache of
    NoCache -> DB.insertBlock block
    ActiveCache ci -> do
      bid <- DB.insertBlock block
      liftIO $ do
        missPrevBlock (cStats ci)
        atomically $ writeTVar (cPrevBlock ci) $ Just (bid, DB.blockHash block)
      pure bid

queryDatum ::
  MonadIO m =>
  CacheStatus ->
  DataHash ->
  ReaderT SqlBackend m (Maybe DB.DatumId)
queryDatum cache hsh = do
  case cache of
    NoCache -> DB.queryDatum $ Generic.dataHashToBytes hsh
    ActiveCache ci -> do
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
  CacheStatus ->
  DataHash ->
  DB.Datum ->
  ReaderT SqlBackend m DB.DatumId
insertDatumAndCache cache hsh dt = do
  datumId <- DB.insertDatum dt
  case cache of
    NoCache -> pure datumId
    ActiveCache ci -> do
      liftIO $
        atomically $
          modifyTVar (cDatum ci) $
            LRU.insert hsh datumId
      pure datumId

-- Stakes
hitCreds :: StrictTVar IO CacheStatistics -> IO ()
hitCreds ref =
  atomically $ modifyTVar ref (\cs -> cs {credsHits = 1 + credsHits cs, credsQueries = 1 + credsQueries cs})

missCreds :: StrictTVar IO CacheStatistics -> IO ()
missCreds ref =
  atomically $ modifyTVar ref (\cs -> cs {credsQueries = 1 + credsQueries cs})

-- Pools
hitPools :: StrictTVar IO CacheStatistics -> IO ()
hitPools ref =
  atomically $ modifyTVar ref (\cs -> cs {poolsHits = 1 + poolsHits cs, poolsQueries = 1 + poolsQueries cs})

missPools :: StrictTVar IO CacheStatistics -> IO ()
missPools ref =
  atomically $ modifyTVar ref (\cs -> cs {poolsQueries = 1 + poolsQueries cs})

-- Datum
hitDatum :: StrictTVar IO CacheStatistics -> IO ()
hitDatum ref =
  atomically $ modifyTVar ref (\cs -> cs {datumHits = 1 + datumHits cs, datumQueries = 1 + datumQueries cs})

missDatum :: StrictTVar IO CacheStatistics -> IO ()
missDatum ref =
  atomically $ modifyTVar ref (\cs -> cs {datumQueries = 1 + datumQueries cs})

-- Assets
hitMAssets :: StrictTVar IO CacheStatistics -> IO ()
hitMAssets ref =
  atomically $ modifyTVar ref (\cs -> cs {multiAssetsHits = 1 + multiAssetsHits cs, multiAssetsQueries = 1 + multiAssetsQueries cs})

missMAssets :: StrictTVar IO CacheStatistics -> IO ()
missMAssets ref =
  atomically $ modifyTVar ref (\cs -> cs {multiAssetsQueries = 1 + multiAssetsQueries cs})

-- Blocks
hitPBlock :: StrictTVar IO CacheStatistics -> IO ()
hitPBlock ref =
  atomically $ modifyTVar ref (\cs -> cs {prevBlockHits = 1 + prevBlockHits cs, prevBlockQueries = 1 + prevBlockQueries cs})

missPrevBlock :: StrictTVar IO CacheStatistics -> IO ()
missPrevBlock ref =
  atomically $ modifyTVar ref (\cs -> cs {prevBlockQueries = 1 + prevBlockQueries cs})
