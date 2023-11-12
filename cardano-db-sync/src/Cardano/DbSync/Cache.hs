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
import Cardano.DbSync.Cache.Types (Cache (..), CacheInternal (..), CacheNew (..), CacheStatistics (..), StakeAddrCache, initCacheStatistics)
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
rollbackCache :: MonadIO m => Cache -> DB.BlockId -> ReaderT SqlBackend m ()
rollbackCache UninitiatedCache _ = pure ()
rollbackCache (Cache cache) blockId = do
  liftIO $ do
    atomically $ writeTVar (cPrevBlock cache) Nothing
    atomically $ modifyTVar (cDatum cache) LRU.cleanup
    void $ rollbackMapEpochInCache cache blockId

getCacheStatistics :: Cache -> IO CacheStatistics
getCacheStatistics cs =
  case cs of
    UninitiatedCache -> pure initCacheStatistics
    Cache ci -> readTVarIO (cStats ci)

queryOrInsertRewardAccount ::
  (MonadBaseControl IO m, MonadIO m) =>
  Cache ->
  CacheNew ->
  Ledger.RewardAcnt StandardCrypto ->
  ReaderT SqlBackend m DB.StakeAddressId
queryOrInsertRewardAccount cache cacheNew rewardAddr = do
  eiAddrId <- queryRewardAccountWithCacheRetBs cache cacheNew rewardAddr
  case eiAddrId of
    Left (_err, bs) -> insertStakeAddress rewardAddr (Just bs)
    Right addrId -> pure addrId

queryOrInsertStakeAddress ::
  (MonadBaseControl IO m, MonadIO m) =>
  Cache ->
  CacheNew ->
  Network ->
  StakeCred ->
  ReaderT SqlBackend m DB.StakeAddressId
queryOrInsertStakeAddress cache cacheNew nw cred =
  queryOrInsertRewardAccount cache cacheNew $ Ledger.RewardAcnt nw cred

-- If the address already exists in the table, it will not be inserted again (due to
-- the uniqueness constraint) but the function will return the 'StakeAddressId'.
insertStakeAddress ::
  (MonadBaseControl IO m, MonadIO m) =>
  Ledger.RewardAcnt StandardCrypto ->
  Maybe ByteString ->
  ReaderT SqlBackend m DB.StakeAddressId
insertStakeAddress rewardAddr stakeCredBs =
  DB.insertStakeAddress $
    DB.StakeAddress
      { DB.stakeAddressHashRaw = addrBs
      , DB.stakeAddressView = Generic.renderRewardAcnt rewardAddr
      , DB.stakeAddressScriptHash = Generic.getCredentialScriptHash $ Ledger.getRwdCred rewardAddr
      }
  where
    addrBs = fromMaybe (Ledger.serialiseRewardAcnt rewardAddr) stakeCredBs

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

queryPoolKeyOrInsert ::
  (MonadBaseControl IO m, MonadIO m) =>
  Text ->
  Trace IO Text ->
  Cache ->
  CacheNew ->
  Bool ->
  PoolKeyHash ->
  ReaderT SqlBackend m DB.PoolHashId
queryPoolKeyOrInsert txt trce cache cacheNew logsWarning hsh = do
  pk <- queryPoolKeyWithCache cache cacheNew hsh
  case pk of
    Right poolHashId -> pure poolHashId
    Left err -> do
      when logsWarning $
        liftIO $
          logWarning trce $
            mconcat
              [ "Failed with "
              , DB.textShow err
              , " while trying to find pool "
              , DB.textShow hsh
              , " for "
              , txt
              , ". We will assume that the pool exists and move on."
              ]
      insertPoolKeyWithCache cache cacheNew hsh

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
