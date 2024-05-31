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
import Cardano.DbSync.AppT (App, MonadAppDB (..), SyncEnv (..), askNetwork, askTrace)
import Cardano.DbSync.Cache.Epoch (rollbackMapEpochInCache)
import qualified Cardano.DbSync.Cache.LRU as LRU
import Cardano.DbSync.Cache.Types (CacheInternal (..), CacheStatistics (..), CacheStatus (..), UpdateCache (..), StakeAddrCache, initCacheStatistics)
import qualified Cardano.DbSync.Era.Shelley.Generic.Util as Generic
import Cardano.DbSync.Era.Shelley.Query
import Cardano.DbSync.Era.Util
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
import Data.Either.Combinators
import qualified Data.Map.Strict as Map
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
rollbackCache :: MonadIO m => CacheStatus -> DB.BlockId -> App ()
rollbackCache NoCache _ = pure ()
rollbackCache (ActiveCache cache) blockId = do
  liftIO $ do
    atomically $ writeTVar (cPrevBlock cache) Nothing
    atomically $ modifyTVar (cDatum cache) LRU.cleanup
  void $ rollbackMapEpochInCache cache blockId

getCacheStatistics :: CacheStatus -> App CacheStatistics
getCacheStatistics cs =
  case cs of
    NoCache -> pure initCacheStatistics
    ActiveCache ci -> readTVarIO (cStats ci)

queryOrInsertRewardAccount ::
  (MonadBaseControl IO m, MonadIO m) =>
  CacheStatus ->
  UpdateCache ->
  Ledger.RewardAccount StandardCrypto ->
  App DB.StakeAddressId
queryOrInsertRewardAccount cache cacheUA rewardAddr = do
  eiAddrId <- queryRewardAccountWithCacheRetBs cache cacheUA rewardAddr
  case eiAddrId of
    Left (_err, bs) -> insertStakeAddress rewardAddr (Just bs)
    Right addrId -> pure addrId

queryOrInsertStakeAddress ::
  CacheStatus ->
  UpdateCache ->
  Network ->
  StakeCred ->
  App DB.StakeAddressId
queryOrInsertStakeAddress cache cacheUA network cred =
  queryOrInsertRewardAccount cache cacheUA $ Ledger.RewardAccount network cred

-- If the address already exists in the table, it will not be inserted again (due to
-- the uniqueness constraint) but the function will return the 'StakeAddressId'.
insertStakeAddress ::
  Ledger.RewardAccount StandardCrypto ->
  Maybe ByteString ->
  App DB.StakeAddressId
insertStakeAddress rewardAddr stakeCredBs =
  dbQueryToApp $
    DB.insertStakeAddress $
      DB.StakeAddress
        { DB.stakeAddressHashRaw = addrBs
        , DB.stakeAddressView = Generic.renderRewardAccount rewardAddr
        , DB.stakeAddressScriptHash = Generic.getCredentialScriptHash $ Ledger.raCredential rewardAddr
        }
  where
    addrBs = fromMaybe (Ledger.serialiseRewardAccount rewardAddr) stakeCredBs

queryRewardAccountWithCacheRetBs ::
  CacheStatus ->
  UpdateCache ->
  Ledger.RewardAccount StandardCrypto ->
  App (Either (DB.LookupFail, ByteString) DB.StakeAddressId)
queryRewardAccountWithCacheRetBs cache cacheUA rwdAcc =
  queryStakeAddrWithCacheRetBs cache cacheUA (Ledger.raNetwork rwdAcc) (Ledger.raCredential rwdAcc)

queryStakeAddrWithCache ::
  CacheStatus ->
  UpdateCache ->
  StakeCred ->
  App (Either DB.LookupFail DB.StakeAddressId)
queryStakeAddrWithCache cache cacheUA cred = do
  network <- askNetwork
  mapLeft fst <$> queryStakeAddrWithCacheRetBs cache cacheUA network cred

queryStakeAddrWithCacheRetBs ::
  CacheStatus ->
  UpdateCache ->
  Network ->
  StakeCred ->
  App (Either (DB.LookupFail, ByteString) DB.StakeAddressId)
queryStakeAddrWithCacheRetBs cache cacheUA network cred = do
  case cache of
    NoCache -> do
      let !bs = Ledger.serialiseRewardAccount (Ledger.RewardAccount network cred)
      mapLeft (,bs) <$> queryStakeAddress bs
    ActiveCache ci -> do
      mp <- liftIO $ readTVarIO (cStakeCreds ci)
      (mAddrId, mp') <- queryStakeAddrAux cacheUA mp (cStats ci) network cred
      liftIO $ atomically $ writeTVar (cStakeCreds ci) mp'
      pure mAddrId

queryStakeAddrAux ::
  UpdateCache ->
  StakeAddrCache ->
  StrictTVar IO CacheStatistics ->
  Network ->
  StakeCred ->
  App (Either (DB.LookupFail, ByteString) DB.StakeAddressId, StakeAddrCache)
queryStakeAddrAux cacheUA mp sts nw cred =
  case Map.lookup cred mp of
    Just addrId -> do
      liftIO $ hitCreds sts
      case cacheUA of
        EvictAndReturn -> pure (Right addrId, Map.delete cred mp)
        _other -> pure (Right addrId, mp)
    Nothing -> do
      liftIO $ missCreds sts
      let !bs = Ledger.serialiseRewardAccount (Ledger.RewardAccount nw cred)
      -- TODO: CMDV
      mAddrId <- mapLeft (,bs) <$> queryStakeAddress bs
      case (mAddrId, cacheUA) of
        (Right addrId, UpdateCache) -> pure (Right addrId, Map.insert cred addrId mp)
        (Right addrId, _) -> pure (Right addrId, mp)
        (err, _) -> pure (err, mp)

queryPoolKeyWithCache ::
  UpdateCache ->
  PoolKeyHash ->
  App (Either DB.LookupFail DB.PoolHashId)
queryPoolKeyWithCache cacheUA hsh = do
  cache <- asks envCache
  case cache of
    NoCache -> do
      mPhId <- dbQueryToApp $ queryPoolHashId (Generic.unKeyHashRaw hsh)
      case mPhId of
        Nothing -> pure $ Left (DB.DbLookupMessage "PoolKeyHash")
        Just phId -> pure $ Right phId
    ActiveCache ci -> do
      mp <- liftIO $ readTVarIO (cPools ci)
      case Map.lookup hsh mp of
        Just phId -> do
          liftIO $ hitPools (cStats ci)
          -- hit so we can't cache even with 'UpdateCache'
          when (cacheUA == EvictAndUpdateCache) $
            liftIO $
              atomically $
                modifyTVar (cPools ci) $
                  Map.delete hsh
          pure $ Right phId
        Nothing -> do
          liftIO $ missPools (cStats ci)
          mPhId <- dbQueryToApp $ queryPoolHashId (Generic.unKeyHashRaw hsh)
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
  CacheStatus ->
  UpdateCache ->
  PoolKeyHash ->
  App DB.PoolHashId
insertPoolKeyWithCache cache cacheUA pHash = do
  case cache of
    NoCache ->
      dbQueryToApp $
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
            dbQueryToApp $
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
  Text ->
  CacheStatus ->
  UpdateCache ->
  Bool ->
  PoolKeyHash ->
  App DB.PoolHashId
queryPoolKeyOrInsert txt cache cacheUA logsWarning hsh = do
  tracer <- askTrace
  pk <- queryPoolKeyWithCache cacheUA hsh
  case pk of
    Right poolHashId -> pure poolHashId
    Left err -> do
      when logsWarning $
        liftIO $
          logWarning tracer $
            mconcat
              [ "Failed with "
              , DB.textShow err
              , " while trying to find pool "
              , DB.textShow hsh
              , " for "
              , txt
              , ". We will assume that the pool exists and move on."
              ]
      insertPoolKeyWithCache cache cacheUA hsh

queryMAWithCache ::
  PolicyID StandardCrypto ->
  AssetName ->
  App (Either (ByteString, ByteString) DB.MultiAssetId)
queryMAWithCache policyId asset = do
  cache <- asks envCache
  case cache of
    NoCache -> do
      let !policyBs = Generic.unScriptHash $ policyID policyId
      let !assetNameBs = Generic.unAssetName asset
      multiAssetId <- dbQueryToApp $ DB.queryMultiAssetId policyBs assetNameBs
      -- TODO: CMDV
      pure $ maybe (Left (policyBs, assetNameBs)) Right multiAssetId
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
          multiAssetId <- dbQueryToApp $ DB.queryMultiAssetId policyBs assetNameBs
          let maId = maybe (Left (policyBs, assetNameBs)) Right multiAssetId
          whenRight maId $
            liftIO . atomically . modifyTVar (cMultiAssets ci) . LRU.insert (policyId, asset)
          pure maId

queryPrevBlockWithCache ::
  Text ->
  ByteString ->
  App DB.BlockId
queryPrevBlockWithCache msg hsh = do
  cache <- asks envCache
  case cache of
    NoCache -> liftLookupFail msg $ dbQueryToApp $ DB.queryBlockId hsh
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
      CacheInternal ->
      App DB.BlockId
    queryFromDb ci = do
      liftIO $ missPrevBlock (cStats ci)
      liftLookupFail msg $ dbQueryToApp $ DB.queryBlockId hsh

insertBlockAndCache :: DB.Block -> App DB.BlockId
insertBlockAndCache block = do
  cache <- asks envCache
  case cache of
    NoCache -> dbQueryToApp $ DB.insertBlock block
    ActiveCache ci -> do
      bid <- dbQueryToApp $ DB.insertBlock block
      liftIO $ do
        missPrevBlock (cStats ci)
        atomically $ writeTVar (cPrevBlock ci) $ Just (bid, DB.blockHash block)
      pure bid

queryDatum :: DataHash -> App (Maybe DB.DatumId)
queryDatum hsh = do
  cache <- asks envCache
  case cache of
    NoCache -> dbQueryToApp $ DB.queryDatum $ Generic.dataHashToBytes hsh
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
          dbQueryToApp $ DB.queryDatum $ Generic.dataHashToBytes hsh

-- This assumes the entry is not cached.
insertDatumAndCache ::
  DataHash ->
  DB.Datum ->
  App DB.DatumId
insertDatumAndCache hsh dt = do
  cache <- asks envCache
  datumId <- dbQueryToApp $ DB.insertDatum dt
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
