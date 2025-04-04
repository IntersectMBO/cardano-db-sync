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
  insertAddressUsingCache,
  queryTxIdWithCache,
  rollbackCache,
  optimiseCaches,
  tryUpdateCacheTx,

  -- * CacheStatistics
  getCacheStatistics,
  module X,
) where

import Cardano.BM.Trace
import qualified Cardano.Db as DB
import qualified Cardano.Db.Schema.Variant.TxOut as V
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types
import Cardano.DbSync.Cache.Epoch (rollbackMapEpochInCache)
import qualified Cardano.DbSync.Cache.FIFO as FIFO
import qualified Cardano.DbSync.Cache.LRU as LRU
import Cardano.DbSync.Cache.Stake as X
import Cardano.DbSync.Cache.Types (CacheAction (..), CacheInternal (..), CacheStatistics (..), CacheStatus (..), StakeCache (..), initCacheStatistics, shouldCache)
import Cardano.DbSync.Cache.Util
import qualified Cardano.DbSync.Era.Shelley.Generic.Util as Generic
import Cardano.DbSync.Era.Util
import Cardano.DbSync.Error
import Cardano.DbSync.Types
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
    atomically $ modifyTVar (cTxIds cache) FIFO.cleanupCache
    void $ rollbackMapEpochInCache cache blockId

-- | When syncing and we get within 2 minutes of the tip, we can optimise the caches
-- and set the flag to True on ActiveCache.leaving the following caches as they are:
-- cPools, cPrevBlock, Cstats, cEpoch
optimiseCaches :: MonadIO m => CacheStatus -> ReaderT SqlBackend m ()
optimiseCaches cache =
  case cache of
    NoCache -> pure ()
    ActiveCache c ->
      withCacheOptimisationCheck c (pure ()) $
        liftIO $ do
          -- empty caches not to be used anymore
          atomically $ modifyTVar (cTxIds c) FIFO.cleanupCache
          atomically $ writeTVar (cStake c) (StakeCache Map.empty (LRU.empty 0))
          atomically $ modifyTVar (cDatum c) (LRU.optimise 0)
          -- empty then limit the capacity of the cache
          atomically $ writeTVar (cMultiAssets c) (LRU.empty 50000)
          -- set the flag to True
          atomically $ writeTVar (cIsCacheOptimised c) True
          pure ()

getCacheStatistics :: CacheStatus -> IO CacheStatistics
getCacheStatistics cs =
  case cs of
    NoCache -> pure initCacheStatistics
    ActiveCache ci -> readTVarIO (cStats ci)

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
              when (shouldCache cacheUA) $
                liftIO $
                  atomically $
                    modifyTVar (cPools ci) $
                      Map.insert hsh phId
              pure $ Right phId

insertAddressUsingCache ::
  (MonadBaseControl IO m, MonadIO m) =>
  CacheStatus ->
  CacheAction ->
  ByteString ->
  V.Address ->
  ReaderT SqlBackend m V.AddressId
insertAddressUsingCache cache cacheUA addrRaw vAdrs = do
  case cache of
    NoCache -> do
      -- Directly query the database for the address ID when no caching is active.
      mAddrId <- DB.queryAddressId addrRaw
      processResult mAddrId
    ActiveCache ci -> do
      -- Use active cache to attempt fetching the address ID from the cache.
      adrs <- liftIO $ readTVarIO (cAddress ci)
      case LRU.lookup addrRaw adrs of
        Just (addrId, adrs') -> do
          -- If found in cache, record a cache hit and update the cache state.
          liftIO $ hitAddress (cStats ci)
          liftIO $ atomically $ writeTVar (cAddress ci) adrs'
          pure addrId
        Nothing -> do
          -- If not found in cache, log a miss, and query the database.
          liftIO $ missAddress (cStats ci)
          mAddrId <- DB.queryAddressId addrRaw
          processWithCache mAddrId ci
  where
    processResult mAddrId =
      case mAddrId of
        -- If address ID isn't found in the database, insert it.
        Nothing -> DB.insertAddress vAdrs
        -- Return the found address ID.
        Just addrId -> pure addrId

    processWithCache mAddrId ci =
      case mAddrId of
        -- If address ID isn't found, insert and possibly cache it.
        Nothing -> do
          addrId <- DB.insertAddress vAdrs
          cacheIfNeeded addrId ci
          pure addrId
        -- If found, optionally cache it.
        Just addrId -> do
          cacheIfNeeded addrId ci
          pure addrId

    cacheIfNeeded addrId ci =
      -- Cache the address ID if the caching action specifies it should be cached.
      when (shouldCache cacheUA) $
        liftIO $
          atomically $
            modifyTVar (cAddress ci) $
              LRU.insert addrRaw addrId

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
          when (shouldCache cacheUA) $
            liftIO $
              atomically $
                modifyTVar (cPools ci) $
                  Map.insert pHash phId
          pure phId

queryPoolKeyOrInsert ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Text ->
  CacheAction ->
  Bool ->
  PoolKeyHash ->
  ReaderT SqlBackend m DB.PoolHashId
queryPoolKeyOrInsert syncEnv txt cacheUA logsWarning hsh = do
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
  where
    trce = getTrace syncEnv
    cache = envCache syncEnv

queryMAWithCache ::
  MonadIO m =>
  CacheStatus ->
  PolicyID StandardCrypto ->
  AssetName ->
  ReaderT SqlBackend m (Either (ByteString, ByteString) DB.MultiAssetId)
queryMAWithCache cache policyId asset =
  case cache of
    NoCache -> queryDb
    ActiveCache ci -> do
      withCacheOptimisationCheck ci queryDb $ do
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
  where
    queryDb = do
      let !policyBs = Generic.unScriptHash $ policyID policyId
      let !assetNameBs = Generic.unAssetName asset
      maybe (Left (policyBs, assetNameBs)) Right <$> DB.queryMultiAssetId policyBs assetNameBs

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

queryTxIdWithCache ::
  MonadIO m =>
  CacheStatus ->
  TxIdLedger ->
  ReaderT SqlBackend m (Either DB.LookupFail DB.TxId)
queryTxIdWithCache cache txIdLedger = do
  case cache of
    -- Direct database query if no cache.
    NoCache -> qTxHash
    ActiveCache ci ->
      withCacheOptimisationCheck ci qTxHash $ do
        -- Read current cache state.
        cacheTx <- liftIO $ readTVarIO (cTxIds ci)

        case FIFO.lookup txIdLedger cacheTx of
          -- Cache hit, return the transaction ID.
          Just txId -> do
            liftIO $ hitTxIds (cStats ci)
            pure $ Right txId
          -- Cache miss.
          Nothing -> do
            eTxId <- qTxHash
            liftIO $ missTxIds (cStats ci)
            case eTxId of
              Right txId -> do
                -- Update cache.
                liftIO $ atomically $ modifyTVar (cTxIds ci) $ FIFO.insert txIdLedger txId
                -- Return ID after updating cache.
                pure $ Right txId
              -- Return lookup failure.
              Left _ -> pure $ Left $ DB.DbLookupTxHash txHash
  where
    txHash = Generic.unTxHash txIdLedger
    qTxHash = DB.queryTxId txHash

tryUpdateCacheTx ::
  MonadIO m =>
  CacheStatus ->
  TxIdLedger ->
  DB.TxId ->
  m ()
tryUpdateCacheTx (ActiveCache ci) ledgerTxId txId =
  liftIO $ atomically $ modifyTVar (cTxIds ci) $ FIFO.insert ledgerTxId txId
tryUpdateCacheTx _ _ _ = pure ()

insertBlockAndCache ::
  (MonadIO m, MonadBaseControl IO m) =>
  CacheStatus ->
  DB.BlockId ->
  DB.Block ->
  ReaderT SqlBackend m ()
insertBlockAndCache cache k block =
  case cache of
    NoCache -> insBlck
    ActiveCache ci ->
      withCacheOptimisationCheck ci insBlck $ do
        insBlck
        liftIO $
          atomically $ writeTVar (cPrevBlock ci) $ Just (k, DB.blockHash block)
  where
    insBlck = DB.insertBlock k block

queryDatum ::
  MonadIO m =>
  CacheStatus ->
  DataHash ->
  ReaderT SqlBackend m (Maybe DB.DatumId)
queryDatum cache hsh = do
  case cache of
    NoCache -> queryDtm
    ActiveCache ci -> do
      withCacheOptimisationCheck ci queryDtm $ do
        mp <- liftIO $ readTVarIO (cDatum ci)
        case LRU.lookup hsh mp of
          Just (datumId, mp') -> do
            liftIO $ hitDatum (cStats ci)
            liftIO $ atomically $ writeTVar (cDatum ci) mp'
            pure $ Just datumId
          Nothing -> do
            liftIO $ missDatum (cStats ci)
            -- miss. The lookup doesn't change the cache on a miss.
            queryDtm
  where
    queryDtm = DB.queryDatum $ Generic.dataHashToBytes hsh

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
    ActiveCache ci ->
      withCacheOptimisationCheck ci (pure datumId) $ do
        liftIO $
          atomically $
            modifyTVar (cDatum ci) $
              LRU.insert hsh datumId
        pure datumId

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

-- Address
hitAddress :: StrictTVar IO CacheStatistics -> IO ()
hitAddress ref =
  atomically $ modifyTVar ref (\cs -> cs {addressHits = 1 + addressHits cs, addressQueries = 1 + addressQueries cs})

missAddress :: StrictTVar IO CacheStatistics -> IO ()
missAddress ref =
  atomically $ modifyTVar ref (\cs -> cs {addressQueries = 1 + addressQueries cs})

-- Blocks
hitPBlock :: StrictTVar IO CacheStatistics -> IO ()
hitPBlock ref =
  atomically $ modifyTVar ref (\cs -> cs {prevBlockHits = 1 + prevBlockHits cs, prevBlockQueries = 1 + prevBlockQueries cs})

missPrevBlock :: StrictTVar IO CacheStatistics -> IO ()
missPrevBlock ref =
  atomically $ modifyTVar ref (\cs -> cs {prevBlockQueries = 1 + prevBlockQueries cs})

-- TxIds
hitTxIds :: StrictTVar IO CacheStatistics -> IO ()
hitTxIds ref =
  atomically $ modifyTVar ref (\cs -> cs {txIdsHits = 1 + txIdsHits cs, txIdsQueries = 1 + txIdsQueries cs})

missTxIds :: StrictTVar IO CacheStatistics -> IO ()
missTxIds ref =
  atomically $ modifyTVar ref (\cs -> cs {txIdsQueries = 1 + txIdsQueries cs})
