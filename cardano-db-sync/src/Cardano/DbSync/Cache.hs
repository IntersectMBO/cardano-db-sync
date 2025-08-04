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
  insertAddressUsingCache,
  insertStakeAddress,
  queryStakeAddrWithCache,
  queryTxIdWithCache,
  rollbackCache,
  cleanCachesForTip,
  optimiseCaches,
  tryUpdateCacheTx,
) where

import Cardano.BM.Trace
import qualified Cardano.Ledger.Address as Ledger
import Cardano.Ledger.BaseTypes (Network)
import Cardano.Ledger.Mary.Value
import qualified Cardano.Ledger.TxIn as Ledger
import Cardano.Prelude
import Control.Concurrent.Class.MonadSTM.Strict (
  modifyTVar,
  readTVarIO,
  writeTVar,
 )
import Data.Either.Combinators
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import qualified Cardano.Db as DB
import qualified Cardano.Db.Schema.Variants.TxOutAddress as VA
import Cardano.DbSync.Api (getTrace)
import Cardano.DbSync.Api.Types (EpochStatistics (..), SyncEnv (..))
import Cardano.DbSync.Cache.Epoch (rollbackMapEpochInCache)
import qualified Cardano.DbSync.Cache.FIFO as FIFO
import qualified Cardano.DbSync.Cache.LRU as LRU
import Cardano.DbSync.Cache.Types (CacheAction (..), CacheInternal (..), CacheStatistics (..), CacheStatus (..), StakeCache (..), shouldCache)
import Cardano.DbSync.DbEvent (liftFail)
import qualified Cardano.DbSync.Era.Shelley.Generic.Util as Generic
import Cardano.DbSync.Era.Shelley.Query
import Cardano.DbSync.Error (SyncNodeError (..), mkSyncNodeCallStack)
import Cardano.DbSync.Types

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
rollbackCache :: CacheStatus -> DB.BlockId -> ExceptT SyncNodeError DB.DbM ()
rollbackCache NoCache _ = pure ()
rollbackCache (ActiveCache cache) blockId = do
  liftIO $ do
    atomically $ writeTVar (cPrevBlock cache) Nothing
    atomically $ modifyTVar (cDatum cache) LRU.cleanup
    atomically $ modifyTVar (cTxIds cache) FIFO.cleanupCache
    void $ rollbackMapEpochInCache cache blockId

-- | When syncing and we get within 2 minutes of the tip, we clean certain caches
-- and set the flag to True on ActiveCache. We disable the following caches:
-- cStake, cDatum, cAddress. We keep: cPools, cPrevBlock, cMultiAssets, cEpoch, cTxIds
cleanCachesForTip :: CacheStatus -> ExceptT SyncNodeError DB.DbM ()
cleanCachesForTip cache =
  case cache of
    NoCache -> pure ()
    ActiveCache c ->
      withCacheCleanedForTipCheck c (pure ()) $
        liftIO $ do
          -- empty caches not to be used anymore
          atomically $ modifyTVar (cTxIds c) FIFO.cleanupCache
          atomically $ writeTVar (cStake c) (StakeCache Map.empty (LRU.empty 0))
          atomically $ modifyTVar (cDatum c) (LRU.optimise 0)
          -- empty then limit the capacity of the cache
          atomically $ writeTVar (cMultiAssets c) (LRU.empty 50000)
          -- set the flag to True
          atomically $ writeTVar (cIsCacheCleanedForTip c) True
          pure ()

-- | Optimise caches during syncing to prevent unbounded growth.
-- This function trims Map-based caches that can grow without bounds.
-- LRU caches are skipped as they have built-in capacity limits.
optimiseCaches :: CacheStatus -> ExceptT SyncNodeError DB.DbM ()
optimiseCaches cache =
  case cache of
    NoCache -> pure ()
    ActiveCache c -> do
      liftIO $ do
        -- Trim pools Map to target size (keep most recent entries)
        atomically $ modifyTVar (cPools c) $ \poolMap ->
          Map.fromList $ take (fromIntegral $ cOptimisePools c) $ Map.toList poolMap
        
        -- Trim stake stable cache to target size
        atomically $ modifyTVar (cStake c) $ \stakeCache ->
          stakeCache { scStableCache = Map.fromList $ take (fromIntegral $ cOptimiseStake c) $ Map.toList (scStableCache stakeCache) }

queryOrInsertRewardAccount ::
  SyncEnv ->
  CacheAction ->
  Ledger.RewardAccount ->
  ExceptT SyncNodeError DB.DbM DB.StakeAddressId
queryOrInsertRewardAccount syncEnv cacheUA rewardAddr = do
  (eiAddrId, bs) <- queryStakeAddrWithCacheRetBs syncEnv cacheUA rewardAddr
  case eiAddrId of
    Just addrId -> pure addrId
    Nothing -> insertStakeAddress rewardAddr (Just bs)

queryOrInsertStakeAddress ::
  SyncEnv ->
  CacheAction ->
  Network ->
  StakeCred ->
  ExceptT SyncNodeError DB.DbM DB.StakeAddressId
queryOrInsertStakeAddress syncEnv cacheUA nw cred =
  queryOrInsertRewardAccount syncEnv cacheUA $ Ledger.RewardAccount nw cred

-- If the address already exists in the table, it will not be inserted again (due to
-- the uniqueness constraint) but the function will return the 'StakeAddressId'.
insertStakeAddress ::
  Ledger.RewardAccount ->
  Maybe ByteString ->
  ExceptT SyncNodeError DB.DbM DB.StakeAddressId
insertStakeAddress rewardAddr stakeCredBs = do
  lift $
    DB.insertStakeAddress $
      DB.StakeAddress
        { DB.stakeAddressHashRaw = addrBs
        , DB.stakeAddressView = Generic.renderRewardAccount rewardAddr
        , DB.stakeAddressScriptHash = Generic.getCredentialScriptHash $ Ledger.raCredential rewardAddr
        }
  where
    addrBs = fromMaybe (Ledger.serialiseRewardAccount rewardAddr) stakeCredBs

queryStakeAddrWithCache ::
  SyncEnv ->
  CacheAction ->
  Network ->
  StakeCred ->
  ExceptT SyncNodeError DB.DbM (Maybe DB.StakeAddressId)
queryStakeAddrWithCache syncEnv cacheUA nw cred =
  fst <$> queryStakeAddrWithCacheRetBs syncEnv cacheUA (Ledger.RewardAccount nw cred)

queryStakeAddrWithCacheRetBs ::
  SyncEnv ->
  CacheAction ->
  Ledger.RewardAccount ->
  ExceptT SyncNodeError DB.DbM (Maybe DB.StakeAddressId, ByteString)
queryStakeAddrWithCacheRetBs syncEnv cacheUA ra@(Ledger.RewardAccount _ cred) = do
  let bs = Ledger.serialiseRewardAccount ra
  case envCache syncEnv of
    NoCache -> (,bs) <$> resolveStakeAddress bs
    ActiveCache ci -> do
      result <- withCacheCleanedForTipCheck ci (resolveStakeAddress bs) $ do
        stakeCache <- liftIO $ readTVarIO (cStake ci)
        case queryStakeCache cred stakeCache of
          Just (addrId, stakeCache') -> do
            liftIO $ hitCreds syncEnv
            case cacheUA of
              EvictAndUpdateCache -> do
                liftIO $ atomically $ writeTVar (cStake ci) $ deleteStakeCache cred stakeCache'
                pure $ Just addrId
              _other -> do
                liftIO $ atomically $ writeTVar (cStake ci) stakeCache'
                pure $ Just addrId
          Nothing -> do
            queryRes <- resolveStakeAddress bs
            liftIO $ missCreds syncEnv
            case queryRes of
              Nothing -> pure queryRes
              Just stakeAddrsId -> do
                let !stakeCache' = case cacheUA of
                      UpdateCache -> stakeCache {scLruCache = LRU.insert cred stakeAddrsId (scLruCache stakeCache)}
                      UpdateCacheStrong -> stakeCache {scStableCache = Map.insert cred stakeAddrsId (scStableCache stakeCache)}
                      _otherwise -> stakeCache
                liftIO $
                  atomically $
                    writeTVar (cStake ci) stakeCache'
                pure $ Just stakeAddrsId
      pure (result, bs)

-- | True if it was found in LRU
queryStakeCache :: StakeCred -> StakeCache -> Maybe (DB.StakeAddressId, StakeCache)
queryStakeCache scred scache = case Map.lookup scred (scStableCache scache) of
  Just addrId -> Just (addrId, scache)
  Nothing -> case LRU.lookup scred (scLruCache scache) of
    Just (addrId, lru') -> Just (addrId, scache {scLruCache = lru'})
    Nothing -> Nothing

deleteStakeCache :: StakeCred -> StakeCache -> StakeCache
deleteStakeCache scred scache =
  scache {scStableCache = Map.delete scred (scStableCache scache)}

queryPoolKeyWithCache ::
  SyncEnv ->
  CacheAction ->
  PoolKeyHash ->
  ExceptT SyncNodeError DB.DbM (Either DB.DbError DB.PoolHashId)
queryPoolKeyWithCache syncEnv cacheUA hsh =
  case envCache syncEnv of
    NoCache -> do
      mPhId <- lift $ DB.queryPoolHashId (Generic.unKeyHashRaw hsh)
      case mPhId of
        Nothing -> pure $ Left $ DB.DbError "queryPoolKeyWithCache: NoCache queryPoolHashId"
        Just phId -> pure $ Right phId
    ActiveCache ci -> do
      mp <- liftIO $ readTVarIO (cPools ci)
      case Map.lookup hsh mp of
        Just phId -> do
          liftIO $ hitPools syncEnv
          -- hit so we can't cache even with 'CacheNew'
          when (cacheUA == EvictAndUpdateCache) $
            liftIO $
              atomically $
                modifyTVar (cPools ci) $
                  Map.delete hsh
          pure $ Right phId
        Nothing -> do
          liftIO $ missPools syncEnv
          mPhId <- lift $ DB.queryPoolHashId (Generic.unKeyHashRaw hsh)
          case mPhId of
            Nothing -> pure $ Left $ DB.DbError "queryPoolKeyWithCache: ActiveCache queryPoolHashId"
            Just phId -> do
              -- missed so we can't evict even with 'EvictAndReturn'
              when (shouldCache cacheUA) $
                liftIO $
                  atomically $
                    modifyTVar (cPools ci) $
                      Map.insert hsh phId
              pure $ Right phId

insertAddressUsingCache ::
  SyncEnv ->
  CacheAction ->
  ByteString ->
  VA.Address ->
  ExceptT SyncNodeError DB.DbM DB.AddressId
insertAddressUsingCache syncEnv cacheUA addrRaw vAdrs = do
  case envCache syncEnv of
    NoCache -> do
      -- Directly query the database for the address ID when no caching is active.
      mAddrId <- lift $ DB.queryAddressId addrRaw
      lift $ processResult mAddrId
    ActiveCache ci -> do
      -- Use active cache to attempt fetching the address ID from the cache.
      adrs <- liftIO $ readTVarIO (cAddress ci)
      case LRU.lookup addrRaw adrs of
        Just (addrId, adrs') -> do
          -- If found in cache, record a cache hit and update the cache state.
          liftIO $ hitAddress syncEnv
          liftIO $ atomically $ writeTVar (cAddress ci) adrs'
          pure addrId
        Nothing -> do
          -- If not found in cache, log a miss, and query the database.
          liftIO $ missAddress syncEnv
          mAddrId <- lift $ DB.queryAddressId addrRaw
          lift $ processWithCache mAddrId ci
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
  SyncEnv ->
  CacheAction ->
  PoolKeyHash ->
  ExceptT SyncNodeError DB.DbM DB.PoolHashId
insertPoolKeyWithCache syncEnv cacheUA pHash =
  case envCache syncEnv of
    NoCache ->
      lift $
        DB.insertPoolHash $
          DB.PoolHash
            { DB.poolHashHashRaw = Generic.unKeyHashRaw pHash
            , DB.poolHashView = Generic.unKeyHashView pHash
            }
    ActiveCache ci -> do
      mp <- liftIO $ readTVarIO (cPools ci)
      case Map.lookup pHash mp of
        Just phId -> do
          liftIO $ hitPools syncEnv
          when (cacheUA == EvictAndUpdateCache) $
            liftIO $
              atomically $
                modifyTVar (cPools ci) $
                  Map.delete pHash
          pure phId
        Nothing -> do
          liftIO $ missPools syncEnv
          phId <-
            lift $
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
  SyncEnv ->
  Text ->
  CacheAction ->
  Bool ->
  PoolKeyHash ->
  ExceptT SyncNodeError DB.DbM DB.PoolHashId
queryPoolKeyOrInsert syncEnv txt cacheUA logsWarning hsh = do
  pk <- queryPoolKeyWithCache syncEnv cacheUA hsh
  case pk of
    Right poolHashId -> pure poolHashId
    Left err -> do
      when logsWarning $
        liftIO $
          logWarning (getTrace syncEnv) $
            mconcat
              [ "Failed with "
              , textShow err
              , " while trying to find pool "
              , textShow hsh
              , " for "
              , txt
              , ". We will assume that the pool exists and move on."
              ]
      insertPoolKeyWithCache syncEnv cacheUA hsh

queryMAWithCache ::
  SyncEnv ->
  PolicyID ->
  AssetName ->
  ExceptT SyncNodeError DB.DbM (Either (ByteString, ByteString) DB.MultiAssetId)
queryMAWithCache syncEnv policyId asset =
  case envCache syncEnv of
    NoCache -> lift queryDb
    ActiveCache ci -> do
      withCacheCleanedForTipCheck ci (lift queryDb) $ do
        mp <- liftIO $ readTVarIO (cMultiAssets ci)
        case LRU.lookup (policyId, asset) mp of
          Just (maId, mp') -> do
            liftIO $ hitMAssets syncEnv
            liftIO $ atomically $ writeTVar (cMultiAssets ci) mp'
            pure $ Right maId
          Nothing -> do
            liftIO $ missMAssets syncEnv
            -- miss. The lookup doesn't change the cache on a miss.
            let !policyBs = Generic.unScriptHash $ policyID policyId
            let !assetNameBs = Generic.unAssetName asset
            maId <- maybe (Left (policyBs, assetNameBs)) Right <$> lift (DB.queryMultiAssetId policyBs assetNameBs)
            whenRight maId $
              liftIO . atomically . modifyTVar (cMultiAssets ci) . LRU.insert (policyId, asset)
            pure maId
  where
    queryDb = do
      let !policyBs = Generic.unScriptHash $ policyID policyId
      let !assetNameBs = Generic.unAssetName asset
      maybe (Left (policyBs, assetNameBs)) Right <$> DB.queryMultiAssetId policyBs assetNameBs

queryPrevBlockWithCache ::
  SyncEnv ->
  ByteString ->
  Text.Text ->
  ExceptT SyncNodeError DB.DbM DB.BlockId
queryPrevBlockWithCache syncEnv hsh errMsg =
  case envCache syncEnv of
    NoCache ->
      liftFail cs $ DB.queryBlockId hsh errMsg
    ActiveCache ci -> do
      mCachedPrev <- liftIO $ readTVarIO (cPrevBlock ci)
      case mCachedPrev of
        -- if the cached block matches the requested hash, we return its db id.
        Just (cachedBlockId, cachedHash) ->
          if cachedHash == hsh
            then do
              liftIO $ hitPBlock syncEnv
              pure cachedBlockId
            else queryFromDb
        Nothing -> queryFromDb
  where
    cs = mkSyncNodeCallStack "queryPrevBlockWithCache"

    queryFromDb ::
      ExceptT SyncNodeError DB.DbM DB.BlockId
    queryFromDb = do
      liftIO $ missPrevBlock syncEnv
      liftFail cs $ DB.queryBlockId hsh errMsg

queryTxIdWithCache ::
  SyncEnv ->
  Ledger.TxId ->
  ExceptT SyncNodeError DB.DbM (Either DB.DbError DB.TxId)
queryTxIdWithCache syncEnv txIdLedger = do
  case envCache syncEnv of
    -- Direct database query if no cache.
    NoCache -> lift qTxHash
    ActiveCache ci ->
      withCacheCleanedForTipCheck ci (lift qTxHash) $ do
        -- Read current cache state.
        cacheTx <- liftIO $ readTVarIO (cTxIds ci)

        case FIFO.lookup txIdLedger cacheTx of
          -- Cache hit, return the transaction ID.
          Just txId -> do
            liftIO $ hitTxIds syncEnv
            pure $ Right txId
          -- Cache miss.
          Nothing -> do
            eTxId <- lift qTxHash
            liftIO $ missTxIds syncEnv
            case eTxId of
              Right txId -> do
                -- Update cache ONLY on successful lookup.
                liftIO $ atomically $ modifyTVar (cTxIds ci) $ FIFO.insert txIdLedger txId
                -- Return ID after updating cache.
                pure $ Right txId
              -- Return lookup failure - DON'T update cache.
              Left err -> pure $ Left err
  where
    txHash = Generic.unTxHash txIdLedger
    qTxHash = do
      result <- DB.queryTxId txHash
      case result of
        Just txId -> pure $ Right txId
        Nothing ->
          pure $ Left $ DB.DbError ("TxId not found for hash: " <> textShow txHash)

tryUpdateCacheTx ::
  MonadIO m =>
  CacheStatus ->
  Ledger.TxId ->
  DB.TxId ->
  m ()
tryUpdateCacheTx (ActiveCache ci) ledgerTxId txId =
  liftIO $ atomically $ modifyTVar (cTxIds ci) $ FIFO.insert ledgerTxId txId
tryUpdateCacheTx _ _ _ = pure ()

insertBlockAndCache ::
  SyncEnv ->
  DB.Block ->
  ExceptT SyncNodeError DB.DbM DB.BlockId
insertBlockAndCache syncEnv block =
  case envCache syncEnv of
    NoCache -> lift insBlck
    ActiveCache ci ->
      withCacheCleanedForTipCheck ci (lift insBlck) $ do
        bid <- lift insBlck
        liftIO $ do
          missPrevBlock syncEnv
          atomically $ writeTVar (cPrevBlock ci) $ Just (bid, DB.blockHash block)
        pure bid
  where
    insBlck = DB.insertBlock block

queryDatum ::
  SyncEnv ->
  DataHash ->
  ExceptT SyncNodeError DB.DbM (Maybe DB.DatumId)
queryDatum syncEnv hsh = do
  case envCache syncEnv of
    NoCache -> lift queryDtm
    ActiveCache ci -> do
      withCacheCleanedForTipCheck ci (lift queryDtm) $ do
        mp <- liftIO $ readTVarIO (cDatum ci)
        case LRU.lookup hsh mp of
          Just (datumId, mp') -> do
            liftIO $ hitDatum syncEnv
            liftIO $ atomically $ writeTVar (cDatum ci) mp'
            pure $ Just datumId
          Nothing -> do
            liftIO $ missDatum syncEnv
            -- miss. The lookup doesn't change the cache on a miss.
            lift queryDtm
  where
    queryDtm = DB.queryDatum $ Generic.dataHashToBytes hsh

-- This assumes the entry is not cached.
insertDatumAndCache ::
  CacheStatus ->
  DataHash ->
  DB.Datum ->
  ExceptT SyncNodeError DB.DbM DB.DatumId
insertDatumAndCache cache hsh dt = do
  datumId <- lift $ DB.insertDatum dt
  case cache of
    NoCache -> pure datumId
    ActiveCache ci ->
      withCacheCleanedForTipCheck ci (pure datumId) $ do
        liftIO $
          atomically $
            modifyTVar (cDatum ci) $
              LRU.insert hsh datumId
        pure datumId

withCacheCleanedForTipCheck ::
  MonadIO m =>
  CacheInternal ->
  m a -> -- Action to perform if cache is cleaned for tip
  m a -> -- Action to perform if cache is not cleaned for tip
  m a
withCacheCleanedForTipCheck ci ifCleanedForTip ifNotCleanedForTip = do
  isCacheCleanedForTip <- liftIO $ readTVarIO (cIsCacheCleanedForTip ci)
  if isCacheCleanedForTip
    then ifCleanedForTip
    else ifNotCleanedForTip

-- Creds
hitCreds :: SyncEnv -> IO ()
hitCreds syncEnv =
  atomically $ modifyTVar (envEpochStatistics syncEnv) $ \epochStats ->
    epochStats {elsCaches = (elsCaches epochStats) {credsHits = 1 + credsHits (elsCaches epochStats), credsQueries = 1 + credsQueries (elsCaches epochStats)}}

missCreds :: SyncEnv -> IO ()
missCreds syncEnv =
  atomically $ modifyTVar (envEpochStatistics syncEnv) $ \epochStats ->
    epochStats {elsCaches = (elsCaches epochStats) {credsQueries = 1 + credsQueries (elsCaches epochStats)}}

-- Pools
hitPools :: SyncEnv -> IO ()
hitPools syncEnv =
  atomically $ modifyTVar (envEpochStatistics syncEnv) $ \epochStats ->
    epochStats {elsCaches = (elsCaches epochStats) {poolsHits = 1 + poolsHits (elsCaches epochStats), poolsQueries = 1 + poolsQueries (elsCaches epochStats)}}

missPools :: SyncEnv -> IO ()
missPools syncEnv =
  atomically $ modifyTVar (envEpochStatistics syncEnv) $ \epochStats ->
    epochStats {elsCaches = (elsCaches epochStats) {poolsQueries = 1 + poolsQueries (elsCaches epochStats)}}

-- Datum
hitDatum :: SyncEnv -> IO ()
hitDatum syncEnv =
  atomically $ modifyTVar (envEpochStatistics syncEnv) $ \epochStats ->
    epochStats {elsCaches = (elsCaches epochStats) {datumHits = 1 + datumHits (elsCaches epochStats), datumQueries = 1 + datumQueries (elsCaches epochStats)}}

missDatum :: SyncEnv -> IO ()
missDatum syncEnv =
  atomically $ modifyTVar (envEpochStatistics syncEnv) $ \epochStats ->
    epochStats {elsCaches = (elsCaches epochStats) {datumQueries = 1 + datumQueries (elsCaches epochStats)}}

-- Assets
hitMAssets :: SyncEnv -> IO ()
hitMAssets syncEnv =
  atomically $ modifyTVar (envEpochStatistics syncEnv) $ \epochStats ->
    epochStats {elsCaches = (elsCaches epochStats) {multiAssetsHits = 1 + multiAssetsHits (elsCaches epochStats), multiAssetsQueries = 1 + multiAssetsQueries (elsCaches epochStats)}}

missMAssets :: SyncEnv -> IO ()
missMAssets syncEnv =
  atomically $ modifyTVar (envEpochStatistics syncEnv) $ \epochStats ->
    epochStats {elsCaches = (elsCaches epochStats) {multiAssetsQueries = 1 + multiAssetsQueries (elsCaches epochStats)}}

-- Address
hitAddress :: SyncEnv -> IO ()
hitAddress syncEnv =
  atomically $ modifyTVar (envEpochStatistics syncEnv) $ \epochStats ->
    epochStats {elsCaches = (elsCaches epochStats) {addressHits = 1 + addressHits (elsCaches epochStats), addressQueries = 1 + addressQueries (elsCaches epochStats)}}

missAddress :: SyncEnv -> IO ()
missAddress syncEnv =
  atomically $ modifyTVar (envEpochStatistics syncEnv) $ \epochStats ->
    epochStats {elsCaches = (elsCaches epochStats) {addressQueries = 1 + addressQueries (elsCaches epochStats)}}

-- Blocks
hitPBlock :: SyncEnv -> IO ()
hitPBlock syncEnv =
  atomically $ modifyTVar (envEpochStatistics syncEnv) $ \epochStats ->
    epochStats {elsCaches = (elsCaches epochStats) {prevBlockHits = 1 + prevBlockHits (elsCaches epochStats), prevBlockQueries = 1 + prevBlockQueries (elsCaches epochStats)}}

missPrevBlock :: SyncEnv -> IO ()
missPrevBlock syncEnv =
  atomically $ modifyTVar (envEpochStatistics syncEnv) $ \epochStats ->
    epochStats {elsCaches = (elsCaches epochStats) {prevBlockQueries = 1 + prevBlockQueries (elsCaches epochStats)}}

-- TxIds
hitTxIds :: SyncEnv -> IO ()
hitTxIds syncEnv =
  atomically $ modifyTVar (envEpochStatistics syncEnv) $ \epochStats ->
    epochStats {elsCaches = (elsCaches epochStats) {txIdsHits = 1 + txIdsHits (elsCaches epochStats), txIdsQueries = 1 + txIdsQueries (elsCaches epochStats)}}

missTxIds :: SyncEnv -> IO ()
missTxIds syncEnv =
  atomically $ modifyTVar (envEpochStatistics syncEnv) $ \epochStats ->
    epochStats {elsCaches = (elsCaches epochStats) {txIdsQueries = 1 + txIdsQueries (elsCaches epochStats)}}
