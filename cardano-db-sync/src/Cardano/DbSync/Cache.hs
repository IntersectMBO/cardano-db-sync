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
import Cardano.DbSync.Cache.Types (CacheInternal (..), CacheStatistics (..), CacheStatus (..), CacheUpdateAction (..), StakeAddrCache, initCacheStatistics)
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
  CacheUpdateAction ->
  Ledger.RewardAccount StandardCrypto ->
  ReaderT SqlBackend m DB.StakeAddressId
queryOrInsertRewardAccount trce cacheStatus cacheUA rewardAddr = do
  eiAddrId <- queryRewardAccountWithCacheRetBs trce cacheStatus rewardAddr
  case eiAddrId of
    Left (_err, bs) -> insertStakeAddress cacheStatus rewardAddr (Just bs)
    Right addrId -> pure addrId

queryOrInsertStakeAddress ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  CacheStatus ->
  CacheUpdateAction ->
  Network ->
  StakeCred ->
  ReaderT SqlBackend m DB.StakeAddressId
queryOrInsertStakeAddress trce cacheStatus cacheUA nw cred =
  queryOrInsertRewardAccount trce cacheStatus cacheUA $ Ledger.RewardAccount nw cred

-- If the address already exists in the table, it will not be inserted again (due to
-- the uniqueness constraint) but the function will return the 'StakeAddressId'.
insertStakeAddress ::
  (MonadBaseControl IO m, MonadIO m) =>
  CacheStatus ->
  Ledger.RewardAccount StandardCrypto ->
  Maybe ByteString ->
  ReaderT SqlBackend m DB.StakeAddressId
insertStakeAddress cacheStatus rewardAddr stakeCredBs = do
  addrId <- DB.insertStakeAddress $
      DB.StakeAddress
        { DB.stakeAddressHashRaw = addrBs
        , DB.stakeAddressView = Generic.renderRewardAccount rewardAddr
        , DB.stakeAddressScriptHash = Generic.getCredentialScriptHash $ Ledger.raCredential rewardAddr
        }
  case cacheStatus of
    NoCache -> pure addrId
    CacheActive ci -> do
      liftIO $ atomically $ modifyTVar (cStakeRawHashes ci) $
        LRU.insert addrBs addrId
      pure addrId
  where
    addrBs = fromMaybe (Ledger.serialiseRewardAccount rewardAddr) stakeCredBs

queryRewardAccountWithCacheRetBs ::
  forall m.
  MonadIO m =>
  Trace IO Text ->
  CacheStatus ->
  CacheUpdateAction ->
  Ledger.RewardAccount StandardCrypto ->
  ReaderT SqlBackend m (Either (DB.LookupFail, ByteString) DB.StakeAddressId)
queryRewardAccountWithCacheRetBs trce cacheStatus cacheUA rwdAcc =
  queryStakeAddrWithCacheRetBs trce cacheStatus cacheUA (Ledger.raNetwork rwdAcc) (Ledger.raCredential rwdAcc)

queryStakeAddrWithCache ::
  forall m.
  MonadIO m =>
  Trace IO Text ->
  CacheStatus ->
  CacheUpdateAction ->
  Network ->
  StakeCred ->
  ReaderT SqlBackend m (Either DB.LookupFail DB.StakeAddressId)
queryStakeAddrWithCache trce cacheStatus cacheUA nw cred =
  mapLeft fst <$> queryStakeAddrWithCacheRetBs trce cacheStatus cacheUA nw cred

queryStakeAddrWithCacheRetBs ::
  forall m.
  MonadIO m =>
  Trace IO Text ->
  CacheStatus ->
  CacheUpdateAction ->
  Network ->
  StakeCred ->
  ReaderT SqlBackend m (Either (DB.LookupFail, ByteString) DB.StakeAddressId)
queryStakeAddrWithCacheRetBs trce cacheStatus cacheUA nw cred = do
  let !bs = Ledger.serialiseRewardAccount (Ledger.RewardAccount nw cred)
  case cacheStatus of
    NoCache -> do
      mapLeft (,bs) <$> queryStakeAddress bs
    CacheActive ci -> do
      currentCache <- liftIO $ readTVarIO (cStakeRawHashes ci)
      let cacheSize = LRU.getSize currentCache
      newCache <-
            if cacheSize < 1
              then do
                liftIO $ logInfo trce "----------------- Cache is empty. Querying all addresses. ---------"
                queryRes <- DB.queryLatestAddresses cacheSize
                pure $ LRU.fromList queryRes currentCache
                -- convert the results into the cache
              else pure currentCache
      case LRU.lookup bs newCache of
        Just (addrId, mp') -> do
          liftIO $ hitCreds (cStats ci)
          liftIO $ atomically $ writeTVar (cStakeRawHashes ci) mp'
          pure $ Right addrId
        Nothing -> do
          liftIO $ missCreds (cStats ci)
          liftIO $ atomically $ writeTVar (cStakeRawHashes ci) newCache
          queryRes <- mapLeft (,bs) <$> queryStakeAddress bs
          case queryRes of
            Left _ -> pure queryRes
            Right stakeAddrsId -> do
              liftIO $ atomically $ modifyTVar (cStakeRawHashes ci) $
               LRU.insert bs stakeAddrsId
              pure $ Right stakeAddrsId

-- queryStakeAddrAux ::
--   MonadIO m =>
--   CacheNew ->
--   StakeAddrCache ->
--   StrictTVar IO CacheStatistics ->
--   Network ->
--   StakeCred ->
--   ReaderT SqlBackend m (Either (DB.LookupFail, ByteString) DB.StakeAddressId, StakeAddrCache)
-- queryStakeAddrAux cacheNew mp sts nw cred =
--   case Map.lookup cred mp of
--     Just addrId -> do
--       liftIO $ hitCreds sts
--       case cacheNew of
--         EvictAndReturn -> pure (Right addrId, Map.delete cred mp)
--         _ -> pure (Right addrId, mp)
--     Nothing -> do
--       liftIO $ missCreds sts
--       let !bs = Ledger.serialiseRewardAccount (Ledger.RewardAccount nw cred)
--       mAddrId <- mapLeft (,bs) <$> queryStakeAddress bs
--       case (mAddrId, cacheNew) of
--         (Right addrId, CacheNew) -> pure (Right addrId, Map.insert cred addrId mp)
--         (Right addrId, _) -> pure (Right addrId, mp)
--         (err, _) -> pure (err, mp)

queryPoolKeyWithCache ::
  MonadIO m =>
  CacheStatus ->
  CacheUpdateAction ->
  PoolKeyHash ->
  ReaderT SqlBackend m (Either DB.LookupFail DB.PoolHashId)
queryPoolKeyWithCache cacheStatus cacheUA hsh =
  case cacheStatus of
    NoCache -> do
      mPhId <- queryPoolHashId (Generic.unKeyHashRaw hsh)
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
          mPhId <- queryPoolHashId (Generic.unKeyHashRaw hsh)
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
  CacheUpdateAction ->
  PoolKeyHash ->
  ReaderT SqlBackend m DB.PoolHashId
insertPoolKeyWithCache cacheStatus cacheUA pHash =
  case cacheStatus of
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
  CacheUpdateAction ->
  Bool ->
  PoolKeyHash ->
  ReaderT SqlBackend m DB.PoolHashId
queryPoolKeyOrInsert txt trce cacheStatus cacheUA logsWarning hsh = do
  pk <- queryPoolKeyWithCache cacheStatus cacheUA hsh
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
      insertPoolKeyWithCache cacheStatus cacheUA hsh

queryMAWithCache ::
  MonadIO m =>
  CacheStatus ->
  PolicyID StandardCrypto ->
  AssetName ->
  ReaderT SqlBackend m (Either (ByteString, ByteString) DB.MultiAssetId)
queryMAWithCache cacheStatus policyId asset =
  case cacheStatus of
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
queryPrevBlockWithCache msg cacheStatus hsh =
  case cacheStatus of
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
insertBlockAndCache cacheStatus block =
  case cacheStatus of
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
queryDatum cacheStatus hsh = do
  case cacheStatus of
    NoCache -> DB.queryDatum $ Generic.dataHashToBytes hsh
    CacheActive ci -> do
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
insertDatumAndCache cacheStatus hsh dt = do
  datumId <- DB.insertDatum dt
  case cacheStatus of
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
