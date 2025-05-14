{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Cardano.DbSync.Cache.Stake where

import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types
import qualified Cardano.DbSync.Cache.LRU as LRU
import Cardano.DbSync.Cache.Types
import Cardano.DbSync.Cache.Util
import qualified Cardano.DbSync.Era.Shelley.Generic.Util as Generic
import Cardano.DbSync.Era.Shelley.Query
import Cardano.DbSync.Types
import qualified Cardano.Ledger.Address as Ledger
import Cardano.Prelude
import Control.Concurrent.Class.MonadSTM.Strict (
  StrictTVar,
  modifyTVar,
  newEmptyTMVarIO,
  readTVarIO,
  takeTMVar,
  writeTVar,
 )
import qualified Control.Concurrent.STM.TBQueue as TBQ
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Either.Combinators
import qualified Data.Map.Strict as Map
import Database.Persist.Postgresql (SqlBackend)

-- | TO be called only by the stake thread
resolveInsertRewardAccount ::
  forall m.
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  CacheAction ->
  RewAccount ->
  ReaderT SqlBackend m DB.StakeAddressId
resolveInsertRewardAccount syncEnv cacheUA ra = do
  eiStakeId <- queryStakeAddrWithCacheRetBs syncEnv cacheUA False ra -- read only
  case eiStakeId of
    Right stakeId -> pure stakeId
    Left (_, bs) -> insertStakeAddress ra (Just bs)

-- | TO be called only by the stake thread
-- If the address already exists in the table, it will not be inserted again (due to
-- the uniqueness constraint) but the function will return the 'StakeAddressId'.
insertStakeAddress ::
  (MonadBaseControl IO m, MonadIO m) =>
  RewAccount ->
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

queryStakeAddrWithCacheRetBs ::
  forall m.
  MonadIO m =>
  SyncEnv ->
  CacheAction ->
  Bool ->
  RewAccount ->
  ReaderT SqlBackend m (Either (DB.LookupFail, ByteString) DB.StakeAddressId)
queryStakeAddrWithCacheRetBs syncEnv cacheUA readOnly ra@(Ledger.RewardAccount _ cred) = do
  case cache of
    NoCache -> queryStakeDB
    ActiveCache ci -> do
      withCacheOptimisationCheck ci queryStakeDB $ do
        stakeCache <- liftIO $ readTVarIO (cStake ci)
        case queryStakeCache cred stakeCache of
          Just (addrId, stakeCache') -> do
            unless readOnly $ liftIO $ hitCreds (cStats ci)
            case cacheUA of
              EvictAndUpdateCache -> do
                unless readOnly $ liftIO $ atomically $ writeTVar (cStake ci) $ deleteStakeCache cred stakeCache'
                pure $ Right addrId
              _other -> do
                unless readOnly $ liftIO $ atomically $ writeTVar (cStake ci) stakeCache'
                pure $ Right addrId
          Nothing -> do
            queryRes <- queryStakeDB
            unless readOnly $ liftIO $ missCreds (cStats ci)
            case queryRes of
              Left _ -> pure queryRes
              Right stakeAddrsId -> do
                let stakeCache' = case cacheUA of
                      UpdateCache -> stakeCache {scLruCache = LRU.insert cred stakeAddrsId (scLruCache stakeCache)}
                      UpdateCacheStrong -> stakeCache {scStableCache = Map.insert cred stakeAddrsId (scStableCache stakeCache)}
                      _otherwise -> stakeCache
                unless readOnly $
                  liftIO $
                    atomically $
                      writeTVar (cStake ci) stakeCache'
                pure $ Right stakeAddrsId
  where
    bs = Ledger.serialiseRewardAccount ra
    queryStakeDB = mapLeft (,bs) <$> resolveStakeAddress bs
    _trce = getTrace syncEnv
    cache = envCache syncEnv

-- | Checks both caches
queryStakeCache :: StakeCred -> StakeCache -> Maybe (DB.StakeAddressId, StakeCache)
queryStakeCache scred scache = case Map.lookup scred (scStableCache scache) of
  Just addrId -> Just (addrId, scache)
  Nothing -> case LRU.lookup scred (scLruCache scache) of
    Just (addrId, lru') -> Just (addrId, scache {scLruCache = lru'})
    Nothing -> Nothing

deleteStakeCache :: StakeCred -> StakeCache -> StakeCache
deleteStakeCache scred scache =
  scache {scStableCache = Map.delete scred (scStableCache scache)}

hitCreds :: StrictTVar IO CacheStatistics -> IO ()
hitCreds ref =
  atomically $ modifyTVar ref (\cs -> cs {credsHits = 1 + credsHits cs, credsQueries = 1 + credsQueries cs})

missCreds :: StrictTVar IO CacheStatistics -> IO ()
missCreds ref =
  atomically $ modifyTVar ref (\cs -> cs {credsQueries = 1 + credsQueries cs})

queryOrInsertStakeAddress ::
  forall m.
  MonadIO m =>
  SyncEnv ->
  CacheAction ->
  StakeCred ->
  ReaderT SqlBackend m DB.StakeAddressId
queryOrInsertStakeAddress syncEnv cacheUA cred =
  queryOrInsertRewardAccount syncEnv cacheUA $ Ledger.RewardAccount (getNetwork syncEnv) cred

queryOrInsertRewardAccount ::
  forall m.
  MonadIO m =>
  SyncEnv ->
  CacheAction ->
  RewAccount ->
  ReaderT SqlBackend m DB.StakeAddressId
queryOrInsertRewardAccount syncEnv cacheUA ra = do
  eiStakeId <- queryStakeAddrWithCacheRetBs syncEnv cacheUA True ra -- read only
  case eiStakeId of
    Right stakeId -> pure stakeId
    Left _ -> liftIO $ do
      resultVar <- newEmptyTMVarIO
      atomically $ TBQ.writeTBQueue (scPriorityQueue stakeChan) $ QueryInsertStake ra cacheUA resultVar
      atomically $ takeTMVar resultVar
  where
    stakeChan = envStakeChans syncEnv
