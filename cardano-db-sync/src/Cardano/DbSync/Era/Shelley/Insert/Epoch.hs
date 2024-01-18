{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Insert.Epoch (
  insertRewards,
  insertInstantRewards,
  insertPoolDepositRefunds,
  insertStakeSlice,
  sumRewardTotal,
) where

import Cardano.BM.Trace (Trace, logInfo)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncEnv (..))
import Cardano.DbSync.Cache (queryOrInsertStakeAddress, queryPoolKeyOrInsert)
import Cardano.DbSync.Cache.Types (Cache, CacheNew (..))
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Error
import Cardano.DbSync.Types
import Cardano.DbSync.Util.Constraint (constraintNameEpochStake, constraintNameReward)
import Cardano.Ledger.BaseTypes (Network)
import qualified Cardano.Ledger.Coin as Shelley
import Cardano.Prelude
import Cardano.Slotting.Slot (EpochNo (..))
import Control.Concurrent.Class.MonadSTM.Strict (readTVarIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Database.Persist.Sql (SqlBackend)

{- HLINT ignore "Use readTVarIO" -}

insertStakeSlice ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Generic.StakeSliceRes ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertStakeSlice _ Generic.NoSlices = pure ()
insertStakeSlice syncEnv (Generic.Slice slice finalSlice) = do
  insertEpochStake syncEnv network (Generic.sliceEpochNo slice) (Map.toList $ Generic.sliceDistr slice)
  when finalSlice $ do
    lift $ DB.updateSetComplete $ unEpochNo $ Generic.sliceEpochNo slice
    size <- lift $ DB.queryEpochStakeCount (unEpochNo $ Generic.sliceEpochNo slice)
    liftIO
      . logInfo tracer
      $ mconcat ["Inserted ", show size, " EpochStake for ", show (Generic.sliceEpochNo slice)]
  where
    tracer :: Trace IO Text
    tracer = getTrace syncEnv

    network :: Network
    network = getNetwork syncEnv

insertEpochStake ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Network ->
  EpochNo ->
  [(StakeCred, (Shelley.Coin, PoolKeyHash))] ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertEpochStake syncEnv nw epochNo stakeChunk = do
  let cache = envCache syncEnv
  DB.ManualDbConstraints {..} <- liftIO $ readTVarIO $ envDbConstraints syncEnv
  dbStakes <- mapM (mkStake cache) stakeChunk
  let chunckDbStakes = splittRecordsEvery 100000 dbStakes
  -- minimising the bulk inserts into hundred thousand chunks to improve performance
  forM_ chunckDbStakes $ \dbs -> lift $ DB.insertManyEpochStakes dbConstraintEpochStake constraintNameEpochStake dbs
  where
    mkStake ::
      (MonadBaseControl IO m, MonadIO m) =>
      Cache ->
      (StakeCred, (Shelley.Coin, PoolKeyHash)) ->
      ExceptT SyncNodeError (ReaderT SqlBackend m) DB.EpochStake
    mkStake cache (saddr, (coin, pool)) = do
      saId <- lift $ queryOrInsertStakeAddress cache CacheNew nw saddr
      poolId <- lift $ queryPoolKeyOrInsert "insertEpochStake" trce cache CacheNew (ioShelley iopts) pool
      pure $
        DB.EpochStake
          { DB.epochStakeAddrId = saId
          , DB.epochStakePoolId = poolId
          , DB.epochStakeAmount = Generic.coinToDbLovelace coin
          , DB.epochStakeEpochNo = unEpochNo epochNo -- The epoch where this delegation becomes valid.
          }

    trce = getTrace syncEnv
    iopts = getInsertOptions syncEnv

insertRewards ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Network ->
  EpochNo ->
  EpochNo ->
  Cache ->
  [(StakeCred, Set Generic.Reward)] ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertRewards syncEnv nw earnedEpoch spendableEpoch cache rewardsChunk = do
  DB.ManualDbConstraints {..} <- liftIO $ readTVarIO $ envDbConstraints syncEnv
  dbRewards <- concatMapM mkRewards rewardsChunk
  let chunckDbRewards = splittRecordsEvery 100000 dbRewards
  -- minimising the bulk inserts into hundred thousand chunks to improve performance
  forM_ chunckDbRewards $ \rws -> lift $ DB.insertManyRewards dbConstraintRewards constraintNameReward rws
  where
    mkRewards ::
      (MonadBaseControl IO m, MonadIO m) =>
      (StakeCred, Set Generic.Reward) ->
      ExceptT SyncNodeError (ReaderT SqlBackend m) [DB.Reward]
    mkRewards (saddr, rset) = do
      saId <- lift $ queryOrInsertStakeAddress cache CacheNew nw saddr
      mapM (prepareReward saId) (Set.toList rset)

    prepareReward ::
      (MonadBaseControl IO m, MonadIO m) =>
      DB.StakeAddressId ->
      Generic.Reward ->
      ExceptT SyncNodeError (ReaderT SqlBackend m) DB.Reward
    prepareReward saId rwd = do
      poolId <- queryPool (Generic.rewardPool rwd)
      pure $
        DB.Reward
          { DB.rewardAddrId = saId
          , DB.rewardType = Generic.rewardSource rwd
          , DB.rewardAmount = Generic.coinToDbLovelace (Generic.rewardAmount rwd)
          , DB.rewardEarnedEpoch = unEpochNo earnedEpoch
          , DB.rewardSpendableEpoch = unEpochNo spendableEpoch
          , DB.rewardPoolId = poolId
          }

    queryPool ::
      (MonadBaseControl IO m, MonadIO m) =>
      PoolKeyHash ->
      ExceptT SyncNodeError (ReaderT SqlBackend m) DB.PoolHashId
    queryPool poolHash =
      lift (queryPoolKeyOrInsert "insertRewards" trce cache CacheNew (ioShelley iopts) poolHash)

    trce = getTrace syncEnv
    iopts = getInsertOptions syncEnv

insertInstantRewards ::
  (MonadBaseControl IO m, MonadIO m) =>
  Network ->
  EpochNo ->
  EpochNo ->
  Cache ->
  [(StakeCred, Set Generic.InstantReward)] ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertInstantRewards nw earnedEpoch spendableEpoch cache rewardsChunk = do
  dbRewards <- concatMapM mkRewards rewardsChunk
  let chunckDbRewards = splittRecordsEvery 100000 dbRewards
  -- minimising the bulk inserts into hundred thousand chunks to improve performance
  forM_ chunckDbRewards $ \rws -> lift $ DB.insertManyInstantRewards rws
  where
    mkRewards ::
      (MonadBaseControl IO m, MonadIO m) =>
      (StakeCred, Set Generic.InstantReward) ->
      ExceptT SyncNodeError (ReaderT SqlBackend m) [DB.InstantReward]
    mkRewards (saddr, rset) = do
      saId <- lift $ queryOrInsertStakeAddress cache CacheNew nw saddr
      pure $ map (prepareReward saId) (Set.toList rset)

    prepareReward ::
      DB.StakeAddressId ->
      Generic.InstantReward ->
      DB.InstantReward
    prepareReward saId rwd =
      DB.InstantReward
        { DB.instantRewardAddrId = saId
        , DB.instantRewardType = Generic.irSource rwd
        , DB.instantRewardAmount = Generic.coinToDbLovelace (Generic.irAmount rwd)
        , DB.instantRewardEarnedEpoch = unEpochNo earnedEpoch
        , DB.instantRewardSpendableEpoch = unEpochNo spendableEpoch
        }

splittRecordsEvery :: Int -> [a] -> [[a]]
splittRecordsEvery val = go
  where
    go [] = []
    go ys =
      let (as, bs) = splitAt val ys
       in as : go bs

insertPoolDepositRefunds ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  EpochNo ->
  Generic.Rewards ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertPoolDepositRefunds syncEnv epochNo refunds = do
  insertRewards syncEnv nw epochNo epochNo (envCache syncEnv) (Map.toList rwds)
  liftIO . logInfo tracer $ "Inserted " <> show (Generic.rewardsCount refunds) <> " deposit refund rewards"
  where
    tracer = getTrace syncEnv
    rwds = Generic.unRewards refunds
    nw = getNetwork syncEnv

sumRewardTotal :: Map StakeCred (Set Generic.Reward) -> Shelley.Coin
sumRewardTotal =
  Shelley.Coin . Map.foldl' sumCoin 0
  where
    sumCoin :: Integer -> Set Generic.Reward -> Integer
    sumCoin !acc sr =
      acc + sum (map (Shelley.unCoin . Generic.rewardAmount) $ Set.toList sr)
