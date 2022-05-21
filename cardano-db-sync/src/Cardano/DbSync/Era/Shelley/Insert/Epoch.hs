{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Era.Shelley.Insert.Epoch
  ( insertRewards
  , insertPoolDepositRefunds
  , insertStakeSlice
  , insertEpochRewardTotalReceived
  , sumRewardTotal
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logInfo)

import qualified Cardano.Db as DB

import qualified Cardano.Ledger.Coin as Shelley

import           Cardano.DbSync.Api
import           Cardano.DbSync.Cache
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import           Cardano.DbSync.Era.Util (liftLookupFail)
import           Cardano.DbSync.Error

import           Cardano.Slotting.Slot (EpochNo (..))

import           Control.Monad.Extra (mapMaybeM)
import           Control.Monad.Trans.Control (MonadBaseControl)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict

import           Database.Persist.Sql (SqlBackend)

{- HLINT ignore "Use readTVarIO" -}

insertEpochRewardTotalReceived
    :: (MonadBaseControl IO m, MonadIO m)
    => EpochNo -> DB.DbLovelace
    -> ReaderT SqlBackend m ()
insertEpochRewardTotalReceived epochNo total =
  void . DB.insertEpochRewardTotalReceived $
    DB.EpochRewardTotalReceived
      { DB.epochRewardTotalReceivedEarnedEpoch = unEpochNo epochNo
      , DB.epochRewardTotalReceivedAmount = total
      }

insertStakeSlice
    :: (MonadBaseControl IO m, MonadIO m)
    => SyncEnv -> Generic.StakeSliceRes
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertStakeSlice _ Generic.NoSlices = pure ()
insertStakeSlice env (Generic.Slice slice finalSlice) = do
    insertEpochStake (envCache env) (Generic.sliceEpochNo slice) (Map.toList $ Generic.sliceDistr slice)
    when finalSlice $ do
      size <- lift $ DB.queryEpochStakeCount (unEpochNo $ Generic.sliceEpochNo slice)
      liftIO . logInfo tracer $ mconcat ["Inserted ", show size, " EpochStake for ", show (Generic.sliceEpochNo slice)]
  where
    tracer :: Trace IO Text
    tracer = getTrace env

insertEpochStake
    :: (MonadBaseControl IO m, MonadIO m)
    => Cache -> EpochNo
    -> [(Generic.StakeCred, (Shelley.Coin, Generic.StakePoolKeyHash))]
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertEpochStake cache epochNo stakeChunk = do
    dbStakes <- mapM mkStake stakeChunk
    lift $ DB.insertManyEpochStakes dbStakes
  where
    mkStake
        :: (MonadBaseControl IO m, MonadIO m)
        => (Generic.StakeCred, (Shelley.Coin, Generic.StakePoolKeyHash))
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) DB.EpochStake
    mkStake (saddr, (coin, pool)) = do
      saId <- liftLookupFail "insertEpochStake.queryStakeAddrWithCache" $ queryStakeAddrWithCache cache CacheNew saddr
      poolId <- liftLookupFail "insertEpochStake.queryPoolKeyWithCache" $ queryPoolKeyWithCache cache CacheNew pool
      pure $
        DB.EpochStake
          { DB.epochStakeAddrId = saId
          , DB.epochStakePoolId = poolId
          , DB.epochStakeAmount = Generic.coinToDbLovelace coin
          , DB.epochStakeEpochNo = unEpochNo epochNo -- The epoch where this delegation becomes valid.
          }

insertRewards
    :: (MonadBaseControl IO m, MonadIO m)
    => EpochNo -> EpochNo -> Cache -> [(Generic.StakeCred, Set Generic.Reward)]
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertRewards earnedEpoch spendableEpoch cache rewardsChunk = do
    dbRewards <- concatMapM mkRewards rewardsChunk
    lift $ DB.insertManyRewards dbRewards
  where
    mkRewards
        :: (MonadBaseControl IO m, MonadIO m)
        => (Generic.StakeCred, Set Generic.Reward)
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) [DB.Reward]
    mkRewards (saddr, rset) = do
      saId <- liftLookupFail "insertRewards.queryStakeAddrWithCache" $ queryStakeAddrWithCache cache CacheNew saddr
      mapMaybeM (prepareReward saId) (Set.toList rset)

    -- For rewards with a null pool, the reward unique key doesn't work.
    -- So we need to manually check that it's not already in the db.
    -- This can happen on rollbacks.
    prepareReward
        :: (MonadBaseControl IO m, MonadIO m)
        => DB.StakeAddressId -> Generic.Reward
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) (Maybe DB.Reward)
    prepareReward saId rwd = do
        mPool <- queryPool (Generic.rewardPool rwd)
        let rwdDb = DB.Reward
                    { DB.rewardAddrId = saId
                    , DB.rewardType = Generic.rewardSource rwd
                    , DB.rewardAmount = Generic.coinToDbLovelace (Generic.rewardAmount rwd)
                    , DB.rewardEarnedEpoch = unEpochNo earnedEpoch
                    , DB.rewardSpendableEpoch = unEpochNo spendableEpoch
                    , DB.rewardPoolId = mPool
                    }
        case DB.rewardPoolId rwdDb of
          Just _ -> pure $ Just rwdDb
          Nothing -> do
            exists <- lift $ DB.queryNullPoolRewardExists rwdDb
            if exists then pure Nothing else pure (Just rwdDb)

    queryPool :: (MonadBaseControl IO m, MonadIO m)
              => Strict.Maybe Generic.StakePoolKeyHash -> ExceptT SyncNodeError (ReaderT SqlBackend m) (Maybe DB.PoolHashId)
    queryPool Strict.Nothing = pure Nothing
    queryPool (Strict.Just poolHash) =
      Just <$> liftLookupFail "insertRewards.queryPoolKeyWithCache" (queryPoolKeyWithCache cache CacheNew poolHash)

insertPoolDepositRefunds
    :: (MonadBaseControl IO m, MonadIO m)
    => SyncEnv -> Generic.Rewards
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertPoolDepositRefunds env refunds = do
    insertRewards (Generic.rwdEpoch refunds) (Generic.rwdEpoch refunds) (envCache env) (Map.toList rwds)
    liftIO . logInfo tracer $ "Inserted " <> show (Generic.elemCount refunds) <> " deposit refund rewards"
  where
    tracer = getTrace env
    rwds = Generic.rwdRewards refunds

sumRewardTotal :: Map Generic.StakeCred (Set Generic.Reward) -> Shelley.Coin
sumRewardTotal =
    Shelley.Coin . Map.foldl' sumCoin 0
  where
    sumCoin :: Integer -> Set Generic.Reward -> Integer
    sumCoin !acc sr =
      acc + sum (map (Shelley.unCoin . Generic.rewardAmount) $ Set.toList sr)
