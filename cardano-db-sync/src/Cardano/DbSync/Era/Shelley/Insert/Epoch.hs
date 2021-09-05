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
  ( insertEpochInterleaved
  , postEpochRewards
  , postEpochStake
  , flushBulkOperation
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logInfo)

import qualified Cardano.Db as DB

import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import           Cardano.DbSync.Era.Shelley.Query

import qualified Cardano.Ledger.Coin as Shelley

import qualified Cardano.Sync.Era.Shelley.Generic as Generic
import           Cardano.Sync.Error
import           Cardano.Sync.LedgerState
import           Cardano.Sync.Types
import           Cardano.Sync.Util

import           Cardano.Slotting.Slot (EpochNo (..))

import           Control.Monad.Class.MonadSTM.Strict (flushTBQueue, readTVar, writeTBQueue,
                   writeTVar)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Except.Extra (hoistEither)

import           Data.List.Split.Internals (chunksOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Database.Persist.Sql (SqlBackend)


{- HLINT ignore "Use readTVarIO" -}

insertEpochInterleaved
     :: (MonadBaseControl IO m, MonadIO m)
     => Trace IO Text -> BulkOperation
     -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertEpochInterleaved tracer bop =
    case bop of
      BulkRewardChunk epochNo _ icache rwds ->
        insertRewards epochNo icache rwds
      BulkRewardReport epochNo _ rewardCount total -> do
        liftIO $ reportRewards epochNo rewardCount
        lift $ insertEpochRewardTotalReceived epochNo total
      BulkStakeDistChunk epochNo _ icache sDistChunk ->
        insertEpochStake tracer icache epochNo sDistChunk
      BulkStakeDistReport epochNo _ count ->
        liftIO $ reportStakeDist epochNo count
  where
    reportStakeDist :: EpochNo -> Int -> IO ()
    reportStakeDist epochNo count =
      logInfo tracer $
        mconcat
          [ "insertEpochInterleaved: Epoch ", textShow (unEpochNo epochNo)
          , ", ", textShow count, " stake addresses"
          ]

    reportRewards :: EpochNo -> Int -> IO ()
    reportRewards epochNo rewardCount =
      logInfo tracer $
        mconcat
          [ "insertEpochInterleaved: Epoch ", textShow (unEpochNo epochNo)
          , ", ", textShow rewardCount, " rewards"
          ]

postEpochRewards
     :: (MonadBaseControl IO m, MonadIO m)
     => LedgerEnv -> Generic.Rewards -> CardanoPoint
     -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
postEpochRewards lenv rwds point = do
  icache <- lift $ updateIndexCache lenv (Generic.rewardsStakeCreds rwds) (Generic.rewardsPoolHashKeys rwds)
  liftIO . atomically $ do
    let epochNo = Generic.rwdEpoch rwds
    forM_ (chunksOf 1000 $ Map.toList (Generic.rwdRewards rwds)) $ \rewardChunk ->
      writeTBQueue (leBulkOpQueue lenv) $ BulkRewardChunk epochNo point icache rewardChunk
    writeTBQueue (leBulkOpQueue lenv) $
      BulkRewardReport epochNo point (length $ Generic.rwdRewards rwds) (sumRewardTotal $ Generic.rwdRewards rwds)

postEpochStake
     :: (MonadBaseControl IO m, MonadIO m)
     => LedgerEnv -> Generic.StakeDist -> CardanoPoint
     -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
postEpochStake lenv smap point = do
  icache <- lift $ updateIndexCache lenv (Generic.stakeDistStakeCreds smap) (Generic.stakeDistPoolHashKeys smap)
  liftIO . atomically $ do
    let epochNo = Generic.sdistEpochNo smap
    forM_ (chunksOf 1000 $ Map.toList (Generic.sdistStakeMap smap)) $ \stakeChunk ->
      writeTBQueue (leBulkOpQueue lenv) $ BulkStakeDistChunk epochNo point icache stakeChunk
    writeTBQueue (leBulkOpQueue lenv) $ BulkStakeDistReport epochNo point (length $ Generic.sdistStakeMap smap)

flushBulkOperation
     :: (MonadBaseControl IO m, MonadIO m)
     => LedgerEnv
     -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
flushBulkOperation lenv = do
    bops <- liftIO $ atomically $ flushTBQueue (leBulkOpQueue lenv)
    unless (null bops) $
      liftIO $ logInfo (leTrace lenv) $ mconcat
        ["Flushing remaining ", show (length bops), " BulkOperations"]
    mapM_ (insertEpochInterleaved (leTrace lenv)) bops

-- -------------------------------------------------------------------------------------------------

insertEpochRewardTotalReceived
    :: (MonadBaseControl IO m, MonadIO m)
    => EpochNo -> Shelley.Coin
    -> ReaderT SqlBackend m ()
insertEpochRewardTotalReceived epochNo total =
  void . DB.insertEpochRewardTotalReceived $
    DB.EpochRewardTotalReceived
      { DB.epochRewardTotalReceivedEarnedEpoch = unEpochNo epochNo
      , DB.epochRewardTotalReceivedAmount = Generic.coinToDbLovelace total
      }

insertEpochStake
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> IndexCache -> EpochNo
    -> [(Generic.StakeCred, (Shelley.Coin, PoolKeyHash))]
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertEpochStake _tracer icache epochNo stakeChunk = do
    dbStakes <- mapM mkStake stakeChunk
    lift $ DB.insertManyEpochStakes dbStakes
  where
    mkStake
        :: MonadBaseControl IO m
        => (Generic.StakeCred, (Shelley.Coin, PoolKeyHash))
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) DB.EpochStake
    mkStake (saddr, (coin, pool)) = do
      saId <- hoistEither $ lookupStakeAddrIdPair "insertEpochStake StakeCred" saddr icache
      poolId <- hoistEither $ lookupPoolIdPair "insertEpochStake PoolKeyHash" pool icache
      pure $
        DB.EpochStake
          { DB.epochStakeAddrId = saId
          , DB.epochStakePoolId = poolId
          , DB.epochStakeAmount = Generic.coinToDbLovelace coin
          , DB.epochStakeEpochNo = unEpochNo epochNo -- The epoch where this delegation becomes valid.
          }

insertRewards
    :: (MonadBaseControl IO m, MonadIO m)
    => EpochNo -> IndexCache -> [(Generic.StakeCred, Set Generic.Reward)]
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertRewards epoch icache rewardsChunk = do
    dbRewards <- concatMapM mkRewards rewardsChunk
    lift $ DB.insertManyRewards dbRewards
  where
    mkRewards
        :: MonadBaseControl IO m
        => (Generic.StakeCred, Set Generic.Reward)
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) [DB.Reward]
    mkRewards (saddr, rset) = do
      saId <- hoistEither $ lookupStakeAddrIdPair "insertRewards StakePool" saddr icache
      forM (Set.toList rset) $ \ rwd ->
        pure $ DB.Reward
                  { DB.rewardAddrId = saId
                  , DB.rewardType = Generic.rewardSource rwd
                  , DB.rewardAmount = Generic.coinToDbLovelace (Generic.rewardAmount rwd)
                  , DB.rewardEarnedEpoch = unEpochNo epoch
                  , DB.rewardSpendableEpoch = 2 + unEpochNo epoch
                  , DB.rewardPoolId = lookupPoolIdPairMaybe (Generic.rewardPool rwd) icache
                  }

-- -------------------------------------------------------------------------------------------------

lookupStakeAddrIdPair
    :: Text -> Generic.StakeCred -> IndexCache
    -> Either SyncNodeError DB.StakeAddressId
lookupStakeAddrIdPair msg scred lcache =
    maybe errMsg Right $ Map.lookup scred (icAddressCache lcache)
  where
    errMsg :: Either SyncNodeError a
    errMsg =
      Left . NEError $
        mconcat [ "lookupStakeAddrIdPair: ", msg, renderByteArray (Generic.unStakeCred scred) ]


lookupPoolIdPairMaybe
    :: Maybe PoolKeyHash -> IndexCache
    -> Maybe DB.PoolHashId
lookupPoolIdPairMaybe mpkh lcache =
    lookup =<< mpkh
  where
    lookup :: PoolKeyHash -> Maybe DB.PoolHashId
    lookup pkh = Map.lookup pkh $ icPoolCache lcache

lookupPoolIdPair
    :: Text -> PoolKeyHash -> IndexCache
    -> Either SyncNodeError DB.PoolHashId
lookupPoolIdPair msg pkh lcache =
    maybe errMsg Right $ Map.lookup pkh (icPoolCache lcache)
  where
    errMsg :: Either SyncNodeError a
    errMsg =
      Left . NEError $
        mconcat [ "lookupPoolIdPair: ", msg, renderByteArray (Generic.unKeyHashRaw pkh) ]

-- -------------------------------------------------------------------------------------------------

updateIndexCache
    :: (MonadBaseControl IO m, MonadIO m)
    => LedgerEnv -> Set Generic.StakeCred -> Set PoolKeyHash
    -> ReaderT SqlBackend m IndexCache
updateIndexCache lenv screds pkhs = do
    oldCache <- liftIO . atomically $ readTVar (leIndexCache lenv)
    newIndexCache <- createNewCache oldCache
    liftIO . atomically $ writeTVar (leIndexCache lenv) newIndexCache
    pure newIndexCache
  where
    createNewCache
        :: (MonadBaseControl IO m, MonadIO m)
        => IndexCache -> ReaderT SqlBackend m IndexCache
    createNewCache oldCache = do
      newAddresses <- newAddressCache (icAddressCache oldCache)
      newPools <- newPoolCache (icPoolCache oldCache)
      pure $ IndexCache
                { icAddressCache = newAddresses
                , icPoolCache = newPools
                }

    newAddressCache
        :: (MonadBaseControl IO m, MonadIO m)
        => Map Generic.StakeCred DB.StakeAddressId
        -> ReaderT SqlBackend m (Map Generic.StakeCred DB.StakeAddressId)
    newAddressCache oldMap = do
      let reduced = Map.restrictKeys oldMap screds
          newCreds = Set.filter (`Map.notMember` reduced) screds
      newPairs <- catMaybes <$> mapM queryStakeAddressIdPair (Set.toList newCreds)
      pure $ Map.union reduced (Map.fromList newPairs)

    newPoolCache
        :: (MonadBaseControl IO m, MonadIO m)
        => Map PoolKeyHash DB.PoolHashId
        -> ReaderT SqlBackend m (Map PoolKeyHash DB.PoolHashId)
    newPoolCache oldMap = do
      let reduced = Map.restrictKeys oldMap pkhs
          newPkhs = Set.filter (`Map.notMember` reduced) pkhs
      newPairs <- catMaybes <$> mapM queryPoolHashIdPair (Set.toList newPkhs)
      pure $ Map.union reduced (Map.fromList newPairs)

sumRewardTotal :: Map Generic.StakeCred (Set Generic.Reward) -> Shelley.Coin
sumRewardTotal =
    Shelley.Coin . Map.foldl' sumCoin 0
  where
    sumCoin :: Integer -> Set Generic.Reward -> Integer
    sumCoin !acc sr =
      acc + sum (map (Shelley.unCoin . Generic.rewardAmount) $ Set.toList sr)
