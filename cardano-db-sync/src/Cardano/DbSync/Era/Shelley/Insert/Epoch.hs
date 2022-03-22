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
  ( finalizeEpochBulkOps
  , forceInsertRewards
  , isEmptyEpochBulkOps
  , insertEpochInterleaved
  , insertPoolDepositRefunds
  , postEpochRewards
  , insertStakeSlice
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logInfo, logWarning)

import qualified Cardano.Db as DB

import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import           Cardano.DbSync.Era.Shelley.Query

import qualified Cardano.Ledger.Coin as Shelley

import           Cardano.DbSync.Error
import           Cardano.DbSync.LedgerState
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import           Cardano.Slotting.Slot (EpochNo (..))

import           Control.Monad.Class.MonadSTM.Strict (StrictTVar, flushTBQueue, isEmptyTBQueue,
                   readTVar, readTVarIO, writeTBQueue, writeTVar)
import           Control.Monad.Extra (mapMaybeM)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Except.Extra (hoistEither)

import           Data.List.Split.Internals (chunksOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Database.Persist.Sql (SqlBackend)

{- HLINT ignore "Use readTVarIO" -}

finalizeEpochBulkOps
     :: (MonadBaseControl IO m, MonadIO m)
     => LedgerEnv
     -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
finalizeEpochBulkOps lenv = do
  bops <- liftIO $ atomically $ flushTBQueue (leBulkOpQueue lenv)
  unless (null bops) $
    liftIO $ logInfo (leTrace lenv) $ mconcat
      ["Flushing remaining ", show (length bops), " BulkOperations"]
  mapM_ (insertEpochInterleaved (leTrace lenv)) bops

-- | This gets called with the full set of rewards. If there are no blocks produced in the last 20%
-- of the slots within an epoch, the rewards will not be updated in the normal way so they will be
-- inserted here.
forceInsertRewards
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> LedgerEnv -> Generic.Rewards -> ReaderT SqlBackend m ()
forceInsertRewards tracer lenv rwds = do
  let mapSize = Generic.elemCount rwds
  count <- fromIntegral <$> DB.queryEpochRewardCount (unEpochNo $ Generic.rwdEpoch rwds)
  when (mapSize > count) $ do
    liftIO . logWarning tracer $ mconcat
                                [ "forceInsertRewards: ", textShow mapSize, " rewards for epoch "
                                , textShow (unEpochNo $ Generic.rwdEpoch rwds), " is "
                                , textShow (Generic.totalAda rwds), " ADA"
                                ]
    icache <- updateIndexCache lenv (Generic.rewardsStakeCreds rwds) (Generic.rewardsPoolHashKeys rwds)
    res <- runExceptT $ insertRewards (Generic.rwdEpoch rwds - 2) icache (Map.toList $ Generic.rwdRewards rwds)
    case res of
      Left err -> liftIO . logWarning tracer $ mconcat [ "forceInsertRewards: ", renderSyncNodeError err ]
      Right () -> DB.transactionCommit

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
  where
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

isEmptyEpochBulkOps
     :: MonadIO m
     => LedgerEnv
     -> ExceptT SyncNodeError (ReaderT SqlBackend m) Bool
isEmptyEpochBulkOps lenv =
  liftIO . atomically $ isEmptyTBQueue (leBulkOpQueue lenv)

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

insertStakeSlice
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> StrictTVar IO IndexCache -> Generic.StakeSliceRes
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertStakeSlice _ _ Generic.NoSlices = pure ()
insertStakeSlice tracer cacheVar (Generic.Slice slice finalSlice) = do
    cache <- liftIO $ readTVarIO cacheVar
    -- cache TVar is not updated. We just use a slice here.
    cacheSlice <- lift $ modifyCache (Generic.stakeDistStakeCreds slice) (Generic.stakeDistPoolHashKeys slice) cache
    insertEpochStake cacheSlice (Generic.sliceEpochNo slice) (Map.toList $ Generic.sliceDistr slice)
    when finalSlice $ do
      size <- lift $ DB.queryEpochStakeCount (unEpochNo $ Generic.sliceEpochNo slice)
      liftIO . logInfo tracer $ mconcat ["Inserted ", show size, " EpochStake for ", show (Generic.sliceEpochNo slice)]

insertEpochStake
    :: (MonadBaseControl IO m, MonadIO m)
    => IndexCache -> EpochNo
    -> [(Generic.StakeCred, (Shelley.Coin, Generic.StakePoolKeyHash))]
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertEpochStake icache epochNo stakeChunk = do
    dbStakes <- mapM mkStake stakeChunk
    lift $ DB.insertManyEpochStakes dbStakes
  where
    mkStake
        :: MonadBaseControl IO m
        => (Generic.StakeCred, (Shelley.Coin, Generic.StakePoolKeyHash))
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
        :: (MonadBaseControl IO m, MonadIO m)
        => (Generic.StakeCred, Set Generic.Reward)
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) [DB.Reward]
    mkRewards (saddr, rset) = do
      saId <- hoistEither $ lookupStakeAddrIdPair "insertRewards StakePool" saddr icache
      mapMaybeM (prepareReward saId) (Set.toList rset)

    -- For rewards with a null pool, the reward unique key doesn't work.
    -- So we need to manually check that it's not already in the db.
    -- This can happen on rollbacks.
    prepareReward
        :: (MonadBaseControl IO m, MonadIO m)
        => DB.StakeAddressId -> Generic.Reward
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) (Maybe DB.Reward)
    prepareReward saId rwd = do
        let rwdDb = DB.Reward
                    { DB.rewardAddrId = saId
                    , DB.rewardType = Generic.rewardSource rwd
                    , DB.rewardAmount = Generic.coinToDbLovelace (Generic.rewardAmount rwd)
                    , DB.rewardEarnedEpoch = earnedEpoch (Generic.rewardSource rwd)
                    , DB.rewardSpendableEpoch = spendableEpoch (Generic.rewardSource rwd)
                    , DB.rewardPoolId = lookupPoolIdPairMaybe (Generic.rewardPool rwd) icache
                    }
        case DB.rewardPoolId rwdDb of
          Just _ -> pure $ Just rwdDb
          Nothing -> do
            exists <- lift $ DB.queryNullPoolRewardExists rwdDb
            if exists then pure Nothing else pure (Just rwdDb)

    -- The earnedEpoch and spendableEpoch functions have been tweaked to match the logic of the ledger.
    earnedEpoch :: DB.RewardSource -> Word64
    earnedEpoch src =
      unEpochNo epoch +
        case src of
          DB.RwdMember -> 0
          DB.RwdLeader -> 0
          DB.RwdReserves -> 1
          DB.RwdTreasury -> 1
          DB.RwdDepositRefund -> 0

    spendableEpoch :: DB.RewardSource -> Word64
    spendableEpoch src =
      unEpochNo epoch +
        case src of
          DB.RwdMember -> 2
          DB.RwdLeader -> 2
          DB.RwdReserves -> 2
          DB.RwdTreasury -> 2
          DB.RwdDepositRefund -> 0

insertPoolDepositRefunds
    :: (MonadBaseControl IO m, MonadIO m)
    => LedgerEnv -> Generic.Rewards
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertPoolDepositRefunds lenv refunds = do
  icache <- lift $ updateIndexCache lenv (Generic.rewardsStakeCreds refunds) (Generic.rewardsPoolHashKeys refunds)
  insertRewards (Generic.rwdEpoch refunds) icache (Map.toList $ Generic.rwdRewards refunds)

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
    :: Maybe Generic.StakePoolKeyHash -> IndexCache
    -> Maybe DB.PoolHashId
lookupPoolIdPairMaybe mpkh lcache =
    lookup =<< mpkh
  where
    lookup :: Generic.StakePoolKeyHash -> Maybe DB.PoolHashId
    lookup pkh = Map.lookup pkh $ icPoolCache lcache

lookupPoolIdPair
    :: Text -> Generic.StakePoolKeyHash -> IndexCache
    -> Either SyncNodeError DB.PoolHashId
lookupPoolIdPair msg pkh lcache =
    maybe errMsg Right $ Map.lookup pkh (icPoolCache lcache)
  where
    errMsg :: Either SyncNodeError a
    errMsg =
      Left . NEError $
        mconcat [ "lookupPoolIdPair: ", msg, renderByteArray (Generic.unStakePoolKeyHash pkh) ]

-- -------------------------------------------------------------------------------------------------

updateIndexCache
    :: (MonadBaseControl IO m, MonadIO m)
    => LedgerEnv -> Set Generic.StakeCred -> Set Generic.StakePoolKeyHash
    -> ReaderT SqlBackend m IndexCache
updateIndexCache lenv screds pkhs = do
    oldCache <- liftIO . atomically $ readTVar (leIndexCache lenv)
    newIndexCache <- modifyCache screds pkhs oldCache
    liftIO . atomically $ writeTVar (leIndexCache lenv) newIndexCache
    pure newIndexCache

modifyCache
    :: (MonadBaseControl IO m, MonadIO m)
    => Set Generic.StakeCred -> Set Generic.StakePoolKeyHash
    -> IndexCache -> ReaderT SqlBackend m IndexCache
modifyCache screds pkhs oldCache = do
    newAddresses <- newAddressCache (icAddressCache oldCache)
    newPools <- newPoolCache (icPoolCache oldCache)
    pure $ IndexCache
              { icAddressCache = newAddresses
              , icPoolCache = newPools
              }
  where
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
        => Map Generic.StakePoolKeyHash DB.PoolHashId
        -> ReaderT SqlBackend m (Map Generic.StakePoolKeyHash DB.PoolHashId)
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
