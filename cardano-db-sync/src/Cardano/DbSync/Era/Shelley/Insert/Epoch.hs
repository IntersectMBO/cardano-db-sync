{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Era.Shelley.Insert.Epoch
  ( insertEpochRewards
  , insertEpochStake
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logInfo)

import qualified Cardano.Db as DB

import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import           Cardano.DbSync.Era.Shelley.Query

import qualified Cardano.Sync.Era.Shelley.Generic as Generic
import           Cardano.Sync.Error
import           Cardano.Sync.LedgerState
import           Cardano.Sync.Types
import           Cardano.Sync.Util

import           Cardano.Slotting.Slot (EpochNo (..))

import           Control.Monad.Class.MonadSTM.Strict (readTVar, writeTVar)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Except.Extra (hoistEither)

import           Data.List.Split.Internals (chunksOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Database.Persist.Sql (SqlBackend, putMany)

import           Ouroboros.Consensus.Cardano.Block (StandardCrypto)

import qualified Shelley.Spec.Ledger.Coin as Shelley
import qualified Shelley.Spec.Ledger.Rewards as Shelley

{- HLINT ignore "Use readTVarIO" -}


insertEpochRewards
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> LedgerEnv -> Generic.Rewards
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertEpochRewards tracer lenv rwds = do
  lcache <- lift $ updateIndexCache lenv (Generic.rewardsStakeCreds rwds) (Generic.rewardsPoolHashKeys rwds)
  insertRewards tracer lcache epochNo (Generic.rwdRewards rwds)
  insertOrphanedRewards tracer lcache epochNo (Generic.rwdOrphaned rwds)
  liftIO . logInfo tracer $
    mconcat
      [ "insertEpochRewards: Epoch ", textShow (unEpochNo epochNo), ", "
      , textShow (length (Generic.rwdRewards rwds)), " rewards, "
      , textShow (length (Generic.rwdOrphaned rwds)), " orphaned_rewards"
      ]
  where
    epochNo :: EpochNo
    epochNo = Generic.rwdEpoch rwds

insertEpochStake
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> LedgerEnv -> Generic.StakeDist
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertEpochStake tracer lenv smap = do
    lcache <- lift $ updateIndexCache lenv (Generic.stakeDistStakeCreds smap) (Generic.stakeDistPoolHashKeys smap)
    forM_ (chunksOf 1000 $ Map.toList (Generic.sdistStakeMap smap)) $ \stakeChunk -> do
      dbStakes <- mapM (mkStake lcache) stakeChunk
      lift $ putMany dbStakes
    liftIO . logInfo tracer $
      mconcat
        [ "insertEpochStake: Epoch ", textShow (unEpochNo $ Generic.sdistEpochNo smap)
        , ", ", textShow (length $ Generic.sdistStakeMap smap), " stake addresses"
        ]
  where
    epoch :: Word64
    epoch = unEpochNo (Generic.sdistEpochNo smap)

    mkStake
        :: MonadBaseControl IO m
        => IndexCache
        -> (Generic.StakeCred, (Shelley.Coin, PoolKeyHash))
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) DB.EpochStake
    mkStake lcache (saddr, (coin, pool)) = do
      saId <- hoistEither $ lookupStakeAddrIdPair "insertRewards StakePool" saddr lcache
      poolId <- hoistEither $ lookupPoolIdPair "insertRewards StakePool" pool lcache
      pure $
        DB.EpochStake
          { DB.epochStakeAddrId = saId
          , DB.epochStakePoolId = poolId
          , DB.epochStakeAmount = Generic.coinToDbLovelace coin
          , DB.epochStakeEpochNo = epoch -- The epoch where this delegation becomes valid.
          }

-- -------------------------------------------------------------------------------------------------

insertRewards
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> IndexCache -> EpochNo -> Map Generic.StakeCred (Set (Shelley.Reward StandardCrypto))
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertRewards _tracer lcache epoch rewards = do
    forM_ (chunksOf 1000 $ Map.toList rewards) $ \rewardsChunk -> do
      dbRewards <- concatMapM mkRewards rewardsChunk
      lift $ putMany dbRewards
  where
    mkRewards
        :: MonadBaseControl IO m
        => (Generic.StakeCred, Set (Shelley.Reward StandardCrypto))
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) [DB.Reward]
    mkRewards (saddr, rset) = do
      saId <- hoistEither $ lookupStakeAddrIdPair "insertRewards StakePool" saddr lcache
      forM (Set.toList rset) $ \ rwd -> do
        poolId <- hoistEither $ lookupPoolIdPair "insertRewards StakePool" (Shelley.rewardPool rwd) lcache
        pure $ DB.Reward
                  { DB.rewardAddrId = saId
                  , DB.rewardType = DB.showRewardType (Shelley.rewardType rwd)
                  , DB.rewardAmount = Generic.coinToDbLovelace (Shelley.rewardAmount rwd)
                  , DB.rewardEpochNo = unEpochNo epoch
                  , DB.rewardPoolId = poolId
                  }

insertOrphanedRewards
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> IndexCache -> EpochNo -> Map Generic.StakeCred (Set (Shelley.Reward StandardCrypto))
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertOrphanedRewards _tracer lcache epoch orphanedRewards =
    -- There are probably not many of these for each epoch, but just in case there
    -- are, it does not hurt to chunk them.
    forM_ (chunksOf 1000 $ Map.toList orphanedRewards) $ \orphanedRewardsChunk -> do
      dbRewards <- concatMapM mkOrphanedReward orphanedRewardsChunk
      lift $ putMany dbRewards
  where
    mkOrphanedReward
        :: MonadBaseControl IO m
        => (Generic.StakeCred, Set (Shelley.Reward StandardCrypto))
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) [DB.OrphanedReward]
    mkOrphanedReward (saddr, rset) = do
      saId <- hoistEither $ lookupStakeAddrIdPair "insertRewards StakePool" saddr lcache
      forM (Set.toList rset) $ \ rwd -> do
        poolId <- hoistEither $ lookupPoolIdPair "insertRewards StakePool" (Shelley.rewardPool rwd) lcache
        pure $ DB.OrphanedReward
                  { DB.orphanedRewardAddrId = saId
                  , DB.orphanedRewardType = DB.showRewardType (Shelley.rewardType rwd)
                  , DB.orphanedRewardAmount = Generic.coinToDbLovelace (Shelley.rewardAmount rwd)
                  , DB.orphanedRewardEpochNo = unEpochNo epoch
                  , DB.orphanedRewardPoolId = poolId
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
