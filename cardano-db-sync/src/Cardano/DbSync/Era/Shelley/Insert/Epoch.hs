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
import           Cardano.DbSync.Era.Util (liftLookupFail)

import qualified Cardano.Sync.Era.Shelley.Generic as Generic
import           Cardano.Sync.Error
import           Cardano.Sync.Util

import           Cardano.Slotting.Slot (EpochNo (..))

import           Control.Monad.Trans.Control (MonadBaseControl)

import           Data.List.Split.Internals (chunksOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Database.Persist.Sql (SqlBackend, putMany)

import           Ouroboros.Consensus.Cardano.Block (StandardCrypto)

import qualified Shelley.Spec.Ledger.Coin as Shelley
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.Rewards as Shelley


insertEpochRewards
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Generic.Rewards
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertEpochRewards tracer rwds = do
  insertRewards tracer epochNo (Generic.rwdRewards rwds)
  insertOrphanedRewards tracer epochNo (Generic.rwdOrphaned rwds)
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
    => Trace IO Text -> Generic.StakeDist
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertEpochStake tracer smap = do
    forM_ (chunksOf 1000 $ Map.toList (Generic.sdistStakeMap smap)) $ \stakeChunk -> do
      dbStakes <- mapM mkStake stakeChunk
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
        :: (MonadBaseControl IO m, MonadIO m)
        => (Generic.StakeCred, (Shelley.Coin, Shelley.KeyHash 'Shelley.StakePool StandardCrypto))
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) DB.EpochStake
    mkStake (saddr, (coin, _)) = do
      (saId, poolId) <- liftLookupFail "insertEpochStake" $ queryStakeAddressAndPool epoch (Generic.unStakeCred saddr)
      pure $
        DB.EpochStake
          { DB.epochStakeAddrId = saId
          , DB.epochStakePoolId = poolId
          , DB.epochStakeAmount = Generic.coinToDbLovelace coin
          , DB.epochStakeEpochNo = epoch -- The epoch where this delegation becomes valid.
          }

-- -----------------------------------------------------------------------------

insertRewards
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> EpochNo -> Map Generic.StakeCred (Set (Shelley.Reward StandardCrypto))
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertRewards _tracer epoch rewards = do
    forM_ (chunksOf 1000 $ Map.toList rewards) $ \rewardsChunk -> do
      dbRewards <- concatMapM mkRewards rewardsChunk
      lift $ putMany dbRewards
  where
    mkRewards
        :: (MonadBaseControl IO m, MonadIO m)
        => (Generic.StakeCred, Set (Shelley.Reward StandardCrypto))
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) [DB.Reward]
    mkRewards (saddr, rset) = do
      saId <- liftLookupFail "insertReward StakeAddress" $ queryStakeAddress (Generic.unStakeCred saddr)
      forM (Set.toList rset) $ \ rwd -> do
        poolId <- liftLookupFail "insertReward StakePool" $ queryStakePoolKeyHash (Shelley.rewardPool rwd)
        pure $ DB.Reward
                  { DB.rewardAddrId = saId
                  , DB.rewardType = DB.showRewardType (Shelley.rewardType rwd)
                  , DB.rewardAmount = Generic.coinToDbLovelace (Shelley.rewardAmount rwd)
                  , DB.rewardEpochNo = unEpochNo epoch
                  , DB.rewardPoolId = poolId
                  }

insertOrphanedRewards
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> EpochNo -> Map Generic.StakeCred (Set (Shelley.Reward StandardCrypto))
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertOrphanedRewards _tracer epoch orphanedRewards =
    -- There are probably not many of these for each epoch, but just in case there
    -- are, it does not hurt to chunk them.
    forM_ (chunksOf 1000 $ Map.toList orphanedRewards) $ \orphanedRewardsChunk -> do
      dbRewards <- concatMapM mkOrphanedReward orphanedRewardsChunk
      lift $ putMany dbRewards
  where
    mkOrphanedReward
        :: (MonadBaseControl IO m, MonadIO m)
        => (Generic.StakeCred, Set (Shelley.Reward StandardCrypto))
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) [DB.OrphanedReward]
    mkOrphanedReward (saddr, rset) = do
      saId <- liftLookupFail "insertReward StakeAddress" $ queryStakeAddress (Generic.unStakeCred saddr)
      forM (Set.toList rset) $ \ rwd -> do
        poolId <- liftLookupFail "insertReward StakePool" $ queryStakePoolKeyHash (Shelley.rewardPool rwd)
        pure $ DB.OrphanedReward
                  { DB.orphanedRewardAddrId = saId
                  , DB.orphanedRewardType = DB.showRewardType (Shelley.rewardType rwd)
                  , DB.orphanedRewardAmount = Generic.coinToDbLovelace (Shelley.rewardAmount rwd)
                  , DB.orphanedRewardEpochNo = unEpochNo epoch
                  , DB.orphanedRewardPoolId = poolId
                  }
