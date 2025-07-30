{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Universal.Adjust (
  adjustEpochRewards,
) where

import Data.List (unzip4)
import Data.List.Extra (chunksOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Cardano.BM.Trace (logInfo)
import Cardano.Prelude hiding (from, groupBy, on)
import Cardano.Slotting.Slot (EpochNo (..))

import qualified Cardano.Db as DB
import Cardano.DbSync.Api (getTrace)
import Cardano.DbSync.Api.Types (SyncEnv)
import Cardano.DbSync.Cache (
  queryPoolKeyWithCache,
  queryStakeAddrWithCache,
 )
import Cardano.DbSync.Cache.Types (CacheAction (..))
import qualified Cardano.DbSync.Era.Shelley.Generic.Rewards as Generic
import Cardano.DbSync.Types (StakeCred)
import Cardano.DbSync.Util (maxBulkSize)
import Cardano.Ledger.BaseTypes (Network)

-- Hlint warns about another version of this operator.
{- HLINT ignore "Redundant ^." -}

-- This is a hack/workaround for an issue related to the `Reward` table.
--
-- Reward payments (either staking rewards or MIR payments) can be made to a valid stake address
-- which is then deregistered before the payment is actually paid out (which happens at the epoch
-- boundary). To fix this, at the start of the epoch we find all the stake addresses which have
-- been de-registered and not reregistered and then delete all rewards for those addresses and that
-- epoch.

adjustEpochRewards ::
  MonadIO m =>
  SyncEnv ->
  Network ->
  EpochNo ->
  Generic.Rewards ->
  Set StakeCred ->
  DB.DbAction m ()
adjustEpochRewards syncEnv nw epochNo rwds creds = do
  let rewardsToDelete =
        [ (cred, rwd)
        | (cred, rewards) <- Map.toList $ Generic.unRewards rwds
        , rwd <- Set.toList rewards
        ]
  liftIO . logInfo (getTrace syncEnv) $
    mconcat
      [ "Removing "
      , if null rewardsToDelete then "0" else textShow (length rewardsToDelete) <> " rewards and "
      , show (length creds)
      , " orphaned rewards"
      ]

  -- Process rewards in batches
  unless (null rewardsToDelete) $ do
    forM_ (chunksOf maxBulkSize rewardsToDelete) $ \batch -> do
      params <- prepareRewardsForDeletion syncEnv nw epochNo batch
      unless (areParamsEmpty params) $
        DB.deleteRewardsBulk params

  -- Handle orphaned rewards in batches
  crds <- catMaybes <$> forM (Set.toList creds) (queryStakeAddrWithCache syncEnv DoNotUpdateCache nw)
  forM_ (chunksOf maxBulkSize crds) $ \batch ->
    DB.deleteOrphanedRewardsBulk (unEpochNo epochNo) batch

prepareRewardsForDeletion ::
  MonadIO m =>
  SyncEnv ->
  Network ->
  EpochNo ->
  [(StakeCred, Generic.Reward)] ->
  DB.DbAction m ([DB.StakeAddressId], [DB.RewardSource], [Word64], [DB.PoolHashId])
prepareRewardsForDeletion syncEnv nw epochNo rewards = do
  -- Process each reward to get parameter tuples
  rewardParams <- forM rewards $ \(cred, rwd) -> do
    mAddrId <- queryStakeAddrWithCache syncEnv DoNotUpdateCache nw cred
    eiPoolId <- queryPoolKeyWithCache syncEnv DoNotUpdateCache (Generic.rewardPool rwd)
    pure $ case (mAddrId, eiPoolId) of
      (Just addrId, Right poolId) ->
        Just (addrId, Generic.rewardSource rwd, unEpochNo epochNo, poolId)
      _otherwise -> Nothing
  -- Filter out Nothings and extract parameters
  let validParams = catMaybes rewardParams
  -- Return the unzipped parameters, or empty lists if none are valid
  if null validParams
    then pure ([], [], [], [])
    else pure $ unzip4 validParams

areParamsEmpty :: ([DB.StakeAddressId], [DB.RewardSource], [Word64], [DB.PoolHashId]) -> Bool
areParamsEmpty (addrs, types, epochs, pools) =
  null addrs && null types && null epochs && null pools
