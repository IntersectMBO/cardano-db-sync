{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Universal.Adjust (
  adjustEpochRewards,
) where

import Cardano.BM.Trace (Trace, logInfo)
import qualified Cardano.Db as DB
import Cardano.DbSync.Cache (
  queryPoolKeyWithCache,
  queryStakeAddrWithCache,
 )
import Cardano.DbSync.Cache.Types (CacheAction (..), CacheStatus)
import qualified Cardano.DbSync.Era.Shelley.Generic.Rewards as Generic
import Cardano.DbSync.Types (StakeCred)
import Cardano.Ledger.BaseTypes (Network)
import Cardano.Prelude hiding (from, groupBy, on)
import Cardano.Slotting.Slot (EpochNo (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (unzip4)
import Data.List.Extra (chunksOf)

-- Hlint warns about another version of this operator.
{- HLINT ignore "Redundant ^." -}

-- This is a hack/workaround for an issue related to the `Reward` table.
--
-- Reward payments (either staking rewards or MIR payments) can be made to a valid stake address
-- which is then deregistered before the payment is actually paid out (which happens at the epoch
-- boundary). To fix this, at the start of the epoch we find all the stake addresses which have
-- been de-registered and not reregistered and then delete all rewards for those addresses and that
-- epoch.

-- Update the adjustEpochRewards function to use bulk operations
adjustEpochRewards ::
  MonadIO m =>
  Trace IO Text ->
  Network ->
  CacheStatus ->
  EpochNo ->
  Generic.Rewards ->
  Set StakeCred ->
  DB.DbAction m ()
adjustEpochRewards trce nw cache epochNo rwds creds = do
  let rewardsToDelete =
        [ (cred, rwd)
        | (cred, rewards) <- Map.toList $ Generic.unRewards rwds
        , rwd <- Set.toList rewards
        ]
  liftIO . logInfo trce $
    mconcat
      [ "Removing "
      , if null rewardsToDelete then "0" else textShow (length rewardsToDelete) <> " rewards and "
      , show (length creds)
      , " orphaned rewards"
      ]

  -- Process rewards in batches
  unless (null rewardsToDelete) $ do
    forM_ (chunksOf maxBatchSize rewardsToDelete) $ \batch -> do
      params <- prepareRewardsForDeletion trce nw cache epochNo batch
      unless (areParamsEmpty params) $
        DB.deleteRewardsBulk params

  -- Handle orphaned rewards in batches too
  crds <- catMaybes <$> forM (Set.toList creds) (queryStakeAddrWithCache trce cache DoNotUpdateCache nw)
  forM_ (chunksOf maxBatchSize crds) $ \batch ->
    DB.deleteOrphanedRewardsBulk (unEpochNo epochNo) batch

prepareRewardsForDeletion ::
  MonadIO m =>
  Trace IO Text ->
  Network ->
  CacheStatus ->
  EpochNo ->
  [(StakeCred, Generic.Reward)] ->
  DB.DbAction m ([DB.StakeAddressId], [DB.RewardSource], [Word64], [DB.PoolHashId])
prepareRewardsForDeletion trce nw cache epochNo rewards = do
  -- Process each reward to get parameter tuples
  rewardParams <- forM rewards $ \(cred, rwd) -> do
    mAddrId <- queryStakeAddrWithCache trce cache DoNotUpdateCache nw cred
    eiPoolId <- queryPoolKeyWithCache cache DoNotUpdateCache (Generic.rewardPool rwd)
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

-- Add this helper function
areParamsEmpty :: ([a], [b], [c], [d]) -> Bool
areParamsEmpty (as, bs, cs, ds) = null as || null bs || null cs || null ds

maxBatchSize :: Int
maxBatchSize = 10000
