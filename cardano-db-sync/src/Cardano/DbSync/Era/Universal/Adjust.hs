{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Universal.Adjust (
  adjustEpochRewards,
) where

import Cardano.BM.Trace (Trace, logInfo)
import qualified Cardano.Db as Db
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
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Database.Esqueleto.Experimental (
  SqlBackend,
  delete,
  from,
  in_,
  table,
  val,
  valList,
  where_,
  (==.),
  (^.),
 )

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
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  Network ->
  CacheStatus ->
  EpochNo ->
  Generic.Rewards ->
  Set StakeCred ->
  ReaderT SqlBackend m ()
adjustEpochRewards trce nw cache epochNo rwds creds = do
  let eraIgnored = Map.toList $ Generic.unRewards rwds
  liftIO
    . logInfo trce
    $ mconcat
      [ "Removing "
      , if null eraIgnored then "" else textShow (length eraIgnored) <> " rewards and "
      , show (length creds)
      , " orphaned rewards"
      ]
  forM_ eraIgnored $ \(cred, rewards) ->
    forM_ (Set.toList rewards) $ \rwd ->
      deleteReward trce nw cache epochNo (cred, rwd)
  crds <- rights <$> forM (Set.toList creds) (queryStakeAddrWithCache trce cache DoNotUpdateCache nw)
  deleteOrphanedRewards epochNo crds

deleteReward ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  Network ->
  CacheStatus ->
  EpochNo ->
  (StakeCred, Generic.Reward) ->
  ReaderT SqlBackend m ()
deleteReward trce nw cache epochNo (cred, rwd) = do
  mAddrId <- queryStakeAddrWithCache trce cache DoNotUpdateCache nw cred
  eiPoolId <- queryPoolKeyWithCache cache DoNotUpdateCache (Generic.rewardPool rwd)
  case (mAddrId, eiPoolId) of
    (Right addrId, Right poolId) -> do
      delete $ do
        rwdDb <- from $ table @Db.Reward
        where_ (rwdDb ^. Db.RewardAddrId ==. val addrId)
        where_ (rwdDb ^. Db.RewardType ==. val (Generic.rewardSource rwd))
        where_ (rwdDb ^. Db.RewardSpendableEpoch ==. val (unEpochNo epochNo))
        where_ (rwdDb ^. Db.RewardPoolId ==. val poolId)
    _otherwise -> pure ()

deleteOrphanedRewards :: MonadIO m => EpochNo -> [Db.StakeAddressId] -> ReaderT SqlBackend m ()
deleteOrphanedRewards (EpochNo epochNo) xs =
  delete $ do
    rwd <- from $ table @Db.Reward
    where_ (rwd ^. Db.RewardSpendableEpoch ==. val epochNo)
    where_ (rwd ^. Db.RewardAddrId `in_` valList xs)
