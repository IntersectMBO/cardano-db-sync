{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Era.Shelley.Adjust
  ( adjustEpochRewards
  ) where

import           Cardano.Prelude hiding (from, groupBy, on)

import           Cardano.BM.Trace (Trace, logInfo)

import qualified Cardano.Db as Db

import           Cardano.Sync.Util

import           Cardano.Slotting.Block (BlockNo (..))
import           Cardano.Slotting.Slot (EpochNo (..))

import           Control.Monad.Extra (mapMaybeM)
import           Control.Monad.Trans.Control (MonadBaseControl)

import           Database.Esqueleto.Legacy (InnerJoin (..), Value (..), delete, from, groupBy, in_,
                   max_, on, select, sum_, unValue, val, valList, where_, (==.), (>=.), (^.))

import           Database.Persist.Sql (SqlBackend)


-- This is a hack/workaround for an issue related to the `Reward` table. The problem is as
-- follows:
--
-- * Rewards for epoch `N` are made available by ledger state towards the end of epoch `N + 1`
--   at which time they are inserted into the database in chunks of say 1000 so interleaved
--   between the insertion of regular block data.
-- * If the stake address for the reward is de-registered after they are extracted from the
--   ledger state but before the end of epoch `N + 1` (and not re-registered) then they should
--   have been orphaned *instead* of being added to the `Reward` table.
--
--   To fix this, we call this function at the start of the epoch `N + 2`, find all the stake
--   addresses that were de-registered (and not re-registered) in epoch `N - 1` and delete any
--   `Reward` table entries destined for that stake address.

adjustEpochRewards
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> EpochNo
    -> ReaderT SqlBackend m ()
adjustEpochRewards tracer epochNo = do
  addrs <- queryOrphanedAddrs epochNo
  unless (null addrs) $ do
    ada <- queryOrphanedRewardAmount epochNo addrs
    liftIO . logInfo tracer $ mconcat
                [ "adjustEpochRewards: epoch ", textShow (unEpochNo epochNo), ", "
                , textShow (length addrs), " orphaned rewards removed ("
                , textShow ada, " ADA)"
                ]
    -- liftIO . logInfo tracer $ "adjustEpochRewards: " <> textShow (sort $ map (unSqlBackendKey . Db.unStakeAddressKey) addrs)
    deleteOrphanedRewards addrs

-- ------------------------------------------------------------------------------------------------

-- TODO: When we know this is correct, the query and the delete should be composed so that
-- the list of StakeAddressIds does not need to be returned to Haskell land.

deleteOrphanedRewards :: MonadIO m => [Db.StakeAddressId] -> ReaderT SqlBackend m ()
deleteOrphanedRewards xs =
  delete . from $ \ rwd ->
    where_ (rwd ^. Db.RewardAddrId `in_` valList xs)

-- TODO: This query is slow and inefficient. Need to replace it with something better.
-- Find all stake addresses that have been de-registered in the specified epoch and not
-- re-registered in the same epoch.
queryOrphanedAddrs :: MonadIO m => EpochNo -> ReaderT SqlBackend m [Db.StakeAddressId]
queryOrphanedAddrs (EpochNo epochNo) =
    mapMaybeM queryReregs =<< queryDeregs
  where
    queryDeregs :: MonadIO m => ReaderT SqlBackend m [(BlockNo, Db.StakeAddressId)]
    queryDeregs = do
        res <- select . from $ \ (sa `InnerJoin` dereg `InnerJoin` tx `InnerJoin` blk) -> do
                   on (tx ^. Db.TxBlockId ==. blk ^. Db.BlockId)
                   on (dereg ^. Db.StakeDeregistrationTxId ==. tx ^. Db.TxId)
                   on (sa ^. Db.StakeAddressId ==. dereg ^. Db.StakeDeregistrationAddrId)
                   where_ (dereg ^. Db.StakeDeregistrationEpochNo ==. val (epochNo + 1))
                   groupBy (sa ^. Db.StakeAddressId)
                   pure (max_ (blk ^. Db.BlockBlockNo), sa ^. Db.StakeAddressId)
        pure $ mapMaybe convertDeregs res

    convertDeregs :: (Value (Maybe (Maybe Word64)), Value Db.StakeAddressId) -> Maybe (BlockNo, Db.StakeAddressId)
    convertDeregs (Value mmw, Value saId) = (\ w -> Just (BlockNo w, saId)) =<< join mmw

    queryReregs :: MonadIO m => (BlockNo, Db.StakeAddressId) -> ReaderT SqlBackend m (Maybe Db.StakeAddressId)
    queryReregs (BlockNo blockNo,  saId) = do
        res <- select . from $ \ (sa `InnerJoin` reg `InnerJoin` tx `InnerJoin` blk) -> do
                   on (tx ^. Db.TxBlockId ==. blk ^. Db.BlockId)
                   on (reg ^. Db.StakeRegistrationTxId ==. tx ^. Db.TxId)
                   on (sa ^. Db.StakeAddressId ==. reg ^. Db.StakeRegistrationAddrId)
                   where_ (reg ^. Db.StakeRegistrationAddrId ==. val saId)
                   where_ (reg ^. Db.StakeRegistrationEpochNo ==. val (epochNo + 1))
                   pure (max_ $ blk ^. Db.BlockBlockNo)
        pure $ case join (unValue =<< listToMaybe res) of
                Nothing -> Just saId
                Just blkNo -> if blkNo < blockNo
                                then Just saId
                                else Nothing

queryOrphanedRewardAmount :: MonadIO m => EpochNo -> [Db.StakeAddressId] -> ReaderT SqlBackend m Db.Ada
queryOrphanedRewardAmount (EpochNo epochNo) xs = do
  res <- select . from $ \ rwd -> do
            where_ (rwd ^. Db.RewardEarnedEpoch >=. val epochNo)
            where_ (rwd ^. Db.RewardAddrId `in_` valList xs)
            pure (sum_ $ rwd ^. Db.RewardAmount)
  pure $ Db.unValueSumAda (listToMaybe res)
