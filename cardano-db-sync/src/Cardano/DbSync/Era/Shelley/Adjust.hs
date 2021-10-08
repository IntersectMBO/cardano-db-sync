{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Era.Shelley.Adjust
  ( adjustEpochRewards
  ) where

import           Cardano.Prelude hiding (from, groupBy, on)

import           Cardano.BM.Trace (Trace, logInfo)

import qualified Cardano.Db as Db

import           Cardano.DbSync.Util

import           Cardano.Slotting.Slot (EpochNo (..))

import           Control.Monad.Trans.Control (MonadBaseControl)

import qualified Data.List.Extra as List

import           Database.Esqueleto.Legacy (InnerJoin (..), delete, from, in_, notExists, on,
                   select, val, valList, where_, (&&.), (==.), (>.), (^.))

import           Database.Persist.Sql (SqlBackend)

-- Hlint warns about another version of this operator.
{- HLINT ignore "Redundant ^." -}

-- This is a hack/workaround for an issue related to the `Reward` table.
--
-- Reward payments (either staking rewards or MIR payments) can be made to a valid stake address
-- which is then deregistered before the payment is actually paid out (which happens at the epoch
-- boundary). To fix this, at the start of the epoch we find all the stake addresses which have
-- been de-registered and not reregistered and then delete all rewards for those addresses and that
-- epoch.

adjustEpochRewards
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> EpochNo
    -> ReaderT SqlBackend m ()
adjustEpochRewards tracer epochNo = do
  when (epochNo >= 2) $ do
    (addrs, ada) <- queryOrphanedRewards epochNo
    unless (null addrs) $ do
      liftIO . logInfo tracer $ mconcat
                  [ "adjustEpochRewards: starting epoch ", textShow (unEpochNo epochNo), ", "
                  , textShow (length addrs), " orphaned rewards removed ("
                  , textShow ada, " ADA)"
                  ]
      deleteOrphanedRewards epochNo addrs

-- ------------------------------------------------------------------------------------------------

deleteOrphanedRewards :: MonadIO m => EpochNo -> [Db.StakeAddressId] -> ReaderT SqlBackend m ()
deleteOrphanedRewards (EpochNo epochNo) xs =
  delete . from $ \ rwd -> do
    where_ (rwd ^. Db.RewardSpendableEpoch ==. val epochNo)
    where_ (rwd ^. Db.RewardAddrId `in_` valList xs)

-- Uses TxId as a proxy for BlockNo.
queryOrphanedRewards :: MonadIO m => EpochNo -> ReaderT SqlBackend m ([Db.StakeAddressId], Db.Ada)
queryOrphanedRewards (EpochNo epochNo) = do
    -- Get payments to addresses that have never been registered.
    res1 <- select . from $ \ rwd -> do
              where_ (rwd ^. Db.RewardSpendableEpoch ==. val epochNo)
              where_ (notExists . from $ \reg ->
                where_ (reg ^. Db.StakeRegistrationAddrId ==. rwd ^. Db.RewardAddrId))
              pure (rwd ^. Db.RewardAddrId, rwd ^. Db.RewardAmount)
    -- Get payments to addresses that have been registered but are now deregistered.
    res2 <- select . from $ \ (dereg `InnerJoin` rwd) -> do
              on (dereg ^. Db.StakeDeregistrationAddrId ==. rwd ^. Db.RewardAddrId)
              where_ (rwd ^. Db.RewardSpendableEpoch ==. val epochNo)
              where_ (notExists . from $ \reg ->
                where_ (reg ^. Db.StakeRegistrationAddrId ==. dereg ^. Db.StakeDeregistrationAddrId
                          &&. reg ^. Db.StakeRegistrationTxId >. dereg ^. Db.StakeDeregistrationTxId))
              pure (dereg ^. Db.StakeDeregistrationAddrId, rwd ^. Db.RewardAmount)
    pure $ convert (map Db.unValue2 $ res1 ++ res2)
  where
    convert :: [(Db.StakeAddressId, Db.DbLovelace)] -> ([Db.StakeAddressId], Db.Ada)
    convert xs = (List.nubOrd (map fst xs), Db.word64ToAda . sum $ map (Db.unDbLovelace . snd) xs)
