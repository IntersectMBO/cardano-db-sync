{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Db.Mock.Unit.Conway.Reward (
  simpleRewards,
  rewardsShelley,
  rollbackBoundary,
) where

import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Mock.ChainSync.Server (IOManager (), addBlock, rollback)
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import qualified Cardano.Mock.Forging.Tx.Shelley as Shelley
import Cardano.Mock.Forging.Types (PoolIndex (..), StakeIndex (..), UTxOIndex (..))
import Cardano.Prelude
import Data.List (last)
import Ouroboros.Network.Block (blockPoint)
import Test.Cardano.Db.Mock.Config
import qualified Test.Cardano.Db.Mock.UnifiedApi as Api
import Test.Cardano.Db.Mock.Validate
import Test.Tasty.HUnit (Assertion ())
import Prelude ()

simpleRewards :: IOManager -> [(Text, Text)] -> Assertion
simpleRewards =
  withFullConfigAndDropDB conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with stake credentials
    void $ Api.registerAllStakeCreds interpreter mockServer
    -- Pools are not registered yet, this takes 2 epochs, so fees of this tx should not
    -- create any rewards
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 1000 1000 0
    -- Fill up epochs
    epochs <- Api.fillEpochs interpreter mockServer 3

    -- Wait for it to sync
    assertBlockNoBackoff dbSync (2 + length epochs)
    -- Verify reward counts
    assertRewardCount dbSync 3
    -- The pool leaders take leader rewards with value 0
    state' <- Api.getConwayLedgerState interpreter
    assertRewardCounts
      dbSync
      state'
      False
      (Just 3)
      [ (StakeIndexPoolLeader (PoolIndex 0), (1, 0, 0, 0, 0))
      , (StakeIndexPoolLeader (PoolIndex 1), (1, 0, 0, 0, 0))
      , (StakeIndexPoolLeader (PoolIndex 2), (1, 0, 0, 0, 0))
      ]

    -- Now that pools are registered, add a tx to fill the fees pot so rewards will be
    -- distributed
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10_000 10_000 0
    -- Fill more epochs
    epochs' <- Api.fillEpochs interpreter mockServer 2
    -- Wait for it to sync
    assertBlockNoBackoff dbSync (3 + length epochs + length epochs')
    -- Verify the new reward counts
    assertRewardCount dbSync 14
    assertRewardCounts
      dbSync
      state'
      True
      (Just 5)
      -- 2 pool leaders also delegate to pools
      [ (StakeIndexPoolLeader (PoolIndexId $ KeyHash "9f1b441b9b781b3c3abb43b25679dc17dbaaf116dddca1ad09dc1de0"), (1, 0, 0, 0, 0))
      , (StakeIndexPoolLeader (PoolIndexId $ KeyHash "5af582399de8c226391bfd21424f34d0b053419c4d93975802b7d107"), (1, 1, 0, 0, 0))
      , (StakeIndexPoolLeader (PoolIndexId $ KeyHash "58eef2925db2789f76ea057c51069e52c5e0a44550f853c6cdf620f8"), (1, 1, 0, 0, 0))
      , (StakeIndexPoolMember 0 (PoolIndex 0), (0, 1, 0, 0, 0))
      , (StakeIndexPoolMember 0 (PoolIndex 1), (0, 1, 0, 0, 0))
      ]
  where
    testLabel = "conwaySimpleRewards"

-- This test is the same as the previous, but in Shelley era. Rewards result
-- should be different because of the old Shelley bug.
-- https://github.com/IntersectMBO/cardano-db-sync/issues/959
--
-- The differenece in rewards is triggered when a reward address of a pool A
-- delegates to a pool B and is not an owner of pool B. In this case it receives
-- leader rewards from pool A and member rewards from pool B. In this test, we
-- have 2 instances of this case, one where A = B and one where A /= B.
rewardsShelley :: IOManager -> [(Text, Text)] -> Assertion
rewardsShelley =
  withFullConfig shelleyConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with stake credentials
    void $ Api.registerAllStakeCreds interpreter mockServer
    -- Pools are not registered yet, this takes 2 epochs, so fees of this tx should not
    -- create any rewards
    void $
      Api.withShelleyFindLeaderAndSubmitTx interpreter mockServer $
        Shelley.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10_000 10_000
    -- Fill up epochs
    epochs <- Api.fillEpochs interpreter mockServer 3
    -- Wait for it to sync
    assertRewardCount dbSync 3

    -- Now that pools are registered, add a tx to fill the fees pot so rewards will be
    -- distributed
    void $
      Api.withShelleyFindLeaderAndSubmitTx interpreter mockServer $
        Shelley.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10_000 10_000
    -- Fill more epochs
    epochs' <- Api.fillEpochs interpreter mockServer 2

    -- Wait for it to sync
    assertBlockNoBackoff dbSync (3 + length epochs + length epochs')
    -- Verify the new reward counts
    state' <- Api.getShelleyLedgerState interpreter
    -- Note we have 2 rewards less compared to Conway
    assertRewardCount dbSync 12
    assertRewardCounts
      dbSync
      state'
      True
      (Just 5)
      -- Here we don't have both leader and member rewards.
      [
        ( StakeIndexPoolLeader (PoolIndexId $ KeyHash "9f1b441b9b781b3c3abb43b25679dc17dbaaf116dddca1ad09dc1de0")
        , (1, 0, 0, 0, 0)
        )
      ,
        ( StakeIndexPoolLeader (PoolIndexId $ KeyHash "5af582399de8c226391bfd21424f34d0b053419c4d93975802b7d107")
        , (1, 0, 0, 0, 0)
        )
      ,
        ( StakeIndexPoolLeader (PoolIndexId $ KeyHash "58eef2925db2789f76ea057c51069e52c5e0a44550f853c6cdf620f8")
        , (1, 0, 0, 0, 0)
        )
      , (StakeIndexPoolMember 0 (PoolIndex 0), (0, 1, 0, 0, 0))
      , (StakeIndexPoolMember 0 (PoolIndex 1), (0, 1, 0, 0, 0))
      ]
  where
    testLabel = "conwayRewardsShelley"

rollbackBoundary :: IOManager -> [(Text, Text)] -> Assertion
rollbackBoundary =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with stake credentials
    void $ Api.registerAllStakeCreds interpreter mockServer
    -- It takes 2 epochs to create rewards
    epochs <- Api.fillEpochs interpreter mockServer 2
    -- Forge a transaction to distribute rewards
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10_000 10_000 0
    -- Create a point to rollback to
    blks <- Api.forgeAndSubmitBlocks interpreter mockServer 50
    -- Fill up the rest of the epoch
    epochs' <- Api.fillUntilNextEpoch interpreter mockServer
    -- Wait for it to sync
    assertRewardCount dbSync 3

    -- Rollback
    atomically $ rollback mockServer (blockPoint $ last blks)
    -- Rollback effects are delayed
    assertBlockNoBackoff dbSync (2 + length (epochs <> blks <> epochs'))

    -- Add the blocks again
    forM_ epochs' $ atomically . addBlock mockServer
    -- Should have the same amount of rewards
    assertBlockNoBackoff dbSync (2 + length (epochs <> blks <> epochs'))
    assertRewardCount dbSync 3
    -- Add some more blocks and verify
    epochs'' <- Api.fillUntilNextEpoch interpreter mockServer
    assertBlockNoBackoff dbSync (2 + length (epochs <> blks <> epochs' <> epochs''))
  where
    testLabel = "conwayRollbackBoundary"
