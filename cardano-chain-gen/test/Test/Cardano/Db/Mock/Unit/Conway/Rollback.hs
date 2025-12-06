{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Test.Cardano.Db.Mock.Unit.Conway.Rollback (
  simpleRollback,
  bigChain,
  restartAndRollback,
  lazyRollback,
  lazyRollbackRestart,
  doubleRollback,
  stakeAddressRollback,
  rollbackChangeTxOrder,
  rollbackFullTx,
  drepDistrRollback,
  poolStatBasicTest,
  poolStatRollbackNoDuplicates,
  poolStatRollbackGeneral,
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Era.Shelley.Generic (unCredentialHash)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.TxCert (ConwayDelegCert (..), Delegatee (..))
import Cardano.Mock.ChainSync.Server (IOManager (), addBlock, rollback)
import Cardano.Mock.Forging.Interpreter (forgeNext)
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import Cardano.Mock.Forging.Tx.Generic (resolvePool)
import qualified Cardano.Mock.Forging.Tx.Generic as Forging
import Cardano.Mock.Forging.Types (PoolIndex (..), StakeIndex (..), UTxOIndex (..))
import Cardano.Prelude
import Data.Maybe.Strict (StrictMaybe (..))
import Ouroboros.Network.Block (blockPoint)
import Test.Cardano.Db.Mock.Config (claFullMode, configPoolStats, conwayConfigDir, initCommandLineArgs, queryDBSync, startDBSync, stopDBSync, withCustomConfigDropDB, withFullConfigDropDB)
import Test.Cardano.Db.Mock.Examples
import Test.Cardano.Db.Mock.UnifiedApi
import Test.Cardano.Db.Mock.Validate (assertBlockNoBackoff, assertEqQuery, assertTxCount)
import Test.Tasty.HUnit (Assertion (), assertBool, assertEqual)
import Prelude (error, head, last)

simpleRollback :: IOManager -> [(Text, Text)] -> Assertion
simpleRollback =
  withFullConfigDropDB conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    -- Forge some blocks
    blk0 <- forgeNext interpreter mockBlock0
    blk1 <- forgeNext interpreter mockBlock1
    blk2 <- forgeNext interpreter mockBlock2
    -- Submit a block
    atomically (addBlock mockServer blk0)

    startDBSync dbSync

    -- Submit some more blocks
    atomically (addBlock mockServer blk1)
    atomically (addBlock mockServer blk2)
    -- Wait for them to sync
    assertBlockNoBackoff dbSync 3

    -- Rollback
    atomically $ rollback mockServer (blockPoint blk1)
    assertBlockNoBackoff dbSync 3 -- Rollback effects are now delayed
  where
    testLabel = "conwaySimpleRollback"

bigChain :: IOManager -> [(Text, Text)] -> Assertion
bigChain =
  withFullConfigDropDB conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    -- Forge some blocks
    forM_ (replicate 101 mockBlock0) (forgeNextAndSubmit interpreter mockServer)

    startDBSync dbSync
    -- Wait for it to sync
    assertBlockNoBackoff dbSync 101

    -- Create a point to rollback to
    blks' <- forM (replicate 100 mockBlock1) (forgeNextAndSubmit interpreter mockServer)
    -- Wait for them to sync
    assertBlockNoBackoff dbSync 201

    -- Forge some more blocks
    forM_ (replicate 5 mockBlock2) (forgeNextAndSubmit interpreter mockServer)
    -- Wait for them to sync
    assertBlockNoBackoff dbSync 206

    -- Rollback
    atomically $ rollback mockServer (blockPoint $ last blks')
    assertBlockNoBackoff dbSync 206 -- Rollback effects are now delayed
  where
    testLabel = "conwayBigChain"

restartAndRollback :: IOManager -> [(Text, Text)] -> Assertion
restartAndRollback =
  withFullConfigDropDB conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    -- Forge some blocks
    forM_ (replicate 101 mockBlock0) (forgeNextAndSubmit interpreter mockServer)

    startDBSync dbSync
    -- Wait for it to sync
    assertBlockNoBackoff dbSync 101

    -- Create a point to rollback to
    blks <- forM (replicate 100 mockBlock0) (forgeNextAndSubmit interpreter mockServer)
    -- Wait for it to sync
    assertBlockNoBackoff dbSync 201

    -- Forge some more blocks
    forM_ (replicate 5 mockBlock2) (forgeNextAndSubmit interpreter mockServer)
    assertBlockNoBackoff dbSync 206

    -- Rollback and restart
    stopDBSync dbSync
    atomically $ rollback mockServer (blockPoint $ last blks)
    startDBSync dbSync

    assertBlockNoBackoff dbSync 206 -- Rollback effects are now delayed
  where
    testLabel = "conwayRestartAndRollback"

lazyRollback :: IOManager -> [(Text, Text)] -> Assertion
lazyRollback =
  withFullConfigDropDB conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Create a point to rollback to
    lastBlk <- last <$> forgeAndSubmitBlocks interpreter mockServer 200
    -- Forge some blocks
    void $ forgeAndSubmitBlocks interpreter mockServer 70
    -- Wait for them to sync
    assertBlockNoBackoff dbSync 270

    rollbackTo interpreter mockServer (blockPoint lastBlk)

    -- Here we create the fork
    void $
      withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkSimpleDCertTx [(StakeIndexNew 1, Conway.mkRegTxCert SNothing)]
    -- Add some more blocks
    void $ forgeAndSubmitBlocks interpreter mockServer 40
    -- Verify the new block count
    assertBlockNoBackoff dbSync 241
  where
    testLabel = "conwayLazyRollback"

lazyRollbackRestart :: IOManager -> [(Text, Text)] -> Assertion
lazyRollbackRestart =
  withFullConfigDropDB conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Create a point to rollback to
    lastBlk <- last <$> forgeAndSubmitBlocks interpreter mockServer 220
    -- Forge some more blocks
    void $ forgeAndSubmitBlocks interpreter mockServer 60
    -- Wait for it to sync
    assertBlockNoBackoff dbSync 280

    -- Rollback and restart
    stopDBSync dbSync
    rollbackTo interpreter mockServer (blockPoint lastBlk)
    startDBSync dbSync

    -- Here we create the fork
    void $
      withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkSimpleDCertTx [(StakeIndexNew 1, Conway.mkRegTxCert SNothing)]
    -- Add some more blocks
    void $ forgeAndSubmitBlocks interpreter mockServer 30
    -- Verify the new block count
    assertBlockNoBackoff dbSync 251
  where
    testLabel = "conwayLazyRollbackRestart"

doubleRollback :: IOManager -> [(Text, Text)] -> Assertion
doubleRollback =
  withFullConfigDropDB conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Create points to rollback to
    lastBlk1 <- last <$> forgeAndSubmitBlocks interpreter mockServer 150
    lastBlk2 <- last <$> forgeAndSubmitBlocks interpreter mockServer 100
    -- Forge some more blocks
    void $ forgeAndSubmitBlocks interpreter mockServer 100
    -- Wait for it to sync
    assertBlockNoBackoff dbSync 350

    -- Rollback to second block point
    rollbackTo interpreter mockServer (blockPoint lastBlk2)
    -- Here we create a fork
    void $
      withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkSimpleDCertTx [(StakeIndexNew 1, Conway.mkRegTxCert SNothing)]
    -- Add some more blocks
    void $ forgeAndSubmitBlocks interpreter mockServer 50

    -- Rollback to first block point
    rollbackTo interpreter mockServer (blockPoint lastBlk1)
    -- Create another fork
    void $
      withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkSimpleDCertTx [(StakeIndexNew 0, Conway.mkRegTxCert $ SJust (Coin 100))]
    -- Add some more blocks
    void $ forgeAndSubmitBlocks interpreter mockServer 50
    -- Wait for it to sync
    assertBlockNoBackoff dbSync 201
  where
    testLabel = "conwayDoubleRollback"

stakeAddressRollback :: IOManager -> [(Text, Text)] -> Assertion
stakeAddressRollback =
  withFullConfigDropDB conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Create a point to rollbackTo
    blk <- forgeNextFindLeaderAndSubmit interpreter mockServer []
    -- Add a stake address tx
    void $ withConwayFindLeaderAndSubmit interpreter mockServer $ \st -> do
      let poolId = resolvePool (PoolIndex 0) st
      tx <-
        Conway.mkSimpleDCertTx
          [ (StakeIndexNew 1, Conway.mkRegTxCert SNothing)
          , (StakeIndexNew 1, Conway.mkTxDelegCert (`ConwayDelegCert` DelegStake poolId))
          ]
          st
      pure [tx]
    -- Wait for it to sync
    assertBlockNoBackoff dbSync 2

    rollbackTo interpreter mockServer (blockPoint blk)

    -- Create a fork
    void $ withConwayFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
      Conway.mkDummyRegisterTx 1 2
    -- Wait for it to sync
    assertBlockNoBackoff dbSync 2
    -- Add another block
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    -- Verify the new block count
    assertBlockNoBackoff dbSync 3
  where
    testLabel = "conwayStakeAddressRollback"

rollbackChangeTxOrder :: IOManager -> [(Text, Text)] -> Assertion
rollbackChangeTxOrder =
  withFullConfigDropDB conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Create a point to rollback to
    blk0 <- forgeNextFindLeaderAndSubmit interpreter mockServer []

    -- Create some transactions
    state' <- getConwayLedgerState interpreter
    let tx0 = Conway.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10_000 500 0 state'
        tx1 = Conway.mkPaymentTx (UTxOIndex 2) (UTxOIndex 3) 10_000 500 0 state'
        tx2 = Conway.mkPaymentTx (UTxOIndex 4) (UTxOIndex 5) 10_000 500 0 state'
    -- Submit them
    void $ withConwayFindLeaderAndSubmit interpreter mockServer $ \_ ->
      sequence [tx0, tx1]
    -- Wait for them to sync
    assertBlockNoBackoff dbSync 2
    assertTxCount dbSync 13

    rollbackTo interpreter mockServer (blockPoint blk0)

    -- Submit the transactions again, in a different order
    void $ withConwayFindLeaderAndSubmit interpreter mockServer $ \_ ->
      sequence [tx1, tx0, tx2]
    -- Verify the new transaction counts
    assertBlockNoBackoff dbSync 2
    assertTxCount dbSync 14
  where
    testLabel = "conwayRollbackChangeTxOrder"

rollbackFullTx :: IOManager -> [(Text, Text)] -> Assertion
rollbackFullTx =
  withFullConfigDropDB conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Create a point to rollback to
    blk0 <- forgeNextFindLeaderAndSubmit interpreter mockServer []
    -- Add some more blocks with full transactions
    void $ withConwayFindLeaderAndSubmit interpreter mockServer $ \state' ->
      sequence
        [ Conway.mkFullTx 0 100 state'
        , Conway.mkFullTx 1 200 state'
        ]
    -- Wait for them to sync
    assertBlockNoBackoff dbSync 2
    assertTxCount dbSync 13

    rollbackTo interpreter mockServer (blockPoint blk0)

    -- Add some more blocks
    void $ withConwayFindLeaderAndSubmit interpreter mockServer $ \state' ->
      sequence
        [ Conway.mkFullTx 0 100 state'
        , Conway.mkFullTx 1 200 state'
        , Conway.mkFullTx 2 200 state'
        ]
    -- Verify the new transaction counts
    assertBlockNoBackoff dbSync 2
    assertTxCount dbSync 14
  where
    testLabel = "conwayRollbackFullTx"

-- | Test for DrepDistr rollback edge case when rolling back to an epoch boundary.
-- Verifies that DrepDistr records are properly deleted during rollback and replay succeeds
-- without duplicate key constraint violations.
drepDistrRollback :: IOManager -> [(Text, Text)] -> Assertion
drepDistrRollback =
  withFullConfigDropDB conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Register stake credentials and DReps
    void $ registerAllStakeCreds interpreter mockServer
    void $ registerDRepsAndDelegateVotes interpreter mockServer

    -- Fill the rest of epoch 0 and cross into epoch 1
    -- This triggers insertDrepDistr for epoch 1 at the epoch boundary (first block of epoch 1)
    epoch0 <- fillUntilNextEpoch interpreter mockServer
    assertBlockNoBackoff dbSync (2 + length epoch0)

    -- Verify DrepDistr for epoch 1 was inserted
    let drepId = Prelude.head Forging.unregisteredDRepIds
    assertEqQuery
      dbSync
      (DB.queryDRepDistrAmount (unCredentialHash drepId) 1)
      10_000
      "Expected DrepDistr for epoch 1 after crossing boundary"

    -- Fill all of epoch 1 and cross into epoch 2
    epoch1 <- fillUntilNextEpoch interpreter mockServer
    assertBlockNoBackoff dbSync (2 + length epoch0 + length epoch1)

    -- Verify DrepDistr for epoch 2 was inserted
    assertEqQuery
      dbSync
      (DB.queryDRepDistrAmount (unCredentialHash drepId) 2)
      10_000
      "Expected DrepDistr for epoch 2 after crossing boundary"

    -- Identify the epoch 2 boundary block (last block of epoch1 list)
    rollbackPoint <- case reverse epoch1 of
      [] -> error "fillUntilNextEpoch returned empty list for epoch 1"
      (epoch2Boundary : _) -> pure $ blockPoint epoch2Boundary

    -- Continue a bit into epoch 2 (after DrepDistr insertion at the boundary)
    blksAfter <- forgeAndSubmitBlocks interpreter mockServer 3
    assertBlockNoBackoff dbSync (2 + length epoch0 + length epoch1 + length blksAfter)

    -- Rollback to the epoch 2 boundary (first block of epoch 2)
    rollbackTo interpreter mockServer rollbackPoint

    -- Create fork - replay through the epoch 2 boundary
    -- This will re-insert DrepDistr for epoch 2
    -- SUCCESS: No duplicate key constraint violation because epoch 2 was properly deleted
    -- (If the fix didn't work, we'd get a unique constraint violation here)
    blksFork <- forgeAndSubmitBlocks interpreter mockServer 5

    -- Verify DrepDistr for epoch 1 still exists (not affected by rollback)
    assertEqQuery
      dbSync
      (DB.queryDRepDistrAmount (unCredentialHash drepId) 1)
      10_000
      "DrepDistr for epoch 1 should still exist after rollback"

    -- Verify final state
    assertBlockNoBackoff dbSync (2 + length epoch0 + length epoch1 + length blksFork)

    -- Verify DrepDistr for both epochs exist after replay
    assertEqQuery
      dbSync
      (DB.queryDRepDistrAmount (unCredentialHash drepId) 2)
      10_000
      "DrepDistr for epoch 2 should be re-inserted after replay through boundary"
  where
    testLabel = "conwayDrepDistrRollback"

poolStatBasicTest :: IOManager -> [(Text, Text)] -> Assertion
poolStatBasicTest =
  withCustomConfigDropDB args (Just configPoolStats) conwayConfigDir testLabel $
    \interpreter mockServer dbSync -> do
      startDBSync dbSync

      -- Test basic pool stats functionality
      void $ registerAllStakeCreds interpreter mockServer
      assertBlockNoBackoff dbSync 1

      -- Create some epochs with pool stats
      void $ forgeAndSubmitBlocks interpreter mockServer 200
      assertBlockNoBackoff dbSync 201

      poolStatCount <- queryDBSync dbSync DB.queryPoolStatCount

      -- Verify pool stats are created and no duplicates exist
      duplicateCount <- queryDBSync dbSync DB.queryPoolStatDuplicates
      assertEqual "Should have no duplicate pool stats" 0 duplicateCount
      assertBool "Should have some pool stats" (poolStatCount > 0)
  where
    args = initCommandLineArgs {claFullMode = False}
    testLabel = "conwayPoolStatBasicTest"

poolStatRollbackNoDuplicates :: IOManager -> [(Text, Text)] -> Assertion
poolStatRollbackNoDuplicates =
  withCustomConfigDropDB args (Just configPoolStats) conwayConfigDir testLabel $
    \interpreter mockServer dbSync -> do
      startDBSync dbSync

      -- Simple setup: create some blocks with pool stats
      void $ registerAllStakeCreds interpreter mockServer
      void $ forgeAndSubmitBlocks interpreter mockServer 200 -- Fill 2 epochs
      assertBlockNoBackoff dbSync 201

      -- Create rollback point
      rollbackBlks <- forgeAndSubmitBlocks interpreter mockServer 50
      assertBlockNoBackoff dbSync 251

      -- Add more blocks to create additional pool stats
      void $ forgeAndSubmitBlocks interpreter mockServer 100 -- Fill 1 more epoch
      assertBlockNoBackoff dbSync 351

      -- Rollback (following exact pattern from bigChain test)
      atomically $ rollback mockServer (blockPoint $ last rollbackBlks)
      assertBlockNoBackoff dbSync 351 -- Delayed rollbackExpand commentComment on line R345ResolvedCode has comments. Press enter to view.

      -- Re-sync some blocks
      void $ forgeAndSubmitBlocks interpreter mockServer 100
      assertBlockNoBackoff dbSync 351 -- Should stay same due to rollbackExpand commentComment on line R349ResolvedCode has comments. Press enter to view.

      -- The main test: no duplicates after rollback + re-sync
      duplicateCount <- queryDBSync dbSync DB.queryPoolStatDuplicates
      assertEqual "Should have no duplicate pool stats after rollback" 0 duplicateCount
  where
    args = initCommandLineArgs {claFullMode = False}
    testLabel = "conwayPoolStatRollbackNoDuplicates"

poolStatRollbackGeneral :: IOManager -> [(Text, Text)] -> Assertion
poolStatRollbackGeneral =
  withCustomConfigDropDB args (Just configPoolStats) conwayConfigDir testLabel $
    \interpreter mockServer dbSync -> do
      startDBSync dbSync

      -- Create pools and stake to generate pool stats
      void $ registerAllStakeCreds interpreter mockServer
      epochBlks1 <- fillEpochs interpreter mockServer 2

      -- Create rollback point
      rollbackBlks <- forgeAndSubmitBlocks interpreter mockServer 10
      let totalBeforeRollback = length epochBlks1 + length rollbackBlks + 1
      assertBlockNoBackoff dbSync totalBeforeRollback

      -- Check initial pool stat count
      initialCount <- queryDBSync dbSync DB.queryPoolStatCount

      -- Forge more blocks to create additional pool stats
      epochBlks2 <- fillEpochs interpreter mockServer 1
      let totalAfterEpoch = totalBeforeRollback + length epochBlks2
      assertBlockNoBackoff dbSync totalAfterEpoch

      -- Verify pool stats increased
      afterCount <- queryDBSync dbSync DB.queryPoolStatCount
      assertBool "Pool stats should have increased" (afterCount > initialCount)

      -- Rollback to previous point
      rollbackTo interpreter mockServer (blockPoint $ last rollbackBlks)
      _ <-
        withConwayFindLeaderAndSubmitTx interpreter mockServer $
          Conway.mkSimpleDCertTx [(StakeIndexNew 1, Conway.mkRegTxCert SNothing)]
      assertBlockNoBackoff dbSync $ totalBeforeRollback + 1

      -- Re-sync the same blocks - should not create duplicates
      epochBlks3 <- fillEpochs interpreter mockServer 1
      let finalTotal = totalBeforeRollback + 1 + length epochBlks3
      assertBlockNoBackoff dbSync finalTotal
      finalCount <- queryDBSync dbSync DB.queryPoolStatCount

      -- Verify count matches and no constraint violations occurred
      assertEqual "Pool stat count should match after rollback" afterCount finalCount

      -- Also verify no duplicates
      duplicateCount <- queryDBSync dbSync DB.queryPoolStatDuplicates
      assertEqual "Should have no duplicate pool stats" 0 duplicateCount
  where
    args = initCommandLineArgs {claFullMode = False}
    testLabel = "conwayPoolStatRollbackGeneral"
