{-# LANGUAGE NumericUnderscores #-}

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
) where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.TxCert (ConwayDelegCert (..), Delegatee (..))
import Cardano.Ledger.Crypto ()
import Cardano.Mock.ChainSync.Server (IOManager (), addBlock, rollback)
import Cardano.Mock.Forging.Interpreter (forgeNext)
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import Cardano.Mock.Forging.Tx.Generic (resolvePool)
import Cardano.Mock.Forging.Types (PoolIndex (..), StakeIndex (..), UTxOIndex (..))
import Cardano.Prelude
import Data.Maybe.Strict (StrictMaybe (..))
import Ouroboros.Network.Block (blockPoint)
import Test.Cardano.Db.Mock.Config
import Test.Cardano.Db.Mock.Examples (mockBlock0, mockBlock1, mockBlock2)
import Test.Cardano.Db.Mock.UnifiedApi
import Test.Cardano.Db.Mock.Validate (assertBlockNoBackoff, assertTxCount)
import Test.Tasty.HUnit (Assertion ())
import Prelude (last)

simpleRollback :: IOManager -> [(Text, Text)] -> Assertion
simpleRollback =
  withFullConfigAndDropDB conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
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
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
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
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
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
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
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
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
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
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
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
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
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
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
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
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
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
