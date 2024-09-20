{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Test.Cardano.Db.Mock.Unit.Conway.Config.MigrateConsumedPruneTxOut (
  basicPrune,
  basicPruneWithAddress,
  pruneWithSimpleRollback,
  pruneWithSimpleRollbackWithAddress,
  pruneWithFullTxRollback,
  pruneWithFullTxRollbackWithAddress,
  pruningShouldKeepSomeTx,
  pruningShouldKeepSomeTxWithAddress,
  pruneAndRollBackOneBlock,
  pruneAndRollBackOneBlockWithAddress,
  noPruneAndRollBack,
  noPruneAndRollBackWithAddress,
  pruneSameBlock,
  pruneSameBlockWithAddress,
  noPruneSameBlock,
  noPruneSameBlockWithAddress,
  migrateAndPruneRestart,
  migrateAndPruneRestartWithAddress,
  pruneRestartMissingFlag,
  pruneRestartMissingFlagWithAddress,
  bootstrapRestartMissingFlag,
  bootstrapRestartMissingFlagWithAddress,
  populateDbRestartWithAddressConfig,
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Config.Types (ForceTxIn (..), SyncInsertOptions (..), SyncNodeConfig (..), TxOutConfig (..), UseTxOutAddress (..))
import Cardano.Mock.ChainSync.Server (IOManager (), addBlock)
import Cardano.Mock.Forging.Interpreter (forgeNext)
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import Cardano.Mock.Forging.Types (UTxOIndex (..))
import Cardano.Prelude
import Ouroboros.Network.Block (blockPoint)
import Test.Cardano.Db.Mock.Config
import Test.Cardano.Db.Mock.Examples (mockBlock0, mockBlock1)
import Test.Cardano.Db.Mock.UnifiedApi
import Test.Cardano.Db.Mock.Validate
import Test.Tasty.HUnit (Assertion ())
import Prelude ()
import qualified Prelude

------------------------------------------------------------------------------
-- Tests
-----------------------------------------------------------------------------
basicPrune :: IOManager -> [(Text, Text)] -> Assertion
basicPrune = performBasicPrune False

basicPruneWithAddress :: IOManager -> [(Text, Text)] -> Assertion
basicPruneWithAddress = performBasicPrune True

performBasicPrune :: Bool -> IOManager -> [(Text, Text)] -> Assertion
performBasicPrune useTxOutAddress = do
  withCustomConfigAndDropDB args (Just $ configPruneForceTxIn useTxOutAddress) cfgDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    let txOutTableType = txOutTableTypeFromConfig dbSync

    -- Add some blocks
    blks <- forgeAndSubmitBlocks interpreter mockServer 50

    -- Add blocks with transactions
    void $
      withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10_000 10_000 0
    void $
      withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx (UTxOIndex 1) (UTxOIndex 0) 10_000 10_000 0

    -- Check tx-out count before pruning
    assertBlockNoBackoff dbSync (fullBlockSize blks)
    assertEqQuery dbSync (DB.queryTxOutCount txOutTableType) 14 "new epoch didn't prune tx_out column that are null"

    blks' <- forgeAndSubmitBlocks interpreter mockServer 48
    assertBlockNoBackoff dbSync (fullBlockSize $ blks <> blks')

    -- Check that tx_out was pruned
    assertEqQuery dbSync (DB.queryTxOutCount txOutTableType) 12 "the pruning didn't work correctly as the tx-out count is incorrect"
    -- Check unspent tx
    assertUnspentTx dbSync
  where
    args = initCommandLineArgs
    testLabel = "conwayConfigPrune"
    fullBlockSize b = fromIntegral $ length b + 2
    cfgDir = conwayConfigDir

pruneWithSimpleRollback :: IOManager -> [(Text, Text)] -> Assertion
pruneWithSimpleRollback = performPruneWithSimpleRollback False

pruneWithSimpleRollbackWithAddress :: IOManager -> [(Text, Text)] -> Assertion
pruneWithSimpleRollbackWithAddress = performPruneWithSimpleRollback True

performPruneWithSimpleRollback :: Bool -> IOManager -> [(Text, Text)] -> Assertion
performPruneWithSimpleRollback useTxOutAddress =
  withCustomConfigAndDropDB cmdLineArgs (Just $ configPruneForceTxIn useTxOutAddress) conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    let txOutTableType = txOutTableTypeFromConfig dbSync
    -- Forge some blocks
    blk0 <- forgeNext interpreter mockBlock0
    blk1 <- forgeNext interpreter mockBlock1
    atomically $ addBlock mockServer blk0

    startDBSync dbSync
    atomically $ addBlock mockServer blk1

    -- Create some payment transactions
    void $
      withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10_000 10_000 0
    void $
      withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx (UTxOIndex 1) (UTxOIndex 0) 10_000 10_000 0
    assertEqQuery dbSync (DB.queryTxOutCount txOutTableType) 14 ""

    -- Submit some blocks
    blks <- forgeAndSubmitBlocks interpreter mockServer 96
    assertBlockNoBackoff dbSync (fullBlockSize blks)
    assertEqQuery dbSync (DB.queryTxOutCount txOutTableType) 12 "the txOut count is incorrect"
    assertEqQuery dbSync (DB.queryTxOutConsumedCount txOutTableType) 0 "Unexpected TxOutConsumedByTxId count after prune"
    assertUnspentTx dbSync

    -- Rollback
    rollbackTo interpreter mockServer (blockPoint blk1)
    assertEqQuery dbSync (DB.queryTxOutConsumedCount txOutTableType) 0 "Unexpected TxOutConsumedByTxId count after rollback"
    assertBlockNoBackoff dbSync (fullBlockSize blks)
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "conwayConfigPruneSimpleRollback"
    fullBlockSize b = fromIntegral $ length b + 4

pruneWithFullTxRollback :: IOManager -> [(Text, Text)] -> Assertion
pruneWithFullTxRollback = performPruneWithFullTxRollback False

pruneWithFullTxRollbackWithAddress :: IOManager -> [(Text, Text)] -> Assertion
pruneWithFullTxRollbackWithAddress = performPruneWithFullTxRollback True

performPruneWithFullTxRollback :: Bool -> IOManager -> [(Text, Text)] -> Assertion
performPruneWithFullTxRollback useTxOutAddress =
  withCustomConfigAndDropDB cmdLineArgs (Just $ configPruneForceTxIn useTxOutAddress) conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    let txOutTableType = txOutTableTypeFromConfig dbSync
    -- Forge a block
    blk0 <- forgeNextFindLeaderAndSubmit interpreter mockServer []
    -- Add some transactions
    void $ withConwayFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Conway.mkFullTx 0 100 st
      tx1 <- Conway.mkFullTx 1 200 st
      pure [tx0, tx1]

    -- Verify tx_out was pruned
    assertBlockNoBackoff dbSync 2
    assertTxCount dbSync 13
    assertUnspentTx dbSync
    assertEqQuery dbSync (DB.queryTxOutCount txOutTableType) 14 "new epoch didn't prune tx_out column that are null"

    -- Rollback
    rollbackTo interpreter mockServer $ blockPoint blk0
    -- Add more transactions
    void $ withConwayFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Conway.mkFullTx 0 100 st
      tx1 <- Conway.mkFullTx 1 200 st
      tx2 <- Conway.mkFullTx 2 200 st
      pure [tx1, tx2, tx0]

    -- Verify tx_out was pruned again
    assertBlockNoBackoff dbSync 2
    assertTxCount dbSync 14
    assertEqQuery dbSync (DB.queryTxOutCount txOutTableType) 16 "new epoch didn't prune tx_out column that are null"
    assertUnspentTx dbSync
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "conwayConfigPruneOnFullRollback"

-- The transactions in the last `2 * securityParam` blocks should not be pruned

pruningShouldKeepSomeTx :: IOManager -> [(Text, Text)] -> Assertion
pruningShouldKeepSomeTx = performPruningShouldKeepSomeTx False

pruningShouldKeepSomeTxWithAddress :: IOManager -> [(Text, Text)] -> Assertion
pruningShouldKeepSomeTxWithAddress = performPruningShouldKeepSomeTx True

performPruningShouldKeepSomeTx :: Bool -> IOManager -> [(Text, Text)] -> Assertion
performPruningShouldKeepSomeTx useTxOutAddress = do
  withCustomConfigAndDropDB cmdLineArgs (Just $ configPrune useTxOutAddress) conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    let txOutTableType = txOutTableTypeFromConfig dbSync
    -- Forge some blocks
    blk1 <- forgeAndSubmitBlocks interpreter mockServer 80
    -- These two blocks/transactions will fall within the last (2 * securityParam) 20
    -- blocks so should not be pruned
    void $
      withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10_000 10_000 0
    void $
      withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx (UTxOIndex 2) (UTxOIndex 3) 10_000 10_000 0
    blk2 <- forgeAndSubmitBlocks interpreter mockServer 18
    -- Verify the two transactions above weren't pruned
    assertBlockNoBackoff dbSync (fromIntegral $ length (blk1 <> blk2) + 2)
    assertEqQuery dbSync (DB.queryTxOutConsumedCount txOutTableType) 2 "Unexpected TxOutConsumedByTxId count after prune"

    -- Add more blocks
    blk3 <- forgeAndSubmitBlocks interpreter mockServer 110
    -- Verify everything has been pruned
    assertBlockNoBackoff dbSync (fromIntegral $ length (blk1 <> blk2 <> blk3) + 2)
    assertTxInCount dbSync 0
    assertEqQuery dbSync (DB.queryTxOutConsumedCount txOutTableType) 0 "Unexpected TxOutConsumedByTxId count after prune"
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "conwayConfigPruneCorrectAmount"

pruneAndRollBackOneBlock :: IOManager -> [(Text, Text)] -> Assertion
pruneAndRollBackOneBlock = performPruneAndRollBackOneBlock False

pruneAndRollBackOneBlockWithAddress :: IOManager -> [(Text, Text)] -> Assertion
pruneAndRollBackOneBlockWithAddress = performPruneAndRollBackOneBlock True

performPruneAndRollBackOneBlock :: Bool -> IOManager -> [(Text, Text)] -> Assertion
performPruneAndRollBackOneBlock useTxOutAddress =
  withCustomConfigAndDropDB cmdLineArgs (Just $ configPruneForceTxIn useTxOutAddress) conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    let txOutTableType = txOutTableTypeFromConfig dbSync
    -- Forge some blocks
    void $ forgeAndSubmitBlocks interpreter mockServer 98
    -- These transactions will fall within the last (2 * securityParam) 20
    -- blocks so should not be pruned
    void $
      withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10_000 10_000 0
    -- Create a block to rollback to
    blk100 <- forgeNextFindLeaderAndSubmit interpreter mockServer []
    state' <- getConwayLedgerState interpreter
    -- Add some more blocks
    let tx1 = Conway.mkPaymentTx (UTxOIndex 2) (UTxOIndex 3) 10_000 500 0 state'
    void $ withConwayFindLeaderAndSubmit interpreter mockServer (\_ -> sequence [tx1])
    -- Verify the last 2 transactions weren't pruned
    assertBlockNoBackoff dbSync 101
    assertEqQuery dbSync (DB.queryTxOutConsumedCount txOutTableType) 2 "Unexpected TxOutConsumedByTxId count before rollback"

    rollbackTo interpreter mockServer (blockPoint blk100)

    -- Add an empty block
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    -- Verify the transactions were removed in the rollback
    assertBlockNoBackoff dbSync 101
    assertEqQuery dbSync (DB.queryTxOutConsumedCount txOutTableType) 1 "Unexpected TxOutConsumedByTxId count after rollback"

    -- Trigger a prune
    void $ forgeAndSubmitBlocks interpreter mockServer 102
    -- Verify everything was pruned
    assertBlockNoBackoff dbSync 203
    assertEqQuery dbSync (DB.queryTxOutConsumedCount txOutTableType) 0 "Unexpected TxOutConsumedByTxId count after rollback"
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "conwayConfigPruneAndRollBack"

noPruneAndRollBack :: IOManager -> [(Text, Text)] -> Assertion
noPruneAndRollBack = performNoPruneAndRollBack False

noPruneAndRollBackWithAddress :: IOManager -> [(Text, Text)] -> Assertion
noPruneAndRollBackWithAddress = performNoPruneAndRollBack True

performNoPruneAndRollBack :: Bool -> IOManager -> [(Text, Text)] -> Assertion
performNoPruneAndRollBack useTxOutAddress =
  withCustomConfigAndDropDB cmdLineArgs (Just $ configConsume useTxOutAddress) conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    let txOutTableType = txOutTableTypeFromConfig dbSync
    -- Forge some blocks
    void $ forgeAndSubmitBlocks interpreter mockServer 98
    -- Add a block with transactions
    void $
      withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10_000 10_000 0
    -- Create a block to rollback to
    blk100 <- forgeNextFindLeaderAndSubmit interpreter mockServer []
    -- Add some more blocks
    state' <- getConwayLedgerState interpreter
    let tx1 = Conway.mkPaymentTx (UTxOIndex 2) (UTxOIndex 3) 10_000 500 0 state'
    void $ withConwayFindLeaderAndSubmit interpreter mockServer $ \_ -> sequence [tx1]

    -- Verify the transactions weren't pruned
    assertBlockNoBackoff dbSync 101
    assertEqQuery dbSync (DB.queryTxOutConsumedCount txOutTableType) 2 "Unexpected TxOutConsumedByTxId count before rollback"

    rollbackTo interpreter mockServer (blockPoint blk100)

    -- Add an empty block
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    -- Verify transactions were removed
    assertBlockNoBackoff dbSync 101
    assertEqQuery dbSync (DB.queryTxOutConsumedCount txOutTableType) 1 "Unexpected TxOutConsumedByTxId count after rollback"

    -- Add some more blocks
    void $ forgeAndSubmitBlocks interpreter mockServer 102
    -- Verify nothing has been pruned
    assertBlockNoBackoff dbSync 203
    assertEqQuery dbSync (DB.queryTxOutConsumedCount txOutTableType) 1 "Unexpected TxOutConsumedByTxId count after rollback"
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "conwayConfigNoPruneAndRollBack"

pruneSameBlock :: IOManager -> [(Text, Text)] -> Assertion
pruneSameBlock = performPruneSameBlock False

pruneSameBlockWithAddress :: IOManager -> [(Text, Text)] -> Assertion
pruneSameBlockWithAddress = performPruneSameBlock True

performPruneSameBlock :: Bool -> IOManager -> [(Text, Text)] -> Assertion
performPruneSameBlock useTxOutAddress =
  withCustomConfigAndDropDB cmdLineArgs (Just $ configPruneForceTxIn useTxOutAddress) conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    let txOutTableType = txOutTableTypeFromConfig dbSync
    -- Forge some blocks
    void $ forgeAndSubmitBlocks interpreter mockServer 76
    blk77 <- forgeNextFindLeaderAndSubmit interpreter mockServer []
    -- Add a block with transactions
    void $ withConwayFindLeaderAndSubmit interpreter mockServer $ \state' -> do
      tx0 <- Conway.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 20_000 20_000 0 state'
      let utxo0 = Prelude.head (Conway.mkUTxOConway tx0)
      tx1 <- Conway.mkPaymentTx (UTxOPair utxo0) (UTxOIndex 2) 10_000 500 0 state'
      pure [tx0, tx1]
    -- Verify the transactions weren't pruned
    assertBlockNoBackoff dbSync 78
    assertEqQuery dbSync (DB.queryTxOutConsumedCount txOutTableType) 2 "Unexpected TxOutConsumedByTxId before rollback"

    -- Trigger a prune
    void $ forgeAndSubmitBlocks interpreter mockServer 22
    -- Verify the transactions were pruned
    assertBlockNoBackoff dbSync 100
    assertEqQuery dbSync (DB.queryTxOutConsumedCount txOutTableType) 0 "Unexpected TxOutConsumedByTxId after prune"

    rollbackTo interpreter mockServer (blockPoint blk77)

    -- Add an empty block
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    -- Verify the transactions were pruned again
    assertBlockNoBackoff dbSync 78
    assertTxInCount dbSync 0
    assertEqQuery dbSync (DB.queryTxOutConsumedCount txOutTableType) 0 "Unexpected TxOutConsumedByTxId after rollback"
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "conwayConfigPruneSameBlock"

noPruneSameBlock :: IOManager -> [(Text, Text)] -> Assertion
noPruneSameBlock = performNoPruneSameBlock False

noPruneSameBlockWithAddress :: IOManager -> [(Text, Text)] -> Assertion
noPruneSameBlockWithAddress = performNoPruneSameBlock True

performNoPruneSameBlock :: Bool -> IOManager -> [(Text, Text)] -> Assertion
performNoPruneSameBlock useTxOutAddress =
  withCustomConfigAndDropDB cmdLineArgs (Just $ configConsume useTxOutAddress) conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge some blocks
    void $ forgeAndSubmitBlocks interpreter mockServer 96
    blk97 <- forgeNextFindLeaderAndSubmit interpreter mockServer []
    -- Add a block with transactions
    void $ withConwayFindLeaderAndSubmit interpreter mockServer $ \state' -> do
      tx0 <- Conway.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 20_000 20_000 0 state'
      let utxo0 = Prelude.head (Conway.mkUTxOConway tx0)
      tx1 <- Conway.mkPaymentTx (UTxOPair utxo0) (UTxOIndex 2) 10_000 500 0 state'
      pure [tx0, tx1]
    -- Add some more empty blocks
    void $ forgeAndSubmitBlocks interpreter mockServer 2
    -- Verify the blocks exist
    assertBlockNoBackoff dbSync 100

    rollbackTo interpreter mockServer (blockPoint blk97)

    -- Verify we haven't pruned anything yet
    assertBlockNoBackoff dbSync 100
    -- Add an empty block
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    -- Verify everything was pruned
    assertBlockNoBackoff dbSync 98
    assertEqQuery dbSync (DB.queryTxOutConsumedCount $ txOutTableTypeFromConfig dbSync) 0 "Unexpected TxOutConsumedByTxId after rollback"
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "conwayConfigNoPruneSameBlock"

migrateAndPruneRestart :: IOManager -> [(Text, Text)] -> Assertion
migrateAndPruneRestart = performMigrateAndPruneRestart False

migrateAndPruneRestartWithAddress :: IOManager -> [(Text, Text)] -> Assertion
migrateAndPruneRestartWithAddress = performMigrateAndPruneRestart True

performMigrateAndPruneRestart :: Bool -> IOManager -> [(Text, Text)] -> Assertion
performMigrateAndPruneRestart useTxOutAddress =
  withCustomConfigAndDropDB cmdLineArgs (Just $ configConsume useTxOutAddress) conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge some blocks
    void $ forgeAndSubmitBlocks interpreter mockServer 50
    -- Wait for them to sync
    assertBlockNoBackoff dbSync 50

    stopDBSync dbSync

    -- Start without tx-out=consumed
    newEnv <- replaceConfigFile "test-db-sync-config.json" dbSync
    startDBSync newEnv
    -- There is a slight delay before the flag is checked
    threadDelay 6_000_000
    -- Expected to fail
    checkStillRuns dbSync
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "conwayConfigMigrateAndPruneRestart"

pruneRestartMissingFlag :: IOManager -> [(Text, Text)] -> Assertion
pruneRestartMissingFlag = performPruneRestartMissingFlag False

pruneRestartMissingFlagWithAddress :: IOManager -> [(Text, Text)] -> Assertion
pruneRestartMissingFlagWithAddress = performPruneRestartMissingFlag True

performPruneRestartMissingFlag :: Bool -> IOManager -> [(Text, Text)] -> Assertion
performPruneRestartMissingFlag useTxOutAddress =
  withCustomConfigAndDropDB cmdLineArgs (Just $ configPruneForceTxIn useTxOutAddress) conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge some blocks
    void $ forgeAndSubmitBlocks interpreter mockServer 50
    -- Wait for them to sync
    assertBlockNoBackoff dbSync 50

    stopDBSync dbSync

    -- Start without tx-out=prune
    newEnv <- replaceConfigFile "test-db-sync-config.json" dbSync
    startDBSync newEnv
    -- There is a slight delay before the flag is checked
    threadDelay 6_000_000
    -- Expected to fail
    checkStillRuns dbSync
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "conwayConfigPruneRestartMissingFlag"

bootstrapRestartMissingFlag :: IOManager -> [(Text, Text)] -> Assertion
bootstrapRestartMissingFlag = performBootstrapRestartMissingFlag False

bootstrapRestartMissingFlagWithAddress :: IOManager -> [(Text, Text)] -> Assertion
bootstrapRestartMissingFlagWithAddress = performBootstrapRestartMissingFlag True

performBootstrapRestartMissingFlag :: Bool -> IOManager -> [(Text, Text)] -> Assertion
performBootstrapRestartMissingFlag useTxOutAddress =
  withCustomConfigAndDropDB cmdLineArgs (Just $ configBootstrap useTxOutAddress) conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge some blocks
    void $ forgeAndSubmitBlocks interpreter mockServer 50
    -- Wait for it to sync
    assertBlockNoBackoff dbSync 50
    assertTxOutCount dbSync 0

    stopDBSync dbSync
    -- Start without tx-out=bootstrap
    newEnv <- replaceConfigFile "test-db-sync-config.json" dbSync
    startDBSync newEnv
    -- There is a slight delay befor the flag is checked
    threadDelay 6_000_000
    -- Expected to fail
    checkStillRuns dbSync
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "conwayConfigBootstrapRestartMissingFlag"

populateDbRestartWithAddressConfig :: IOManager -> [(Text, Text)] -> Assertion
populateDbRestartWithAddressConfig =
  withCustomConfigAndDropDB cmdLineArgs (Just $ configConsume False) conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge some blocks
    void $ forgeAndSubmitBlocks interpreter mockServer 50
    -- Wait for them to sync
    assertBlockNoBackoff dbSync 50

    stopDBSync dbSync

    let newDBSync =
          dbSync
            { dbSyncConfig =
                (dbSyncConfig dbSync)
                  { dncInsertOptions =
                      (dncInsertOptions $ dbSyncConfig dbSync)
                        { sioTxOut = TxOutConsumedPrune (ForceTxIn False) (UseTxOutAddress True)
                        }
                  }
            }
    -- Start without tx-out=prune
    newEnv <- replaceConfigFile "test-db-sync-config.json" newDBSync
    startDBSync newEnv
    -- There is a slight delay before the flag is checked
    threadDelay 6_000_000
    -- Expected to fail
    checkStillRuns newDBSync
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "conwayPopulateDbRestartWithAddressConfig"
