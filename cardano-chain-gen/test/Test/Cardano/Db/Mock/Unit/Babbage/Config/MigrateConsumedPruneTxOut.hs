{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Test.Cardano.Db.Mock.Unit.Babbage.Config.MigrateConsumedPruneTxOut (
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
) where

import Cardano.Db (TxOutTableType (..))
import qualified Cardano.Db as DB
import Cardano.Mock.ChainSync.Server (IOManager, addBlock)
import Cardano.Mock.Forging.Interpreter (forgeNext)
import qualified Cardano.Mock.Forging.Tx.Babbage as Babbage
import Cardano.Mock.Forging.Types (UTxOIndex (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Class.MonadSTM.Strict (atomically)
import Control.Monad (void)
import Data.Text (Text)
import Ouroboros.Consensus.Block (blockPoint)
import Test.Cardano.Db.Mock.Config (
  babbageConfigDir,
  configBootstrap,
  configConsume,
  configPrune,
  configPruneForceTxIn,
  initCommandLineArgs,
  replaceConfigFile,
  startDBSync,
  stopDBSync,
  txOutTableTypeFromConfig,
  withCustomConfigAndDropDB,
 )
import Test.Cardano.Db.Mock.Examples (mockBlock0, mockBlock1)
import Test.Cardano.Db.Mock.UnifiedApi (
  forgeAndSubmitBlocks,
  forgeNextFindLeaderAndSubmit,
  getBabbageLedgerState,
  rollbackTo,
  withBabbageFindLeaderAndSubmit,
  withBabbageFindLeaderAndSubmitTx,
 )
import Test.Cardano.Db.Mock.Validate (assertBlockNoBackoff, assertEqQuery, assertTxCount, assertTxInCount, assertTxOutCount, assertUnspentTx, checkStillRuns)
import Test.Tasty.HUnit (Assertion)

------------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------------
basicPrune :: IOManager -> [(Text, Text)] -> Assertion
basicPrune = peformBasicPrune False

basicPruneWithAddress :: IOManager -> [(Text, Text)] -> Assertion
basicPruneWithAddress = peformBasicPrune True

peformBasicPrune :: Bool -> IOManager -> [(Text, Text)] -> Assertion
peformBasicPrune useTxOutAddress = do
  withCustomConfigAndDropDB cmdLineArgs (Just $ configPruneForceTxIn useTxOutAddress) babbageConfigDir testLabel $ \interpreter mockServer dbSyncEnv -> do
    let txOutTableType = txOutTableTypeFromConfig dbSyncEnv
    startDBSync dbSyncEnv
    -- add 50 block
    b1 <- forgeAndSubmitBlocks interpreter mockServer 50
    -- add 2 blocks with tx
    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ Babbage.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000
    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ Babbage.mkPaymentTx (UTxOIndex 1) (UTxOIndex 0) 10000 10000
    assertBlockNoBackoff dbSyncEnv (fromIntegral $ length b1 + 2)
    -- check tx-out count before any pruning has happened
    assertEqQuery dbSyncEnv (DB.queryTxOutCount txOutTableType) 14 "new epoch didn't prune tx_out column that are null"
    -- add other blocks to instantiate the pruning
    b2 <- forgeAndSubmitBlocks interpreter mockServer 48
    assertBlockNoBackoff dbSyncEnv (fromIntegral $ length (b1 <> b2) + 2)

    -- check that the tx_out has been pruned
    assertEqQuery dbSyncEnv (DB.queryTxOutCount txOutTableType) 12 "the pruning didn't work correctly as the tx-out count is incorrect"
    -- check Unspent tx match after pruning
    assertUnspentTx dbSyncEnv
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "configPrune"

pruneWithSimpleRollback :: IOManager -> [(Text, Text)] -> Assertion
pruneWithSimpleRollback = peformPruneWithSimpleRollback False

pruneWithSimpleRollbackWithAddress :: IOManager -> [(Text, Text)] -> Assertion
pruneWithSimpleRollbackWithAddress = peformPruneWithSimpleRollback True

peformPruneWithSimpleRollback :: Bool -> IOManager -> [(Text, Text)] -> Assertion
peformPruneWithSimpleRollback useTxOutAddress = do
  withCustomConfigAndDropDB cmdLineArgs (Just $ configPruneForceTxIn useTxOutAddress) babbageConfigDir testLabel $ \interpreter mockServer dbSyncEnv -> do
    let txOutTableType = txOutTableTypeFromConfig dbSyncEnv
    blk0 <- forgeNext interpreter mockBlock0
    blk1 <- forgeNext interpreter mockBlock1
    atomically $ addBlock mockServer blk0
    startDBSync dbSyncEnv
    atomically $ addBlock mockServer blk1
    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ Babbage.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000
    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ Babbage.mkPaymentTx (UTxOIndex 1) (UTxOIndex 0) 10000 10000
    assertEqQuery dbSyncEnv (DB.queryTxOutCount txOutTableType) 14 ""
    b1 <- forgeAndSubmitBlocks interpreter mockServer 96
    assertBlockNoBackoff dbSyncEnv (fullBlockSize b1)
    assertEqQuery dbSyncEnv (DB.queryTxOutCount txOutTableType) 12 "the txOut count is incorrect"
    assertEqQuery dbSyncEnv (DB.queryTxOutConsumedCount txOutTableType) 0 "Unexpected TxOutConsumedByTxId count after prune"
    assertUnspentTx dbSyncEnv

    rollbackTo interpreter mockServer (blockPoint blk1)
    assertEqQuery dbSyncEnv (DB.queryTxOutConsumedCount txOutTableType) 0 "Unexpected TxOutConsumedByTxId cout after rollback"
    assertBlockNoBackoff dbSyncEnv $ fullBlockSize b1
  where
    fullBlockSize b = fromIntegral $ length b + 4
    cmdLineArgs = initCommandLineArgs
    testLabel = "configPruneSimpleRollback"

pruneWithFullTxRollback :: IOManager -> [(Text, Text)] -> Assertion
pruneWithFullTxRollback = performPruneWithFullTxRollback False

pruneWithFullTxRollbackWithAddress :: IOManager -> [(Text, Text)] -> Assertion
pruneWithFullTxRollbackWithAddress = performPruneWithFullTxRollback True

performPruneWithFullTxRollback :: Bool -> IOManager -> [(Text, Text)] -> Assertion
performPruneWithFullTxRollback useTxOutAddress = do
  withCustomConfigAndDropDB cmdLineArgs (Just $ configPruneForceTxIn useTxOutAddress) babbageConfigDir testLabel $ \interpreter mockServer dbSyncEnv -> do
    let txOutTableType = txOutTableTypeFromConfig dbSyncEnv
    startDBSync dbSyncEnv
    blk0 <- forgeNextFindLeaderAndSubmit interpreter mockServer []
    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Babbage.mkFullTx 0 100 st
      tx1 <- Babbage.mkFullTx 1 200 st
      pure [tx0, tx1]
    assertBlockNoBackoff dbSyncEnv 2
    assertTxCount dbSyncEnv 13
    assertUnspentTx dbSyncEnv
    assertEqQuery dbSyncEnv (DB.queryTxOutCount txOutTableType) 14 "new epoch didn't prune tx_out column that are null"
    rollbackTo interpreter mockServer $ blockPoint blk0
    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Babbage.mkFullTx 0 100 st
      tx1 <- Babbage.mkFullTx 1 200 st
      tx2 <- Babbage.mkFullTx 2 200 st
      pure [tx1, tx2, tx0]
    assertBlockNoBackoff dbSyncEnv 2
    assertTxCount dbSyncEnv 14
    assertEqQuery dbSyncEnv (DB.queryTxOutCount txOutTableType) 16 "new epoch didn't prune tx_out column that are null"
    assertUnspentTx dbSyncEnv
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "configPruneOnFullRollback"

-- The tx in the last, 2 x securityParam worth of blocks should not be pruned.
-- In these tests, 2 x securityParam = 20 blocks.
pruningShouldKeepSomeTx :: IOManager -> [(Text, Text)] -> Assertion
pruningShouldKeepSomeTx = performPruningShouldKeepSomeTx False

pruningShouldKeepSomeTxWithAddress :: IOManager -> [(Text, Text)] -> Assertion
pruningShouldKeepSomeTxWithAddress = performPruningShouldKeepSomeTx True

performPruningShouldKeepSomeTx :: Bool -> IOManager -> [(Text, Text)] -> Assertion
performPruningShouldKeepSomeTx useTxOutAddress = do
  withCustomConfigAndDropDB cmdLineArgs (Just $ configPrune useTxOutAddress) babbageConfigDir testLabel $ \interpreter mockServer dbSyncEnv -> do
    startDBSync dbSyncEnv
    let txOutTableType = txOutTableTypeFromConfig dbSyncEnv
    b1 <- forgeAndSubmitBlocks interpreter mockServer 80
    -- these two blocs + tx will fall withing the last 20 blocks so should not be pruned
    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ Babbage.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000
    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ Babbage.mkPaymentTx (UTxOIndex 2) (UTxOIndex 3) 10000 10000
    b2 <- forgeAndSubmitBlocks interpreter mockServer 18
    assertBlockNoBackoff dbSyncEnv (fromIntegral $ length (b1 <> b2) + 2)
    -- the two marked TxOutConsumedByTxId should not be pruned
    assertEqQuery dbSyncEnv (DB.queryTxOutConsumedCount TxOutCore) 2 "Unexpected TxOutConsumedByTxId count after prune"
    -- add more blocks to instantiate another prune
    b3 <- forgeAndSubmitBlocks interpreter mockServer 110
    assertBlockNoBackoff dbSyncEnv (fromIntegral $ length (b1 <> b2 <> b3) + 2)
    -- the prune should have removed all
    assertTxInCount dbSyncEnv 0
    assertEqQuery dbSyncEnv (DB.queryTxOutConsumedCount txOutTableType) 0 "Unexpected TxOutConsumedByTxId count after prune"
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "configPruneCorrectAmount"

-- prune with rollback
pruneAndRollBackOneBlock :: IOManager -> [(Text, Text)] -> Assertion
pruneAndRollBackOneBlock = performPruneAndRollBackOneBlock False

pruneAndRollBackOneBlockWithAddress :: IOManager -> [(Text, Text)] -> Assertion
pruneAndRollBackOneBlockWithAddress = performPruneAndRollBackOneBlock True

performPruneAndRollBackOneBlock :: Bool -> IOManager -> [(Text, Text)] -> Assertion
performPruneAndRollBackOneBlock useTxOutAddress = do
  withCustomConfigAndDropDB cmdLineArgs (Just $ configPruneForceTxIn useTxOutAddress) babbageConfigDir testLabel $ \interpreter mockServer dbSyncEnv -> do
    startDBSync dbSyncEnv
    let txOutTableType = txOutTableTypeFromConfig dbSyncEnv
    void $ forgeAndSubmitBlocks interpreter mockServer 98
    -- add 2 blocks with tx
    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ Babbage.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000
    -- add an empty block then fill it with a tx so we can use blk100 as point to rollback
    blk100 <- forgeNextFindLeaderAndSubmit interpreter mockServer []
    st <- getBabbageLedgerState interpreter
    let Right tx1 = Babbage.mkPaymentTx (UTxOIndex 2) (UTxOIndex 3) 10000 500 st
    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \_st ->
      Right [tx1]
    assertBlockNoBackoff dbSyncEnv 101
    -- the 2 tx have been marked but not pruned as they are withing the last 20 blocks
    assertEqQuery dbSyncEnv (DB.queryTxOutConsumedCount txOutTableType) 2 "Unexpected TxOutConsumedByTxId count before rollback"
    rollbackTo interpreter mockServer $ blockPoint blk100
    -- add an empty block
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSyncEnv 101
    -- there should only be 1 tx marked now as the other was deleted in rollback
    assertEqQuery dbSyncEnv (DB.queryTxOutConsumedCount txOutTableType) 1 "Unexpected TxOutConsumedByTxId count after rollback"
    -- cause another prune
    void $ forgeAndSubmitBlocks interpreter mockServer 102
    assertBlockNoBackoff dbSyncEnv 203
    -- everything should be pruned
    assertEqQuery dbSyncEnv (DB.queryTxOutConsumedCount txOutTableType) 0 "Unexpected TxOutConsumedByTxId count after rollback"
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "configPruneAndRollBack"

-- consume with rollback
noPruneAndRollBack :: IOManager -> [(Text, Text)] -> Assertion
noPruneAndRollBack = performNoPruneAndRollBack False

noPruneAndRollBackWithAddress :: IOManager -> [(Text, Text)] -> Assertion
noPruneAndRollBackWithAddress = performNoPruneAndRollBack True

performNoPruneAndRollBack :: Bool -> IOManager -> [(Text, Text)] -> Assertion
performNoPruneAndRollBack useTxOutAddress = do
  withCustomConfigAndDropDB cmdLineArgs (Just $ configConsume useTxOutAddress) babbageConfigDir testLabel $ \interpreter mockServer dbSyncEnv -> do
    startDBSync dbSyncEnv
    let txOutTableType = txOutTableTypeFromConfig dbSyncEnv
    void $ forgeAndSubmitBlocks interpreter mockServer 98
    -- add 2 blocks with tx
    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ Babbage.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000
    -- add an empty block then fill it with a tx so we can use blk100 as point to rollback
    blk100 <- forgeNextFindLeaderAndSubmit interpreter mockServer []
    st <- getBabbageLedgerState interpreter
    let Right tx1 = Babbage.mkPaymentTx (UTxOIndex 2) (UTxOIndex 3) 10000 500 st
    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \_st ->
      Right [tx1]
    assertBlockNoBackoff dbSyncEnv 101
    -- the 2 tx have been marked but not pruned as they are withing the last 20 blocks
    assertEqQuery dbSyncEnv (DB.queryTxOutConsumedCount txOutTableType) 2 "Unexpected TxOutConsumedByTxId count before rollback"
    rollbackTo interpreter mockServer $ blockPoint blk100
    -- add an empty block
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSyncEnv 101
    -- there should only be 1 tx marked now as the other was deleted in rollback
    assertEqQuery dbSyncEnv (DB.queryTxOutConsumedCount txOutTableType) 1 "Unexpected TxOutConsumedByTxId count after rollback"
    -- cause another prune
    void $ forgeAndSubmitBlocks interpreter mockServer 102
    assertBlockNoBackoff dbSyncEnv 203
    -- everything should be pruned
    assertEqQuery dbSyncEnv (DB.queryTxOutConsumedCount txOutTableType) 1 "Unexpected TxOutConsumedByTxId count after rollback"
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "configPruneAndRollBack"

pruneSameBlock :: IOManager -> [(Text, Text)] -> Assertion
pruneSameBlock = performPruneSameBlock False

pruneSameBlockWithAddress :: IOManager -> [(Text, Text)] -> Assertion
pruneSameBlockWithAddress = performPruneSameBlock True

performPruneSameBlock :: Bool -> IOManager -> [(Text, Text)] -> Assertion
performPruneSameBlock useTxOutAddress =
  withCustomConfigAndDropDB cmdLineArgs (Just $ configPruneForceTxIn useTxOutAddress) babbageConfigDir testLabel $ \interpreter mockServer dbSyncEnv -> do
    startDBSync dbSyncEnv
    let txOutTableType = txOutTableTypeFromConfig dbSyncEnv
    void $ forgeAndSubmitBlocks interpreter mockServer 76
    blk77 <- forgeNextFindLeaderAndSubmit interpreter mockServer []
    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Babbage.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 20000 20000 st
      let utxo0 = head (Babbage.mkUTxOBabbage tx0)
      tx1 <- Babbage.mkPaymentTx (UTxOPair utxo0) (UTxOIndex 2) 10000 500 st
      pure [tx0, tx1]
    assertBlockNoBackoff dbSyncEnv 78
    assertEqQuery dbSyncEnv (DB.queryTxOutConsumedCount txOutTableType) 2 "Unexpected TxOutConsumedByTxId before rollback"
    void $ forgeAndSubmitBlocks interpreter mockServer 22
    assertBlockNoBackoff dbSyncEnv 100
    assertEqQuery dbSyncEnv (DB.queryTxOutConsumedCount txOutTableType) 0 "Unexpected TxOutConsumedByTxId after prune"
    rollbackTo interpreter mockServer (blockPoint blk77)
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSyncEnv 78
    assertTxInCount dbSyncEnv 0
    assertEqQuery dbSyncEnv (DB.queryTxOutConsumedCount txOutTableType) 0 "Unexpected TxOutConsumedByTxId after rollback"
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "configPruneSameBlock"

noPruneSameBlock :: IOManager -> [(Text, Text)] -> Assertion
noPruneSameBlock = performNoPruneSameBlock False

noPruneSameBlockWithAddress :: IOManager -> [(Text, Text)] -> Assertion
noPruneSameBlockWithAddress = performNoPruneSameBlock True

performNoPruneSameBlock :: Bool -> IOManager -> [(Text, Text)] -> Assertion
performNoPruneSameBlock useTxOutAddress =
  withCustomConfigAndDropDB cmdLineArgs (Just $ configConsume useTxOutAddress) babbageConfigDir testLabel $ \interpreter mockServer dbSyncEnv -> do
    startDBSync dbSyncEnv
    let txOutTableType = txOutTableTypeFromConfig dbSyncEnv
    void $ forgeAndSubmitBlocks interpreter mockServer 96
    blk97 <- forgeNextFindLeaderAndSubmit interpreter mockServer []
    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Babbage.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 20000 20000 st
      let utxo0 = head (Babbage.mkUTxOBabbage tx0)
      tx1 <- Babbage.mkPaymentTx (UTxOPair utxo0) (UTxOIndex 2) 10000 500 st
      pure [tx0, tx1]
    void $ forgeAndSubmitBlocks interpreter mockServer 2
    assertBlockNoBackoff dbSyncEnv 100
    rollbackTo interpreter mockServer (blockPoint blk97)
    assertBlockNoBackoff dbSyncEnv 100
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSyncEnv 98
    assertEqQuery dbSyncEnv (DB.queryTxOutConsumedCount txOutTableType) 0 "Unexpected TxOutConsumedByTxId after rollback"
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "configNoPruneSameBlock"

migrateAndPruneRestart :: IOManager -> [(Text, Text)] -> Assertion
migrateAndPruneRestart = performMigrateAndPruneRestart False

migrateAndPruneRestartWithAddress :: IOManager -> [(Text, Text)] -> Assertion
migrateAndPruneRestartWithAddress = performMigrateAndPruneRestart True

performMigrateAndPruneRestart :: Bool -> IOManager -> [(Text, Text)] -> Assertion
performMigrateAndPruneRestart useTxOutAddress = do
  withCustomConfigAndDropDB cmdLineArgs (Just $ configConsume useTxOutAddress) babbageConfigDir testLabel $ \interpreter mockServer dbSyncEnv -> do
    startDBSync dbSyncEnv
    void $ forgeAndSubmitBlocks interpreter mockServer 50
    assertBlockNoBackoff dbSyncEnv 50
    -- stop
    stopDBSync dbSyncEnv
    -- update the syncParams to include new params
    newEnv <- replaceConfigFile "test-db-sync-config.json" dbSyncEnv
    startDBSync newEnv
    -- there is a slight delay before flag is checked
    threadDelay 6000000
    -- checkStillRuns uses `poll` due to this being inside Async and passes along our thrown exception
    checkStillRuns dbSyncEnv
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "configMigrateAndPruneRestart"

pruneRestartMissingFlag :: IOManager -> [(Text, Text)] -> Assertion
pruneRestartMissingFlag = performPruneRestartMissingFlag False

pruneRestartMissingFlagWithAddress :: IOManager -> [(Text, Text)] -> Assertion
pruneRestartMissingFlagWithAddress = performPruneRestartMissingFlag True

performPruneRestartMissingFlag :: Bool -> IOManager -> [(Text, Text)] -> Assertion
performPruneRestartMissingFlag useTxOutAddress = do
  withCustomConfigAndDropDB cmdLineArgs (Just $ configPruneForceTxIn useTxOutAddress) babbageConfigDir testLabel $ \interpreter mockServer dbSyncEnv -> do
    startDBSync dbSyncEnv
    void $ forgeAndSubmitBlocks interpreter mockServer 50
    assertBlockNoBackoff dbSyncEnv 50
    -- stop
    stopDBSync dbSyncEnv
    -- update the syncParams to include new params
    newEnv <- replaceConfigFile "test-db-sync-config.json" dbSyncEnv
    startDBSync newEnv
    -- there is a slight delay before flag is checked
    threadDelay 6000000
    -- checkStillRuns uses `poll` due to this being inside Async and passes along our thrown exception
    checkStillRuns dbSyncEnv
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "configPruneRestartMissingFlag"

bootstrapRestartMissingFlag :: IOManager -> [(Text, Text)] -> Assertion
bootstrapRestartMissingFlag = performBootstrapRestartMissingFlag False

bootstrapRestartMissingFlagWithAddress :: IOManager -> [(Text, Text)] -> Assertion
bootstrapRestartMissingFlagWithAddress = performBootstrapRestartMissingFlag True

performBootstrapRestartMissingFlag :: Bool -> IOManager -> [(Text, Text)] -> Assertion
performBootstrapRestartMissingFlag useTxOutAddress = do
  withCustomConfigAndDropDB cmdLineArgs (Just $ configBootstrap useTxOutAddress) babbageConfigDir testLabel $ \interpreter mockServer dbSyncEnv -> do
    startDBSync dbSyncEnv
    void $ forgeAndSubmitBlocks interpreter mockServer 50
    assertBlockNoBackoff dbSyncEnv 50
    assertTxOutCount dbSyncEnv 0
    -- stop
    stopDBSync dbSyncEnv
    -- update the syncParams to include new params
    newEnv <- replaceConfigFile "test-db-sync-config.json" dbSyncEnv
    startDBSync newEnv
    -- there is a slight delay before flag is checked
    threadDelay 6000000
    -- checkStillRuns uses `poll` due to this being inside Async and passes along our thrown exception
    checkStillRuns dbSyncEnv
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "configBootstrapRestartMissingFlag"
