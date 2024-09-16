{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Test.Cardano.Db.Mock.Unit.Babbage.Config.MigrateConsumedPruneTxOut (
  txConsumedColumnCheck,
  basicPrune,
  pruneWithSimpleRollback,
  pruneWithFullTxRollback,
  pruningShouldKeepSomeTx,
  pruneAndRollBackOneBlock,
  noPruneAndRollBack,
  pruneSameBlock,
  noPruneSameBlock,
  migrateAndPruneRestart,
  pruneRestartMissingFlag,
  bootstrapRestartMissingFlag,
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Config.Types
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
  CommandLineArgs (..),
  babbageConfigDir,
  initCommandLineArgs,
  mkCustomSyncNodeConfig,
  replaceConfigFile,
  startDBSync,
  stopDBSync,
  withCustomConfig,
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

migrateTxOutPruneForceTxIn :: SyncNodeConfig -> SyncNodeConfig
migrateTxOutPruneForceTxIn cfg = do
  cfg {dncInsertOptions = (dncInsertOptions cfg) {sioTxOut = TxOutPrune (ForceTxIn True)}}

migrateTxOutPruneConfig :: SyncNodeConfig -> SyncNodeConfig
migrateTxOutPruneConfig cfg = do
  cfg {dncInsertOptions = (dncInsertOptions cfg) {sioTxOut = TxOutPrune (ForceTxIn False)}}

migrateTxOutConsumeConfig :: SyncNodeConfig -> SyncNodeConfig
migrateTxOutConsumeConfig cfg = do
  cfg {dncInsertOptions = (dncInsertOptions cfg) {sioTxOut = TxOutConsumed (ForceTxIn False)}}

txConsumedColumnCheck :: IOManager -> [(Text, Text)] -> Assertion
txConsumedColumnCheck ioManager metadata = do
  syncNodeConfig <- mkCustomSyncNodeConfig babbageConfigDir args migrateTxOutPruneForceTxIn
  withCustomConfig args (Just syncNodeConfig) babbageConfigDir testLabel action ioManager metadata
  where
    action interpreter mockServer dbSyncEnv = do
      void $
        withBabbageFindLeaderAndSubmitTx interpreter mockServer $
          Babbage.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 500

      startDBSync dbSyncEnv
      assertBlockNoBackoff dbSyncEnv 1
      assertEqQuery dbSyncEnv DB.queryTxConsumedColumnExists True "missing consumed_by_tx_id column when flag --consumed-tx-out active"

    args = initCommandLineArgs
    testLabel = "configTxConsumedColumnCheck"

basicPrune :: IOManager -> [(Text, Text)] -> Assertion
basicPrune ioManager metadata = do
  syncNodeConfig <- mkCustomSyncNodeConfig babbageConfigDir args migrateTxOutPruneForceTxIn
  withCustomConfig args (Just syncNodeConfig) babbageConfigDir testLabel action ioManager metadata
  where
    action interpreter mockServer dbSyncEnv = do
      startDBSync dbSyncEnv
      -- add 50 block
      b1 <- forgeAndSubmitBlocks interpreter mockServer 50
      -- add 2 blocks with tx
      void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ Babbage.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000
      void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ Babbage.mkPaymentTx (UTxOIndex 1) (UTxOIndex 0) 10000 10000
      assertBlockNoBackoff dbSyncEnv (fromIntegral $ length b1 + 2)
      -- check tx-out count before any pruning has happened
      assertEqQuery dbSyncEnv DB.queryTxOutCount 14 "new epoch didn't prune tx_out column that are null"
      -- add other blocks to instantiate the pruning
      b2 <- forgeAndSubmitBlocks interpreter mockServer 48
      assertBlockNoBackoff dbSyncEnv (fromIntegral $ length (b1 <> b2) + 2)

      -- check that the tx_out has been pruned
      assertEqQuery dbSyncEnv DB.queryTxOutCount 12 "the pruning didn't work correctly as the tx-out count is incorrect"
      -- check Unspent tx match after pruning
      assertUnspentTx dbSyncEnv

    args = initCommandLineArgs
    testLabel = "configPrune"

pruneWithSimpleRollback :: IOManager -> [(Text, Text)] -> Assertion
pruneWithSimpleRollback ioManager metadata = do
  syncNodeConfig <- mkCustomSyncNodeConfig babbageConfigDir args migrateTxOutPruneForceTxIn
  withCustomConfig args (Just syncNodeConfig) babbageConfigDir testLabel action ioManager metadata
  where
    action interpreter mockServer dbSyncEnv = do
      blk0 <- forgeNext interpreter mockBlock0
      blk1 <- forgeNext interpreter mockBlock1
      atomically $ addBlock mockServer blk0
      startDBSync dbSyncEnv
      atomically $ addBlock mockServer blk1
      void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ Babbage.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000
      void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ Babbage.mkPaymentTx (UTxOIndex 1) (UTxOIndex 0) 10000 10000
      assertEqQuery dbSyncEnv DB.queryTxOutCount 14 ""
      b1 <- forgeAndSubmitBlocks interpreter mockServer 96
      assertBlockNoBackoff dbSyncEnv (fullBlockSize b1)
      assertEqQuery dbSyncEnv DB.queryTxOutCount 12 "the txOut count is incorrect"
      assertEqQuery dbSyncEnv DB.queryTxOutConsumedCount 0 "Unexpected TxOutConsumedByTxId count after prune"
      assertUnspentTx dbSyncEnv

      rollbackTo interpreter mockServer (blockPoint blk1)
      assertEqQuery dbSyncEnv DB.queryTxOutConsumedCount 0 "Unexpected TxOutConsumedByTxId cout after rollback"
      assertBlockNoBackoff dbSyncEnv $ fullBlockSize b1

    args = initCommandLineArgs
    fullBlockSize b = fromIntegral $ length b + 4
    testLabel = "configPruneSimpleRollback"

pruneWithFullTxRollback :: IOManager -> [(Text, Text)] -> Assertion
pruneWithFullTxRollback ioManager metadata = do
  syncNodeConfig <- mkCustomSyncNodeConfig babbageConfigDir args migrateTxOutPruneForceTxIn
  withCustomConfig args (Just syncNodeConfig) babbageConfigDir testLabel action ioManager metadata
  where
    action interpreter mockServer dbSyncEnv = do
      startDBSync dbSyncEnv
      blk0 <- forgeNextFindLeaderAndSubmit interpreter mockServer []
      void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
        tx0 <- Babbage.mkFullTx 0 100 st
        tx1 <- Babbage.mkFullTx 1 200 st
        pure [tx0, tx1]
      assertBlockNoBackoff dbSyncEnv 2
      assertTxCount dbSyncEnv 13
      assertUnspentTx dbSyncEnv
      assertEqQuery dbSyncEnv DB.queryTxOutCount 14 "new epoch didn't prune tx_out column that are null"
      rollbackTo interpreter mockServer $ blockPoint blk0
      void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
        tx0 <- Babbage.mkFullTx 0 100 st
        tx1 <- Babbage.mkFullTx 1 200 st
        tx2 <- Babbage.mkFullTx 2 200 st
        pure [tx1, tx2, tx0]
      assertBlockNoBackoff dbSyncEnv 2
      assertTxCount dbSyncEnv 14
      assertEqQuery dbSyncEnv DB.queryTxOutCount 16 "new epoch didn't prune tx_out column that are null"
      assertUnspentTx dbSyncEnv

    args = initCommandLineArgs
    testLabel = "configPruneOnFullRollback"

-- The tx in the last, 2 x securityParam worth of blocks should not be pruned.
-- In these tests, 2 x securityParam = 20 blocks.
pruningShouldKeepSomeTx :: IOManager -> [(Text, Text)] -> Assertion
pruningShouldKeepSomeTx ioManager metadata = do
  syncNodeConfig <- mkCustomSyncNodeConfig babbageConfigDir args migrateTxOutPruneConfig
  withCustomConfig args (Just syncNodeConfig) babbageConfigDir testLabel action ioManager metadata
  where
    action interpreter mockServer dbSyncEnv = do
      startDBSync dbSyncEnv
      b1 <- forgeAndSubmitBlocks interpreter mockServer 80
      -- these two blocs + tx will fall withing the last 20 blocks so should not be pruned
      void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ Babbage.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000
      void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ Babbage.mkPaymentTx (UTxOIndex 2) (UTxOIndex 3) 10000 10000
      b2 <- forgeAndSubmitBlocks interpreter mockServer 18
      assertBlockNoBackoff dbSyncEnv (fromIntegral $ length (b1 <> b2) + 2)
      -- the two marked TxOutConsumedByTxId should not be pruned
      assertEqQuery dbSyncEnv DB.queryTxOutConsumedCount 2 "Unexpected TxOutConsumedByTxId count after prune"
      -- add more blocks to instantiate another prune
      b3 <- forgeAndSubmitBlocks interpreter mockServer 110
      assertBlockNoBackoff dbSyncEnv (fromIntegral $ length (b1 <> b2 <> b3) + 2)
      -- the prune should have removed all
      assertTxInCount dbSyncEnv 0
      assertEqQuery dbSyncEnv DB.queryTxOutConsumedCount 0 "Unexpected TxOutConsumedByTxId count after prune"

    args = initCommandLineArgs
    testLabel = "configPruneCorrectAmount"

-- prune with rollback
pruneAndRollBackOneBlock :: IOManager -> [(Text, Text)] -> Assertion
pruneAndRollBackOneBlock ioManager metadata = do
  syncNodeConfig <- mkCustomSyncNodeConfig babbageConfigDir args migrateTxOutPruneForceTxIn
  withCustomConfig args (Just syncNodeConfig) babbageConfigDir testLabel action ioManager metadata
  where
    action interpreter mockServer dbSyncEnv = do
      startDBSync dbSyncEnv
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
      assertEqQuery dbSyncEnv DB.queryTxOutConsumedCount 2 "Unexpected TxOutConsumedByTxId count before rollback"
      rollbackTo interpreter mockServer $ blockPoint blk100
      -- add an empty block
      void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
      assertBlockNoBackoff dbSyncEnv 101
      -- there should only be 1 tx marked now as the other was deleted in rollback
      assertEqQuery dbSyncEnv DB.queryTxOutConsumedCount 1 "Unexpected TxOutConsumedByTxId count after rollback"
      -- cause another prune
      void $ forgeAndSubmitBlocks interpreter mockServer 102
      assertBlockNoBackoff dbSyncEnv 203
      -- everything should be pruned
      assertEqQuery dbSyncEnv DB.queryTxOutConsumedCount 0 "Unexpected TxOutConsumedByTxId count after rollback"

    args = initCommandLineArgs
    testLabel = "configPruneAndRollBack"

-- consume with rollback
noPruneAndRollBack :: IOManager -> [(Text, Text)] -> Assertion
noPruneAndRollBack ioManager metadata = do
  syncNodeConfig <- mkCustomSyncNodeConfig babbageConfigDir args migrateTxOutConsumeConfig
  withCustomConfig args (Just syncNodeConfig) babbageConfigDir testLabel action ioManager metadata
  where
    action interpreter mockServer dbSyncEnv = do
      startDBSync dbSyncEnv
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
      assertEqQuery dbSyncEnv DB.queryTxOutConsumedCount 2 "Unexpected TxOutConsumedByTxId count before rollback"
      rollbackTo interpreter mockServer $ blockPoint blk100
      -- add an empty block
      void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
      assertBlockNoBackoff dbSyncEnv 101
      -- there should only be 1 tx marked now as the other was deleted in rollback
      assertEqQuery dbSyncEnv DB.queryTxOutConsumedCount 1 "Unexpected TxOutConsumedByTxId count after rollback"
      -- cause another prune
      void $ forgeAndSubmitBlocks interpreter mockServer 102
      assertBlockNoBackoff dbSyncEnv 203
      -- everything should be pruned
      assertEqQuery dbSyncEnv DB.queryTxOutConsumedCount 1 "Unexpected TxOutConsumedByTxId count after rollback"

    args = initCommandLineArgs
    testLabel = "configPruneAndRollBack"

pruneSameBlock :: IOManager -> [(Text, Text)] -> Assertion
pruneSameBlock ioManager metadata = do
  syncNodeConfig <- mkCustomSyncNodeConfig babbageConfigDir args migrateTxOutPruneForceTxIn
  withCustomConfig args (Just syncNodeConfig) babbageConfigDir testLabel action ioManager metadata
  where
    action interpreter mockServer dbSyncEnv = do
      startDBSync dbSyncEnv
      void $ forgeAndSubmitBlocks interpreter mockServer 76
      blk77 <- forgeNextFindLeaderAndSubmit interpreter mockServer []
      void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
        tx0 <- Babbage.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 20000 20000 st
        let utxo0 = head (Babbage.mkUTxOBabbage tx0)
        tx1 <- Babbage.mkPaymentTx (UTxOPair utxo0) (UTxOIndex 2) 10000 500 st
        pure [tx0, tx1]
      assertBlockNoBackoff dbSyncEnv 78
      assertEqQuery dbSyncEnv DB.queryTxOutConsumedCount 2 "Unexpected TxOutConsumedByTxId before rollback"
      void $ forgeAndSubmitBlocks interpreter mockServer 22
      assertBlockNoBackoff dbSyncEnv 100
      assertEqQuery dbSyncEnv DB.queryTxOutConsumedCount 0 "Unexpected TxOutConsumedByTxId after prune"
      rollbackTo interpreter mockServer (blockPoint blk77)
      void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
      assertBlockNoBackoff dbSyncEnv 78
      assertTxInCount dbSyncEnv 0
      assertEqQuery dbSyncEnv DB.queryTxOutConsumedCount 0 "Unexpected TxOutConsumedByTxId after rollback"

    args = initCommandLineArgs
    testLabel = "configPruneSameBlock"

noPruneSameBlock :: IOManager -> [(Text, Text)] -> Assertion
noPruneSameBlock ioManager metadata = do
  syncNodeConfig <- mkCustomSyncNodeConfig babbageConfigDir args migrateTxOutPruneForceTxIn
  withCustomConfig args (Just syncNodeConfig) babbageConfigDir testLabel action ioManager metadata
  where
    action interpreter mockServer dbSyncEnv = do
      startDBSync dbSyncEnv
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
      assertEqQuery dbSyncEnv DB.queryTxOutConsumedCount 0 "Unexpected TxOutConsumedByTxId after rollback"

    args = initCommandLineArgs
    testLabel = "configNoPruneSameBlock"

migrateAndPruneRestart :: IOManager -> [(Text, Text)] -> Assertion
migrateAndPruneRestart ioManager metadata = do
  syncNodeConfig <- mkCustomSyncNodeConfig babbageConfigDir args migrateTxOutPruneForceTxIn
  withCustomConfig args (Just syncNodeConfig) babbageConfigDir testLabel action ioManager metadata
  where
    action interpreter mockServer dbSyncEnv = do
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

    args = initCommandLineArgs
    testLabel = "configMigrateAndPruneRestart"

pruneRestartMissingFlag :: IOManager -> [(Text, Text)] -> Assertion
pruneRestartMissingFlag ioManager metadata = do
  syncNodeConfig <- mkCustomSyncNodeConfig babbageConfigDir args migrateTxOutPruneForceTxIn
  withCustomConfig args (Just syncNodeConfig) babbageConfigDir testLabel action ioManager metadata
  where
    action interpreter mockServer dbSyncEnv = do
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

    args = initCommandLineArgs
    testLabel = "configPruneRestartMissingFlag"

bootstrapRestartMissingFlag :: IOManager -> [(Text, Text)] -> Assertion
bootstrapRestartMissingFlag ioManager metadata = do
  syncNodeConfig <- mkCustomSyncNodeConfig babbageConfigDir args migrateTxOutPruneForceTxIn
  withCustomConfig args (Just syncNodeConfig) babbageConfigDir testLabel action ioManager metadata
  where
    action interpreter mockServer dbSyncEnv = do
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

    args = initCommandLineArgs {claBootstrap = True}
    testLabel = "configBootstrapRestartMissingFlag"
