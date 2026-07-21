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
basicPrune :: DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion
basicPrune source = performBasicPrune source False

basicPruneWithAddress :: DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion
basicPruneWithAddress source = performBasicPrune source True

performBasicPrune :: DB.PGPassSource -> Bool -> IOManager -> [(Text, Text)] -> Assertion
performBasicPrune source useTxOutAddress = do
  withCustomConfigDropDB args (Just $ configPruneForceTxIn useTxOutAddress) source cfgDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    let txOutVariantType = txOutVariantTypeFromConfig dbSync

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
    assertEqQuery dbSync (DB.queryTxOutCount txOutVariantType) 14 "new epoch didn't prune tx_out column that are null"

    blks' <- forgeAndSubmitBlocks interpreter mockServer 48
    assertBlockNoBackoff dbSync (fullBlockSize $ blks <> blks')

    -- Check that tx_out was pruned
    assertEqQuery dbSync (DB.queryTxOutCount txOutVariantType) 12 "the pruning didn't work correctly as the tx-out count is incorrect"
    -- Check unspent tx
    assertUnspentTx dbSync
  where
    args = initCommandLineArgs
    testLabel = "conwayConfigPrune"
    fullBlockSize b = fromIntegral $ length b + 2
    cfgDir = conwayConfigDir

pruneWithSimpleRollback :: DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion
pruneWithSimpleRollback source = performPruneWithSimpleRollback source False

pruneWithSimpleRollbackWithAddress :: DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion
pruneWithSimpleRollbackWithAddress source = performPruneWithSimpleRollback source True

performPruneWithSimpleRollback :: DB.PGPassSource -> Bool -> IOManager -> [(Text, Text)] -> Assertion
performPruneWithSimpleRollback source useTxOutAddress =
  withCustomConfigDropDB cmdLineArgs (Just $ configPruneForceTxIn useTxOutAddress) source conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    let txOutVariantType = txOutVariantTypeFromConfig dbSync
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
    assertEqQuery dbSync (DB.queryTxOutCount txOutVariantType) 14 ""

    -- Submit some blocks
    blks <- forgeAndSubmitBlocks interpreter mockServer 96
    assertBlockNoBackoff dbSync (fullBlockSize blks)
    assertEqQuery dbSync (DB.queryTxOutCount txOutVariantType) 12 "the txOut count is incorrect"
    assertEqQuery dbSync (DB.queryTxOutConsumedCount txOutVariantType) 0 "Unexpected TxOutConsumedByTxId count after prune"
    assertUnspentTx dbSync

    -- Rollback
    rbBlocks <- rollbackTo interpreter mockServer (blockPoint blk1)
    assertEqQuery dbSync (DB.queryTxOutConsumedCount txOutVariantType) 0 "Unexpected TxOutConsumedByTxId count after rollback"
    assertBlockNoBackoff dbSync (2 + length rbBlocks)
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "conwayConfigPruneSimpleRollback"
    fullBlockSize b = fromIntegral $ length b + 4

pruneWithFullTxRollback :: DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion
pruneWithFullTxRollback source = performPruneWithFullTxRollback source False

pruneWithFullTxRollbackWithAddress :: DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion
pruneWithFullTxRollbackWithAddress source = performPruneWithFullTxRollback source True

performPruneWithFullTxRollback :: DB.PGPassSource -> Bool -> IOManager -> [(Text, Text)] -> Assertion
performPruneWithFullTxRollback source useTxOutAddress =
  withCustomConfigDropDB cmdLineArgs (Just $ configPruneForceTxIn useTxOutAddress) source conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    let txOutVariantType = txOutVariantTypeFromConfig dbSync
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
    assertEqQuery dbSync (DB.queryTxOutCount txOutVariantType) 14 "new epoch didn't prune tx_out column that are null"

    -- Rollback
    rbBlocks <- rollbackTo interpreter mockServer $ blockPoint blk0
    -- Add more transactions
    void $ withConwayFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Conway.mkFullTx 0 100 st
      tx1 <- Conway.mkFullTx 1 200 st
      tx2 <- Conway.mkFullTx 2 200 st
      pure [tx1, tx2, tx0]

    -- Verify tx_out was pruned again
    assertBlockNoBackoff dbSync (2 + length rbBlocks)
    assertTxCount dbSync 15
    assertEqQuery dbSync (DB.queryTxOutCount txOutVariantType) 16 "new epoch didn't prune tx_out column that are null"
    assertUnspentTx dbSync
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "conwayConfigPruneOnFullRollback"

-- The transactions in the last `2 * securityParam` blocks should not be pruned

pruningShouldKeepSomeTx :: DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion
pruningShouldKeepSomeTx source = performPruningShouldKeepSomeTx source False

pruningShouldKeepSomeTxWithAddress :: DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion
pruningShouldKeepSomeTxWithAddress source = performPruningShouldKeepSomeTx source True

performPruningShouldKeepSomeTx :: DB.PGPassSource -> Bool -> IOManager -> [(Text, Text)] -> Assertion
performPruningShouldKeepSomeTx source useTxOutAddress = do
  withCustomConfigDropDB cmdLineArgs (Just $ configPrune useTxOutAddress) source conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    let txOutVariantType = txOutVariantTypeFromConfig dbSync
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
    assertEqQuery dbSync (DB.queryTxOutConsumedCount txOutVariantType) 2 "Unexpected TxOutConsumedByTxId count after prune"

    -- Add more blocks
    blk3 <- forgeAndSubmitBlocks interpreter mockServer 110
    -- Verify everything has been pruned
    assertBlockNoBackoff dbSync (fromIntegral $ length (blk1 <> blk2 <> blk3) + 2)
    assertTxInCount dbSync 0
    assertEqQuery dbSync (DB.queryTxOutConsumedCount txOutVariantType) 0 "Unexpected TxOutConsumedByTxId count after prune"
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "conwayConfigPruneCorrectAmount"

pruneAndRollBackOneBlock :: DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion
pruneAndRollBackOneBlock source = performPruneAndRollBackOneBlock source False

pruneAndRollBackOneBlockWithAddress :: DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion
pruneAndRollBackOneBlockWithAddress source = performPruneAndRollBackOneBlock source True

performPruneAndRollBackOneBlock :: DB.PGPassSource -> Bool -> IOManager -> [(Text, Text)] -> Assertion
performPruneAndRollBackOneBlock source useTxOutAddress =
  withCustomConfigDropDB cmdLineArgs (Just $ configPruneForceTxIn useTxOutAddress) source conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    let txOutVariantType = txOutVariantTypeFromConfig dbSync
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
    assertEqQuery dbSync (DB.queryTxOutConsumedCount txOutVariantType) 2 "Unexpected TxOutConsumedByTxId count before rollback"

    rbBlocks <- rollbackTo interpreter mockServer (blockPoint blk100)

    -- Verify the transactions were removed in the rollback (100 is max block number at blk100)
    assertBlockNoBackoff dbSync (100 + length rbBlocks)
    assertEqQuery dbSync (DB.queryTxOutConsumedCount txOutVariantType) 1 "Unexpected TxOutConsumedByTxId count after rollback"

    -- Trigger a prune
    void $ forgeAndSubmitBlocks interpreter mockServer 102
    -- Verify everything was pruned
    assertBlockNoBackoff dbSync (100 + length rbBlocks + 102)
    assertEqQuery dbSync (DB.queryTxOutConsumedCount txOutVariantType) 0 "Unexpected TxOutConsumedByTxId count after rollback"
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "conwayConfigPruneAndRollBack"

noPruneAndRollBack :: DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion
noPruneAndRollBack source = performNoPruneAndRollBack source False

noPruneAndRollBackWithAddress :: DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion
noPruneAndRollBackWithAddress source = performNoPruneAndRollBack source True

performNoPruneAndRollBack :: DB.PGPassSource -> Bool -> IOManager -> [(Text, Text)] -> Assertion
performNoPruneAndRollBack source useTxOutAddress =
  withCustomConfigDropDB cmdLineArgs (Just $ configConsume useTxOutAddress) source conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    let txOutVariantType = txOutVariantTypeFromConfig dbSync
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
    assertEqQuery dbSync (DB.queryTxOutConsumedCount txOutVariantType) 2 "Unexpected TxOutConsumedByTxId count before rollback"

    rbBlocks <- rollbackTo interpreter mockServer (blockPoint blk100)

    -- Verify transactions were removed (100 is max block number at blk100)
    assertBlockNoBackoff dbSync (100 + length rbBlocks)
    assertEqQuery dbSync (DB.queryTxOutConsumedCount txOutVariantType) 1 "Unexpected TxOutConsumedByTxId count after rollback"

    -- Add some more blocks
    void $ forgeAndSubmitBlocks interpreter mockServer 102
    -- Verify nothing has been pruned
    assertBlockNoBackoff dbSync (100 + length rbBlocks + 102)
    assertEqQuery dbSync (DB.queryTxOutConsumedCount txOutVariantType) 1 "Unexpected TxOutConsumedByTxId count after rollback"
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "conwayConfigNoPruneAndRollBack"

pruneSameBlock :: DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion
pruneSameBlock source = performPruneSameBlock source False

pruneSameBlockWithAddress :: DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion
pruneSameBlockWithAddress source = performPruneSameBlock source True

performPruneSameBlock :: DB.PGPassSource -> Bool -> IOManager -> [(Text, Text)] -> Assertion
performPruneSameBlock source useTxOutAddress =
  withCustomConfigDropDB cmdLineArgs (Just $ configPruneForceTxIn useTxOutAddress) source conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    let txOutVariantType = txOutVariantTypeFromConfig dbSync
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
    assertEqQuery dbSync (DB.queryTxOutConsumedCount txOutVariantType) 2 "Unexpected TxOutConsumedByTxId before rollback"

    -- Trigger a prune
    void $ forgeAndSubmitBlocks interpreter mockServer 22
    -- Verify the transactions were pruned
    assertBlockNoBackoff dbSync 100
    assertEqQuery dbSync (DB.queryTxOutConsumedCount txOutVariantType) 0 "Unexpected TxOutConsumedByTxId after prune"

    rbBlocks <- rollbackTo interpreter mockServer (blockPoint blk77)
    let rbBlocksLength = length rbBlocks

    -- Verify the transactions were pruned again (77 is max block number at blk77, + rbBlocks)
    assertBlockNoBackoff dbSync (77 + rbBlocksLength)
    assertTxInCount dbSync 0
    assertEqQuery dbSync (DB.queryTxOutConsumedCount txOutVariantType) 0 "Unexpected TxOutConsumedByTxId after rollback"
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "conwayConfigPruneSameBlock"

noPruneSameBlock :: DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion
noPruneSameBlock source = performNoPruneSameBlock source False

noPruneSameBlockWithAddress :: DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion
noPruneSameBlockWithAddress source = performNoPruneSameBlock source True

performNoPruneSameBlock :: DB.PGPassSource -> Bool -> IOManager -> [(Text, Text)] -> Assertion
performNoPruneSameBlock source useTxOutAddress =
  withCustomConfigDropDB cmdLineArgs (Just $ configConsume useTxOutAddress) source conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
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

    rbBlocks <- rollbackTo interpreter mockServer (blockPoint blk97)

    -- Verify we haven't pruned anything yet
    assertBlockNoBackoff dbSync 100
    -- Add an empty block
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    -- Verify everything was pruned
    assertBlockNoBackoff dbSync (98 + length rbBlocks)
    assertEqQuery dbSync (DB.queryTxOutConsumedCount $ txOutVariantTypeFromConfig dbSync) 0 "Unexpected TxOutConsumedByTxId after rollback"
  where
    cmdLineArgs = initCommandLineArgs
    testLabel = "conwayConfigNoPruneSameBlock"

migrateAndPruneRestart :: DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion
migrateAndPruneRestart source = performMigrateAndPruneRestart source False

migrateAndPruneRestartWithAddress :: DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion
migrateAndPruneRestartWithAddress source = performMigrateAndPruneRestart source True

performMigrateAndPruneRestart :: DB.PGPassSource -> Bool -> IOManager -> [(Text, Text)] -> Assertion
performMigrateAndPruneRestart source useTxOutAddress =
  withCustomConfigDropDB cmdLineArgs (Just $ configConsume useTxOutAddress) source conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
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

pruneRestartMissingFlag :: DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion
pruneRestartMissingFlag source = performPruneRestartMissingFlag source False

pruneRestartMissingFlagWithAddress :: DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion
pruneRestartMissingFlagWithAddress source = performPruneRestartMissingFlag source True

performPruneRestartMissingFlag :: DB.PGPassSource -> Bool -> IOManager -> [(Text, Text)] -> Assertion
performPruneRestartMissingFlag source useTxOutAddress =
  withCustomConfigDropDB cmdLineArgs (Just $ configPruneForceTxIn useTxOutAddress) source conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
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

bootstrapRestartMissingFlag :: DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion
bootstrapRestartMissingFlag source = performBootstrapRestartMissingFlag source False

bootstrapRestartMissingFlagWithAddress :: DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion
bootstrapRestartMissingFlagWithAddress source = performBootstrapRestartMissingFlag source True

performBootstrapRestartMissingFlag :: DB.PGPassSource -> Bool -> IOManager -> [(Text, Text)] -> Assertion
performBootstrapRestartMissingFlag source useTxOutAddress =
  withCustomConfigDropDB cmdLineArgs (Just $ configBootstrap useTxOutAddress) source conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
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

populateDbRestartWithAddressConfig :: DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion
populateDbRestartWithAddressConfig source =
  withCustomConfigDropDB cmdLineArgs (Just $ configConsume False) source conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
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
