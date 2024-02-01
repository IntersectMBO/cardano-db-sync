{-# LANGUAGE NumericUnderscores #-}

module Test.Cardano.Db.Mock.Unit.Conway.Tx (
  addSimpleTx,
  addSimpleTxShelley,
  addSimpleTxNoLedger,
  consumeSameBlock,
  addTxMetadata,
  addTxMetadataDisabled,
  addTxMetadataWhitelist,
) where

import Cardano.Ledger.Shelley.TxAuxData (Metadatum (..))
import Cardano.Mock.ChainSync.Server (IOManager ())
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import qualified Cardano.Mock.Forging.Tx.Shelley as Shelley
import Cardano.Mock.Forging.Types (UTxOIndex (..))
import Cardano.Mock.Query (queryNullTxDepositExists, queryTxMetadataCount)
import Cardano.Prelude hiding (head)
import qualified Data.Map as Map
import Test.Cardano.Db.Mock.Config
import qualified Test.Cardano.Db.Mock.UnifiedApi as UnifiedApi
import Test.Cardano.Db.Mock.Validate
import Test.Tasty.HUnit (Assertion ())
import Prelude (head)

addSimpleTx :: IOManager -> [(Text, Text)] -> Assertion
addSimpleTx =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    -- Forge a block
    void $
      UnifiedApi.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10_000 500

    startDBSync dbSync
    -- Verify it syncs
    assertBlockNoBackoff dbSync 1
    assertTxCount dbSync 12
    -- When ledger is enabled, tx.deposits should not be null
    assertEqQuery dbSync queryNullTxDepositExists False "Unexpected null tx deposits"
  where
    testLabel = "conwayAddSimpleTx"

addSimpleTxShelley :: IOManager -> [(Text, Text)] -> Assertion
addSimpleTxShelley =
  withFullConfig shelleyConfigDir testLabel $ \interpreter mockServer dbSync -> do
    -- Forge a shelley block
    void $
      UnifiedApi.withShelleyFindLeaderAndSubmitTx interpreter mockServer $
        Shelley.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10_000 500

    startDBSync dbSync
    -- Verify it syncs
    assertBlockNoBackoff dbSync 1
    assertTxCount dbSync 12
  where
    testLabel = "conwayAddSimpleTxShelley"

addSimpleTxNoLedger :: IOManager -> [(Text, Text)] -> Assertion
addSimpleTxNoLedger = do
  withCustomConfig args Nothing conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    -- Forge a block
    void $
      UnifiedApi.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10_000 500

    startDBSync dbSync
    -- Verify it syncs
    assertBlockNoBackoff dbSync 1
    assertTxCount dbSync 12
    -- When ledger is disabled, tx.deposits should be null
    assertEqQuery dbSync queryNullTxDepositExists True "Unexpected null tx deposits"
  where
    args =
      initCommandLineArgs
        { claConfigFilename = "test-db-sync-config-no-ledger.json"
        , claFullMode = False
        }
    testLabel = "conwayConfigLedgerDisabled"

consumeSameBlock :: IOManager -> [(Text, Text)] -> Assertion
consumeSameBlock =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge some transactions
    void $ UnifiedApi.withConwayFindLeaderAndSubmit interpreter mockServer $ \state' -> do
      tx0 <- Conway.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 20_000 20_000 state'
      let utxo0 = head (Conway.mkUTxOConway tx0)
      -- Create a transaction with UTxOs from tx0
      tx1 <- Conway.mkPaymentTx (UTxOPair utxo0) (UTxOIndex 2) 10_000 500 state'
      pure [tx0, tx1]

    -- Verify the new transaction count
    assertBlockNoBackoff dbSync 1
    assertTxCount dbSync 13
  where
    testLabel = "conwayConsumeSameBlock"

addTxMetadata :: IOManager -> [(Text, Text)] -> Assertion
addTxMetadata = do
  withCustomConfigAndDropDB args Nothing cfgDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Add blocks with transactions
    void $
      UnifiedApi.withConwayFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
        let txBody = Conway.mkDummyTxBody
            auxData = Map.fromList [(1, I 1), (2, I 2)]
         in Right (Conway.mkAuxDataTx True txBody auxData)

    -- Wait for it to sync
    assertBlockNoBackoff dbSync 1
    -- Should have tx metadata
    assertEqBackoff dbSync queryTxMetadataCount 2 [] "Expected tx metadata"
  where
    args =
      initCommandLineArgs
        { claFullMode = False
        }
    testLabel = "conwayConfigMetadataEnabled"
    cfgDir = conwayConfigDir

addTxMetadataWhitelist :: IOManager -> [(Text, Text)] -> Assertion
addTxMetadataWhitelist = do
  withCustomConfigAndDropDB args Nothing cfgDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Add blocks with transactions
    void $ do
      UnifiedApi.withConwayFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
        let txBody = Conway.mkDummyTxBody
            auxData = Map.fromList [(1, I 1), (2, I 2)]
         in Right (Conway.mkAuxDataTx True txBody auxData)

    -- Wait for it to sync
    assertBlockNoBackoff dbSync 1
    -- Should have tx metadata
    assertEqBackoff dbSync queryTxMetadataCount 1 [] "Expected tx metadata"
  where
    args =
      initCommandLineArgs
        { claConfigFilename = "test-db-sync-config-keep-metadata.json"
        , claFullMode = False
        }
    testLabel = "conwayConfigMetadataKeep"
    cfgDir = conwayConfigDir

addTxMetadataDisabled :: IOManager -> [(Text, Text)] -> Assertion
addTxMetadataDisabled = do
  withCustomConfigAndDropDB args Nothing cfgDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Add blocks with transactions
    void $
      UnifiedApi.withConwayFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
        let txBody = Conway.mkDummyTxBody
            auxData = Map.singleton 1 (I 1)
         in Right (Conway.mkAuxDataTx True txBody auxData)

    -- Wait for it to sync
    assertBlockNoBackoff dbSync 1
    -- Should have tx metadata
    assertEqBackoff dbSync queryTxMetadataCount 0 [] "Expected tx metadata"
  where
    args =
      initCommandLineArgs
        { claConfigFilename = "test-db-sync-config-no-metadata.json"
        , claFullMode = False
        }
    testLabel = "conwayConfigMetadataDisabled"
    cfgDir = conwayConfigDir
