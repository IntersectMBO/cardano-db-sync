{-# LANGUAGE NumericUnderscores #-}

module Test.Cardano.Db.Mock.Unit.Conway.Tx (
  addSimpleTx,
  addSimpleTxShelley,
  addSimpleTxNoLedger,
  consumeSameBlock,
  addTxMetadata,
  addTxMetadataDisabled,
  addTxMetadataWhitelist,
  addTxMetadataWhitelistMultiple,
) where

import Cardano.Api.Ledger (Coin (..))
import Cardano.DbSync.Config (SyncNodeConfig (..))
import Cardano.DbSync.Config.Types (MetadataConfig (..), SyncInsertOptions (..))
import Cardano.Ledger.Shelley.TxAuxData (Metadatum (..))
import Cardano.Mock.ChainSync.Server (IOManager ())
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import qualified Cardano.Mock.Forging.Tx.Shelley as Shelley
import Cardano.Mock.Forging.Types (UTxOIndex (..))
import Cardano.Mock.Query (queryNullTxDepositExists, queryTxMetadataCount)
import Cardano.Prelude hiding (head)
import Data.List.NonEmpty (fromList)
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
addTxMetadata ioManager metadata = do
  syncNodeConfig <- mksNodeConfig
  withCustomConfigAndDropDB args (Just syncNodeConfig) cfgDir testLabel action ioManager metadata
  where
    action = \interpreter mockServer dbSync -> do
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

    args = initCommandLineArgs {claFullMode = False}
    testLabel = "conwayConfigMetadataEnabled"

    cfgDir = conwayConfigDir

    mksNodeConfig :: IO SyncNodeConfig
    mksNodeConfig = do
      initConfigFile <- mkSyncNodeConfig cfgDir args
      let dncInsertOptions' = dncInsertOptions initConfigFile
      pure $
        initConfigFile
          { dncInsertOptions = dncInsertOptions' {sioMetadata = MetadataEnable}
          }

addTxMetadataDisabled :: IOManager -> [(Text, Text)] -> Assertion
addTxMetadataDisabled ioManager metadata = do
  syncNodeConfig <- mksNodeConfig
  withCustomConfigAndDropDB args (Just syncNodeConfig) cfgDir testLabel action ioManager metadata
  where
    action = \interpreter mockServer dbSync -> do
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
      assertEqBackoff dbSync queryTxMetadataCount 0 [] "Expected tx metadata"

    args = initCommandLineArgs {claFullMode = False}
    testLabel = "conwayConfigMetadataDisabled"

    cfgDir = conwayConfigDir

    mksNodeConfig :: IO SyncNodeConfig
    mksNodeConfig = do
      initConfigFile <- mkSyncNodeConfig cfgDir args
      let dncInsertOptions' = dncInsertOptions initConfigFile
      pure $
        initConfigFile
          { dncInsertOptions = dncInsertOptions' {sioMetadata = MetadataDisable}
          }

-- 2 blocks each with 4 metadata entries.
-- The whitelist has one tx metadata key which is in the first block
-- so only the TX in the first block should have tx metadata kept.
addTxMetadataWhitelist :: IOManager -> [(Text, Text)] -> Assertion
addTxMetadataWhitelist ioManager metadata = do
  syncNodeConfig <- mksNodeConfig
  withCustomConfigAndDropDB args (Just syncNodeConfig) cfgDir testLabel action ioManager metadata
  where
    action = \interpreter mockServer dbSync -> do
      startDBSync dbSync
      -- Add transactions with metadata
      void $ do
        UnifiedApi.withConwayFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
          let txBody = Conway.mkDummyTxBodyWithCoin $ Coin 1_000
              auxData = Map.fromList [(1, I 1), (2, I 2), (3, I 3), (4, I 4)]
           in Right (Conway.mkAuxDataTx True txBody auxData)
      void $ do
        UnifiedApi.withConwayFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
          let txBody = Conway.mkDummyTxBodyWithCoin $ Coin 2_000
              auxData = Map.fromList [(5, I 5), (6, I 6), (7, I 7), (8, I 8)]
           in Right (Conway.mkAuxDataTx True txBody auxData)

      assertBlockNoBackoff dbSync 2
      -- Should have first block's tx metadata
      assertEqBackoff dbSync queryTxMetadataCount 4 [] "Expected tx metadata"

    args = initCommandLineArgs {claFullMode = False}
    testLabel = "conwayConfigMetadataWhitelist"

    cfgDir = conwayConfigDir

    -- match all metadata keys of value 1
    mksNodeConfig :: IO SyncNodeConfig
    mksNodeConfig = do
      initConfigFile <- mkSyncNodeConfig cfgDir args
      let dncInsertOptions' = dncInsertOptions initConfigFile
      pure $
        initConfigFile
          { dncInsertOptions = dncInsertOptions' {sioMetadata = MetadataKeys $ fromList [1]}
          }

-- 2 blocks each with 4 metadata entries
-- The whitelist is set to keys [1,6] each key in in different TX
-- so all TxMetadata should be kept from both blocks.
addTxMetadataWhitelistMultiple :: IOManager -> [(Text, Text)] -> Assertion
addTxMetadataWhitelistMultiple ioManager metadata = do
  syncNodeConfig <- mksNodeConfig
  withCustomConfigAndDropDB args (Just syncNodeConfig) cfgDir testLabel action ioManager metadata
  where
    action = \interpreter mockServer dbSync -> do
      startDBSync dbSync
      -- Add transactions with metadata
      void $ do
        UnifiedApi.withConwayFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
          let txBody = Conway.mkDummyTxBodyWithCoin $ Coin 1_000
              auxData = Map.fromList [(1, I 1), (2, I 2), (3, I 3), (4, I 4)]
           in Right (Conway.mkAuxDataTx True txBody auxData)
      void $ do
        UnifiedApi.withConwayFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
          let txBody = Conway.mkDummyTxBodyWithCoin $ Coin 2_000
              auxData = Map.fromList [(5, I 5), (6, I 6), (7, I 7), (8, I 8)]
           in Right (Conway.mkAuxDataTx True txBody auxData)

      assertBlockNoBackoff dbSync 2
      -- Should have both block's tx metadata
      assertEqBackoff dbSync queryTxMetadataCount 8 [] "Expected tx metadata"

    args = initCommandLineArgs {claFullMode = False}
    testLabel = "conwayConfigMetadataWhitelist"

    cfgDir = conwayConfigDir

    -- match all metadata keys of value 1
    mksNodeConfig :: IO SyncNodeConfig
    mksNodeConfig = do
      initConfigFile <- mkSyncNodeConfig cfgDir args
      let dncInsertOptions' = dncInsertOptions initConfigFile
      pure $
        initConfigFile
          { dncInsertOptions = dncInsertOptions' {sioMetadata = MetadataKeys $ fromList [1, 6]}
          }
