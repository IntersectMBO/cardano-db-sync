{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Db.Mock.Unit.Conway.Plutus (
  -- * Plutus Send Scripts
  simpleScript,
  unlockScriptSameBlock,
  failedScript,
  failedScriptFees,
  failedScriptSameBlock,
  multipleScripts,
  multipleScriptsRollback,
  multipleScriptsSameBlock,
  multipleScriptsFailed,
  multipleScriptsFailedSameBlock,
) where

import Cardano.Crypto.Hash.Class (hashToBytes)
import Cardano.Db as DB
import Cardano.DbSync.Era.Shelley.Generic.Util (renderAddress)
import Cardano.Ledger.Alonzo.Scripts.Data (hashData)
import Cardano.Ledger.SafeHash (extractHash)
import Cardano.Mock.ChainSync.Server (IOManager ())
import Cardano.Mock.Forging.Interpreter (withConwayLedgerState)
import qualified Cardano.Mock.Forging.Tx.Alonzo.ScriptsExamples as Examples
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import Cardano.Mock.Forging.Types (MockBlock (..), NodeId (..), TxEra (..), UTxOIndex (..))
import Cardano.Prelude hiding (head)
import Ouroboros.Consensus.Shelley.Eras (StandardConway ())
import Ouroboros.Network.Block (genesisPoint)
import Test.Cardano.Db.Mock.Config
import qualified Test.Cardano.Db.Mock.UnifiedApi as Api
import Test.Cardano.Db.Mock.Validate
import Test.Tasty.HUnit (Assertion ())
import Prelude (head, tail, (!!))

simpleScript :: IOManager -> [(Text, Text)] -> Assertion
simpleScript =
  withFullConfigAndDropDB conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with stake credentials
    void $ Api.registerAllStakeCreds interpreter mockServer
    -- Fill the rest of the epoch
    epoch <- Api.fillUntilNextEpoch interpreter mockServer

    -- Forge a block with a script
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkLockByScriptTx (UTxOIndex 0) [Conway.TxOutNoInline True] 20_000 20_000

    -- Verify the outputs match expected
    assertBlockNoBackoff dbSync (length epoch + 2)
    assertEqQuery
      dbSync
      (map getOutFields <$> DB.queryScriptOutputs)
      [expectedFields]
      "Unexpected script outputs"
  where
    testLabel = "conwaySimpleScript"
    getOutFields txOut =
      ( DB.txOutAddress txOut
      , DB.txOutAddressHasScript txOut
      , DB.txOutValue txOut
      , DB.txOutDataHash txOut
      )
    expectedFields =
      ( renderAddress Examples.alwaysSucceedsScriptAddr
      , True
      , DB.DbLovelace 20_000
      , Just $
          hashToBytes (extractHash $ hashData @StandardConway Examples.plutusDataList)
      )

unlockScriptSameBlock :: IOManager -> [(Text, Text)] -> Assertion
unlockScriptSameBlock =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with stake credentials
    void $ Api.registerAllStakeCreds interpreter mockServer
    -- Forge lock/unlock scripts in the same block
    void $ Api.withConwayFindLeaderAndSubmit interpreter mockServer $ \state' -> do
      tx0 <-
        Conway.mkLockByScriptTx
          (UTxOIndex 0)
          [Conway.TxOutNoInline True]
          20_000
          20_000
          state'
      let utxo0 = head (Conway.mkUTxOConway tx0)
      tx1 <-
        Conway.mkUnlockScriptTx
          [UTxOPair utxo0]
          (UTxOIndex 1)
          (UTxOIndex 2)
          True
          10_000
          500
          state'
      pure [tx0, tx1]

    -- Verify script counts
    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (1, 1, 1, 1, 1, 1, 0, 0)
  where
    testLabel = "conwayUnlockScriptSameBlock"

failedScript :: IOManager -> [(Text, Text)] -> Assertion
failedScript =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with a script
    tx <-
      withConwayLedgerState interpreter $
        Conway.mkLockByScriptTx (UTxOIndex 0) [Conway.TxOutNoInline False] 20_000 20_000
    void $
      Api.forgeNextAndSubmit interpreter mockServer $
        MockBlock [TxConway tx] (NodeId 1)

    -- Forge another block with a failing unlock script
    let utxo = head (Conway.mkUTxOConway tx)
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkUnlockScriptTx
          [UTxOPair utxo]
          (UTxOIndex 1)
          (UTxOIndex 2)
          False -- Force failure
          10_000
          500

    -- Verify the invalid tx counts
    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (0, 0, 0, 0, 1, 0, 1, 1)
  where
    testLabel = "conwayFailedScript"

failedScriptFees :: IOManager -> [(Text, Text)] -> Assertion
failedScriptFees =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with a lock script
    tx <-
      withConwayLedgerState interpreter $
        Conway.mkLockByScriptTx (UTxOIndex 0) [Conway.TxOutNoInline False] 20_000 20_000
    void $
      Api.forgeNextAndSubmit interpreter mockServer $
        MockBlock [TxConway tx] (NodeId 1)

    -- Forge another block with a failing unlock script
    let utxo = head (Conway.mkUTxOConway tx)
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkUnlockScriptTx
          [UTxOPair utxo]
          (UTxOIndex 1)
          (UTxOIndex 2)
          False -- Force failure
          10_000
          500

    -- Verify fees
    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (0, 0, 0, 0, 1, 0, 1, 1)
    assertNonZeroFeesContract dbSync
  where
    testLabel = "conwayFailedScriptFees"

failedScriptSameBlock :: IOManager -> [(Text, Text)] -> Assertion
failedScriptSameBlock =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with stake registrations
    void $ Api.registerAllStakeCreds interpreter mockServer
    -- Forge a single block with lock/unlock scripts
    void $ Api.withConwayFindLeaderAndSubmit interpreter mockServer $ \state' -> do
      -- Create a lock script
      tx0 <-
        Conway.mkLockByScriptTx
          (UTxOIndex 0)
          [Conway.TxOutNoInline False]
          20_000
          20_000
          state'
      -- Extract the utxo
      let utxo = head (Conway.mkUTxOConway tx0)
      -- Create a failing unlock script
      tx1 <-
        Conway.mkUnlockScriptTx
          [UTxOPair utxo]
          (UTxOIndex 1)
          (UTxOIndex 2)
          False -- Force failure
          10_000
          500
          state'

      pure [tx0, tx1]

    -- Verify invalid tx counts
    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (0, 0, 0, 0, 1, 0, 1, 1)
  where
    testLabel = "conwayFailedScriptSameBlock"

multipleScripts :: IOManager -> [(Text, Text)] -> Assertion
multipleScripts =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge multiple script transactions
    tx0 <-
      withConwayLedgerState interpreter $
        Conway.mkLockByScriptTx
          (UTxOIndex 0)
          (map Conway.TxOutNoInline [True, False, True])
          20_000
          20_000
    let utxo = Conway.mkUTxOConway tx0
        pair1 = head utxo
        pair2 = utxo !! 2
    tx1 <-
      withConwayLedgerState interpreter $
        Conway.mkUnlockScriptTx
          [UTxOPair pair1, UTxOPair pair2]
          (UTxOIndex 1)
          (UTxOIndex 2)
          True
          10_000
          500

    -- Submit the txs in separate blocks
    void $
      Api.forgeNextAndSubmit interpreter mockServer $
        MockBlock [TxConway tx0] (NodeId 1)
    void $
      Api.forgeNextAndSubmit interpreter mockServer $
        MockBlock [TxConway tx1] (NodeId 1)

    -- Verify tx counts
    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (1, 2, 1, 1, 3, 2, 0, 0)
  where
    testLabel = "conwayMultipleScripts"

multipleScriptsRollback :: IOManager -> [(Text, Text)] -> Assertion
multipleScriptsRollback =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Create multiple scripts
    tx0 <-
      withConwayLedgerState interpreter $
        Conway.mkLockByScriptTx
          (UTxOIndex 0)
          (map Conway.TxOutNoInline [True, False, True])
          20_000
          20_000
    let utxo = Conway.mkUTxOConway tx0
        pair1 = head utxo
        pair2 = utxo !! 2
    tx1 <-
      withConwayLedgerState interpreter $
        Conway.mkUnlockScriptTx
          [UTxOPair pair1, UTxOPair pair2]
          (UTxOIndex 1)
          (UTxOIndex 2)
          True
          10_000
          500

    -- Submit the txs in separate blocks
    void $
      Api.forgeNextAndSubmit interpreter mockServer $
        MockBlock [TxConway tx0] (NodeId 1)
    void $
      Api.forgeNextAndSubmit interpreter mockServer $
        MockBlock [TxConway tx1] (NodeId 1)

    -- Wait for it to sync
    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (1, 2, 1, 1, 3, 2, 0, 0)

    -- Roll back to genesis
    Api.rollbackTo interpreter mockServer genesisPoint
    -- Forge another block
    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer []

    -- Submit the txs again
    void $
      Api.forgeNextAndSubmit interpreter mockServer $
        MockBlock [TxConway tx0] (NodeId 1)
    void $
      Api.forgeNextAndSubmit interpreter mockServer $
        MockBlock [TxConway tx1] (NodeId 1)

    -- Verify tx counts
    assertBlockNoBackoff dbSync 3
    assertAlonzoCounts dbSync (1, 2, 1, 1, 3, 2, 0, 0)
  where
    testLabel = "conwayMultipleScriptsRollback"

multipleScriptsSameBlock :: IOManager -> [(Text, Text)] -> Assertion
multipleScriptsSameBlock =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a single block with multiple scripts
    void $ Api.withConwayFindLeaderAndSubmit interpreter mockServer $ \state' -> do
      tx0 <-
        Conway.mkLockByScriptTx
          (UTxOIndex 0)
          (map Conway.TxOutNoInline [True, False, True])
          20_000
          20_000
          state'
      let utxo = Conway.mkUTxOConway tx0
          pairs = [head utxo, utxo !! 2] -- Omit the failing (middle) script
      tx1 <-
        Conway.mkUnlockScriptTx
          (map UTxOPair pairs)
          (UTxOIndex 1)
          (UTxOIndex 2)
          True
          10_000
          500
          state'
      pure [tx0, tx1]

    -- Verify tx counts
    assertBlockNoBackoff dbSync 1
    assertAlonzoCounts dbSync (1, 2, 1, 1, 3, 2, 0, 0)
  where
    testLabel = "conwayMultipleScriptsSameBlock"

multipleScriptsFailed :: IOManager -> [(Text, Text)] -> Assertion
multipleScriptsFailed =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with multiple scripts
    tx0 <-
      withConwayLedgerState interpreter $
        Conway.mkLockByScriptTx
          (UTxOIndex 0)
          (map Conway.TxOutNoInline [True, False, True])
          20_000
          20_000
    void $
      Api.forgeNextAndSubmit interpreter mockServer $
        MockBlock [TxConway tx0] (NodeId 1)

    -- Forge another block with failing scripts
    let utxos = Conway.mkUTxOConway tx0
    tx1 <-
      withConwayLedgerState interpreter $
        Conway.mkUnlockScriptTx
          (map UTxOPair utxos)
          (UTxOIndex 1)
          (UTxOIndex 2)
          False -- Force failure
          10_000
          500
    void $
      Api.forgeNextAndSubmit interpreter mockServer $
        MockBlock [TxConway tx1] (NodeId 1)

    -- Verify failed txs
    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (0, 0, 0, 0, 3, 0, 1, 1)
  where
    testLabel = "conwayMultipleScriptsFailed"

multipleScriptsFailedSameBlock :: IOManager -> [(Text, Text)] -> Assertion
multipleScriptsFailedSameBlock =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Add multiple scripts in the same block
    void $ Api.withConwayFindLeaderAndSubmit interpreter mockServer $ \state' -> do
      tx0 <-
        Conway.mkLockByScriptTx
          (UTxOIndex 0)
          (map Conway.TxOutNoInline [True, False, True])
          20_000
          20_000
          state'
      let utxos = tail (Conway.mkUTxOConway tx0)
      tx1 <-
        Conway.mkUnlockScriptTx
          (map UTxOPair utxos)
          (UTxOIndex 1)
          (UTxOIndex 2)
          False -- Force failure
          10_000
          500
          state'

      pure [tx0, tx1]

    -- Verify failed txs
    assertBlockNoBackoff dbSync 1
    assertAlonzoCounts dbSync (0, 0, 0, 0, 3, 0, 1, 1)
  where
    testLabel = "conwayMultipleScriptsFailedSameBlock"
