{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Test.Cardano.Db.Mock.Unit.Conway.InlineAndReference (
  unlockDatumOutput,
  unlockDatumOutputSameBlock,
  inlineDatumCBOR,
  spendRefScript,
  spendRefScriptSameBlock,
  spendCollateralOutput,
  spendCollateralOutputRollback,
  spendCollateralOutputSameBlock,
  referenceInputUnspend,
  supplyScriptsTwoWays,
  supplyScriptsTwoWaysSameBlock,
  referenceMintingScript,
  referenceDelegation,
) where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Mock.ChainSync.Server (IOManager (), ServerHandle ())
import Cardano.Mock.Forging.Interpreter (Interpreter (), withConwayLedgerState)
import qualified Cardano.Mock.Forging.Tx.Alonzo.ScriptsExamples as Examples
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import Cardano.Mock.Forging.Types
import Cardano.Prelude hiding (head)
import Data.ByteString.Short (fromShort)
import qualified Data.Map as Map
import Ouroboros.Network.Block (blockPoint)
import Test.Cardano.Db.Mock.Config
import qualified Test.Cardano.Db.Mock.UnifiedApi as Api
import Test.Cardano.Db.Mock.Validate
import Test.Tasty.HUnit (Assertion ())
import Prelude (head, (!!))

unlockDatumOutput :: IOManager -> [(Text, Text)] -> Assertion
unlockDatumOutput =
  withFullConfigAndDropDB conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block
    void $ Api.registerAllStakeCreds interpreter mockServer
    -- Forge a tx with a lock script
    tx <-
      withConwayLedgerState interpreter $
        Conway.mkLockByScriptTx
          (UTxOIndex 0)
          [Conway.TxOutInline True Conway.InlineDatum Conway.NoReferenceScript]
          20_000
          20_000
    -- Add it to a block
    void $
      Api.forgeNextAndSubmit interpreter mockServer $
        MockBlock [TxConway tx] (NodeId 1)

    let utxo = head (Conway.mkUTxOConway tx)
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkUnlockScriptTxBabbage
          [UTxOPair utxo]
          (UTxOIndex 1)
          (UTxOIndex 2)
          [UTxOPair utxo]
          False
          True
          10_000
          500

    -- Wait for it to sync
    assertBlockNoBackoff dbSync 3
    -- Verify script counts
    -- (scripts, redeemers, datums, colInputs, scriptOutputs, reedererTxIn, invalidTxs,
    --  txInInvalidTxs, redeemerData, referenceTxIn, collTxOut, inlineDatum, referenceScripts)
    assertBabbageCounts dbSync (1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0)
  where
    testLabel = "conwayUnlockDatumOutput"

unlockDatumOutputSameBlock :: IOManager -> [(Text, Text)] -> Assertion
unlockDatumOutputSameBlock =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with stake creds
    void $ Api.registerAllStakeCreds interpreter mockServer

    -- We try to make this test as crazy as possible by keeping inputs and outputs in the
    -- same blocks, using unnecessary reference inputs and adding unnecessary fields to
    -- collateral output.
    txs' <- withConwayLedgerState interpreter $ \state' -> do
      -- Create a lock script
      tx0 <-
        Conway.mkLockByScriptTx
          (UTxOIndex 0)
          [ Conway.TxOutInline True Conway.InlineDatum Conway.NoReferenceScript
          , Conway.TxOutInline True Conway.NotInlineDatum (Conway.ReferenceScript False)
          ]
          20_000
          20_000
          state'
      -- Create an unlock script in the same block
      let utxo = head (Conway.mkUTxOConway tx0)
      tx1 <-
        Conway.mkUnlockScriptTxBabbage
          [UTxOPair utxo]
          (UTxOIndex 1)
          (UTxOIndex 2)
          [UTxOPair utxo, UTxOIndex 2]
          True
          True
          10_000
          500
          state'

      pure [tx0, tx1]
    -- Add the transactions to a block
    void $
      Api.forgeNextAndSubmit interpreter mockServer $
        MockBlock (TxConway <$> txs') (NodeId 1)

    -- Wait for it to sync
    assertBlockNoBackoff dbSync 2
    -- Verify script counts
    -- (scripts, redeemers, datums, colInputs, scriptOutputs, reedererTxIn, invalidTxs,
    --  txInInvalidTxs, redeemerData, referenceTxIn, collTxOut, inlineDatum, referenceScripts)
    assertBabbageCounts dbSync (2, 1, 1, 1, 2, 1, 0, 0, 1, 2, 1, 1, 1)
  where
    testLabel = "conwayUnlockDatumOutputSameBlock"

inlineDatumCBOR :: IOManager -> [(Text, Text)] -> Assertion
inlineDatumCBOR =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with stake credentials
    void $ Api.registerAllStakeCreds interpreter mockServer
    -- Forge a transaction with inline datum cbor
    tx <-
      withConwayLedgerState interpreter $
        Conway.mkLockByScriptTx
          (UTxOIndex 0)
          [ Conway.TxOutInline
              True
              (Conway.InlineDatumCBOR Examples.plutusDataEncLen)
              Conway.NoReferenceScript
          ]
          20_000
          20_000
    -- Add it to a block
    void $
      Api.forgeNextAndSubmit interpreter mockServer $
        MockBlock [TxConway tx] (NodeId 1)

    -- Wait for it to sync
    assertBlockNoBackoff dbSync 2
    -- Compare expected cbor content
    assertDatumCBOR dbSync (fromShort Examples.plutusDataEncLen)
  where
    testLabel = "conwayInlineDatumCBOR"

spendRefScript :: IOManager -> [(Text, Text)] -> Assertion
spendRefScript =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a blockr with stake credentials
    void $ Api.registerAllStakeCreds interpreter mockServer
    -- Forge a tx with a script
    tx <-
      withConwayLedgerState interpreter $
        Conway.mkLockByScriptTx
          (UTxOIndex 0)
          [Conway.TxOutInline True Conway.NotInlineDatum (Conway.ReferenceScript True)]
          20_000
          20_000
    -- Add it to a block
    void $
      Api.forgeNextAndSubmit interpreter mockServer $
        MockBlock [TxConway tx] (NodeId 1)

    -- Spend the utxo from above
    let utxo = head (Conway.mkUTxOConway tx)
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkUnlockScriptTxBabbage
          [UTxOPair utxo]
          (UTxOIndex 1)
          (UTxOAddress Examples.alwaysSucceedsScriptAddr)
          [UTxOPair utxo]
          False
          True
          10_000
          500

    -- Wait for it to sync
    assertBlockNoBackoff dbSync 3
    -- Verify script counts
    -- (scripts, redeemers, datums, colInputs, scriptOutputs, reedererTxIn, invalidTxs,
    --  txInInvalidTxs, redeemerData, referenceTxIn, collTxOut, inlineDatum, referenceScripts)
    -- redeemers, redeemerTxIn, redeemerData
    --                  actual (1, 0, 1, 1, 2, 0, 0, 0, 0, 1, 1, 0, 1)
    assertBabbageCounts dbSync (1, 1, 1, 1, 2, 1, 0, 0, 1, 1, 1, 0, 1)
  where
    testLabel = "conwayRefScript"

spendRefScriptSameBlock :: IOManager -> [(Text, Text)] -> Assertion
spendRefScriptSameBlock =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with stake credentials
    void $ Api.registerAllStakeCreds interpreter mockServer
    -- Forge transactions with scripts
    txs' <- withConwayLedgerState interpreter $ \state' -> do
      -- Forge a tx with ref a ref script
      tx0 <-
        Conway.mkLockByScriptTx
          (UTxOIndex 0)
          [ Conway.TxOutInline True Conway.NotInlineDatum (Conway.ReferenceScript True)
          , Conway.TxOutInline True Conway.NotInlineDatum (Conway.ReferenceScript False)
          ]
          20_000
          20_000
          state'
      -- Spend the outputs
      let utxo = head (Conway.mkUTxOConway tx0)
      tx1 <-
        Conway.mkUnlockScriptTxBabbage
          [UTxOPair utxo]
          (UTxOIndex 1)
          (UTxOIndex 2)
          [UTxOPair utxo, UTxOIndex 2]
          True
          True
          10_000
          500
          state'

      pure [tx0, tx1]

    -- Add them to a block
    void $
      Api.forgeNextAndSubmit interpreter mockServer $
        MockBlock (TxConway <$> txs') (NodeId 1)

    -- Wait for it to sync
    assertBlockNoBackoff dbSync 2
    -- Verify script counts
    -- (scripts, redeemers, datums, colInputs, scriptOutputs, reedererTxIn, invalidTxs,
    --  txInInvalidTxs, redeemerData, referenceTxIn, collTxOut, inlineDatum, referenceScripts)
    assertBabbageCounts dbSync (2, 1, 1, 1, 2, 1, 0, 0, 1, 2, 1, 0, 2)
  where
    testLabel = "conwaySpendRefScriptSameBlock"

spendCollateralOutput :: IOManager -> [(Text, Text)] -> Assertion
spendCollateralOutput =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with stake credentials
    void $ Api.registerAllStakeCreds interpreter mockServer

    -- Forge a transaction with a script
    tx0 <-
      withConwayLedgerState interpreter $
        Conway.mkLockByScriptTx (UTxOIndex 0) [Conway.TxOutNoInline False] 20_000 20_000
    -- Add it to a block
    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer [TxConway tx0]

    -- tx fails, so its collateral output becomes actual output
    let utxo0 = head (Conway.mkUTxOConway tx0)
    tx1 <-
      withConwayLedgerState interpreter $
        Conway.mkUnlockScriptTxBabbage
          [UTxOInput (fst utxo0)]
          (UTxOIndex 1)
          (UTxOIndex 2)
          [UTxOPair utxo0]
          True
          False
          10_000
          500
    -- Add it to a block
    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer [TxConway tx1]
    -- Wait for it to sync
    assertBlockNoBackoff dbSync 3

    -- Spend collateral output from tx1
    let utxo1 = head (Conway.mkUTxOCollConway tx1)
    tx2 <-
      withConwayLedgerState interpreter $
        Conway.mkUnlockScriptTxBabbage
          [UTxOPair utxo1]
          (UTxOIndex 3)
          (UTxOIndex 1)
          [UTxOPair utxo1]
          False
          True
          10_000
          500
    -- Add it to a block
    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer [TxConway tx2]

    -- Wait for it to sync
    assertBlockNoBackoff dbSync 4
    -- Verify script counts
    -- (scripts, redeemers, datums, colInputs, scriptOutputs, reedererTxIn, invalidTxs,
    --  txInInvalidTxs, redeemerData, referenceTxIn, collTxOut, inlineDatum, referenceScripts)
    assertBabbageCounts dbSync (1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1)
  where
    testLabel = "conwaySpendCollateralOutput"

spendCollateralOutputRollback :: IOManager -> [(Text, Text)] -> Assertion
spendCollateralOutputRollback =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with stake credentials
    blk <- Api.registerAllStakeCreds interpreter mockServer
    -- Create and spend collateral
    mkSpendCollOutput interpreter mockServer dbSync 0

    -- Rollback past action above
    Api.rollbackTo interpreter mockServer (blockPoint blk)
    -- Forge and submit another block
    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer []
    -- Create and spend collateral
    mkSpendCollOutput interpreter mockServer dbSync 1
  where
    testLabel = "conwaySpendCollateralOtputRollback"

    mkSpendCollOutput ::
      Interpreter ->
      ServerHandle IO CardanoBlock ->
      DBSyncEnv ->
      Int ->
      IO ()
    mkSpendCollOutput interpreter mockServer dbSync n = do
      -- Forge a tx with a lock script
      tx0 <-
        withConwayLedgerState interpreter $
          Conway.mkLockByScriptTx (UTxOIndex 0) [Conway.TxOutNoInline False] 20_000 20_000
      -- Add it to a block
      void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer [TxConway tx0]

      -- Create a failing tx so its collateral becomes actual output
      let utxo0 = head (Conway.mkUTxOConway tx0)
      tx1 <-
        withConwayLedgerState interpreter $
          Conway.mkUnlockScriptTxBabbage
            [UTxOInput (fst utxo0)]
            (UTxOIndex 1)
            (UTxOIndex 2)
            [UTxOPair utxo0]
            True
            False
            10_000
            500
      -- Add it to a block
      void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer [TxConway tx1]
      -- Wait for it to sync
      assertBlockNoBackoff dbSync (n + 3)

      -- Create a succeeding transaction that spends the failing tx's outputs
      let utxo1 = head (Conway.mkUTxOCollConway tx1)
      tx2 <-
        withConwayLedgerState interpreter $
          Conway.mkUnlockScriptTxBabbage
            [UTxOPair utxo1]
            (UTxOIndex 3)
            (UTxOIndex 1)
            [UTxOPair utxo1]
            False
            True
            10_000
            500
      -- Add it to a block
      void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer [TxConway tx2]

      -- Wait for it to sync
      assertBlockNoBackoff dbSync (n + 4)
      assertBabbageCounts dbSync (1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1)

spendCollateralOutputSameBlock :: IOManager -> [(Text, Text)] -> Assertion
spendCollateralOutputSameBlock =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with stake credentials
    void $ Api.registerAllStakeCreds interpreter mockServer

    -- Create txs creating and spending collaterals
    txs' <- withConwayLedgerState interpreter $ \state' -> do
      -- Create a lock script
      tx0 <-
        Conway.mkLockByScriptTx
          (UTxOIndex 0)
          [Conway.TxOutNoInline False]
          20_000
          20_000
          state'
      -- Create a failing tx so its collateral output becomes actual output
      let utxo0 = head (Conway.mkUTxOConway tx0)
      tx1 <-
        Conway.mkUnlockScriptTxBabbage
          [UTxOPair utxo0]
          (UTxOIndex 1)
          (UTxOIndex 2)
          [UTxOPair utxo0]
          True
          False
          10_000
          500
          state'
      -- Spend the collateral above
      let utxo1 = head (Conway.mkUTxOCollConway tx1)
      tx2 <-
        Conway.mkUnlockScriptTxBabbage
          [UTxOPair utxo1]
          (UTxOIndex 3)
          (UTxOIndex 4)
          [UTxOPair utxo1]
          False
          True
          10_000
          500
          state'

      pure [tx0, tx1, tx2]

    -- Submit the txs to a new block
    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer (map TxConway txs')

    -- Wait for it to sync
    assertBlockNoBackoff dbSync 2
    assertBabbageCounts dbSync (1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1)
  where
    testLabel = "conwaySpindCollateralOutputSameBlock"

referenceInputUnspend :: IOManager -> [(Text, Text)] -> Assertion
referenceInputUnspend =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with stake credentials
    void $ Api.registerAllStakeCreds interpreter mockServer

    -- Create some transactions
    txs' <- withConwayLedgerState interpreter $ \state' -> do
      -- Create a tx with a lock script
      tx0 <-
        Conway.mkLockByScriptTx
          (UTxOIndex 0)
          [ Conway.TxOutInline True Conway.InlineDatum (Conway.ReferenceScript True)
          , Conway.TxOutInline True Conway.InlineDatum (Conway.ReferenceScript True)
          ]
          20_000
          20_000
          state'

      let utxos = Conway.mkUTxOConway tx0
          (utxo0, utxo1) = (head utxos, utxos !! 1)
      -- Create a tx with a reference to an unspent input
      tx1 <-
        Conway.mkUnlockScriptTxBabbage
          [UTxOPair utxo0]
          (UTxOIndex 1)
          (UTxOIndex 2)
          [UTxOPair utxo1] -- Unspent input
          False
          True
          10_000
          500
          state'

      pure [tx0, tx1]

    -- Add txs above to a new block
    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer (map TxConway txs')

    -- Wait for it to sync
    assertBlockNoBackoff dbSync 2
    assertBabbageCounts dbSync (1, 1, 1, 1, 2, 1, 0, 0, 1, 1, 1, 2, 2)
  where
    testLabel = "conwayReferenceInputUnspend"

supplyScriptsTwoWays :: IOManager -> [(Text, Text)] -> Assertion
supplyScriptsTwoWays =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with stake credentials
    void $ Api.registerAllStakeCreds interpreter mockServer
    -- Create a lock script
    tx0 <-
      withConwayLedgerState interpreter $
        Conway.mkLockByScriptTx
          (UTxOIndex 0)
          [ Conway.TxOutInline True Conway.InlineDatum (Conway.ReferenceScript True)
          , Conway.TxOutNoInline True
          ]
          20_000
          20_000
    -- Add it to a block
    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer [TxConway tx0]

    -- Create a tx that uses a reference to an unspent input
    let utxos = Conway.mkUTxOConway tx0
        (utxo0, utxo1) = (head utxos, utxos !! 1)
    tx1 <-
      withConwayLedgerState interpreter $
        Conway.mkUnlockScriptTxBabbage
          [UTxOPair utxo0, UTxOPair utxo1]
          (UTxOIndex 1)
          (UTxOIndex 2)
          [UTxOPair utxo0]
          False
          True
          10_000
          500
    -- Add it to a block
    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer [TxConway tx1]

    -- Wait for it to sync
    assertBlockNoBackoff dbSync 3
    assertBabbageCounts dbSync (1, 2, 1, 1, 2, 2, 0, 0, 1, 1, 1, 1, 1)
  where
    testLabel = "conwaySupplyScriptToWays"

supplyScriptsTwoWaysSameBlock :: IOManager -> [(Text, Text)] -> Assertion
supplyScriptsTwoWaysSameBlock =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with stake credentials
    void $ Api.registerAllStakeCreds interpreter mockServer
    -- Forge all the transactions in a single block
    txs' <- withConwayLedgerState interpreter $ \state' -> do
      -- Create a tx with one script referenced and one for the witnesses
      tx0 <-
        Conway.mkLockByScriptTx
          (UTxOIndex 0)
          [ Conway.TxOutInline True Conway.InlineDatum (Conway.ReferenceScript True)
          , Conway.TxOutNoInline True
          ]
          20_000
          20_000
          state'

      -- Create a tx that references an unspent input
      let utxos = Conway.mkUTxOConway tx0
          (utxo0, utxo1) = (head utxos, utxos !! 1)
      tx1 <-
        Conway.mkUnlockScriptTxBabbage
          [UTxOPair utxo0, UTxOPair utxo1]
          (UTxOIndex 1)
          (UTxOIndex 2)
          [UTxOPair utxo0]
          False
          True
          10_000
          500
          state'

      pure [tx0, tx1]

    -- Add the txs above to a block
    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer (map TxConway txs')

    -- Wait for it to sync
    assertBlockNoBackoff dbSync 2
    assertBabbageCounts dbSync (1, 2, 1, 1, 2, 2, 0, 0, 1, 1, 1, 1, 1)
  where
    testLabel = "conwaySupplyScriptsTwoWaysSameBlock"

referenceMintingScript :: IOManager -> [(Text, Text)] -> Assertion
referenceMintingScript =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with stake credentials
    void $ Api.registerAllStakeCreds interpreter mockServer
    -- Forge all the transactions in a single block
    txs' <- withConwayLedgerState interpreter $ \state' -> do
      -- Create a tx with one script referenced and one for the witnesses
      tx0 <-
        Conway.mkLockByScriptTx
          (UTxOIndex 0)
          [Conway.TxOutInline True Conway.InlineDatum (Conway.ReferenceScript True)]
          20_000
          20_000
          state'

      -- Use a reference to an output which has a minting script
      let utxo = head (Conway.mkUTxOConway tx0)
          val =
            MultiAsset $
              Map.singleton
                (PolicyID Examples.alwaysSucceedsScriptHash)
                (Map.singleton (head Examples.assetNames) 1)
      tx1 <-
        Conway.mkMultiAssetsScriptTx
          [UTxOIndex 0]
          (UTxOIndex 1)
          [(UTxOAddressNew 0, MaryValue (Coin 10_000) mempty)]
          [UTxOPair utxo]
          val
          True
          100
          state'

      pure [tx0, tx1]

    -- Add the txs above to a block
    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer (map TxConway txs')

    -- Wait for it to sync
    assertBlockNoBackoff dbSync 2
    assertBabbageCounts dbSync (1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1)
  where
    testLabel = "conwayReferenceMintingScript"

-- TODO[sgillespie]: This looks the same as @referenceMintingScript@
referenceDelegation :: IOManager -> [(Text, Text)] -> Assertion
referenceDelegation =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with stake credentials
    void $ Api.registerAllStakeCreds interpreter mockServer
    -- Forge all the transactions in a single block
    txs' <- withConwayLedgerState interpreter $ \state' -> do
      -- Create a tx with one script referenced
      tx0 <-
        Conway.mkLockByScriptTx
          (UTxOIndex 0)
          [Conway.TxOutInline True Conway.InlineDatum (Conway.ReferenceScript True)]
          20_000
          20_000
          state'

      -- Create a tx with a reference to an output which has a minting script
      let utxo = head (Conway.mkUTxOConway tx0)
          val =
            MultiAsset $
              Map.singleton
                (PolicyID Examples.alwaysSucceedsScriptHash)
                (Map.singleton (head Examples.assetNames) 1)
      tx1 <-
        Conway.mkMultiAssetsScriptTx
          [UTxOIndex 0]
          (UTxOIndex 1)
          [(UTxOAddressNew 0, MaryValue (Coin 10_000) mempty)]
          [UTxOPair utxo]
          val
          True
          100
          state'

      pure [tx0, tx1]

    -- Add the txs above to a block
    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer (map TxConway txs')

    -- Wait for it to sync
    assertBlockNoBackoff dbSync 2
    assertBabbageCounts dbSync (1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1)
  where
    testLabel = "conwayReferenceDelegation"
