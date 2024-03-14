{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Test.Cardano.Db.Mock.Unit.Babbage.InlineAndReference (
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

import Cardano.Ledger.Coin
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Mock.ChainSync.Server (IOManager)
import Cardano.Mock.Forging.Interpreter (withBabbageLedgerState)
import Cardano.Mock.Forging.Tx.Alonzo.ScriptsExamples (
  alwaysSucceedsScriptAddr,
  alwaysSucceedsScriptHash,
  assetNames,
  plutusDataEncLen,
 )
import qualified Cardano.Mock.Forging.Tx.Babbage as Babbage
import Cardano.Mock.Forging.Types (
  MockBlock (..),
  NodeId (..),
  TxEra (..),
  UTxOIndex (..),
 )
import Control.Monad (void)
import qualified Data.ByteString.Short as SBS
import qualified Data.Map as Map
import Data.Text (Text)
import Ouroboros.Network.Block (blockPoint)
import Test.Cardano.Db.Mock.Config (babbageConfigDir, startDBSync, withFullConfig, withFullConfigAndDropDB)
import Test.Cardano.Db.Mock.UnifiedApi (
  forgeNextAndSubmit,
  forgeNextFindLeaderAndSubmit,
  registerAllStakeCreds,
  rollbackTo,
  withBabbageFindLeaderAndSubmitTx,
 )
import Test.Cardano.Db.Mock.Validate (assertBabbageCounts, assertBlockNoBackoff, assertDatumCBOR)
import Test.Tasty.HUnit (Assertion)

unlockDatumOutput :: IOManager -> [(Text, Text)] -> Assertion
unlockDatumOutput =
  withFullConfigAndDropDB babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    -- We don't use withBabbageFindLeaderAndSubmitTx here, because we want access to the tx.
    tx0 <-
      withBabbageLedgerState interpreter $
        Babbage.mkLockByScriptTx (UTxOIndex 0) [Babbage.TxOutInline True Babbage.InlineDatum Babbage.NoReferenceScript] 20000 20000
    void $ forgeNextAndSubmit interpreter mockServer $ MockBlock [TxBabbage tx0] (NodeId 1)

    let utxo0 = head (Babbage.mkUTxOBabbage tx0)
    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkUnlockScriptTxBabbage [UTxOPair utxo0] (UTxOIndex 1) (UTxOIndex 2) [UTxOPair utxo0] False True 10000 500

    assertBlockNoBackoff dbSync 3
    assertBabbageCounts dbSync (1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0)
  where
    testLabel = "unlockDatumOutput"

unlockDatumOutputSameBlock :: IOManager -> [(Text, Text)] -> Assertion
unlockDatumOutputSameBlock =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    -- We try to make this test as crazy as possible, by keeping inputs and outputs in the same blocks, using unnecessary reference
    -- inputs and adding unnnecessary fields to the collateral output.
    txs' <- withBabbageLedgerState interpreter $ \st -> do
      tx0 <-
        Babbage.mkLockByScriptTx
          (UTxOIndex 0)
          [Babbage.TxOutInline True Babbage.InlineDatum Babbage.NoReferenceScript, Babbage.TxOutInline True Babbage.NotInlineDatum (Babbage.ReferenceScript False)]
          20000
          20000
          st
      let utxo0 = head (Babbage.mkUTxOBabbage tx0)
      tx1 <-
        Babbage.mkUnlockScriptTxBabbage
          [UTxOPair utxo0]
          (UTxOIndex 1)
          (UTxOIndex 2)
          [UTxOPair utxo0, UTxOIndex 2]
          True
          True
          10000
          500
          st
      pure [tx0, tx1]
    void $ forgeNextAndSubmit interpreter mockServer $ MockBlock (TxBabbage <$> txs') (NodeId 1)

    assertBlockNoBackoff dbSync 2
    assertBabbageCounts dbSync (2, 1, 1, 1, 2, 1, 0, 0, 1, 2, 1, 1, 1)
  where
    testLabel = "unlockDatumOutputSameBlock"

inlineDatumCBOR :: IOManager -> [(Text, Text)] -> Assertion
inlineDatumCBOR =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    -- We don't use withBabbageFindLeaderAndSubmitTx here, because we want access to the tx.
    tx0 <-
      withBabbageLedgerState interpreter $
        Babbage.mkLockByScriptTx (UTxOIndex 0) [Babbage.TxOutInline True (Babbage.InlineDatumCBOR plutusDataEncLen) Babbage.NoReferenceScript] 20000 20000
    void $ forgeNextAndSubmit interpreter mockServer $ MockBlock [TxBabbage tx0] (NodeId 1)

    assertBlockNoBackoff dbSync 2
    assertDatumCBOR dbSync $ SBS.fromShort plutusDataEncLen
  where
    testLabel = "inlineDatumCBOR"

spendRefScript :: IOManager -> [(Text, Text)] -> Assertion
spendRefScript =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    -- We don't use withBabbageFindLeaderAndSubmitTx here, because we want access to the tx.
    tx0 <-
      withBabbageLedgerState interpreter $
        Babbage.mkLockByScriptTx (UTxOIndex 0) [Babbage.TxOutInline True Babbage.NotInlineDatum (Babbage.ReferenceScript True)] 20000 20000
    void $ forgeNextAndSubmit interpreter mockServer $ MockBlock [TxBabbage tx0] (NodeId 1)

    let utxo0 = head (Babbage.mkUTxOBabbage tx0)
    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkUnlockScriptTxBabbage [UTxOPair utxo0] (UTxOIndex 1) (UTxOAddress alwaysSucceedsScriptAddr) [UTxOPair utxo0] False True 10000 500

    assertBlockNoBackoff dbSync 3
    assertBabbageCounts dbSync (1, 1, 1, 1, 2, 1, 0, 0, 1, 1, 1, 0, 1)
  where
    testLabel = "spendRefScript"

spendRefScriptSameBlock :: IOManager -> [(Text, Text)] -> Assertion
spendRefScriptSameBlock =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    txs' <- withBabbageLedgerState interpreter $ \st -> do
      tx0 <-
        Babbage.mkLockByScriptTx
          (UTxOIndex 0)
          [ Babbage.TxOutInline True Babbage.NotInlineDatum (Babbage.ReferenceScript True)
          , Babbage.TxOutInline True Babbage.NotInlineDatum (Babbage.ReferenceScript False)
          ]
          20000
          20000
          st
      let utxo0 = head (Babbage.mkUTxOBabbage tx0)
      tx1 <-
        Babbage.mkUnlockScriptTxBabbage
          [UTxOPair utxo0]
          (UTxOIndex 1)
          (UTxOIndex 2)
          [UTxOPair utxo0, UTxOIndex 2]
          True
          True
          10000
          500
          st
      pure [tx0, tx1]
    void $ forgeNextAndSubmit interpreter mockServer $ MockBlock (TxBabbage <$> txs') (NodeId 1)

    assertBlockNoBackoff dbSync 2
    assertBabbageCounts dbSync (2, 1, 1, 1, 2, 1, 0, 0, 1, 2, 1, 0, 2)
  where
    testLabel = "spendRefScriptSameBlock"

spendCollateralOutput :: IOManager -> [(Text, Text)] -> Assertion
spendCollateralOutput =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    tx0 <-
      withBabbageLedgerState interpreter $
        Babbage.mkLockByScriptTx (UTxOIndex 0) [Babbage.TxOutNoInline False] 20000 20000
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer [TxBabbage tx0]

    -- tx fails so its collateral output become actual output.
    let utxo0 = head (Babbage.mkUTxOBabbage tx0)
    tx1 <-
      withBabbageLedgerState interpreter $
        Babbage.mkUnlockScriptTxBabbage [UTxOInput (fst utxo0)] (UTxOIndex 1) (UTxOIndex 2) [UTxOPair utxo0] True False 10000 500
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer [TxBabbage tx1]
    assertBlockNoBackoff dbSync 3

    let utxo1 = head (Babbage.mkUTxOCollBabbage tx1)
    tx2 <-
      withBabbageLedgerState interpreter $
        Babbage.mkUnlockScriptTxBabbage [UTxOPair utxo1] (UTxOIndex 3) (UTxOIndex 1) [UTxOPair utxo1] False True 10000 500
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer [TxBabbage tx2]

    assertBlockNoBackoff dbSync 4
    assertBabbageCounts dbSync (1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1)
  where
    testLabel = "spendCollateralOutput"

spendCollateralOutputRollback :: IOManager -> [(Text, Text)] -> Assertion
spendCollateralOutputRollback =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    blk0 <- registerAllStakeCreds interpreter mockServer
    action interpreter mockServer dbSync 0
    rollbackTo interpreter mockServer (blockPoint blk0)
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    action interpreter mockServer dbSync 1
  where
    testLabel = "spendCollateralOutputRollback"
    action interpreter mockServer dbSync n = do
      tx0 <-
        withBabbageLedgerState interpreter $
          Babbage.mkLockByScriptTx (UTxOIndex 0) [Babbage.TxOutNoInline False] 20000 20000
      void $ forgeNextFindLeaderAndSubmit interpreter mockServer [TxBabbage tx0]

      -- tx fails so its collateral output become actual output.
      let utxo0 = head (Babbage.mkUTxOBabbage tx0)
      tx1 <-
        withBabbageLedgerState interpreter $
          Babbage.mkUnlockScriptTxBabbage [UTxOInput (fst utxo0)] (UTxOIndex 1) (UTxOIndex 2) [UTxOPair utxo0] True False 10000 500
      void $ forgeNextFindLeaderAndSubmit interpreter mockServer [TxBabbage tx1]
      assertBlockNoBackoff dbSync $ n + 3

      let utxo1 = head (Babbage.mkUTxOCollBabbage tx1)
      tx2 <-
        withBabbageLedgerState interpreter $
          Babbage.mkUnlockScriptTxBabbage [UTxOPair utxo1] (UTxOIndex 3) (UTxOIndex 1) [UTxOPair utxo1] False True 10000 500
      void $ forgeNextFindLeaderAndSubmit interpreter mockServer [TxBabbage tx2]

      assertBlockNoBackoff dbSync $ n + 4
      assertBabbageCounts dbSync (1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1)

spendCollateralOutputSameBlock :: IOManager -> [(Text, Text)] -> Assertion
spendCollateralOutputSameBlock =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    txs' <- withBabbageLedgerState interpreter $ \st -> do
      tx0 <- Babbage.mkLockByScriptTx (UTxOIndex 0) [Babbage.TxOutNoInline False] 20000 20000 st

      -- tx fails so its collateral output become actual output.
      let utxo0 = head (Babbage.mkUTxOBabbage tx0)
      tx1 <- Babbage.mkUnlockScriptTxBabbage [UTxOPair utxo0] (UTxOIndex 1) (UTxOIndex 2) [UTxOPair utxo0] True False 10000 500 st
      let utxo1 = head (Babbage.mkUTxOCollBabbage tx1)
      tx2 <- Babbage.mkUnlockScriptTxBabbage [UTxOPair utxo1] (UTxOIndex 3) (UTxOIndex 4) [UTxOPair utxo1] False True 10000 500 st
      pure [tx0, tx1, tx2]
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer (TxBabbage <$> txs')

    assertBlockNoBackoff dbSync 2
    assertBabbageCounts dbSync (1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1)
  where
    testLabel = "spendCollateralOutputSameBlock"

referenceInputUnspend :: IOManager -> [(Text, Text)] -> Assertion
referenceInputUnspend =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    txs' <- withBabbageLedgerState interpreter $ \st -> do
      tx0 <-
        Babbage.mkLockByScriptTx
          (UTxOIndex 0)
          [ Babbage.TxOutInline True Babbage.InlineDatum (Babbage.ReferenceScript True)
          , Babbage.TxOutInline True Babbage.InlineDatum (Babbage.ReferenceScript True)
          ]
          20000
          20000
          st

      let (utxo0 : utxo1 : _) = Babbage.mkUTxOBabbage tx0
      -- use a reference to an input which is not spend.
      tx1 <- Babbage.mkUnlockScriptTxBabbage [UTxOPair utxo0] (UTxOIndex 1) (UTxOIndex 2) [UTxOPair utxo1] False True 10000 500 st
      pure [tx0, tx1]
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer (TxBabbage <$> txs')

    assertBlockNoBackoff dbSync 2
    assertBabbageCounts dbSync (1, 1, 1, 1, 2, 1, 0, 0, 1, 1, 1, 2, 2)
  where
    testLabel = "referenceInputUnspend"

supplyScriptsTwoWays :: IOManager -> [(Text, Text)] -> Assertion
supplyScriptsTwoWays =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    tx0 <-
      withBabbageLedgerState interpreter $
        Babbage.mkLockByScriptTx
          (UTxOIndex 0)
          [ Babbage.TxOutInline True Babbage.InlineDatum (Babbage.ReferenceScript True)
          , Babbage.TxOutNoInline True
          ]
          20000
          20000
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer [TxBabbage tx0]

    let (utxo0 : utxo1 : _) = Babbage.mkUTxOBabbage tx0
    -- use a reference to an input which is not spend.
    tx1 <-
      withBabbageLedgerState interpreter $
        Babbage.mkUnlockScriptTxBabbage [UTxOPair utxo0, UTxOPair utxo1] (UTxOIndex 1) (UTxOIndex 2) [UTxOPair utxo0] False True 10000 500
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer [TxBabbage tx1]

    assertBlockNoBackoff dbSync 3
    assertBabbageCounts dbSync (1, 2, 1, 1, 2, 2, 0, 0, 1, 1, 1, 1, 1)
  where
    testLabel = "supplyScriptsTwoWays"

supplyScriptsTwoWaysSameBlock :: IOManager -> [(Text, Text)] -> Assertion
supplyScriptsTwoWaysSameBlock =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    txs' <- withBabbageLedgerState interpreter $ \st -> do
      -- one script referenced and one for the witnesses
      tx0 <-
        Babbage.mkLockByScriptTx
          (UTxOIndex 0)
          [ Babbage.TxOutInline True Babbage.InlineDatum (Babbage.ReferenceScript True)
          , Babbage.TxOutNoInline True
          ]
          20000
          20000
          st

      let (utxo0 : utxo1 : _) = Babbage.mkUTxOBabbage tx0
      -- use a reference to an input which is not spend.
      tx1 <- Babbage.mkUnlockScriptTxBabbage [UTxOPair utxo0, UTxOPair utxo1] (UTxOIndex 1) (UTxOIndex 2) [UTxOPair utxo0] False True 10000 500 st
      pure [tx0, tx1]
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer (TxBabbage <$> txs')

    assertBlockNoBackoff dbSync 2
    assertBabbageCounts dbSync (1, 2, 1, 1, 2, 2, 0, 0, 1, 1, 1, 1, 1)
  where
    testLabel = "supplyScriptsTwoWaysSameBlock"

referenceMintingScript :: IOManager -> [(Text, Text)] -> Assertion
referenceMintingScript =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    txs' <- withBabbageLedgerState interpreter $ \st -> do
      -- one script referenced and one for the witnesses
      tx0 <-
        Babbage.mkLockByScriptTx
          (UTxOIndex 0)
          [Babbage.TxOutInline True Babbage.InlineDatum (Babbage.ReferenceScript True)]
          20000
          20000
          st

      let utxo0 = head $ Babbage.mkUTxOBabbage tx0
      -- use a reference to an output which has a minting script.
      let val0 = MultiAsset $ Map.singleton (PolicyID alwaysSucceedsScriptHash) (Map.singleton (head assetNames) 1)
      tx1 <-
        Babbage.mkMAssetsScriptTx
          [UTxOIndex 0]
          (UTxOIndex 1)
          [(UTxOAddressNew 0, MaryValue (Coin 10000) mempty)]
          [UTxOPair utxo0]
          val0
          True
          100
          st
      pure [tx0, tx1]
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer (TxBabbage <$> txs')

    assertBlockNoBackoff dbSync 2
    assertBabbageCounts dbSync (1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1)
  where
    testLabel = "referenceMintingScript"

referenceDelegation :: IOManager -> [(Text, Text)] -> Assertion
referenceDelegation =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    txs' <- withBabbageLedgerState interpreter $ \st -> do
      -- one script referenced and one for the witnesses
      tx0 <-
        Babbage.mkLockByScriptTx
          (UTxOIndex 0)
          [Babbage.TxOutInline True Babbage.InlineDatum (Babbage.ReferenceScript True)]
          20000
          20000
          st

      let utxo0 = head $ Babbage.mkUTxOBabbage tx0
      -- use a reference to an output which has a minting script.
      let val0 = MultiAsset $ Map.singleton (PolicyID alwaysSucceedsScriptHash) (Map.singleton (head assetNames) 1)
      tx1 <-
        Babbage.mkMAssetsScriptTx
          [UTxOIndex 0]
          (UTxOIndex 1)
          [(UTxOAddressNew 0, MaryValue (Coin 10000) mempty)]
          [UTxOPair utxo0]
          val0
          True
          100
          st
      pure [tx0, tx1]
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer (TxBabbage <$> txs')

    assertBlockNoBackoff dbSync 2
    assertBabbageCounts dbSync (1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1)
  where
    testLabel = "referenceDelegation"
