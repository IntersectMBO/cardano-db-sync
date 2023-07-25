{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Db.Mock.Unit.Babbage.Plutus (
  -- plutus spend scripts
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
  -- plutus cert scripts
  registrationScriptTx,
  deregistrationsScriptTx,
  deregistrationScriptTx,
  deregistrationsScriptTxs,
  deregistrationsScriptTx',
  deregistrationsScriptTx'',
  -- plutus MultiAsset scripts
  mintMultiAsset,
  mintMultiAssets,
  swapMultiAssets,
) where

import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Db as DB
import Cardano.DbSync.Era.Shelley.Generic.Util (renderAddress)
import Cardano.Ledger.Alonzo.Scripts.Data (hashData)
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.SafeHash (extractHash)
import Cardano.Ledger.Shelley.TxCert
import Cardano.Mock.ChainSync.Server (IOManager)
import Cardano.Mock.Forging.Interpreter (withBabbageLedgerState)
import Cardano.Mock.Forging.Tx.Alonzo.ScriptsExamples (
  alwaysMintScriptAddr,
  alwaysMintScriptHash,
  alwaysSucceedsScriptAddr,
  alwaysSucceedsScriptHash,
  assetNames,
  plutusDataList,
 )
import qualified Cardano.Mock.Forging.Tx.Babbage as Babbage
import Cardano.Mock.Forging.Types (
  MockBlock (..),
  NodeId (..),
  StakeIndex (..),
  TxEra (..),
  UTxOIndex (..),
 )
import Control.Monad (void)
import qualified Data.Map as Map
import Data.Text (Text)
import Ouroboros.Consensus.Cardano.Block (StandardBabbage)
import Ouroboros.Network.Block (genesisPoint)
import Test.Cardano.Db.Mock.Config (babbageConfigDir, startDBSync, withFullConfig)
import Test.Cardano.Db.Mock.UnifiedApi (
  fillUntilNextEpoch,
  forgeNextAndSubmit,
  forgeNextFindLeaderAndSubmit,
  registerAllStakeCreds,
  rollbackTo,
  withBabbageFindLeaderAndSubmit,
  withBabbageFindLeaderAndSubmitTx,
 )
import Test.Cardano.Db.Mock.Validate (
  assertAlonzoCounts,
  assertBlockNoBackoff,
  assertEqQuery,
  assertNonZeroFeesContract,
  assertScriptCert,
 )
import Test.Tasty.HUnit (Assertion)

----------------------------------------------------------------------------------------------------------
-- Plutus Spend Scripts
----------------------------------------------------------------------------------------------------------

simpleScript :: IOManager -> [(Text, Text)] -> Assertion
simpleScript =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    a <- fillUntilNextEpoch interpreter mockServer

    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkLockByScriptTx (UTxOIndex 0) [Babbage.TxOutNoInline True] 20000 20000

    assertBlockNoBackoff dbSync (fromIntegral $ length a + 2)
    assertEqQuery dbSync (fmap getOutFields <$> DB.queryScriptOutputs) [expectedFields] "Unexpected script outputs"
  where
    testLabel = "simpleScript"
    getOutFields txOut = (DB.txOutAddress txOut, DB.txOutAddressHasScript txOut, DB.txOutValue txOut, DB.txOutDataHash txOut)
    expectedFields =
      ( renderAddress alwaysSucceedsScriptAddr
      , True
      , DB.DbLovelace 20000
      , Just $ Crypto.hashToBytes (extractHash $ hashData @StandardBabbage plutusDataList)
      )

unlockScriptSameBlock :: IOManager -> [(Text, Text)] -> Assertion
unlockScriptSameBlock =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Babbage.mkLockByScriptTx (UTxOIndex 0) [Babbage.TxOutNoInline True] 20000 20000 st
      let utxo0 = head (Babbage.mkUTxOBabbage tx0)
      tx1 <- Babbage.mkUnlockScriptTx [UTxOPair utxo0] (UTxOIndex 1) (UTxOIndex 2) True 10000 500 st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (1, 1, 1, 1, 1, 1, 0, 0)
  where
    testLabel = "unlockScriptSameBlock"

failedScript :: IOManager -> [(Text, Text)] -> Assertion
failedScript =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    tx0 <- withBabbageLedgerState interpreter $ Babbage.mkLockByScriptTx (UTxOIndex 0) [Babbage.TxOutNoInline False] 20000 20000
    void $ forgeNextAndSubmit interpreter mockServer $ MockBlock [TxBabbage tx0] (NodeId 1)

    let utxo0 = head (Babbage.mkUTxOBabbage tx0)
    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkUnlockScriptTx [UTxOPair utxo0] (UTxOIndex 1) (UTxOIndex 2) False 10000 500

    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (0, 0, 0, 0, 1, 0, 1, 1)
  where
    testLabel = "failedScript"

failedScriptFees :: IOManager -> [(Text, Text)] -> Assertion
failedScriptFees =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    tx0 <- withBabbageLedgerState interpreter $ Babbage.mkLockByScriptTx (UTxOIndex 0) [Babbage.TxOutNoInline False] 20000 20000
    void $ forgeNextAndSubmit interpreter mockServer $ MockBlock [TxBabbage tx0] (NodeId 1)

    let utxo0 = head (Babbage.mkUTxOBabbage tx0)
    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkUnlockScriptTx [UTxOPair utxo0] (UTxOIndex 1) (UTxOIndex 2) False 10000 500

    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (0, 0, 0, 0, 1, 0, 1, 1)
    assertNonZeroFeesContract dbSync
  where
    testLabel = "failedScriptFees"

failedScriptSameBlock :: IOManager -> [(Text, Text)] -> Assertion
failedScriptSameBlock =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Babbage.mkLockByScriptTx (UTxOIndex 0) [Babbage.TxOutNoInline False] 20000 20000 st
      let utxo0 = head (Babbage.mkUTxOBabbage tx0)
      tx1 <- Babbage.mkUnlockScriptTx [UTxOPair utxo0] (UTxOIndex 1) (UTxOIndex 2) False 10000 500 st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (0, 0, 0, 0, 1, 0, 1, 1)
  where
    testLabel = "failedScriptSameBlock"

multipleScripts :: IOManager -> [(Text, Text)] -> Assertion
multipleScripts =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    tx0 <- withBabbageLedgerState interpreter $ Babbage.mkLockByScriptTx (UTxOIndex 0) (Babbage.TxOutNoInline <$> [True, False, True]) 20000 20000
    let utxo = Babbage.mkUTxOBabbage tx0
        pair1 = head utxo
        pair2 = utxo !! 2
    tx1 <-
      withBabbageLedgerState interpreter $
        Babbage.mkUnlockScriptTx [UTxOPair pair1, UTxOPair pair2] (UTxOIndex 1) (UTxOIndex 2) True 10000 500

    void $ forgeNextAndSubmit interpreter mockServer $ MockBlock [TxBabbage tx0] (NodeId 1)
    void $ forgeNextAndSubmit interpreter mockServer $ MockBlock [TxBabbage tx1] (NodeId 1)

    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (1, 2, 1, 1, 3, 2, 0, 0)
  where
    testLabel = "multipleScripts"

multipleScriptsRollback :: IOManager -> [(Text, Text)] -> Assertion
multipleScriptsRollback =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    tx0 <- withBabbageLedgerState interpreter $ Babbage.mkLockByScriptTx (UTxOIndex 0) (Babbage.TxOutNoInline <$> [True, False, True]) 20000 20000
    let utxo = Babbage.mkUTxOBabbage tx0
        pair1 = head utxo
        pair2 = utxo !! 2
    tx1 <-
      withBabbageLedgerState interpreter $
        Babbage.mkUnlockScriptTx [UTxOPair pair1, UTxOPair pair2] (UTxOIndex 1) (UTxOIndex 2) True 10000 500

    void $ forgeNextAndSubmit interpreter mockServer $ MockBlock [TxBabbage tx0] (NodeId 1)
    void $ forgeNextAndSubmit interpreter mockServer $ MockBlock [TxBabbage tx1] (NodeId 1)

    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (1, 2, 1, 1, 3, 2, 0, 0)

    rollbackTo interpreter mockServer genesisPoint
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []

    void $ forgeNextAndSubmit interpreter mockServer $ MockBlock [TxBabbage tx0] (NodeId 1)
    void $ forgeNextAndSubmit interpreter mockServer $ MockBlock [TxBabbage tx1] (NodeId 1)
    assertBlockNoBackoff dbSync 3

    assertAlonzoCounts dbSync (1, 2, 1, 1, 3, 2, 0, 0)
  where
    testLabel = "multipleScriptsRollback"

multipleScriptsSameBlock :: IOManager -> [(Text, Text)] -> Assertion
multipleScriptsSameBlock =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Babbage.mkLockByScriptTx (UTxOIndex 0) (Babbage.TxOutNoInline <$> [True, False, True]) 20000 20000 st
      let utxo = Babbage.mkUTxOBabbage tx0
          pair1 = head utxo
          pair2 = utxo !! 2
      tx1 <- Babbage.mkUnlockScriptTx [UTxOPair pair1, UTxOPair pair2] (UTxOIndex 1) (UTxOIndex 2) True 10000 500 st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 1
    assertAlonzoCounts dbSync (1, 2, 1, 1, 3, 2, 0, 0)
  where
    testLabel = "multipleScriptsSameBlock"

multipleScriptsFailed :: IOManager -> [(Text, Text)] -> Assertion
multipleScriptsFailed =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    tx0 <- withBabbageLedgerState interpreter $ Babbage.mkLockByScriptTx (UTxOIndex 0) (Babbage.TxOutNoInline <$> [True, False, True]) 20000 20000
    void $ forgeNextAndSubmit interpreter mockServer $ MockBlock [TxBabbage tx0] (NodeId 1)

    let utxos = Babbage.mkUTxOBabbage tx0
    tx1 <-
      withBabbageLedgerState interpreter $
        Babbage.mkUnlockScriptTx (UTxOPair <$> [head utxos, utxos !! 1, utxos !! 2]) (UTxOIndex 1) (UTxOIndex 2) False 10000 500
    void $ forgeNextAndSubmit interpreter mockServer $ MockBlock [TxBabbage tx1] (NodeId 1)

    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (0, 0, 0, 0, 3, 0, 1, 1)
  where
    testLabel = "multipleScriptsFailed"

multipleScriptsFailedSameBlock :: IOManager -> [(Text, Text)] -> Assertion
multipleScriptsFailedSameBlock =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Babbage.mkLockByScriptTx (UTxOIndex 0) (Babbage.TxOutNoInline <$> [True, False, True]) 20000 20000 st

      let utxos = tail $ Babbage.mkUTxOBabbage tx0
      tx1 <- Babbage.mkUnlockScriptTx (UTxOPair <$> [head utxos, utxos !! 1, utxos !! 2]) (UTxOIndex 1) (UTxOIndex 2) False 10000 500 st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 1
    assertAlonzoCounts dbSync (0, 0, 0, 0, 3, 0, 1, 1)
  where
    testLabel = "multipleScriptsFailedSameBlock"

----------------------------------------------------------------------------------------------------------
-- Plutus Cert Scripts
----------------------------------------------------------------------------------------------------------

registrationScriptTx :: IOManager -> [(Text, Text)] -> Assertion
registrationScriptTx =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkSimpleDCertTx [(StakeIndexScript True, ShelleyTxCertDelegCert . ShelleyRegCert)]
    assertBlockNoBackoff dbSync 1
    assertScriptCert dbSync (0, 0, 0, 1)
  where
    testLabel = "registrationScriptTx"

deregistrationScriptTx :: IOManager -> [(Text, Text)] -> Assertion
deregistrationScriptTx =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Babbage.mkSimpleDCertTx [(StakeIndexScript True, ShelleyTxCertDelegCert . ShelleyRegCert)] st
      tx1 <- Babbage.mkScriptDCertTx [(StakeIndexScript True, True, ShelleyTxCertDelegCert . ShelleyUnRegCert)] True st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 1
    assertScriptCert dbSync (1, 0, 0, 1)
  where
    testLabel = "deregistrationScriptTx"

deregistrationsScriptTxs :: IOManager -> [(Text, Text)] -> Assertion
deregistrationsScriptTxs =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Babbage.mkSimpleDCertTx [(StakeIndexScript True, ShelleyTxCertDelegCert . ShelleyRegCert)] st
      tx1 <- Babbage.mkScriptDCertTx [(StakeIndexScript True, True, ShelleyTxCertDelegCert . ShelleyUnRegCert)] True st
      tx2 <- Babbage.mkSimpleDCertTx [(StakeIndexScript True, ShelleyTxCertDelegCert . ShelleyRegCert)] st
      tx3 <- Babbage.mkScriptDCertTx [(StakeIndexScript True, True, ShelleyTxCertDelegCert . ShelleyUnRegCert)] True st
      pure [tx0, tx1, Babbage.addValidityInterval 1000 tx2, Babbage.addValidityInterval 2000 tx3]

    assertBlockNoBackoff dbSync 1
    assertScriptCert dbSync (2, 0, 0, 1)
    assertAlonzoCounts dbSync (1, 2, 1, 0, 0, 0, 0, 0)
  where
    testLabel = "deregistrationsScriptTxs"

deregistrationsScriptTx :: IOManager -> [(Text, Text)] -> Assertion
deregistrationsScriptTx =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Babbage.mkSimpleDCertTx [(StakeIndexScript True, ShelleyTxCertDelegCert . ShelleyRegCert)] st
      tx1 <-
        Babbage.mkScriptDCertTx
          [ (StakeIndexScript True, True, ShelleyTxCertDelegCert . ShelleyUnRegCert)
          , (StakeIndexScript True, False, ShelleyTxCertDelegCert . ShelleyRegCert)
          , (StakeIndexScript True, True, ShelleyTxCertDelegCert . ShelleyUnRegCert)
          ]
          True
          st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 1
    assertScriptCert dbSync (2, 0, 0, 1)
    assertAlonzoCounts dbSync (1, 2, 1, 0, 0, 0, 0, 0)
  where
    testLabel = "deregistrationsScriptTx"

-- Like previous but missing a redeemer. This is a known ledger issue
deregistrationsScriptTx' :: IOManager -> [(Text, Text)] -> Assertion
deregistrationsScriptTx' =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Babbage.mkSimpleDCertTx [(StakeIndexScript True, ShelleyTxCertDelegCert . ShelleyRegCert)] st
      tx1 <-
        Babbage.mkScriptDCertTx
          [ (StakeIndexScript True, False, ShelleyTxCertDelegCert . ShelleyUnRegCert)
          , (StakeIndexScript True, False, ShelleyTxCertDelegCert . ShelleyRegCert)
          , (StakeIndexScript True, True, ShelleyTxCertDelegCert . ShelleyUnRegCert)
          ]
          True
          st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 1
    -- TODO: This is a bug! The first field should be 2. However the deregistrations
    -- are missing the redeemers
    assertScriptCert dbSync (0, 0, 0, 1)
    assertAlonzoCounts dbSync (1, 1, 1, 0, 0, 0, 0, 0)
  where
    testLabel = "deregistrationsScriptTx'"

-- Like previous but missing the other redeemer. This is a known ledger issue
deregistrationsScriptTx'' :: IOManager -> [(Text, Text)] -> Assertion
deregistrationsScriptTx'' =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Babbage.mkSimpleDCertTx [(StakeIndexScript True, ShelleyTxCertDelegCert . ShelleyRegCert)] st
      tx1 <-
        Babbage.mkScriptDCertTx
          [ (StakeIndexScript True, True, ShelleyTxCertDelegCert . ShelleyUnRegCert)
          , (StakeIndexScript True, False, ShelleyTxCertDelegCert . ShelleyRegCert)
          , (StakeIndexScript True, False, ShelleyTxCertDelegCert . ShelleyUnRegCert)
          ]
          True
          st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 1
    assertScriptCert dbSync (2, 0, 0, 1)
    assertAlonzoCounts dbSync (1, 1, 1, 0, 0, 0, 0, 0)
  where
    testLabel = "deregistrationsScriptTx''"

----------------------------------------------------------------------------------------------------------
-- Plutus MultiAsset Scripts
----------------------------------------------------------------------------------------------------------

mintMultiAsset :: IOManager -> [(Text, Text)] -> Assertion
mintMultiAsset =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ \st -> do
      let val0 = MultiAsset $ Map.singleton (PolicyID alwaysMintScriptHash) (Map.singleton (head assetNames) 1)
      Babbage.mkMAssetsScriptTx [UTxOIndex 0] (UTxOIndex 1) [(UTxOAddressNew 0, MaryValue 10000 mempty)] [] val0 True 100 st

    assertBlockNoBackoff dbSync 1
    assertAlonzoCounts dbSync (1, 1, 1, 1, 0, 0, 0, 0)
  where
    testLabel = "mintMultiAsset"

mintMultiAssets :: IOManager -> [(Text, Text)] -> Assertion
mintMultiAssets =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      let assets0 = Map.fromList [(head assetNames, 10), (assetNames !! 1, 4)]
      let policy0 = PolicyID alwaysMintScriptHash
      let policy1 = PolicyID alwaysSucceedsScriptHash
      let val1 = MultiAsset $ Map.fromList [(policy0, assets0), (policy1, assets0)]
      tx0 <- Babbage.mkMAssetsScriptTx [UTxOIndex 0] (UTxOIndex 1) [(UTxOAddressNew 0, MaryValue 10000 mempty)] [] val1 True 100 st
      tx1 <- Babbage.mkMAssetsScriptTx [UTxOIndex 2] (UTxOIndex 3) [(UTxOAddressNew 0, MaryValue 10000 mempty)] [] val1 True 200 st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 1
    assertAlonzoCounts dbSync (2, 4, 1, 2, 0, 0, 0, 0)
  where
    testLabel = "mintMultiAssets"

swapMultiAssets :: IOManager -> [(Text, Text)] -> Assertion
swapMultiAssets =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      let assetsMinted0 = Map.fromList [(head assetNames, 10), (assetNames !! 1, 4)]
      let policy0 = PolicyID alwaysMintScriptHash
      let policy1 = PolicyID alwaysSucceedsScriptHash
      let mintValue0 = MultiAsset $ Map.fromList [(policy0, assetsMinted0), (policy1, assetsMinted0)]
      let assets0 = Map.fromList [(head assetNames, 5), (assetNames !! 1, 2)]
      let outValue0 = MaryValue 20 $ MultiAsset $ Map.fromList [(policy0, assets0), (policy1, assets0)]

      tx0 <-
        Babbage.mkMAssetsScriptTx
          [UTxOIndex 0]
          (UTxOIndex 1)
          [(UTxOAddress alwaysSucceedsScriptAddr, outValue0), (UTxOAddress alwaysMintScriptAddr, outValue0)]
          []
          mintValue0
          True
          100
          st

      let utxos = Babbage.mkUTxOBabbage tx0
      tx1 <-
        Babbage.mkMAssetsScriptTx
          [UTxOPair (head utxos), UTxOPair (utxos !! 1), UTxOIndex 2]
          (UTxOIndex 3)
          [ (UTxOAddress alwaysSucceedsScriptAddr, outValue0)
          , (UTxOAddress alwaysMintScriptAddr, outValue0)
          , (UTxOAddressNew 0, outValue0)
          , (UTxOAddressNew 0, outValue0)
          ]
          []
          mintValue0
          True
          200
          st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 1
    assertAlonzoCounts dbSync (2, 6, 1, 2, 4, 2, 0, 0)
  where
    testLabel = "swapMultiAssets"
