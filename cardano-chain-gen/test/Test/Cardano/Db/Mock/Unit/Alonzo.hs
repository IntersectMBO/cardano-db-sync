{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Db.Mock.Unit.Alonzo (
  unitTests,
) where

import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Db as DB
import Cardano.DbSync.Era.Shelley.Generic.Util
import Cardano.Ledger.Alonzo.Scripts.Data
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Credential
import Cardano.Ledger.Keys
import Cardano.Ledger.Mary.Value
import Cardano.Ledger.SafeHash
import Cardano.Ledger.Shelley.TxBody
import Cardano.Mock.ChainSync.Server
import Cardano.Mock.Forging.Interpreter
import qualified Cardano.Mock.Forging.Tx.Alonzo as Alonzo
import Cardano.Mock.Forging.Tx.Alonzo.ScriptsExamples
import Cardano.Mock.Forging.Tx.Generic
import Cardano.Mock.Forging.Types
import Cardano.SMASH.Server.PoolDataLayer
import Cardano.SMASH.Server.Types
import Control.Monad
import qualified Data.Map as Map
import Data.Text (Text)
import Ouroboros.Consensus.Cardano.Block hiding (CardanoBlock)
import Test.Cardano.Db.Mock.Config
import Test.Cardano.Db.Mock.UnifiedApi
import Test.Cardano.Db.Mock.Validate
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)

import qualified Test.Cardano.Db.Mock.Unit.Alonzo.Simple as AlzSimple
import qualified Test.Cardano.Db.Mock.Unit.Alonzo.Tx as AlzTx
import qualified Test.Cardano.Db.Mock.Unit.Alonzo.Stake as AlzStake
import qualified Test.Cardano.Db.Mock.Unit.Alonzo.Reward as AlzReward

{- HLINT ignore "Reduce duplication" -}

unitTests :: IOManager -> [(Text, Text)] -> TestTree
unitTests iom knownMigrations =
  testGroup
    "Alonzo unit tests"
    [ testGroup
        "simple"
        [ test "simple forge blocks" AlzSimple.forgeBlocks
        , test "sync one block" AlzSimple.addSimple
        , test "restart db-sync" AlzSimple.restartDBSync
        , test "sync small chain" AlzSimple.addSimpleChain
        ]
    , testGroup
        "blocks with txs"
        [ test "simple tx" AlzTx.addSimpleTx
        , test "consume utxo same block" AlzTx.consumeSameBlock
        ]
    , testGroup
        "stake addresses"
        [ test "(de)registrations" AlzStake.registrationTx
        , test "(de)registrations in same block" AlzStake.registrationsSameBlock
        , test "(de)registrations in same tx" AlzStake.registrationsSameTx
        , test "stake address pointers" AlzStake.stakeAddressPtr
        , test "stake address pointers deregistration" AlzStake.stakeAddressPtrDereg
        , test "stake address pointers. Use before registering." AlzStake.stakeAddressPtrUseBefore
        ]
    , testGroup
        "rewards"
        [ test "rewards simple" AlzReward.simpleRewards
        , test "rewards with deregistration" AlzReward.rewardsDeregistration
        , test "rewards with reregistration. Fixed in Babbage." AlzReward.rewardsReregistration
        , test "Mir Cert" AlzReward.mirReward
        , test "Mir rollback" AlzReward.mirRewardRollback
        , test "Mir Cert deregistration" AlzReward.mirRewardDereg
        , -- , test "test rewards empty last part of epoch" rewardsEmptyChainLast
          --        , test "test delta rewards" rewardsDelta -- See the same test on Babbage for the reason it was disabled.
          test "rollback on epoch boundary" AlzReward.rollbackBoundary
        , test "single MIR Cert multiple outputs" AlzReward.singleMIRCertMultiOut
        ]
    , testGroup
        "stake distribution"
        [ test "stake distribution from genesis" AlzStake.stakeDistGenesis
        , test "2000 delegations" AlzStake.delegations2000
        , test "2001 delegations" AlzStake.delegations2001
        , test "8000 delegations" AlzStake.delegations8000
        , test "many delegations" AlzStake.delegationsMany
        , test "many delegations, sparse chain" AlzStake.delegationsManyNotDense
        ]
    , testGroup
        "plutus spend scripts"
        [ test "simple script lock" simpleScript
        , test "unlock script in same block" unlockScriptSameBlock
        , test "failed script" failedScript
        , test "failed script in same block" failedScriptSameBlock
        , test "multiple scripts unlocked" multipleScripts
        , test "multiple scripts unlocked same block" multipleScriptsSameBlock
        , test "multiple scripts failed" multipleScriptsFailed
        , test "multiple scripts failed same block" multipleScriptsFailedSameBlock
        ]
    , testGroup
        "plutus cert scripts"
        [ test "stake scripts" registrationScriptTx
        , test "stake scripts deregistration" deregistrationScriptTx
        , test "multiple stake scripts deregistration" deregistrationsScriptTxs
        , test "multiple stake scripts deregistration in same tx" deregistrationsScriptTx
        , test "multiple stake scripts deregistration in same tx missing redeemer 1" deregistrationsScriptTx'
        , test "multiple stake scripts deregistration in same tx missing redeemer 2" deregistrationsScriptTx''
        ]
    , testGroup
        "MultiAssets plutus scripts"
        [ test "mint simple multi asset" mintMultiAsset
        , test "mint many multi assets" mintMultiAssets
        , test "swap many multi assets" swapMultiAssets
        ]
    , testGroup
        "pools and smash"
        [ test "pool registration" poolReg
        , test "query pool that's not registered" nonexistantPoolQuery
        , test "pool deregistration" poolDeReg
        , test "pool multiple deregistration" poolDeRegMany
        , test "delist pool" poolDelist
        ]
    ]
  where
    test :: String -> (IOManager -> [(Text, Text)] -> Assertion) -> TestTree
    test str action = testCase str (action iom knownMigrations)


simpleScript :: IOManager -> [(Text, Text)] -> Assertion
simpleScript =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    a <- fillUntilNextEpoch interpreter mockServer

    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkLockByScriptTx (UTxOIndex 0) [True] 20000 20000

    assertBlockNoBackoff dbSync (fromIntegral $ length a + 2)
    assertEqQuery dbSync (fmap getOutFields <$> DB.queryScriptOutputs) [expectedFields] "Unexpected script outputs"
  where
    testLabel = "simpleScript-alonzo"
    getOutFields txOut = (DB.txOutAddress txOut, DB.txOutAddressHasScript txOut, DB.txOutValue txOut, DB.txOutDataHash txOut)
    expectedFields =
      ( renderAddress alwaysSucceedsScriptAddr
      , True
      , DB.DbLovelace 20000
      , Just $ Crypto.hashToBytes (extractHash $ hashData @StandardAlonzo plutusDataList)
      )

_unlockScript :: IOManager -> [(Text, Text)] -> Assertion
_unlockScript =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    -- We don't use withAlonzoFindLeaderAndSubmitTx here, because we want access to the tx.
    tx0 <- withAlonzoLedgerState interpreter $ Alonzo.mkLockByScriptTx (UTxOIndex 0) [True] 20000 20000
    void $ forgeNextAndSubmit interpreter mockServer $ MockBlock [TxAlonzo tx0] (NodeId 1)

    let utxo0 = head (Alonzo.mkUTxOAlonzo tx0)
    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkUnlockScriptTx [UTxOPair utxo0] (UTxOIndex 1) (UTxOIndex 2) True 10000 500

    assertBlockNoBackoff dbSync 3
    assertAlonzoCounts dbSync (1, 1, 1, 1, 1, 1, 0, 0)
  where
    testLabel = "unlockScript-alonzo"

unlockScriptSameBlock :: IOManager -> [(Text, Text)] -> Assertion
unlockScriptSameBlock =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkLockByScriptTx (UTxOIndex 0) [True] 20000 20000 st
      let utxo0 = head (Alonzo.mkUTxOAlonzo tx0)
      tx1 <- Alonzo.mkUnlockScriptTx [UTxOPair utxo0] (UTxOIndex 1) (UTxOIndex 2) True 10000 500 st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (1, 1, 1, 1, 1, 1, 0, 0)
  where
    testLabel = "unlockScriptSameBlock-alonzo"

failedScript :: IOManager -> [(Text, Text)] -> Assertion
failedScript =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    tx0 <- withAlonzoLedgerState interpreter $ Alonzo.mkLockByScriptTx (UTxOIndex 0) [False] 20000 20000
    void $ forgeNextAndSubmit interpreter mockServer $ MockBlock [TxAlonzo tx0] (NodeId 1)

    let utxo0 = head (Alonzo.mkUTxOAlonzo tx0)
    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkUnlockScriptTx [UTxOPair utxo0] (UTxOIndex 1) (UTxOIndex 2) False 10000 500

    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (0, 0, 0, 0, 1, 0, 1, 1)
  where
    testLabel = "failedScript-alonzo"

failedScriptSameBlock :: IOManager -> [(Text, Text)] -> Assertion
failedScriptSameBlock =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkLockByScriptTx (UTxOIndex 0) [False] 20000 20000 st
      let utxo0 = head (Alonzo.mkUTxOAlonzo tx0)
      tx1 <- Alonzo.mkUnlockScriptTx [UTxOPair utxo0] (UTxOIndex 1) (UTxOIndex 2) False 10000 500 st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (0, 0, 0, 0, 1, 0, 1, 1)
  where
    testLabel = "failedScriptSameBlock-alonzo"

multipleScripts :: IOManager -> [(Text, Text)] -> Assertion
multipleScripts =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    tx0 <- withAlonzoLedgerState interpreter $ Alonzo.mkLockByScriptTx (UTxOIndex 0) [True, False, True] 20000 20000
    let utxo = Alonzo.mkUTxOAlonzo tx0
        pair1 = head utxo
        pair2 = utxo !! 2
    tx1 <-
      withAlonzoLedgerState interpreter $
        Alonzo.mkUnlockScriptTx [UTxOPair pair1, UTxOPair pair2] (UTxOIndex 1) (UTxOIndex 2) True 10000 500

    void $ forgeNextAndSubmit interpreter mockServer $ MockBlock [TxAlonzo tx0] (NodeId 1)
    void $ forgeNextAndSubmit interpreter mockServer $ MockBlock [TxAlonzo tx1] (NodeId 1)

    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (1, 2, 1, 1, 3, 2, 0, 0)
  where
    testLabel = "multipleScripts-alonzo"

multipleScriptsSameBlock :: IOManager -> [(Text, Text)] -> Assertion
multipleScriptsSameBlock =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkLockByScriptTx (UTxOIndex 0) [True, False, True] 20000 20000 st
      let utxo = Alonzo.mkUTxOAlonzo tx0
          pair1 = head utxo
          pair2 = utxo !! 2
      tx1 <- Alonzo.mkUnlockScriptTx [UTxOPair pair1, UTxOPair pair2] (UTxOIndex 1) (UTxOIndex 2) True 10000 500 st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 1
    assertAlonzoCounts dbSync (1, 2, 1, 1, 3, 2, 0, 0)
  where
    testLabel = "multipleScriptsSameBlock-alonzo"

multipleScriptsFailed :: IOManager -> [(Text, Text)] -> Assertion
multipleScriptsFailed =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    tx0 <- withAlonzoLedgerState interpreter $ Alonzo.mkLockByScriptTx (UTxOIndex 0) [True, False, True] 20000 20000
    void $ forgeNextAndSubmit interpreter mockServer $ MockBlock [TxAlonzo tx0] (NodeId 1)

    let utxos = Alonzo.mkUTxOAlonzo tx0
    tx1 <-
      withAlonzoLedgerState interpreter $
        Alonzo.mkUnlockScriptTx (UTxOPair <$> [head utxos, utxos !! 1, utxos !! 2]) (UTxOIndex 1) (UTxOIndex 2) False 10000 500
    void $ forgeNextAndSubmit interpreter mockServer $ MockBlock [TxAlonzo tx1] (NodeId 1)

    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (0, 0, 0, 0, 3, 0, 1, 1)
  where
    testLabel = "multipleScriptsFailed-alonzo"

multipleScriptsFailedSameBlock :: IOManager -> [(Text, Text)] -> Assertion
multipleScriptsFailedSameBlock =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkLockByScriptTx (UTxOIndex 0) [True, False, True] 20000 20000 st

      let utxos = tail $ Alonzo.mkUTxOAlonzo tx0
      tx1 <- Alonzo.mkUnlockScriptTx (UTxOPair <$> [head utxos, utxos !! 1, utxos !! 2]) (UTxOIndex 1) (UTxOIndex 2) False 10000 500 st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 1
    assertAlonzoCounts dbSync (0, 0, 0, 0, 3, 0, 1, 1)
  where
    testLabel = "multipleScriptsFailedSameBlock-alonzo"

registrationScriptTx :: IOManager -> [(Text, Text)] -> Assertion
registrationScriptTx =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [(StakeIndexScript True, DCertDeleg . RegKey)]
    assertBlockNoBackoff dbSync 1
    assertScriptCert dbSync (0, 0, 0, 1)
  where
    testLabel = "registrationScriptTx-alonzo"

deregistrationScriptTx :: IOManager -> [(Text, Text)] -> Assertion
deregistrationScriptTx =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkSimpleDCertTx [(StakeIndexScript True, DCertDeleg . RegKey)] st
      tx1 <- Alonzo.mkScriptDCertTx [(StakeIndexScript True, True, DCertDeleg . DeRegKey)] True st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 1
    assertScriptCert dbSync (1, 0, 0, 1)
  where
    testLabel = "deregistrationScriptTx-alonzo"

deregistrationsScriptTxs :: IOManager -> [(Text, Text)] -> Assertion
deregistrationsScriptTxs =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkSimpleDCertTx [(StakeIndexScript True, DCertDeleg . RegKey)] st
      tx1 <- Alonzo.mkScriptDCertTx [(StakeIndexScript True, True, DCertDeleg . DeRegKey)] True st
      tx2 <- Alonzo.mkSimpleDCertTx [(StakeIndexScript True, DCertDeleg . RegKey)] st
      tx3 <- Alonzo.mkScriptDCertTx [(StakeIndexScript True, True, DCertDeleg . DeRegKey)] True st
      pure [tx0, tx1, Alonzo.addValidityInterval 1000 tx2, Alonzo.addValidityInterval 2000 tx3]

    assertBlockNoBackoff dbSync 1
    assertScriptCert dbSync (2, 0, 0, 1)
    assertAlonzoCounts dbSync (1, 2, 1, 0, 0, 0, 0, 0)
  where
    testLabel = "deregistrationsScriptTxs-alonzo"

deregistrationsScriptTx :: IOManager -> [(Text, Text)] -> Assertion
deregistrationsScriptTx =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkSimpleDCertTx [(StakeIndexScript True, DCertDeleg . RegKey)] st
      tx1 <-
        Alonzo.mkScriptDCertTx
          [ (StakeIndexScript True, True, DCertDeleg . DeRegKey)
          , (StakeIndexScript True, False, DCertDeleg . RegKey)
          , (StakeIndexScript True, True, DCertDeleg . DeRegKey)
          ]
          True
          st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 1
    assertScriptCert dbSync (2, 0, 0, 1)
    assertAlonzoCounts dbSync (1, 2, 1, 0, 0, 0, 0, 0)
  where
    testLabel = "deregistrationsScriptTx-alonzo"

-- Like previous but missing a redeemer. This is a known ledger issue
deregistrationsScriptTx' :: IOManager -> [(Text, Text)] -> Assertion
deregistrationsScriptTx' =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkSimpleDCertTx [(StakeIndexScript True, DCertDeleg . RegKey)] st
      tx1 <-
        Alonzo.mkScriptDCertTx
          [ (StakeIndexScript True, False, DCertDeleg . DeRegKey)
          , (StakeIndexScript True, False, DCertDeleg . RegKey)
          , (StakeIndexScript True, True, DCertDeleg . DeRegKey)
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
    testLabel = "deregistrationsScriptTx'-alonzo"

-- Like previous but missing the other redeemer. This is a known ledger issue
deregistrationsScriptTx'' :: IOManager -> [(Text, Text)] -> Assertion
deregistrationsScriptTx'' =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkSimpleDCertTx [(StakeIndexScript True, DCertDeleg . RegKey)] st
      tx1 <-
        Alonzo.mkScriptDCertTx
          [ (StakeIndexScript True, True, DCertDeleg . DeRegKey)
          , (StakeIndexScript True, False, DCertDeleg . RegKey)
          , (StakeIndexScript True, False, DCertDeleg . DeRegKey)
          ]
          True
          st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 1
    assertScriptCert dbSync (2, 0, 0, 1)
    assertAlonzoCounts dbSync (1, 1, 1, 0, 0, 0, 0, 0)
  where
    testLabel = "deregistrationsScriptTx''-alonzo"

mintMultiAsset :: IOManager -> [(Text, Text)] -> Assertion
mintMultiAsset =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ \st -> do
      let val0 = MultiAsset $ Map.singleton (PolicyID alwaysMintScriptHash) (Map.singleton (head assetNames) 1)
      Alonzo.mkMAssetsScriptTx [UTxOIndex 0] (UTxOIndex 1) [(UTxOAddressNew 0, MaryValue 10000 mempty)] val0 True 100 st

    assertBlockNoBackoff dbSync 1
    assertAlonzoCounts dbSync (1, 1, 1, 1, 0, 0, 0, 0)
  where
    testLabel = "mintMultiAsset-alonzo"

mintMultiAssets :: IOManager -> [(Text, Text)] -> Assertion
mintMultiAssets =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      let assets0 = Map.fromList [(head assetNames, 10), (assetNames !! 1, 4)]
      let policy0 = PolicyID alwaysMintScriptHash
      let policy1 = PolicyID alwaysSucceedsScriptHash
      let val1 = MultiAsset $ Map.fromList [(policy0, assets0), (policy1, assets0)]
      tx0 <- Alonzo.mkMAssetsScriptTx [UTxOIndex 0] (UTxOIndex 1) [(UTxOAddressNew 0, MaryValue 10000 mempty)] val1 True 100 st
      tx1 <- Alonzo.mkMAssetsScriptTx [UTxOIndex 2] (UTxOIndex 3) [(UTxOAddressNew 0, MaryValue 10000 mempty)] val1 True 200 st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 1
    assertAlonzoCounts dbSync (2, 4, 1, 2, 0, 0, 0, 0)
  where
    testLabel = "mintMultiAssets-alonzo"

swapMultiAssets :: IOManager -> [(Text, Text)] -> Assertion
swapMultiAssets =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      let assetsMinted0 = Map.fromList [(head assetNames, 10), (assetNames !! 1, 4)]
      let policy0 = PolicyID alwaysMintScriptHash
      let policy1 = PolicyID alwaysSucceedsScriptHash
      let mintValue0 = MultiAsset $ Map.fromList [(policy0, assetsMinted0), (policy1, assetsMinted0)]
      let assets0 = Map.fromList [(head assetNames, 5), (assetNames !! 1, 2)]
      let outValue0 = MaryValue 20 $ MultiAsset $ Map.fromList [(policy0, assets0), (policy1, assets0)]

      tx0 <-
        Alonzo.mkMAssetsScriptTx
          [UTxOIndex 0]
          (UTxOIndex 1)
          [(UTxOAddress alwaysSucceedsScriptAddr, outValue0), (UTxOAddress alwaysMintScriptAddr, outValue0)]
          mintValue0
          True
          100
          st

      let utxos = Alonzo.mkUTxOAlonzo tx0
      tx1 <-
        Alonzo.mkMAssetsScriptTx
          [UTxOPair (head utxos), UTxOPair (utxos !! 1), UTxOIndex 2]
          (UTxOIndex 3)
          [ (UTxOAddress alwaysSucceedsScriptAddr, outValue0)
          , (UTxOAddress alwaysMintScriptAddr, outValue0)
          , (UTxOAddressNew 0, outValue0)
          , (UTxOAddressNew 0, outValue0)
          ]
          mintValue0
          True
          200
          st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 1
    assertAlonzoCounts dbSync (2, 6, 1, 2, 4, 2, 0, 0)
  where
    testLabel = "swapMultiAssets-alonzo"

poolReg :: IOManager -> [(Text, Text)] -> Assertion
poolReg =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync 1
    initCounter <- runQuery dbSync poolCountersQuery
    assertEqual "Unexpected init pool counter" (3, 0, 3, 2, 0, 0) initCounter

    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkDCertPoolTx
          [
            ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Alonzo.consPoolParamsTwoOwners
            )
          ]

    assertBlockNoBackoff dbSync 2
    assertPoolCounters dbSync (addPoolCounters (1, 1, 1, 2, 0, 1) initCounter)
    st <- getAlonzoLedgerState interpreter
    assertPoolLayerCounters dbSync (0, 0) [(PoolIndexNew 0, (Right False, False, True))] st
  where
    testLabel = "poolReg-alonzo"

-- Issue https://github.com/input-output-hk/cardano-db-sync/issues/997
nonexistantPoolQuery :: IOManager -> [(Text, Text)] -> Assertion
nonexistantPoolQuery =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync 1

    st <- getAlonzoLedgerState interpreter
    assertPoolLayerCounters dbSync (0, 0) [(PoolIndexNew 0, (Left RecordDoesNotExist, False, False))] st
  where
    testLabel = "nonexistantPoolQuery-alonzo"

poolDeReg :: IOManager -> [(Text, Text)] -> Assertion
poolDeReg =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync 1
    initCounter <- runQuery dbSync poolCountersQuery
    assertEqual "Unexpected init pool counter" (3, 0, 3, 2, 0, 0) initCounter

    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkDCertPoolTx
          [
            ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Alonzo.consPoolParamsTwoOwners
            )
          , ([], PoolIndexNew 0, \_ poolId -> DCertPool $ RetirePool poolId 1)
          ]

    assertBlockNoBackoff dbSync 2
    assertPoolCounters dbSync (addPoolCounters (1, 1, 1, 2, 1, 1) initCounter)

    st <- getAlonzoLedgerState interpreter
    -- Not retired yet, because epoch has not changed
    assertPoolLayerCounters dbSync (0, 0) [(PoolIndexNew 0, (Right False, False, True))] st

    -- change epoch
    a <- fillUntilNextEpoch interpreter mockServer

    assertBlockNoBackoff dbSync (fromIntegral $ length a + 2)
    -- these counters are the same
    assertPoolCounters dbSync (addPoolCounters (1, 1, 1, 2, 1, 1) initCounter)

    -- the pool is now retired, since the epoch changed.
    assertPoolLayerCounters dbSync (1, 0) [(PoolIndexNew 0, (Right True, False, False))] st
  where
    testLabel = "poolDeReg-alonzo"

poolDeRegMany :: IOManager -> [(Text, Text)] -> Assertion
poolDeRegMany =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync 1
    initCounter <- runQuery dbSync poolCountersQuery
    assertEqual "Unexpected init pool counter" (3, 0, 3, 2, 0, 0) initCounter

    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkDCertPoolTx
          [ -- register

            ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Alonzo.consPoolParamsTwoOwners
            )
          , -- de register
            ([], PoolIndexNew 0, mkPoolDereg 4)
          , -- register

            ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Alonzo.consPoolParamsTwoOwners
            )
          , -- register with different owner and reward address

            ( [StakeIndexNew 2, StakeIndexNew 1, StakeIndexNew 0]
            , PoolIndexNew 0
            , Alonzo.consPoolParamsTwoOwners
            )
          ]

    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <-
        Alonzo.mkDCertPoolTx
          [ -- register

            ( [StakeIndexNew 2, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Alonzo.consPoolParamsTwoOwners
            )
          ]
          st

      tx1 <-
        Alonzo.mkDCertPoolTx
          [ -- deregister
            ([] :: [StakeIndex], PoolIndexNew 0, mkPoolDereg 4)
          , -- register

            ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Alonzo.consPoolParamsTwoOwners
            )
          , -- deregister
            ([] :: [StakeIndex], PoolIndexNew 0, mkPoolDereg 1)
          ]
          st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 3
    -- TODO fix PoolOwner and PoolRelay unique key
    assertPoolCounters dbSync (addPoolCounters (1, 1, 5, 10, 3, 5) initCounter)

    st <- getAlonzoLedgerState interpreter
    -- Not retired yet, because epoch has not changed
    assertPoolLayerCounters dbSync (0, 0) [(PoolIndexNew 0, (Right False, False, True))] st

    -- change epoch
    a <- fillUntilNextEpoch interpreter mockServer

    assertBlockNoBackoff dbSync (fromIntegral $ length a + 3)
    -- these counters are the same
    assertPoolCounters dbSync (addPoolCounters (1, 1, 5, 10, 3, 5) initCounter)

    -- from all these certificates only the latest matters. So it will retire
    -- on epoch 0
    assertPoolLayerCounters dbSync (1, 0) [(PoolIndexNew 0, (Right True, False, False))] st
  where
    testLabel = "poolDeRegMany-alonzo"
    mkPoolDereg ::
      EpochNo ->
      [StakeCredential StandardCrypto] ->
      KeyHash 'StakePool StandardCrypto ->
      DCert StandardCrypto
    mkPoolDereg epochNo _creds keyHash = DCertPool $ RetirePool keyHash epochNo

poolDelist :: IOManager -> [(Text, Text)] -> Assertion
poolDelist =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync 1
    initCounter <- runQuery dbSync poolCountersQuery
    assertEqual "Unexpected init pool counter" (3, 0, 3, 2, 0, 0) initCounter

    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkDCertPoolTx
          [
            ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Alonzo.consPoolParamsTwoOwners
            )
          ]

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync 3
    st <- getAlonzoLedgerState interpreter
    assertPoolLayerCounters dbSync (0, 0) [(PoolIndexNew 0, (Right False, False, True))] st

    let poolKeyHash = resolvePool (PoolIndexNew 0) st
    let poolId = dbToServantPoolId $ unKeyHashRaw poolKeyHash
    poolLayer <- getPoolLayer dbSync
    void $ dlAddDelistedPool poolLayer poolId

    -- This is not async, so we don't need to do exponential backoff
    -- delisted not retired
    assertPoolLayerCounters dbSync (0, 1) [(PoolIndexNew 0, (Right False, True, True))] st

    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkDCertPoolTx
          [([], PoolIndexNew 0, \_ poolHash -> DCertPool $ RetirePool poolHash 1)]

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync 5
    -- delisted and pending retirement
    assertPoolLayerCounters dbSync (0, 1) [(PoolIndexNew 0, (Right False, True, True))] st

    a <- fillUntilNextEpoch interpreter mockServer

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync (fromIntegral $ 5 + length a + 1)
    -- delisted and retired
    assertPoolLayerCounters dbSync (1, 1) [(PoolIndexNew 0, (Right True, True, False))] st
  where
    testLabel = "poolDelist-alonzo"
