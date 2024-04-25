{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Db.Mock.Unit.Alonzo (
  unitTests,
) where

import Cardano.Mock.ChainSync.Server (IOManager)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

import qualified Test.Cardano.Db.Mock.Unit.Alonzo.Config as AlzConfig
import qualified Test.Cardano.Db.Mock.Unit.Alonzo.Plutus as AlzPlutus
import qualified Test.Cardano.Db.Mock.Unit.Alonzo.PoolAndSmash as AlzPnS
import qualified Test.Cardano.Db.Mock.Unit.Alonzo.Reward as AlzReward
import qualified Test.Cardano.Db.Mock.Unit.Alonzo.Simple as AlzSimple
import qualified Test.Cardano.Db.Mock.Unit.Alonzo.Stake as AlzStake
import qualified Test.Cardano.Db.Mock.Unit.Alonzo.Tx as AlzTx

{- HLINT ignore "Reduce duplication" -}

unitTests :: IOManager -> [(Text, Text)] -> TestTree
unitTests iom knownMigrations =
  testGroup
    "Alonzo unit tests"
    [ testGroup
        "config"
        [ testCase "default insert config" AlzConfig.defaultInsertConfig
        , testCase "insert config" AlzConfig.insertConfig
        ]
    , testGroup
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
        [ test "simple script lock" AlzPlutus.simpleScript
        , test "unlock script in same block" AlzPlutus.unlockScriptSameBlock
        , test "failed script" AlzPlutus.failedScript
        , test "failed script in same block" AlzPlutus.failedScriptSameBlock
        , test "multiple scripts unlocked" AlzPlutus.multipleScripts
        , test "multiple scripts unlocked same block" AlzPlutus.multipleScriptsSameBlock
        , test "multiple scripts failed" AlzPlutus.multipleScriptsFailed
        , test "multiple scripts failed same block" AlzPlutus.multipleScriptsFailedSameBlock
        ]
    , testGroup
        "plutus cert scripts"
        [ test "stake scripts" AlzPlutus.registrationScriptTx
        , test "stake scripts deregistration" AlzPlutus.deregistrationScriptTx
        , test "multiple stake scripts deregistration" AlzPlutus.deregistrationsScriptTxs
        , test "multiple stake scripts deregistration in same tx" AlzPlutus.deregistrationsScriptTx
        , test "multiple stake scripts deregistration in same tx missing redeemer 1" AlzPlutus.deregistrationsScriptTx'
        , test "multiple stake scripts deregistration in same tx missing redeemer 2" AlzPlutus.deregistrationsScriptTx''
        ]
    , testGroup
        "MultiAssets plutus scripts"
        [ test "mint simple multi asset" AlzPlutus.mintMultiAsset
        , test "mint many multi assets" AlzPlutus.mintMultiAssets
        , test "swap many multi assets" AlzPlutus.swapMultiAssets
        ]
    , testGroup
        "pools and smash"
        [ test "pool registration" AlzPnS.poolReg
        , test "query pool that's not registered" AlzPnS.nonexistantPoolQuery
        , test "pool deregistration" AlzPnS.poolDeReg
        , test "pool multiple deregistration" AlzPnS.poolDeRegMany
        , test "delist pool" AlzPnS.poolDelist
        ]
    ]
  where
    test :: String -> (IOManager -> [(Text, Text)] -> Assertion) -> TestTree
    test str action = testCase str (action iom knownMigrations)
