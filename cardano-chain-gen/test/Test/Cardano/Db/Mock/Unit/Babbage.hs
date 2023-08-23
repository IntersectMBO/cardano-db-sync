{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Cardano.Db.Mock.Unit.Babbage (
  unitTests,
) where

import Cardano.Mock.ChainSync.Server (IOManager)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (expectFail)
import Test.Tasty.HUnit (Assertion, testCase)

import qualified Test.Cardano.Db.Mock.Unit.Babbage.CommandLineArg.ConfigFile as ConfigFile
import qualified Test.Cardano.Db.Mock.Unit.Babbage.CommandLineArg.EpochDisabled as EpochDisabled
import qualified Test.Cardano.Db.Mock.Unit.Babbage.CommandLineArg.ForceIndex as ForceIndex
import qualified Test.Cardano.Db.Mock.Unit.Babbage.CommandLineArg.MigrateConsumedPruneTxOut as MigrateConsumedPruneTxOut
import qualified Test.Cardano.Db.Mock.Unit.Babbage.InlineAndReference as BabInlineRef
import qualified Test.Cardano.Db.Mock.Unit.Babbage.Other as BabOther
import qualified Test.Cardano.Db.Mock.Unit.Babbage.Plutus as BabPlutus
import qualified Test.Cardano.Db.Mock.Unit.Babbage.Reward as BabReward
import qualified Test.Cardano.Db.Mock.Unit.Babbage.Rollback as BabRollback
import qualified Test.Cardano.Db.Mock.Unit.Babbage.Simple as BabSimple
import qualified Test.Cardano.Db.Mock.Unit.Babbage.Stake as BabStake
import qualified Test.Cardano.Db.Mock.Unit.Babbage.Tx as BabTx

unitTests :: IOManager -> [(Text, Text)] -> TestTree
unitTests iom knownMigrations =
  testGroup
    "Babbage unit tests"
    [ testGroup
        "simple"
        [ test "simple forge blocks" BabSimple.forgeBlocks
        , test "sync one block" BabSimple.addSimple
        , test "sync small chain" BabSimple.addSimpleChain
        , test "restart db-sync" BabSimple.restartDBSync
        , test "node restart" BabSimple.nodeRestart
        , test "node restart boundary" BabSimple.nodeRestartBoundary
        ]
    , testGroup
        "Command Line Arguments"
        [ testGroup
            "consumed-tx-out + prune-tx-out"
            [ test "flag check" MigrateConsumedPruneTxOut.commandLineArgCheck
            , test "basic prune" MigrateConsumedPruneTxOut.basicPrune
            , test "prune with simple rollback" MigrateConsumedPruneTxOut.pruneWithSimpleRollback
            , test "prune with full tx rollback" MigrateConsumedPruneTxOut.pruneWithFullTxRollback
            , test "pruning should keep some tx" MigrateConsumedPruneTxOut.pruningShouldKeepSomeTx
            , test "prune and rollback one block" MigrateConsumedPruneTxOut.pruneAndRollBackOneBlock
            , test "no pruning and rollback" MigrateConsumedPruneTxOut.noPruneAndRollBack
            , test "prune same block" MigrateConsumedPruneTxOut.pruneSameBlock
            , test "no pruning same block" MigrateConsumedPruneTxOut.noPruneSameBlock
            , expectFail $ test "restart with new consumed set to false" MigrateConsumedPruneTxOut.migrateAndPruneRestart
            , expectFail $ test "set prune flag, restart missing prune flag" MigrateConsumedPruneTxOut.pruneRestartMissingFlag
            ]
        , testGroup
            "config"
            [ expectFail $ test "fails if incorrect or no config file given" ConfigFile.checkConfigFileArg
            ]
        , testGroup
            "disable-epoch"
            [ test "Epoch doesn't update when disabled" EpochDisabled.checkEpochDisabledArg
            , test "Epoch updates when enabled" EpochDisabled.checkEpochEnabled
            ]
        , testGroup
            "force-indexes"
            [ test "check force-index adds indexes" ForceIndex.checkForceIndexesArg
            , test "check no force-index doesn't add indexes" ForceIndex.checkNoForceIndexesArg
            ]
        ]
    , testGroup
        "rollbacks"
        [ test "simple rollback" BabRollback.simpleRollback
        , test "sync bigger chain" BabRollback.bigChain
        , test "rollback while db-sync is off" BabRollback.restartAndRollback
        , --          , test "rollback further" rollbackFurther disabled
          test "big rollbacks executed lazily" BabRollback.lazyRollback
        , test "lazy rollback on restart" BabRollback.lazyRollbackRestart
        , test "rollback while rollbacking" BabRollback.doubleRollback
        , test "rollback stake address cache" BabRollback.stakeAddressRollback
        , test "rollback change order of txs" BabRollback.rollbackChangeTxOrder
        , test "rollback full tx" BabRollback.rollbackFullTx
        ]
    , testGroup
        "different configs"
        [ test "genesis config without pool" BabOther.configNoPools
        , test "genesis config without stakes" BabOther.configNoStakes
        ]
    , testGroup
        "blocks with txs"
        [ test "simple tx" BabTx.addSimpleTx
        , test "simple tx in Shelley era" BabTx.addSimpleTxShelley
        , test "consume utxo same block" BabTx.consumeSameBlock
        ]
    , testGroup
        "stake addresses"
        [ test "(de)registrations" BabStake.registrationTx
        , test "(de)registrations in same block" BabStake.registrationsSameBlock
        , test "(de)registrations in same tx" BabStake.registrationsSameTx
        , test "stake address pointers" BabStake.stakeAddressPtr
        , test "stake address pointers deregistration" BabStake.stakeAddressPtrDereg
        , test "stake address pointers. Use before registering." BabStake.stakeAddressPtrUseBefore
        ]
    , testGroup
        "stake distribution"
        [ test "stake distribution from genesis" BabStake.stakeDistGenesis
        , test "2000 delegations" BabStake.delegations2000
        , test "2001 delegations" BabStake.delegations2001
        , test "8000 delegations" BabStake.delegations8000
        , test "many delegations" BabStake.delegationsMany
        , test "many delegations, sparse chain" BabStake.delegationsManyNotDense
        ]
    , testGroup
        "rewards"
        [ test "rewards simple" BabReward.simpleRewards
        , test "shelley rewards from multiple sources" BabReward.rewardsShelley
        , test "rewards with deregistration" BabReward.rewardsDeregistration
        , test "rewards with reregistration. Fixed in Babbage." BabReward.rewardsReregistration
        , test "Mir Cert" BabReward.mirReward
        , -- , test "Mir rollback" mirRewardRollback
          test "Mir Cert Shelley" BabReward.mirRewardShelley
        , test "Mir Cert deregistration" BabReward.mirRewardDereg
        , -- , test "test rewards empty last part of epoch" rewardsEmptyChainLast
          -- , test "test delta rewards" rewardsDelta -- We disable the test. See in the test for more.
          test "rollback on epoch boundary" BabReward.rollbackBoundary
        , test "single MIR Cert multiple outputs" BabReward.singleMIRCertMultiOut
        ]
    , testGroup
        "plutus spend scripts"
        [ test "simple script lock" BabPlutus.simpleScript
        , test "unlock script in same block" BabPlutus.unlockScriptSameBlock
        , test "failed script" BabPlutus.failedScript
        , test "failed script fees" BabPlutus.failedScriptFees
        , test "failed script in same block" BabPlutus.failedScriptSameBlock
        , test "multiple scripts unlocked" BabPlutus.multipleScripts
        , test "multiple scripts unlocked rollback" BabPlutus.multipleScriptsRollback
        , test "multiple scripts unlocked same block" BabPlutus.multipleScriptsSameBlock
        , test "multiple scripts failed" BabPlutus.multipleScriptsFailed
        , test "multiple scripts failed same block" BabPlutus.multipleScriptsFailedSameBlock
        ]
    , testGroup
        "plutus cert scripts"
        [ test "stake scripts" BabPlutus.registrationScriptTx
        , test "stake scripts deregistration" BabPlutus.deregistrationScriptTx
        , test "multiple stake scripts deregistration" BabPlutus.deregistrationsScriptTxs
        , test "multiple stake scripts deregistration in same tx" BabPlutus.deregistrationsScriptTx
        , test "multiple stake scripts deregistration in same tx missing redeemer 1" BabPlutus.deregistrationsScriptTx'
        , test "multiple stake scripts deregistration in same tx missing redeemer 2" BabPlutus.deregistrationsScriptTx''
        ]
    , testGroup
        "MultiAssets plutus scripts"
        [ test "mint simple multi asset" BabPlutus.mintMultiAsset
        , test "mint many multi assets" BabPlutus.mintMultiAssets
        , test "swap many multi assets" BabPlutus.swapMultiAssets
        ]
    , testGroup
        "pools and smash"
        [ test "pool registration" BabOther.poolReg
        , test "query pool that's not registered" BabOther.nonexistantPoolQuery
        , test "pool deregistration" BabOther.poolDeReg
        , test "pool multiple deregistration" BabOther.poolDeRegMany
        , test "delist pool" BabOther.poolDelist
        ]
    , testGroup
        "Babbage inline and reference"
        [ test "spend inline datum" BabInlineRef.unlockDatumOutput
        , test "spend inline datum same block" BabInlineRef.unlockDatumOutputSameBlock
        , test "inline datum with non canonical CBOR" BabInlineRef.inlineDatumCBOR
        , test "spend reference script" BabInlineRef.spendRefScript
        , test "spend reference script same block" BabInlineRef.spendRefScriptSameBlock
        , test "spend collateral output of invalid tx" BabInlineRef.spendCollateralOutput
        , test "spend collateral output of invalid tx rollback" BabInlineRef.spendCollateralOutputRollback
        , test "spend collateral output of invalid tx same block" BabInlineRef.spendCollateralOutputSameBlock
        , test "reference input to output which is not spent" BabInlineRef.referenceInputUnspend
        , test "supply and run script which is both reference and in witnesses" BabInlineRef.supplyScriptsTwoWays
        , test "supply and run script which is both reference and in witnesses same block" BabInlineRef.supplyScriptsTwoWaysSameBlock
        , test "reference script as minting" BabInlineRef.referenceMintingScript
        , test "reference script as delegation" BabInlineRef.referenceDelegation
        ]
    , testGroup
        "Hard Fork"
        [ test "fork from Alonzo to Babbage fixed epoch" BabOther.forkFixedEpoch
        , test "fork from Alonzo to Babbage and rollback" BabOther.rollbackFork
        --          TODO fix this test.
        --          , test "fork from Alonzo to Babbage using proposal" forkWithProposal
        ]
    ]
  where
    test :: String -> (IOManager -> [(Text, Text)] -> Assertion) -> TestTree
    test str action = testCase str (action iom knownMigrations)
