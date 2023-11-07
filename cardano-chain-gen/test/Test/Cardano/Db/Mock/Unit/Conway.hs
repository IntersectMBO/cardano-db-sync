module Test.Cardano.Db.Mock.Unit.Conway (unitTests) where

import Cardano.Mock.ChainSync.Server (IOManager ())
import Cardano.Prelude
import qualified Test.Cardano.Db.Mock.Unit.Conway.CommandLineArg.ConfigFile as ConfigFile
import qualified Test.Cardano.Db.Mock.Unit.Conway.CommandLineArg.EpochDisabled as EpochDisabled
import qualified Test.Cardano.Db.Mock.Unit.Conway.CommandLineArg.ForceIndex as ForceIndex
import qualified Test.Cardano.Db.Mock.Unit.Conway.CommandLineArg.MigrateConsumedPruneTxOut as MigrateConsumedPruneTxOut
import qualified Test.Cardano.Db.Mock.Unit.Conway.Config as ConConfig
import qualified Test.Cardano.Db.Mock.Unit.Conway.Simple as Simple
import Test.Tasty (TestTree (), testGroup)
import Test.Tasty.ExpectedFailure (expectFail)
import Test.Tasty.HUnit (Assertion (), testCase)
import Prelude (String ())

unitTests :: IOManager -> [(Text, Text)] -> TestTree
unitTests iom knownMigrations =
  testGroup
    "Conway unit tests"
    [ testGroup
        "config"
        [ testCase "conway genesis and hash" ConConfig.conwayGenesis
        , testCase "missing conway genesis file" ConConfig.missingConwayGenesis
        , testCase "no conway genesis file" ConConfig.noConwayGenesis
        , testCase "no conway genesis hash" ConConfig.noConwayGenesisHash
        , testCase "mismatched conway genesis hash" ConConfig.wrongConwayGenesisHash
        ]
    , testGroup
        "simple"
        [ test "simple forge blocks" Simple.forgeBlocks
        , test "sync one block" Simple.addSimple
        , test "sync small chain" Simple.addSimpleChain
        , test "restart db-sync" Simple.restartDBSync
        , test "node restart" Simple.nodeRestart
        , test "node restart boundary" Simple.nodeRestartBoundary
        ]
    , testGroup
        "Command Line Arguments"
        [ testGroup
            "consumed-tx-out and prune-tx-out"
            [ test "flag check" MigrateConsumedPruneTxOut.commandLineArgCheck
            , test "basic prune" MigrateConsumedPruneTxOut.basicPrune
            , test "prune with simple rollback" MigrateConsumedPruneTxOut.pruneWithSimpleRollback
            , test "prune with full tx rollback" MigrateConsumedPruneTxOut.pruneWithFullTxRollback
            , test "pruning should keep some tx" MigrateConsumedPruneTxOut.pruningShouldKeepSomeTx
            , test "prune and rollback one block" MigrateConsumedPruneTxOut.pruneAndRollBackOneBlock
            , test "no pruning and rollback" MigrateConsumedPruneTxOut.noPruneAndRollBack
            , test "prune same block" MigrateConsumedPruneTxOut.pruneSameBlock
            , test "no pruning same block" MigrateConsumedPruneTxOut.noPruneSameBlock
            , expectFail $
                test
                  "restart with new consumed set to false"
                  MigrateConsumedPruneTxOut.migrateAndPruneRestart
            , expectFail $
                test
                  "set prune flag, restart missing prune flag"
                  MigrateConsumedPruneTxOut.pruneRestartMissingFlag
            , expectFail $
                test
                  "set bootstrap flag, restart missing bootstrap flag"
                  MigrateConsumedPruneTxOut.bootstrapRestartMissingFlag
            ]
        , testGroup
            "config"
            [ expectFail $
                test
                  "fails if incorrect or no config file given"
                  ConfigFile.checkConfigFileArg
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
    ]
  where
    test :: String -> (IOManager -> [(Text, Text)] -> Assertion) -> TestTree
    test str action = testCase str (action iom knownMigrations)
