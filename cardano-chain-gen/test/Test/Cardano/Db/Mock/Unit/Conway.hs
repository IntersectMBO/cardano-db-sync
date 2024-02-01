module Test.Cardano.Db.Mock.Unit.Conway (unitTests) where

import Cardano.Mock.ChainSync.Server (IOManager ())
import Cardano.Prelude
import qualified Test.Cardano.Db.Mock.Unit.Conway.CommandLineArg.ConfigFile as ConfigFile
import qualified Test.Cardano.Db.Mock.Unit.Conway.CommandLineArg.EpochDisabled as EpochDisabled
import qualified Test.Cardano.Db.Mock.Unit.Conway.CommandLineArg.ForceIndex as ForceIndex
import qualified Test.Cardano.Db.Mock.Unit.Conway.Config.MigrateConsumedPruneTxOut as MigrateConsumedPruneTxOut
import qualified Test.Cardano.Db.Mock.Unit.Conway.Config.Parse as Config
import qualified Test.Cardano.Db.Mock.Unit.Conway.InlineAndReference as InlineRef
import qualified Test.Cardano.Db.Mock.Unit.Conway.Other as Other
import qualified Test.Cardano.Db.Mock.Unit.Conway.Plutus as Plutus
import qualified Test.Cardano.Db.Mock.Unit.Conway.Reward as Reward
import qualified Test.Cardano.Db.Mock.Unit.Conway.Rollback as Rollback
import qualified Test.Cardano.Db.Mock.Unit.Conway.Simple as Simple
import qualified Test.Cardano.Db.Mock.Unit.Conway.Stake as Stake
import qualified Test.Cardano.Db.Mock.Unit.Conway.Tx as Tx
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
        [ testCase "conway genesis and hash" Config.conwayGenesis
        , testCase "missing conway genesis file" Config.missingConwayGenesis
        , testCase "no conway genesis file" Config.noConwayGenesis
        , testCase "no conway genesis hash" Config.noConwayGenesisHash
        , testCase "mismatched conway genesis hash" Config.wrongConwayGenesisHash
        , testCase "default insert config" Config.defaultInsertConfig
        , testCase "insert config" Config.insertConfig
        , testGroup
            "tx-out"
            [ test "consumed_by_tx_id column check" MigrateConsumedPruneTxOut.txConsumedColumnCheck
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
            "config"
            [ expectFail $
                test
                  "fails if incorrect config file given"
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
    , testGroup
        "rollbacks"
        [ test "simple rollback" Rollback.simpleRollback
        , test "sync bigger chain" Rollback.bigChain
        , test "rollback while db-sync is off" Rollback.restartAndRollback
        , test "big rollback executed lazily" Rollback.lazyRollback
        , test "lazy rollback on restart" Rollback.lazyRollbackRestart
        , test "rollback while rollbacking" Rollback.doubleRollback
        , test "rollback stake address cache" Rollback.stakeAddressRollback
        , test "rollback change order of txs" Rollback.rollbackChangeTxOrder
        , test "rollback full tx" Rollback.rollbackFullTx
        ]
    , testGroup
        "different configs"
        [ test "genesis config without pool" Other.configNoPools
        , test "genesis config without stakes" Other.configNoStakes
        ]
    , testGroup
        "blocks with txs"
        [ test "simple tx" Tx.addSimpleTx
        , test "simple tx in Shelley era" Tx.addSimpleTxShelley
        , test "simple tx with ledger disabled" Tx.addSimpleTxNoLedger
        , test "consume utxo same block" Tx.consumeSameBlock
        , test "tx with metadata" Tx.addTxMetadata
        , test "tx with metadata disabled" Tx.addTxMetadataDisabled
        , test "tx with metadata whitelist" Tx.addTxMetadataWhitelist
        ]
    , testGroup
        "stake addresses"
        [ test "(de)registrations" Stake.registrationTx
        , test "(de)registrations in same block" Stake.registrationsSameBlock
        , test "(de)registrations in same tx" Stake.registrationsSameTx
        , test "stake address pointers" Stake.stakeAddressPtr
        , test "stake address pointers deregistration" Stake.stakeAddressPtrDereg
        , test "stake address pointers. Use before registering." Stake.stakeAddressPtrUseBefore
        , test "register stake creds" Stake.registerStakeCreds
        , test "register stake creds with shelley disabled" Stake.registerStakeCredsNoShelley
        ]
    , testGroup
        "stake distribution"
        [ test "stake distribution from genesis" Stake.stakeDistGenesis
        , test "2000 delegations" Stake.delegations2000
        , test "2001 delegations" Stake.delegations2001
        , test "8000 delegations" Stake.delegations8000
        , test "many delegations" Stake.delegationsMany
        , test "many delegations, sparse chain" Stake.delegationsManyNotDense
        ]
    , testGroup
        "rewards"
        [ test "rewards simple" Reward.simpleRewards
        , test "shelley rewards from multiple sources" Reward.rewardsShelley
        , test "rollback on epoch boundary" Reward.rollbackBoundary
        ]
    , testGroup
        "plutus send scripts"
        [ test "simple script lock" Plutus.simpleScript
        , test "unlock script in same block" Plutus.unlockScriptSameBlock
        , test "unlock script with plutus disabled" Plutus.unlockScriptNoPlutus
        , test "failed script" Plutus.failedScript
        , test "failed script fees" Plutus.failedScriptFees
        , test "failed script in same block" Plutus.failedScriptSameBlock
        , test "multiple scripts unlocked" Plutus.multipleScripts
        , test "multiple scripts unlocked rollback" Plutus.multipleScriptsRollback
        , test "multiple scripts unlocked same block" Plutus.multipleScriptsSameBlock
        , test "multiple scripts failed" Plutus.multipleScriptsFailed
        , test "multiple scripts failed same block" Plutus.multipleScriptsFailedSameBlock
        ]
    , testGroup
        "plutus cert scripts"
        [ test "stake scripts" Plutus.registrationScriptTx
        , test "stake scripts deregistration" Plutus.deregistrationScriptTx
        , test "multiple stake scripts deregistration" Plutus.deregistrationsScriptTxs
        , test "multiple stake scripts in same tx" Plutus.deregistrationScriptTx
        , test
            "multiple stake scripts deregistration in same tx missing redeemer 1"
            Plutus.deregistrationsScriptTx'
        , test
            "multiple stake scripts deregistration in same tx missing redeemer 2"
            Plutus.deregistrationsScriptTx''
        ]
    , testGroup
        "MultiAssets plutus scripts"
        [ test "mint simple multi asset" Plutus.mintMultiAsset
        , test "mint many multi assets" Plutus.mintMultiAssets
        , test "swap many multi assets" Plutus.swapMultiAssets
        , test "swap with multi assets disabled" Plutus.swapMultiAssetsDisabled
        ]
    , testGroup
        "Pools and smash"
        [ test "pool registration" Other.poolReg
        , test "query pool that's not registered" Other.nonexistentPoolQuery
        , test "pool deregistration" Other.poolDeReg
        , test "multiple deregistration" Other.poolDeRegMany
        , test "delist pool" Other.poolDelist
        ]
    , testGroup
        "Inline and reference"
        [ test "spend inline datum" InlineRef.unlockDatumOutput
        , test "spend inline datum same block" InlineRef.unlockDatumOutputSameBlock
        , test "inline datum with noncanonical CBOR" InlineRef.inlineDatumCBOR
        , test "spend reference script" InlineRef.spendRefScript
        , test "spend reference script same block" InlineRef.spendRefScriptSameBlock
        , test "spend collateral output of invalid tx" InlineRef.spendCollateralOutput
        , test
            "spend collateral output of invalid tx rollback"
            InlineRef.spendCollateralOutputRollback
        , test
            "spend collateral output of invalid tx same block"
            InlineRef.spendCollateralOutputSameBlock
        , test
            "reference input to output which is not spent"
            InlineRef.referenceInputUnspend
        , test
            "supply and run script which is both reference and in witnesses"
            InlineRef.supplyScriptsTwoWays
        , test
            "supply and run script which is both reference and in witnesses same block"
            InlineRef.supplyScriptsTwoWaysSameBlock
        , test "reference script as minting" InlineRef.referenceMintingScript
        , test "reference script as delegation" InlineRef.referenceDelegation
        ]
    , testGroup
        "Hard Fork"
        [ test "fork from Babbage to Conway fixed epoch" Other.forkFixedEpoch
        , test "fork from Babbage to Conway and rollback" Other.rollbackFork
        , test "fork with protocol change proposal" Other.forkParam
        ]
    ]
  where
    test :: String -> (IOManager -> [(Text, Text)] -> Assertion) -> TestTree
    test str action = testCase str (action iom knownMigrations)
