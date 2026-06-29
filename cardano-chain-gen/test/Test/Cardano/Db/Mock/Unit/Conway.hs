module Test.Cardano.Db.Mock.Unit.Conway (unitTests) where

import qualified Cardano.Db as DB
import Cardano.Mock.ChainSync.Server (IOManager ())
import Cardano.Prelude
import qualified Test.Cardano.Db.Mock.Unit.Conway.CommandLineArg.ConfigFile as ConfigFile
import qualified Test.Cardano.Db.Mock.Unit.Conway.Config.JsonbInSchema as Config
import qualified Test.Cardano.Db.Mock.Unit.Conway.Config.MigrateConsumedPruneTxOut as MigrateConsumedPruneTxOut
import qualified Test.Cardano.Db.Mock.Unit.Conway.Config.Parse as Config
import qualified Test.Cardano.Db.Mock.Unit.Conway.Config.Schema as Schema
import qualified Test.Cardano.Db.Mock.Unit.Conway.Epoch as Epoch
import qualified Test.Cardano.Db.Mock.Unit.Conway.Governance as Governance
import qualified Test.Cardano.Db.Mock.Unit.Conway.InlineAndReference as InlineRef
import qualified Test.Cardano.Db.Mock.Unit.Conway.Other as Other
import qualified Test.Cardano.Db.Mock.Unit.Conway.Plutus as Plutus
import qualified Test.Cardano.Db.Mock.Unit.Conway.Reward as Reward
import qualified Test.Cardano.Db.Mock.Unit.Conway.Rollback as Rollback
import qualified Test.Cardano.Db.Mock.Unit.Conway.Simple as Simple
import qualified Test.Cardano.Db.Mock.Unit.Conway.Stake as Stake
import qualified Test.Cardano.Db.Mock.Unit.Conway.Tx as Tx
import Test.Cardano.Db.Mock.Validate (expectFailSilent)
import Test.Tasty (DependencyType (..), TestTree (), dependentTestGroup)
import Test.Tasty.HUnit (Assertion (), testCase)
import Prelude (String ())

unitTests :: IOManager -> [(Text, Text)] -> DB.PGPassSource -> TestTree
unitTests iom knownMigrations source =
  dependentTestGroup
    "Conway unit tests"
    AllFinish
    [ dependentTestGroup
        "config"
        AllFinish
        [ testCase "conway genesis and hash" (Config.conwayGenesis source)
        , testCase "missing conway genesis file" (Config.missingConwayGenesis source)
        , testCase "no conway genesis hash" (Config.noConwayGenesisHash source)
        , testCase "mismatched conway genesis hash" (Config.wrongConwayGenesisHash source)
        , testCase "default insert config" (Config.defaultInsertConfig source)
        , testCase "insert config" (Config.insertConfig source)
        , dependentTestGroup
            "simple"
            AllFinish
            [ test "simple forge blocks" Simple.forgeBlocks
            , test "sync one block" Simple.addSimple
            , test "sync small chain" Simple.addSimpleChain
            , test "restart db-sync" Simple.restartDBSync
            , test "node restart" Simple.nodeRestart
            , test "node restart boundary" Simple.nodeRestartBoundary
            ]
        , dependentTestGroup
            "jsonb-in-schema"
            AllFinish
            [ test "jsonb in schema true" Config.configRemoveJsonbFromSchemaEnabled
            , test "jsonb in schema false" Config.configRemoveJsonbFromSchemaDisabled
            , test
                "remove jsonb from schema and add back"
                Config.configJsonbInSchemaShouldRemoveThenAdd
            ]
        , dependentTestGroup
            "Schema"
            AllFinish
            [ test "validate schema table columns" Schema.validateSchemaColumns
            , test "validate schema table columns address variant" Schema.validateVariantAddressSchemaColumns
            ]
        , dependentTestGroup
            "tx-out"
            AllFinish
            [ test "basic prune" MigrateConsumedPruneTxOut.basicPrune
            , test "prune with simple rollback" MigrateConsumedPruneTxOut.pruneWithSimpleRollback
            , test "prune with full tx rollback" MigrateConsumedPruneTxOut.pruneWithFullTxRollback
            , test "pruning should keep some tx" MigrateConsumedPruneTxOut.pruningShouldKeepSomeTx
            , test "prune and rollback one block" MigrateConsumedPruneTxOut.pruneAndRollBackOneBlock
            , test "no pruning and rollback" MigrateConsumedPruneTxOut.noPruneAndRollBack
            , test "prune same block" MigrateConsumedPruneTxOut.pruneSameBlock
            , test "no pruning same block" MigrateConsumedPruneTxOut.noPruneSameBlock
            , expectFailSilent
                "restart with new consumed set to false"
                $ MigrateConsumedPruneTxOut.migrateAndPruneRestart source iom knownMigrations
            , expectFailSilent
                "set prune flag, restart missing prune flag"
                $ MigrateConsumedPruneTxOut.pruneRestartMissingFlag source iom knownMigrations
            , expectFailSilent
                "set bootstrap flag, restart missing bootstrap flag"
                $ MigrateConsumedPruneTxOut.bootstrapRestartMissingFlag source iom knownMigrations
            ]
        , dependentTestGroup
            "tx-out with use_address_table config"
            AllFinish
            [ test "basic prune, with use_address_table config" MigrateConsumedPruneTxOut.basicPruneWithAddress
            , test "prune with simple rollback, with use_address_table config" MigrateConsumedPruneTxOut.pruneWithSimpleRollbackWithAddress
            , test "prune with full tx rollback, with use_address_table config" MigrateConsumedPruneTxOut.pruneWithFullTxRollbackWithAddress
            , test "pruning should keep some tx, with use_address_table config" MigrateConsumedPruneTxOut.pruningShouldKeepSomeTxWithAddress
            , test "prune and rollback one block, with use_address_table config" MigrateConsumedPruneTxOut.pruneAndRollBackOneBlockWithAddress
            , test "no pruning and rollback, with use_address_table config" MigrateConsumedPruneTxOut.noPruneAndRollBackWithAddress
            , test "prune same block, with use_address_table config" MigrateConsumedPruneTxOut.pruneSameBlockWithAddress
            , test "no pruning same block, with use_address_table config" MigrateConsumedPruneTxOut.noPruneSameBlockWithAddress
            , expectFailSilent
                "restart with new consumed set to false, with use_address_table config"
                $ MigrateConsumedPruneTxOut.migrateAndPruneRestartWithAddress source iom knownMigrations
            , expectFailSilent
                "set prune flag, restart missing prune flag, with use_address_table config"
                $ MigrateConsumedPruneTxOut.pruneRestartMissingFlagWithAddress source iom knownMigrations
            , expectFailSilent
                "set bootstrap flag, restart missing bootstrap flag, with use_address_table config"
                $ MigrateConsumedPruneTxOut.bootstrapRestartMissingFlagWithAddress source iom knownMigrations
            , expectFailSilent
                "populate db then reset with use_address_table config config active"
                $ MigrateConsumedPruneTxOut.populateDbRestartWithAddressConfig source iom knownMigrations
            ]
        ]
    , dependentTestGroup
        "Command Line Arguments"
        AllFinish
        [ dependentTestGroup
            "config"
            AllFinish
            [ expectFailSilent
                "fails if incorrect config file given"
                $ ConfigFile.checkConfigFileArg source iom knownMigrations
            ]
        ]
    , dependentTestGroup
        "Epoch"
        AllFinish
        [ test "Epoch view is empty when disabled" Epoch.checkEpochDisabledArg
        , test "Epoch view is populated when enabled" Epoch.checkEpochEnabled
        , test "Epoch current view updates live within an epoch" Epoch.checkEpochCurrentLiveUpdates
        , test "Epoch rollback refreshes epoch_finalized" Epoch.checkEpochRollbackStaleFinalized
        , test "Epoch rollback to first block of epoch" Epoch.checkEpochRollbackToFirstOfEpoch
        , test "Epoch rollback to last block of epoch" Epoch.checkEpochRollbackToLastOfEpoch
        ]
    , dependentTestGroup
        "rollbacks"
        AllFinish
        [ test "simple rollback" Rollback.simpleRollback
        , test "drepDistr rollback" Rollback.drepDistrRollback
        , test "sync bigger chain" Rollback.bigChain
        , test "rollback while db-sync is off" Rollback.restartAndRollback
        , test "large rollback while db-sync is off" Rollback.restartAndRollbackLarge
        , test "big rollback executed lazily" Rollback.lazyRollback
        , test "lazy rollback on restart" Rollback.lazyRollbackRestart
        , test "rollback while rollbacking" Rollback.doubleRollback
        , test "rollback stake address cache" Rollback.stakeAddressRollback
        , test "rollback change order of txs" Rollback.rollbackChangeTxOrder
        , test "rollback full tx" Rollback.rollbackFullTx
        , test "basic pool stats functionality" Rollback.poolStatBasicTest
        , test "pool stat rollback no duplicates" Rollback.poolStatRollbackNoDuplicates
        , test "pool stat rollback general" Rollback.poolStatRollbackGeneral
        , test "ada pots max supply and rollback" Rollback.adaPots
        ]
    , dependentTestGroup
        "different configs"
        AllFinish
        [ test "genesis config without pool" Other.configNoPools
        , test "genesis config without stakes" Other.configNoStakes
        ]
    , dependentTestGroup
        "blocks with txs"
        AllFinish
        [ test "simple tx" Tx.addSimpleTx
        , test "simple tx in Shelley era" Tx.addSimpleTxShelley
        , test "simple tx with ledger disabled" Tx.addSimpleTxNoLedger
        , test "tx with treasury donation" Tx.addTxTreasuryDonation
        , test "consume utxo same block" Tx.consumeSameBlock
        , test "block size reflects block body" Tx.blockSizeReflectsBlockBody
        , test "tx with metadata" Tx.addTxMetadata
        , test "tx with metadata disabled" Tx.addTxMetadataDisabled
        , test "tx with metadata whitelist" Tx.addTxMetadataWhitelist
        ]
    , dependentTestGroup
        "stake addresses"
        AllFinish
        [ test "(de)registrations" Stake.registrationTx
        , test "(de)registrations in same block" Stake.registrationsSameBlock
        , test "(de)registrations in same tx" Stake.registrationsSameTx
        , test "stake address pointers" Stake.stakeAddressPtr
        , test "stake address pointers deregistration" Stake.stakeAddressPtrDereg
        , test "stake address pointers. Use before registering." Stake.stakeAddressPtrUseBefore
        , test "stake address pointers have NULL stake_address_id in Conway" Stake.stakeAddressPtrNullInConway
        , test "register stake creds" Stake.registerStakeCreds
        , test "register stake creds with shelley disabled" Stake.registerStakeCredsNoShelley
        ]
    , dependentTestGroup
        "stake distribution"
        AllFinish
        [ test "stake distribution from genesis" Stake.stakeDistGenesis
        , test "2000 delegations" Stake.delegations2000
        , test "2001 delegations" Stake.delegations2001
        , test "8000 delegations" Stake.delegations8000
        , test "many delegations" Stake.delegationsMany
        , test "many delegations, sparse chain" Stake.delegationsManyNotDense
        ]
    , dependentTestGroup
        "rewards"
        AllFinish
        [ test "rewards simple" Reward.simpleRewards
        , test "shelley rewards from multiple sources" Reward.rewardsShelley
        , test "rollback on epoch boundary" Reward.rollbackBoundary
        ]
    , dependentTestGroup
        "plutus send scripts"
        AllFinish
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
    , -- TODO: re-enable after fixing credential format for cardano-node 10.7.1
      -- PraosCredentialsSource vs NodeOperationalCertificate mismatch
      -- dependentTestGroup
      --   "plutus cert scripts"
      --   [ test "stake scripts" Plutus.registrationScriptTx
      --   , test "stake scripts deregistration" Plutus.deregistrationScriptTx
      --   , test "multiple stake scripts deregistration" Plutus.deregistrationsScriptTxs
      --   , test "multiple stake scripts in same tx" Plutus.deregistrationScriptTx
      --   , test
      --       "multiple stake scripts deregistration in same tx missing redeemer 1"
      --       Plutus.deregistrationsScriptTx'
      --   , test
      --       "multiple stake scripts deregistration in same tx missing redeemer 2"
      --       Plutus.deregistrationsScriptTx''
      --   ]
      -- , dependentTestGroup
      --   "MultiAssets plutus scripts"
      --   [ test "mint simple multi asset" Plutus.mintMultiAsset
      --   , test "mint many multi assets" Plutus.mintMultiAssets
      --   , test "swap many multi assets" Plutus.swapMultiAssets
      --   , test "swap with multi assets disabled" Plutus.swapMultiAssetsDisabled
      --   ]
      -- , dependentTestGroup
      --   "Pools and smash"
      --   [ test "pool registration" Other.poolReg
      --   , test "query pool that's not registered" Other.nonexistentPoolQuery
      --   , test "pool deregistration" Other.poolDeReg
      --   , test "multiple deregistration" Other.poolDeRegMany
      --   , test "delist pool" Other.poolDelist
      --   ]
      dependentTestGroup
        "Inline and reference"
        AllFinish
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
    , dependentTestGroup
        "Hard Fork"
        AllFinish
        [ test "fork from Babbage to Conway fixed epoch" Other.forkFixedEpoch
        , test "fork from Babbage to Conway and rollback" Other.rollbackFork
        , test "fork with protocol change proposal" Other.forkParam
        ]
    , dependentTestGroup
        "Governance"
        AllFinish
        [ test "drep distribution" Governance.drepDistr
        , test "new committee member" Governance.newCommittee
        , test "chained committee proposals" Governance.chainedNewCommittee
        , test "rollback new committee member" Governance.rollbackNewCommittee
        , test "rollback new committee member proposal" Governance.rollbackNewCommitteeProposal
        , test "update constitution" Governance.updateConstitution
        , test "treasury withdrawal" Governance.treasuryWithdrawal
        , test "parameter change" Governance.parameterChange
        , test "hard fork" Governance.hardFork
        , test "hard fork post block" Governance.hardForkPostBlock
        , test "rollback hardfork" Governance.rollbackHardFork
        , test "info action" Governance.infoAction
        ]
    ]
  where
    test :: String -> (DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion) -> TestTree
    test str action = testCase str (action source iom knownMigrations)
