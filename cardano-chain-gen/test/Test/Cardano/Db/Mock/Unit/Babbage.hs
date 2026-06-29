{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Cardano.Db.Mock.Unit.Babbage (
  unitTests,
) where

import qualified Cardano.Db as DB
import Cardano.Mock.ChainSync.Server (IOManager)
import Data.Text (Text)
import Test.Tasty (DependencyType (..), TestTree, dependentTestGroup)
import Test.Tasty.HUnit (Assertion, testCase)

import qualified Test.Cardano.Db.Mock.Unit.Babbage.Reward as BabReward
import qualified Test.Cardano.Db.Mock.Unit.Babbage.Simple as BabSimple
import qualified Test.Cardano.Db.Mock.Unit.Babbage.Tx as BabTx

unitTests :: IOManager -> [(Text, Text)] -> DB.PGPassSource -> TestTree
unitTests iom knownMigrations source =
  dependentTestGroup
    "Babbage unit tests"
    AllFinish
    [ dependentTestGroup
        "simple"
        AllFinish
        [ test "simple forge blocks" BabSimple.forgeBlocks
        , test "sync one block" BabSimple.addSimple
        , test "sync small chain" BabSimple.addSimpleChain
        , test "restart db-sync" BabSimple.restartDBSync
        , test "node restart" BabSimple.nodeRestart
        , test "node restart boundary" BabSimple.nodeRestartBoundary
        ]
    , dependentTestGroup
        "blocks with txs"
        AllFinish
        [ test "simple tx" BabTx.addSimpleTx
        , test "simple tx in Shelley era" BabTx.addSimpleTxShelley
        , test "consume utxo same block" BabTx.consumeSameBlock
        ]
    , dependentTestGroup
        "rewards"
        AllFinish
        [ -- test "rewards simple" BabReward.simpleRewards TODO: possible upstream changed in shelley genesis
          test "rewards with deregistration" BabReward.rewardsDeregistration
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
    ]
  where
    test :: String -> (DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion) -> TestTree
    test str action = testCase str (action source iom knownMigrations)
