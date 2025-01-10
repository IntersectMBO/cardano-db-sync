{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Cardano.Db.Mock.Unit.Babbage (
  unitTests,
) where

import Cardano.Mock.ChainSync.Server (IOManager)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)

import qualified Test.Cardano.Db.Mock.Unit.Babbage.Reward as BabReward
import qualified Test.Cardano.Db.Mock.Unit.Babbage.Simple as BabSimple
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
        "blocks with txs"
        [ test "simple tx" BabTx.addSimpleTx
        , test "simple tx in Shelley era" BabTx.addSimpleTxShelley
        , test "consume utxo same block" BabTx.consumeSameBlock
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
    ]
  where
    test :: String -> (IOManager -> [(Text, Text)] -> Assertion) -> TestTree
    test str action = testCase str (action iom knownMigrations)
