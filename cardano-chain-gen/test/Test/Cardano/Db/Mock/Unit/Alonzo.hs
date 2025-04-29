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
import qualified Test.Cardano.Db.Mock.Unit.Alonzo.Simple as AlzSimple
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
    ]
  where
    test :: String -> (IOManager -> [(Text, Text)] -> Assertion) -> TestTree
    test str action = testCase str (action iom knownMigrations)
