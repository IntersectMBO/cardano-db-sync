{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Db.Mock.Unit.Alonzo (
  unitTests,
) where

import qualified Cardano.Db as DB
import Cardano.Mock.ChainSync.Server (IOManager)
import Data.Text (Text)
import Test.Tasty (DependencyType (..), TestTree, dependentTestGroup)
import Test.Tasty.HUnit (Assertion, testCase)

import qualified Test.Cardano.Db.Mock.Unit.Alonzo.Config as AlzConfig
import qualified Test.Cardano.Db.Mock.Unit.Alonzo.Simple as AlzSimple
import qualified Test.Cardano.Db.Mock.Unit.Alonzo.Tx as AlzTx

{- HLINT ignore "Reduce duplication" -}

unitTests :: IOManager -> [(Text, Text)] -> DB.PGPassSource -> TestTree
unitTests iom knownMigrations source =
  dependentTestGroup
    "Alonzo unit tests"
    AllFinish
    [ dependentTestGroup
        "config"
        AllFinish
        [ testCase "default insert config" (AlzConfig.defaultInsertConfig source)
        , testCase "insert config" (AlzConfig.insertConfig source)
        ]
    , dependentTestGroup
        "simple"
        AllFinish
        [ test "simple forge blocks" AlzSimple.forgeBlocks
        , test "sync one block" AlzSimple.addSimple
        , test "restart db-sync" AlzSimple.restartDBSync
        , test "sync small chain" AlzSimple.addSimpleChain
        ]
    , dependentTestGroup
        "blocks with txs"
        AllFinish
        [ test "simple tx" AlzTx.addSimpleTx
        , test "consume utxo same block" AlzTx.consumeSameBlock
        ]
    ]
  where
    test :: String -> (DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion) -> TestTree
    test str action = testCase str (action source iom knownMigrations)
