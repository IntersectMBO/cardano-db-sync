module Test.Cardano.Db.Mock.Unit.Conway (unitTests) where

import Cardano.Mock.ChainSync.Server (IOManager ())
import Cardano.Prelude
import qualified Test.Cardano.Db.Mock.Unit.Conway.Config as ConConfig
import qualified Test.Cardano.Db.Mock.Unit.Conway.Simple as Simple
import Test.Tasty (TestTree (), testGroup)
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
    ]
  where
    test :: String -> (IOManager -> [(Text, Text)] -> Assertion) -> TestTree
    test str action = testCase str (action iom knownMigrations)
