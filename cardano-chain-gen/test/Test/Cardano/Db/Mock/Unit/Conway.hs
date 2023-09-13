module Test.Cardano.Db.Mock.Unit.Conway (unitTests) where

import Cardano.Mock.ChainSync.Server (IOManager ())
import Cardano.Prelude
import qualified Test.Cardano.Db.Mock.Unit.Conway.Config as ConConfig
import Test.Tasty (TestTree (), testGroup)
import Test.Tasty.HUnit (testCase)
import Prelude ()

unitTests :: IOManager -> [(Text, Text)] -> TestTree
unitTests _ _ =
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
    ]
