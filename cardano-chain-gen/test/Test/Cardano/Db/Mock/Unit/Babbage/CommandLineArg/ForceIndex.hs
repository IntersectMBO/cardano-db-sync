{-# LANGUAGE NumericUnderscores #-}

module Test.Cardano.Db.Mock.Unit.Babbage.CommandLineArg.ForceIndex (
  checkForceIndexesArg,
  checkNoForceIndexesArg,
)
where

import qualified Cardano.Db as DB
import Cardano.Mock.ChainSync.Server (IOManager)
import Data.Text (Text)
import GHC.Conc.IO (threadDelay)
import Test.Cardano.Db.Mock.Config (CommandLineArgs (..), babbageConfigDir, initCommandLineArgs, startDBSync, withCustomConfig)
import Test.Cardano.Db.Mock.Validate (assertEqQuery)
import Test.Tasty.HUnit (Assertion)

checkForceIndexesArg :: IOManager -> [(Text, Text)] -> Assertion
checkForceIndexesArg =
  withCustomConfig commandLineForceIndexArgs babbageConfigDir testLabel $ \_ _ dbSyncEnv -> do
    startDBSync dbSyncEnv
    threadDelay 3_000_000
    assertEqQuery dbSyncEnv DB.queryPgIndexesCount 160 "there wasn't the correct number of indexes"
  where
    testLabel = "CLAcheckForceIndexesArg"
    commandLineForceIndexArgs =
      initCommandLineArgs
        { claForceIndexes = True
        }

checkNoForceIndexesArg :: IOManager -> [(Text, Text)] -> Assertion
checkNoForceIndexesArg =
  withCustomConfig commandLineNoForceIndexArgs babbageConfigDir testLabel $ \_ _ dbSyncEnv -> do
    startDBSync dbSyncEnv
    threadDelay 3_000_000
    assertEqQuery dbSyncEnv DB.queryPgIndexesCount 95 "there wasn't the correct number of indexes"
  where
    testLabel = "CLAcheckNoForceIndexesArg"
    commandLineNoForceIndexArgs =
      initCommandLineArgs
        { claForceIndexes = False
        }
