{-# LANGUAGE NumericUnderscores #-}

module Test.Cardano.Db.Mock.Unit.Babbage.CommandLineArg.ForceIndex (
  checkForceIndexesArg,
  checkNoForceIndexesArg
)
where

import Cardano.Mock.ChainSync.Server (IOManager)
import Data.Text (Text)
import Test.Cardano.Db.Mock.Config (CommandLineArgs (..), babbageConfigDir, initCommandLineArgs, withCustomConfig, startDBSync)
import Test.Tasty.HUnit (Assertion)
import qualified Cardano.Db as DB
import Test.Cardano.Db.Mock.Validate (assertEqQuery)
import GHC.Conc.IO (threadDelay)

checkForceIndexesArg :: IOManager -> [(Text, Text)] -> Assertion
checkForceIndexesArg =
  withCustomConfig commandLineForceIndexArgs babbageConfigDir testLabel $ \_ _ dbSyncEnv -> do
    startDBSync dbSyncEnv
    -- assertBlockNoBackoff dbSyncEnv 0
    threadDelay 3_000_000
    assertEqQuery dbSyncEnv DB.queryPgIndexesCount 143 "there wasn't the correct number of indexes"
  where
    testLabel = "CLAcheckForceIndexesArg"
    commandLineForceIndexArgs = initCommandLineArgs {claForceIndexes = True}

checkNoForceIndexesArg :: IOManager -> [(Text, Text)] -> Assertion
checkNoForceIndexesArg =
  withCustomConfig commandLineNoForceIndexArgs babbageConfigDir testLabel $ \_ _ dbSyncEnv -> do
    startDBSync dbSyncEnv
    -- assertBlockNoBackoff dbSyncEnv 0
    threadDelay 3_000_000
    assertEqQuery dbSyncEnv DB.queryPgIndexesCount 78 "there wasn't the correct number of indexes"
  where
    testLabel = "CLAcheckNoForceIndexesArg"
    commandLineNoForceIndexArgs = initCommandLineArgs {claForceIndexes = False}
