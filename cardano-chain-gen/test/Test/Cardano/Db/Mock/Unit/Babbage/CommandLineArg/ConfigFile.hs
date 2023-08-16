module Test.Cardano.Db.Mock.Unit.Babbage.CommandLineArg.ConfigFile (
  checkConfigFileArg,
)
where

import Cardano.Mock.ChainSync.Server (IOManager)
import Data.Text (Text)
import Test.Cardano.Db.Mock.Config (CommandLineArgs (..), babbageConfigDir, initCommandLineArgs, withCustomConfig)
import Test.Cardano.Db.Mock.Validate (checkStillRuns)
import Test.Tasty.HUnit (Assertion)

-- this test fails as incorrect configuration file given
checkConfigFileArg :: IOManager -> [(Text, Text)] -> Assertion
checkConfigFileArg =
  withCustomConfig commandLineConfigArgs babbageConfigDir testLabel $ \_ _ dbSyncEnv -> do
    -- poll dbSync to see if it's running, which it shouldn't
    checkStillRuns dbSyncEnv
  where
    testLabel = "CLAcheckConfigFileArg"
    commandLineConfigArgs = initCommandLineArgs {claHasConfigFile = False}
