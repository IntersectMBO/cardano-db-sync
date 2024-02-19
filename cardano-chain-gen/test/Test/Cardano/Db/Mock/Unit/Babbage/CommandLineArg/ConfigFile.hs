module Test.Cardano.Db.Mock.Unit.Babbage.CommandLineArg.ConfigFile (
  checkConfigFileArg,
  configResetJsonb,
  configNoResetJsonb,
)
where

import qualified Cardano.Db as DB
import Cardano.Mock.ChainSync.Server (IOManager)
import Data.Text (Text)
import Test.Cardano.Db.Mock.Config (CommandLineArgs (..), babbageConfigDir, initCommandLineArgs, startDBSync, withCustomConfig, withCustomConfigAndDropDB)
import Test.Cardano.Db.Mock.Validate (assertEqQuery, checkStillRuns)
import Test.Tasty.HUnit (Assertion)

-- this test fails as incorrect configuration file given
checkConfigFileArg :: IOManager -> [(Text, Text)] -> Assertion
checkConfigFileArg =
  withCustomConfig commandLineConfigArgs Nothing babbageConfigDir testLabel $ \_ _ dbSyncEnv -> do
    -- poll dbSync to see if it's running, which it shouldn't
    checkStillRuns dbSyncEnv
  where
    testLabel = "CLAcheckConfigFileArg"
    commandLineConfigArgs = initCommandLineArgs {claHasConfigFile = False}

configResetJsonb :: IOManager -> [(Text, Text)] -> Assertion
configResetJsonb =
  withCustomConfigAndDropDB
    cmdLineArgs
    Nothing
    babbageConfigDir
    testLabel
    ( \_interpreter _mockServer dbSync -> do
        startDBSync dbSync
        assertEqQuery
          dbSync
          DB.queryJsonbTypeExists
          True
          "if the resetjsonb option has been used the types in the specific db collumns need to be jsonb"
    )
  where
    cmdLineArgs = initCommandLineArgs {claResetJsonb = True}
    testLabel = "babbageCLAResetJsonb"

configNoResetJsonb :: IOManager -> [(Text, Text)] -> Assertion
configNoResetJsonb =
  withCustomConfigAndDropDB
    cmdLineArgs
    Nothing
    babbageConfigDir
    testLabel
    ( \_interpreter _mockServer dbSync -> do
        startDBSync dbSync
        assertEqQuery
          dbSync
          DB.queryJsonbTypeExists
          False
          "There should be no jsonb types in database if option isn't active"
    )
  where
    cmdLineArgs = initCommandLineArgs {claResetJsonb = False}
    testLabel = "babbageCLANoResetJsonb"
