{-# LANGUAGE NumericUnderscores #-}

module Test.Cardano.Db.Mock.Unit.Conway.CommandLineArg.ConfigFile (
  checkConfigFileArg,
) where

import qualified Cardano.Db as DB
import Cardano.Mock.ChainSync.Server (IOManager ())
import Cardano.Prelude
import Test.Cardano.Db.Mock.Config
import Test.Cardano.Db.Mock.Validate (checkStillRuns)
import Test.Tasty.HUnit (Assertion ())
import Prelude ()

checkConfigFileArg :: DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion
checkConfigFileArg source =
  withCustomConfig cliArgs Nothing source conwayConfigDir testLabel $ \_ _ dbSync -> do
    startDBSync dbSync
    -- There is a slight delay before the flag is checked
    threadDelay 2_000_000

    -- Expected to fail
    checkStillRuns dbSync
  where
    cliArgs = initCommandLineArgs {claConfigFilename = "does-not-exist"}
    testLabel = "conwayCLACheckConfigFileArg"
