{-# LANGUAGE NumericUnderscores #-}

module Test.Cardano.Db.Mock.Unit.Conway.CommandLineArg.ConfigFile (
  checkConfigFileArg,
) where

import Cardano.Mock.ChainSync.Server (IOManager ())
import Cardano.Prelude
import Test.Cardano.Db.Mock.Config
import Test.Cardano.Db.Mock.Validate (checkStillRuns)
import Test.Tasty.HUnit (Assertion ())
import Prelude ()

checkConfigFileArg :: IOManager -> [(Text, Text)] -> Assertion
checkConfigFileArg =
  withCustomConfig cliArgs conwayConfigDir testLabel $ \_ _ dbSync -> do
    startDBSync dbSync
    -- There is a slight delay before the flag is checked
    threadDelay 2_000_000

    -- Expected to fail
    checkStillRuns dbSync
  where
    cliArgs = initCommandLineArgs {claHasConfigFile = False} -- Invalid
    testLabel = "conwayCLACheckConfigFileArg"
