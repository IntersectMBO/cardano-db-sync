{-# LANGUAGE NumericUnderscores #-}

module Test.Cardano.Db.Mock.Unit.Conway.CommandLineArg.ConfigFile (
  checkConfigFileArg,
  configResetJsonb,
  configNoResetJsonb,
) where

import qualified Cardano.Db as DB
import Cardano.Mock.ChainSync.Server (IOManager ())
import Cardano.Prelude
import Test.Cardano.Db.Mock.Config
import Test.Cardano.Db.Mock.Validate (assertEqQuery, checkStillRuns)
import Test.Tasty.HUnit (Assertion ())
import Prelude ()

checkConfigFileArg :: IOManager -> [(Text, Text)] -> Assertion
checkConfigFileArg =
  withCustomConfig cliArgs Nothing conwayConfigDir testLabel $ \_ _ dbSync -> do
    startDBSync dbSync
    -- There is a slight delay before the flag is checked
    threadDelay 2_000_000

    -- Expected to fail
    checkStillRuns dbSync
  where
    cliArgs = initCommandLineArgs {claConfigFilename = "does-not-exist"}
    testLabel = "conwayCLACheckConfigFileArg"

configResetJsonb :: IOManager -> [(Text, Text)] -> Assertion
configResetJsonb =
  withCustomConfigAndDropDB
    cmdLineArgs
    Nothing
    conwayConfigDir
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
    testLabel = "conwayCLAResetJsonb"

configNoResetJsonb :: IOManager -> [(Text, Text)] -> Assertion
configNoResetJsonb =
  withCustomConfigAndDropDB
    cmdLineArgs
    Nothing
    conwayConfigDir
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
    testLabel = "conwayCLANoResetJsonb"
