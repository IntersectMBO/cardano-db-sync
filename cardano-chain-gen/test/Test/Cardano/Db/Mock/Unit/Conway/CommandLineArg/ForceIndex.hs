module Test.Cardano.Db.Mock.Unit.Conway.CommandLineArg.ForceIndex (
  checkForceIndexesArg,
  checkNoForceIndexesArg,
) where

import qualified Cardano.Db as DB
import Cardano.Mock.ChainSync.Server (IOManager ())
import Cardano.Prelude
import Test.Cardano.Db.Mock.Config
import Test.Cardano.Db.Mock.Validate (assertEqQuery)
import Test.Tasty.HUnit (Assertion ())
import Prelude ()

checkForceIndexesArg :: IOManager -> [(Text, Text)] -> Assertion
checkForceIndexesArg =
  withCustomConfig cliArgs Nothing conwayConfigDir testLabel $ \_ _ dbSync -> do
    startDBSync dbSync

    -- Verify number of DB indexes
    assertEqQuery dbSync DB.queryPgIndexesCount 169 "unexpected number of indexes"
  where
    cliArgs = initCommandLineArgs {claForceIndexes = True}
    testLabel = "conwayCLACheckForceIndexesArg"

checkNoForceIndexesArg :: IOManager -> [(Text, Text)] -> Assertion
checkNoForceIndexesArg =
  withCustomConfigAndDropDB cliArgs Nothing conwayConfigDir testLabel $ \_ _ dbSync -> do
    startDBSync dbSync

    -- Verify number of DB indexes
    assertEqQuery dbSync DB.queryPgIndexesCount 104 "unexpected number of indexes"
  where
    cliArgs = initCommandLineArgs {claForceIndexes = False}
    testLabel = "conwayCLACheckNoForceIndexesArg"
