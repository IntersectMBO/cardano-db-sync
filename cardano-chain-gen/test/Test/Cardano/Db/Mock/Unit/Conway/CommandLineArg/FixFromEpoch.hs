module Test.Cardano.Db.Mock.Unit.Conway.CommandLineArg.FixFromEpoch (
  fixFromEpochRunsOnce,
) where

import qualified Cardano.Db as DB
import Cardano.Mock.ChainSync.Server (IOManager ())
import Cardano.Prelude
import Test.Cardano.Db.Mock.Config
import qualified Test.Cardano.Db.Mock.UnifiedApi as Api
import Test.Cardano.Db.Mock.Validate (assertBlockNoBackoff, assertEqQuery)
import Test.Tasty.HUnit (Assertion ())
import Prelude ()

-- | @--fix-from-epoch@ overrides the completion marker, so the repair has to stop itself
-- after one sweep rather than restarting on every reconnect.
fixFromEpochRunsOnce :: IOManager -> [(Text, Text)] -> Assertion
fixFromEpochRunsOnce =
  withCustomConfig cliArgs Nothing conwayConfigDir testLabel $ \interpreter server dbSync -> do
    blks <- Api.forgeAndSubmitBlocks interpreter server 10
    startDBSync dbSync

    -- Reached only if the sweep handed over to syncing.
    assertBlockNoBackoff dbSync (length blks)

    assertEqQuery
      dbSync
      (length . filter (== DB.ProtoMinorFixed) <$> DB.queryAllExtraMigrations)
      1
      "Expected exactly one ProtoMinorFixed marker"

    blks' <- Api.forgeAndSubmitBlocks interpreter server 5
    assertBlockNoBackoff dbSync (length blks + length blks')
  where
    cliArgs = initCommandLineArgs {claFixFromEpoch = Just 0}
    testLabel = "conwayCLAFixFromEpoch"
