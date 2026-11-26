module Test.Cardano.Db.Mock.Unit.Conway.ProtoMinor (
  repairsTruncatedProtoMinor,
  forceFixBypassesMarker,
  onlyFixExits,
) where

import qualified Cardano.Db as DB
import Cardano.Mock.ChainSync.Server (IOManager ())
import Cardano.Prelude
import Test.Cardano.Db.Mock.Config
import qualified Test.Cardano.Db.Mock.UnifiedApi as Api
import Test.Cardano.Db.Mock.Validate (assertBlockNoBackoff, assertEqQuery)
import Test.Tasty.HUnit (Assertion (), assertFailure)
import Prelude ()

-- | The sweep only rewrites a block whose header minor is at or above @0x8000@, so the
-- forged headers have to carry a CIP-0203 tag before the stored values can be put back to
-- what the old narrow column held. The ledger is ignored because a restart without a
-- snapshot resumes from genesis, which would re-insert the blocks and hide the repair.
repairsTruncatedProtoMinor :: IOManager -> [(Text, Text)] -> Assertion
repairsTruncatedProtoMinor =
  withCustomConfig cliArgs (Just configLedgerIgnore) conwayConfigDir testLabel $
    \interpreter server dbSync -> do
      blks <- Api.forgeAndSubmitBlocks interpreter server 10
      startDBSync dbSync
      assertBlockNoBackoff dbSync (length blks)

      let forged = fromIntegral $ length blks
      assertEqQuery
        dbSync
        (DB.countBlockProtoMinor tagged)
        forged
        "Forged headers did not reach the database tagged"

      stopDBSync dbSync
      void $ queryDBSync dbSync $ do
        _ <- DB.truncateBlockProtoMinor truncated
        DB.deleteExtraMigration DB.ProtoMinorFixed
      startDBSync dbSync

      assertEqQuery
        dbSync
        (DB.countBlockProtoMinor truncated)
        0
        "The repair left a truncated proto_minor behind"
      assertEqQuery
        dbSync
        (DB.countBlockProtoMinor tagged)
        forged
        "The repair did not restore the header's proto_minor"
      assertBlockNoBackoff dbSync (length blks)
  where
    cliArgs = initCommandLineArgs {claForgedProtoMinor = Just (fromIntegral tagged)}
    tagged, truncated :: Int64
    tagged = 0x4D8001
    -- 'tagged' through the old 'Word16' field and signed @int2@ encoder.
    truncated = -32767
    testLabel = "conwayProtoMinorRepair"

-- | @--force-fix@ exists for the truncation the gate cannot see, so it has to sweep with the
-- completion marker in place and nothing in the database looking broken.
forceFixBypassesMarker :: IOManager -> [(Text, Text)] -> Assertion
forceFixBypassesMarker =
  withCustomConfig cliArgs (Just configLedgerIgnore) conwayConfigDir testLabel $
    \interpreter server dbSync -> do
      blks <- Api.forgeAndSubmitBlocks interpreter server 10
      startDBSync dbSync
      assertBlockNoBackoff dbSync (length blks)

      -- The first run found nothing to do and recorded the repair as done.
      assertEqQuery
        dbSync
        (elem DB.ProtoMinorFixed <$> DB.queryAllExtraMigrations)
        True
        "Expected the first run to record the repair as done"

      stopDBSync dbSync
      void $ queryDBSync dbSync (DB.truncateBlockProtoMinor truncated)
      startDBSync dbSync

      assertEqQuery
        dbSync
        (DB.countBlockProtoMinor truncated)
        0
        "--force-fix did not sweep past the completion marker"
  where
    cliArgs =
      initCommandLineArgs
        { claForgedProtoMinor = Just (fromIntegral tagged)
        , claForceFix = True
        }
    tagged, truncated :: Int64
    tagged = 0x4D8001
    truncated = -32767
    testLabel = "conwayProtoMinorForceFix"

-- | @--only-fix@ promises to exit instead of carrying on into syncing. db-sync runs in this
-- process here, so the exit shows up as its thread finishing rather than as a process exit.
onlyFixExits :: IOManager -> [(Text, Text)] -> Assertion
onlyFixExits =
  withCustomConfig cliArgs (Just configLedgerIgnore) conwayConfigDir testLabel $
    \interpreter server dbSync -> do
      void $ Api.forgeAndSubmitBlocks interpreter server 10
      startDBSync dbSync

      exited <- waitForExit 150 dbSync
      unless exited $ assertFailure "db-sync kept running after --only-fix"
  where
    cliArgs =
      initCommandLineArgs
        { claForgedProtoMinor = Just 0x4D8001
        , claForceFix = True
        , claOnlyFix = True
        }
    waitForExit :: Int -> DBSyncEnv -> IO Bool
    waitForExit 0 _ = pure False
    waitForExit n env = do
      mResult <- pollDBSync env
      case mResult of
        Just _ -> pure True
        Nothing -> threadDelay 200000 >> waitForExit (n - 1) env
    testLabel = "conwayProtoMinorOnlyFix"
