module Test.Cardano.Db.Mock.Unit.Conway.ProtoMinor (
  repairsTruncatedProtoMinor,
) where

import qualified Cardano.Db as DB
import Cardano.Mock.ChainSync.Server (IOManager ())
import Cardano.Prelude
import Test.Cardano.Db.Mock.Config
import qualified Test.Cardano.Db.Mock.UnifiedApi as Api
import Test.Cardano.Db.Mock.Validate (assertBlockNoBackoff, assertEqQuery)
import Test.Tasty.HUnit (Assertion ())
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
