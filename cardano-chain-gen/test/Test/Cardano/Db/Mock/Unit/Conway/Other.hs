{-# LANGUAGE NumericUnderscores #-}

module Test.Cardano.Db.Mock.Unit.Conway.Other (
  configNoPools,
  configNoStakes,
) where

import Cardano.Mock.ChainSync.Server (IOManager ())
import Cardano.Mock.Forging.Interpreter (forgeNext)
import Cardano.Mock.Forging.Types (ForgingError (..))
import Cardano.Prelude
import Test.Cardano.Db.Mock.Config
import Test.Cardano.Db.Mock.Examples (mockBlock0)
import Test.Cardano.Db.Mock.Validate (assertBlocksCount, assertTxCount, checkStillRuns)
import Test.Tasty.HUnit (Assertion (), assertFailure)
import Prelude ()

configNoPools :: IOManager -> [(Text, Text)] -> Assertion
configNoPools =
  withFullConfig "config-conway-no-pools" testLabel $ \_ _ dbSync -> do
    startDBSync dbSync

    -- Wait for it to sync
    assertBlocksCount dbSync 2
    assertTxCount dbSync 6

    -- Restart
    stopDBSync dbSync
    startDBSync dbSync
    -- Nothing changes, so polling assertions doesn't help here We have to pause
    -- and check if anything crashed.
    threadDelay 3_000_000

    -- Verify it's still running
    checkStillRuns dbSync
    -- Verify the initial blocks again
    assertBlocksCount dbSync 2
    assertTxCount dbSync 6
  where
    testLabel = "conwayConfigNoPools"

configNoStakes :: IOManager -> [(Text, Text)] -> Assertion
configNoStakes =
  withFullConfig "config-conway-no-stakes" testLabel $ \interpreter _ dbSync -> do
    startDBSync dbSync

    -- Wait for it to sync
    assertBlocksCount dbSync 2
    assertTxCount dbSync 7

    -- Restart
    stopDBSync dbSync
    startDBSync dbSync
    -- Nothing changes, so polling assertions doesn't help here We have to pause
    -- and check if anything crashed.
    threadDelay 3_000_000

    -- Verify it's still running
    checkStillRuns dbSync
    -- Verify the initial blocks again
    assertBlocksCount dbSync 2
    assertTxCount dbSync 7

    -- Try to forge a block (expected to fail)
    errBlk <- try $ forgeNext interpreter mockBlock0
    case errBlk of
      Right _ -> assertFailure "expected to fail"
      -- A pool with no stakes can't create blocks
      Left WentTooFar {} -> pure ()
      -- TODO add an option to disable fingerprint validation for tests like this.
      Left (EmptyFingerprint _ _) -> pure ()
      Left err -> assertFailure $ "Expected WentTooFar, got " <> show err
  where
    testLabel = "conwayConfigNoStakes"
