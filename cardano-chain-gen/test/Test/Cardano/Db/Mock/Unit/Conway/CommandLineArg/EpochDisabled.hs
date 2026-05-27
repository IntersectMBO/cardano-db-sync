{-# LANGUAGE NumericUnderscores #-}

module Test.Cardano.Db.Mock.Unit.Conway.CommandLineArg.EpochDisabled (
  checkEpochDisabledArg,
  checkEpochEnabled,
  checkEpochCurrentLiveUpdates,
  checkEpochRollbackStaleFinalized,
) where

import Cardano.Db as DB
import Cardano.Mock.ChainSync.Server (IOManager ())
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import Cardano.Mock.Forging.Types (UTxOIndex (..))
import Cardano.Prelude
import Ouroboros.Network.Block (blockPoint)
import Test.Cardano.Db.Mock.Config
import Test.Cardano.Db.Mock.UnifiedApi (fillUntilNextEpoch, forgeAndSubmitBlocks, rollbackTo, withConwayFindLeaderAndSubmitTx)
import Test.Cardano.Db.Mock.Validate (assertBlockNoBackoff, assertEqQuery)
import Test.Tasty.HUnit (Assertion ())
import Prelude (last)

checkEpochDisabledArg :: IOManager -> [(Text, Text)] -> Assertion
checkEpochDisabledArg =
  withCustomConfigDropDB initCommandLineArgs (Just configEpochDisable) conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge some blocks
    void $ forgeAndSubmitBlocks interpreter mockServer 50
    -- Add two blocks with transactions
    void $
      withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10_000 10_000 0
    void $
      withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx (UTxOIndex 1) (UTxOIndex 0) 10_000 10_000 0
    -- Add some more empty blocks
    void $ forgeAndSubmitBlocks interpreter mockServer 60

    -- Verify the epoch view stays empty when disabled
    assertBlockNoBackoff dbSync 112
    assertEqQuery dbSync DB.queryEpochCount 0 "epoch updated"
  where
    testLabel = "conwayCLACheckEpochDisabledArg"

checkEpochEnabled :: IOManager -> [(Text, Text)] -> Assertion
checkEpochEnabled =
  withCustomConfig initCommandLineArgs (Just configEpochEnable) conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge some blocks
    void $ forgeAndSubmitBlocks interpreter mockServer 50
    -- Add two blocks with transactions
    void $
      withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10_000 10_000 0
    void $
      withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx (UTxOIndex 1) (UTxOIndex 0) 10_000 10_000 0
    -- Add some more empty blocks
    void $ forgeAndSubmitBlocks interpreter mockServer 60

    -- 112 blocks crosses one epoch boundary: epoch 0 in epoch_finalized + epoch 1 in epoch_current.
    assertBlockNoBackoff dbSync 112
    assertEqQuery dbSync DB.queryEpochCount 2 "epoch not updated"
  where
    testLabel = "conwayCLACheckEpochEnabledConfig"

checkEpochCurrentLiveUpdates :: IOManager -> [(Text, Text)] -> Assertion
checkEpochCurrentLiveUpdates =
  withCustomConfig initCommandLineArgs (Just configEpochEnable) conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ forgeAndSubmitBlocks interpreter mockServer 10
    assertBlockNoBackoff dbSync 10
    assertEqQuery dbSync (blkCountFor 0) 10 "epoch_current did not reflect first batch"

    void $ forgeAndSubmitBlocks interpreter mockServer 25
    assertBlockNoBackoff dbSync 35
    assertEqQuery dbSync (blkCountFor 0) 35 "epoch_current did not update after more blocks"
  where
    testLabel = "conwayCLACheckEpochCurrentLiveUpdates"

    blkCountFor :: Word64 -> DB.DbM Word64
    blkCountFor n = do
      res <- DB.queryEpochEntry n
      pure $ case res of
        Right ep -> DB.epochBlkCount ep
        Left _ -> 0

-- | Rollback across an epoch boundary: after rolling back into epoch 0,
--   the epoch view should report only the surviving blocks of epoch 0.
checkEpochRollbackStaleFinalized :: IOManager -> [(Text, Text)] -> Assertion
checkEpochRollbackStaleFinalized =
  withCustomConfigDropDB initCommandLineArgs (Just configEpochEnable) conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    let preRollbackBlocks = 90
    targetBlks <- forgeAndSubmitBlocks interpreter mockServer preRollbackBlocks
    let rollbackPoint = blockPoint (last targetBlks)
    boundaryBlks <- fillUntilNextEpoch interpreter mockServer
    -- Wait for db-sync to process the boundary block so epoch 0 is
    -- finalized into epoch_finalized before we trigger the rollback.
    assertBlockNoBackoff dbSync (preRollbackBlocks + length boundaryBlks)
    void $ rollbackTo interpreter mockServer rollbackPoint
    -- Wait for db-sync to finish processing the rollback + dummy block.
    let postRollbackBlockNo = preRollbackBlocks + 1
    assertBlockNoBackoff dbSync postRollbackBlockNo

    -- preRollbackBlocks + 1 dummy block forged by rollbackTo.
    let expectedBlkCount = fromIntegral postRollbackBlockNo :: Word64
    assertEqQuery
      dbSync
      (blkCountFor 0)
      expectedBlkCount
      "epoch view block count after rollback across epoch boundary"
  where
    testLabel = "conwayCLACheckEpochRollbackStaleFinalized"

    blkCountFor :: Word64 -> DB.DbM Word64
    blkCountFor n = do
      res <- DB.queryEpochEntry n
      pure $ case res of
        Right ep -> DB.epochBlkCount ep
        Left _ -> 0
