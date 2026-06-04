{-# LANGUAGE NumericUnderscores #-}

module Test.Cardano.Db.Mock.Unit.Conway.Epoch (
  checkEpochDisabledArg,
  checkEpochEnabled,
  checkEpochCurrentLiveUpdates,
  checkEpochRollbackStaleFinalized,
  checkEpochRollbackToFirstOfEpoch,
  checkEpochRollbackToLastOfEpoch,
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
import Prelude (error, last)

-- | Block count for an epoch from the public epoch view (epoch_finalized + epoch_current).
blkCountFor :: Word64 -> DB.DbM Word64
blkCountFor n = do
  res <- DB.queryEpochEntry n
  pure $ case res of
    Right ep -> DB.epochBlkCount ep
    Left _ -> 0

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

-- | Rollback into the middle of epoch 0 after epoch 0 was finalised: the
--   surviving epoch should be reported via epoch_current.
checkEpochRollbackStaleFinalized :: IOManager -> [(Text, Text)] -> Assertion
checkEpochRollbackStaleFinalized =
  withCustomConfigDropDB initCommandLineArgs (Just configEpochEnable) conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    let preRollbackBlocks = 90
    targetBlks <- forgeAndSubmitBlocks interpreter mockServer preRollbackBlocks
    let rollbackPoint = blockPoint (last targetBlks)
    boundaryBlks <- fillUntilNextEpoch interpreter mockServer
    -- Wait for db-sync to finalise epoch 0 before triggering the rollback.
    assertBlockNoBackoff dbSync (preRollbackBlocks + length boundaryBlks)
    void $ rollbackTo interpreter mockServer rollbackPoint
    let postRollbackBlockNo = preRollbackBlocks + 1
    assertBlockNoBackoff dbSync postRollbackBlockNo

    let expectedBlkCount = fromIntegral postRollbackBlockNo :: Word64
    assertEqQuery
      dbSync
      (blkCountFor 0)
      expectedBlkCount
      "epoch view block count after rollback across epoch boundary"
  where
    testLabel = "conwayCLACheckEpochRollbackStaleFinalized"

-- | Rollback target = first block of a new epoch (the boundary block survives).
--   epoch 0's finalised row must be preserved.
checkEpochRollbackToFirstOfEpoch :: IOManager -> [(Text, Text)] -> Assertion
checkEpochRollbackToFirstOfEpoch =
  withCustomConfigDropDB initCommandLineArgs (Just configEpochEnable) conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    epoch0Blks <- fillUntilNextEpoch interpreter mockServer
    let firstOfEpoch1 = last epoch0Blks
        rollbackPoint = blockPoint firstOfEpoch1
        epoch0FinalisedCount = fromIntegral (length epoch0Blks - 1) :: Word64
    void $ forgeAndSubmitBlocks interpreter mockServer 5
    assertBlockNoBackoff dbSync (length epoch0Blks + 5)

    void $ rollbackTo interpreter mockServer rollbackPoint
    assertBlockNoBackoff dbSync (length epoch0Blks + 1)

    assertEqQuery dbSync (blkCountFor 0) epoch0FinalisedCount "epoch 0 should stay finalised when the boundary block survives"
    assertEqQuery dbSync (blkCountFor 1) 2 "epoch 1 should contain boundary block + rollback dummy"
  where
    testLabel = "conwayCheckEpochRollbackToFirstOfEpoch"

-- | Rollback target = last block of an epoch (the boundary block is removed).
--   epoch 0's stale finalised row must be dropped during rollback; the next
--   forged block re-finalises epoch 0 with only the surviving blocks.
checkEpochRollbackToLastOfEpoch :: IOManager -> [(Text, Text)] -> Assertion
checkEpochRollbackToLastOfEpoch =
  withCustomConfigDropDB initCommandLineArgs (Just configEpochEnable) conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    epoch0Blks <- fillUntilNextEpoch interpreter mockServer
    let lastOfEpoch0 = case reverse epoch0Blks of
          _boundary : prev : _ -> prev
          _ -> error "fillUntilNextEpoch must produce at least two blocks"
        rollbackPoint = blockPoint lastOfEpoch0
        epoch0SurvivingCount = fromIntegral (length epoch0Blks - 1) :: Word64
    void $ forgeAndSubmitBlocks interpreter mockServer 5
    assertBlockNoBackoff dbSync (length epoch0Blks + 5)

    void $ rollbackTo interpreter mockServer rollbackPoint
    assertBlockNoBackoff dbSync (length epoch0Blks)

    assertEqQuery dbSync (blkCountFor 0) epoch0SurvivingCount "epoch 0 finalised count must match surviving blocks"
    assertEqQuery dbSync (blkCountFor 1) 1 "epoch 1 should contain only the rollback dummy"
  where
    testLabel = "conwayCheckEpochRollbackToLastOfEpoch"
