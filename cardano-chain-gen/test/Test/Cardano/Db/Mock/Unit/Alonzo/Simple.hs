module Test.Cardano.Db.Mock.Unit.Alonzo.Simple (
  forgeBlocks,
  addSimple,
  addSimpleChain,
  restartDBSync,
) where

import Cardano.Ledger.BaseTypes (BlockNo (BlockNo))
import Cardano.Mock.ChainSync.Server (IOManager, addBlock)
import Cardano.Mock.Forging.Interpreter (forgeNext)
import Control.Concurrent.Class.MonadSTM.Strict (MonadSTM (atomically))
import Control.Monad (void)
import Data.Text (Text)
import Ouroboros.Network.Block (blockNo)
import Test.Cardano.Db.Mock.Config (alonzoConfigDir, startDBSync, stopDBSync, withFullConfig, withFullConfigAndDropDB)
import Test.Cardano.Db.Mock.Examples (mockBlock0, mockBlock1, mockBlock2)
import Test.Cardano.Db.Mock.UnifiedApi (forgeNextAndSubmit)
import Test.Cardano.Db.Mock.Validate (assertBlockNoBackoff)
import Test.Tasty.HUnit (Assertion, assertBool)

forgeBlocks :: IOManager -> [(Text, Text)] -> Assertion
forgeBlocks = do
  withFullConfigAndDropDB alonzoConfigDir testLabel $ \interpreter _mockServer _dbSync -> do
    _block0 <- forgeNext interpreter mockBlock0
    _block1 <- forgeNext interpreter mockBlock1
    block2 <- forgeNext interpreter mockBlock2
    let blkNo = blockNo block2
    assertBool (show blkNo <> " /= " <> "3") $
      blkNo == BlockNo 3
  where
    testLabel = "forgeBlocks-alonzo"

addSimple :: IOManager -> [(Text, Text)] -> Assertion
addSimple =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    -- Given a mock block, translate it into a real block and submit it to the
    -- chainsync server
    void $ forgeNextAndSubmit interpreter mockServer mockBlock0
    -- start db-sync and let it sync
    startDBSync dbSync
    assertBlockNoBackoff dbSync 1
  where
    testLabel = "addSimple-alonzo"

addSimpleChain :: IOManager -> [(Text, Text)] -> Assertion
addSimpleChain =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    -- translate the blocks to real Cardano blocks.
    blk0 <- forgeNext interpreter mockBlock0
    blk1 <- forgeNext interpreter mockBlock1
    blk2 <- forgeNext interpreter mockBlock2
    atomically $ addBlock mockServer blk0
    -- start db-sync and let it sync
    startDBSync dbSync
    -- add more blocks
    atomically $ addBlock mockServer blk1
    atomically $ addBlock mockServer blk2
    assertBlockNoBackoff dbSync 3
  where
    testLabel = "addSimpleChain-alonzo"

restartDBSync :: IOManager -> [(Text, Text)] -> Assertion
restartDBSync =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    void $ forgeNextAndSubmit interpreter mockServer mockBlock0
    -- start db-sync and let it sync
    startDBSync dbSync
    assertBlockNoBackoff dbSync 1

    stopDBSync dbSync
    -- The server sees a separate client here
    startDBSync dbSync
    assertBlockNoBackoff dbSync 1
  where
    testLabel = "restartDBSync-alonzo"
