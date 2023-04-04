module Test.Cardano.Db.Mock.Unit.Babbage.Simple (
  forgeBlocks,
  addSimple,
  addSimpleChain,
  restartDBSync,
  nodeRestart,
  nodeRestartBoundary,
) where

import Cardano.Ledger.BaseTypes (BlockNo (BlockNo))
import Cardano.Mock.ChainSync.Server (IOManager, addBlock, restartServer)
import Cardano.Mock.Forging.Interpreter (forgeNext)
import Control.Concurrent.Class.MonadSTM.Strict (atomically)
import Control.Monad (void)
import Data.Text (Text)
import Ouroboros.Network.Block (blockNo)
import Test.Cardano.Db.Mock.Config (babbageConfigDir, startDBSync, stopDBSync, withFullConfig)
import Test.Cardano.Db.Mock.Examples (mockBlock0, mockBlock1, mockBlock2)
import Test.Cardano.Db.Mock.UnifiedApi (fillUntilNextEpoch, forgeAndSubmitBlocks, forgeNextAndSubmit)
import Test.Cardano.Db.Mock.Validate (assertBlockNoBackoff)
import Test.Tasty.HUnit (Assertion, assertBool)

forgeBlocks :: IOManager -> [(Text, Text)] -> Assertion
forgeBlocks = do
  withFullConfig babbageConfigDir testLabel $ \interpreter _mockServer _dbSync -> do
    _block0 <- forgeNext interpreter mockBlock0
    _block1 <- forgeNext interpreter mockBlock1
    block2 <- forgeNext interpreter mockBlock2
    let blkNo = blockNo block2
    assertBool (show blkNo <> " /= " <> "3") $
      blkNo
        == BlockNo 3
  where
    testLabel = "forgeBlocks"

addSimple :: IOManager -> [(Text, Text)] -> Assertion
addSimple =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    -- Given a mock block, translate it into a real block and submit it to the
    -- chainsync server
    void $ forgeNextAndSubmit interpreter mockServer mockBlock0
    -- start db-sync and let it sync
    startDBSync dbSync
    assertBlockNoBackoff dbSync 1
  where
    testLabel = "addSimple"

addSimpleChain :: IOManager -> [(Text, Text)] -> Assertion
addSimpleChain =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
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
    testLabel = "addSimpleChain"

restartDBSync :: IOManager -> [(Text, Text)] -> Assertion
restartDBSync =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    void $ forgeNextAndSubmit interpreter mockServer mockBlock0
    -- start db-sync and let it sync
    startDBSync dbSync
    assertBlockNoBackoff dbSync 1

    stopDBSync dbSync
    -- The server sees a separate client here
    startDBSync dbSync
    assertBlockNoBackoff dbSync 1
  where
    testLabel = "restartDBSync"

nodeRestart :: IOManager -> [(Text, Text)] -> Assertion
nodeRestart =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ forgeAndSubmitBlocks interpreter mockServer 5
    assertBlockNoBackoff dbSync 5

    restartServer mockServer

    void $ forgeAndSubmitBlocks interpreter mockServer 5
    assertBlockNoBackoff dbSync 10
  where
    testLabel = "nodeRestart"

nodeRestartBoundary :: IOManager -> [(Text, Text)] -> Assertion
nodeRestartBoundary =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    blks <- fillUntilNextEpoch interpreter mockServer
    assertBlockNoBackoff dbSync $ length blks

    restartServer mockServer

    void $ forgeAndSubmitBlocks interpreter mockServer 5
    assertBlockNoBackoff dbSync $ 5 + length blks
  where
    testLabel = "nodeRestartBoundary"
