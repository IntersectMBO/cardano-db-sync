module Test.Cardano.Db.Mock.Unit.Conway.Simple (
  forgeBlocks,
  addSimple,
  addSimpleChain,
  restartDBSync,
  nodeRestart,
  nodeRestartBoundary,
) where

import Cardano.Ledger.BaseTypes (BlockNo (..))
import Cardano.Mock.ChainSync.Server (IOManager, addBlock, restartServer)
import Cardano.Mock.Forging.Interpreter (forgeNext)
import Cardano.Prelude
import Ouroboros.Network.Block (blockNo)
import Test.Cardano.Db.Mock.Config
import Test.Cardano.Db.Mock.Examples (mockBlock0, mockBlock1, mockBlock2)
import Test.Cardano.Db.Mock.UnifiedApi (fillUntilNextEpoch, forgeAndSubmitBlocks, forgeNextAndSubmit)
import Test.Cardano.Db.Mock.Validate (assertBlockNoBackoff)
import Test.Tasty.HUnit (Assertion (), assertBool)
import Prelude ()

forgeBlocks :: IOManager -> [(Text, Text)] -> Assertion
forgeBlocks = do
  withFullConfigAndDropDB conwayConfigDir testLabel $ \interpreter _ _ -> do
    void $ forgeNext interpreter mockBlock0
    void $ forgeNext interpreter mockBlock1
    block <- forgeNext interpreter mockBlock2

    let blkNo = blockNo block
    assertBool (show blkNo <> " /= " <> "3") $
      blkNo == BlockNo 3
  where
    testLabel = "conwayForgeBlocks"

addSimple :: IOManager -> [(Text, Text)] -> Assertion
addSimple =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    -- Translate the mock block until a real one and submit it to the chain server
    void $ forgeNextAndSubmit interpreter mockServer mockBlock0

    startDBSync dbSync
    assertBlockNoBackoff dbSync 1
  where
    testLabel = "conwayAddSimple"

addSimpleChain :: IOManager -> [(Text, Text)] -> Assertion
addSimpleChain =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    -- Create some mock blocks
    blk0 <- forgeNext interpreter mockBlock0
    blk1 <- forgeNext interpreter mockBlock1
    blk2 <- forgeNext interpreter mockBlock2
    -- Transform a mock block into a real one
    atomically $ addBlock mockServer blk0

    startDBSync dbSync

    -- Transform the other blocks
    atomically $ addBlock mockServer blk1
    atomically $ addBlock mockServer blk2

    assertBlockNoBackoff dbSync 3
  where
    testLabel = "conwayAddSimpleChain"

restartDBSync :: IOManager -> [(Text, Text)] -> Assertion
restartDBSync =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    void $ forgeNextAndSubmit interpreter mockServer mockBlock0

    startDBSync dbSync
    assertBlockNoBackoff dbSync 1

    stopDBSync dbSync
    startDBSync dbSync
    assertBlockNoBackoff dbSync 1
  where
    testLabel = "conwayRestartDBSync"

nodeRestart :: IOManager -> [(Text, Text)] -> Assertion
nodeRestart =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    void $ forgeAndSubmitBlocks interpreter mockServer 5
    startDBSync dbSync
    assertBlockNoBackoff dbSync 5

    restartServer mockServer

    void $ forgeAndSubmitBlocks interpreter mockServer 5
    assertBlockNoBackoff dbSync 10
  where
    testLabel = "conwayNodeRestart"

nodeRestartBoundary :: IOManager -> [(Text, Text)] -> Assertion
nodeRestartBoundary =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    blks <- fillUntilNextEpoch interpreter mockServer
    assertBlockNoBackoff dbSync $ length blks

    restartServer mockServer

    void $ forgeAndSubmitBlocks interpreter mockServer 5
    assertBlockNoBackoff dbSync $ 5 + length blks
  where
    testLabel = "conwayNodeRestartBoundary"
