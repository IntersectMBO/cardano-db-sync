{-# LANGUAGE NumericUnderscores #-}

module Test.Cardano.Db.Mock.Unit where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Tracer (nullTracer)
import           Data.Text (Text)
import           System.FilePath hiding (isValid)

import           Cardano.Ledger.Slot (BlockNo (..))

import           Ouroboros.Network.Block (blockNo, blockPoint)

import           Cardano.Mock.ChainSync.Server
import           Cardano.Mock.Forging.Interpreter
import           Cardano.Mock.Forging.Types
import qualified Cardano.Mock.Forging.Tx.Alonzo as Alonzo
import qualified Cardano.Mock.Forging.Tx.Shelley as Shelley

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (assertBool, assertFailure, testCase)

import           Test.Cardano.Db.Mock.Config
import           Test.Cardano.Db.Mock.Examples
import           Test.Cardano.Db.Mock.Validate

unitTests :: IOManager -> [(Text, Text)] -> TestTree
unitTests iom knownMigrations =
  testGroup "unit tests"
    [ testCase "Forge some blocks" forgeBlocks
    , testCase "Add one Simple block" (testSimpleRewards iom knownMigrations)
    ]

rootTestDir :: FilePath
rootTestDir = "test/testfiles"

configDir ::  FilePath
configDir = rootTestDir </> "config"

forgeBlocks :: IO ()
forgeBlocks = do
    recreateDir testDir
    cfg <- mkConfig configDir testDir
    interpreter <- initInterpreter (protocolInfoForging cfg) nullTracer
    _block0 <- forgeNext interpreter mockBlock0
    _block1 <- forgeNext interpreter mockBlock1
    block2 <- forgeNext interpreter mockBlock2
    let blkNo = blockNo block2
    assertBool (show blkNo <> " /= " <> "2")
      $ blkNo == BlockNo 2
  where
    testDir = rootTestDir </> "temp/forgeBlocks"

addSimple :: IOManager -> [(Text, Text)] -> IO ()
addSimple =
    withFullConfig configDir testDir $ \interpreter mockServer asyncDBSync -> do
      -- translate the block to a real Cardano block.
      blk <- forgeNext interpreter mockBlock0
      atomically $ addBlock mockServer blk
      -- start db-sync and let it sync
      _ <- asyncDBSync
      assertBlockNoBackoff 0
  where
    testDir = rootTestDir </> "temp/addSimple"

addSimpleChain :: IOManager -> [(Text, Text)] -> IO ()
addSimpleChain =
    withFullConfig configDir testDir $ \interpreter mockServer asyncDBSync -> do
      -- translate the blocks to real Cardano blocks.
      blk0 <- forgeNext interpreter mockBlock0
      blk1 <- forgeNext interpreter mockBlock1
      blk2 <- forgeNext interpreter mockBlock2
      atomically $ addBlock mockServer blk0
      -- start db-sync and let it sync
      _ <- asyncDBSync
      -- add more blocks
      atomically $ addBlock mockServer blk1
      atomically $ addBlock mockServer blk2
      assertBlockNoBackoff 2
  where
    testDir = rootTestDir </> "temp/addSimpleChain"

restartDBSync :: IOManager -> [(Text, Text)] -> IO ()
restartDBSync =
    withFullConfig configDir testDir $ \interpreter mockServer asyncDBSync -> do
      -- translate the block to a real Cardano block.
      blk <- forgeNext interpreter mockBlock0
      atomically $ addBlock mockServer blk
      -- start db-sync and let it sync
      dbSync <- asyncDBSync
      assertBlockNoBackoff 0

      cancel dbSync
      -- The server sees a separate client here
      _dbSync' <- asyncDBSync
      assertBlockNoBackoff 0
  where
    testDir = rootTestDir </> "temp/restartDBSync"

simpleRollback :: IOManager -> [(Text, Text)] -> IO ()
simpleRollback = do
    withFullConfig configDir testDir $ \interpreter mockServer asyncDBSync -> do
      blk0 <- forgeNext interpreter mockBlock0
      blk1 <- forgeNext interpreter mockBlock1
      blk2 <- forgeNext interpreter mockBlock2
      atomically $ addBlock mockServer blk0
      -- start db-sync and let it sync
      _ <- asyncDBSync
      atomically $ addBlock mockServer blk1
      atomically $ addBlock mockServer blk2
      assertBlockNoBackoff 2

      atomically $ rollback mockServer (blockPoint blk1)
      assertBlockNoBackoff 1
  where
    testDir = rootTestDir </> "temp/simpleRollback"

bigChain :: IOManager -> [(Text, Text)] -> IO ()
bigChain =
    withFullConfig configDir testDir $ \interpreter mockServer asyncDBSync -> do
      blks <- forM (replicate 101 mockBlock0) (forgeNext interpreter)
      atomically $ forM_ blks $ addBlock mockServer
      _ <- asyncDBSync
      assertBlockNoBackoff 100

      blks' <- forM (replicate 100 mockBlock1) (forgeNext interpreter)
      atomically $ forM_ blks' $ addBlock mockServer
      assertBlockNoBackoff 200

      blks'' <- forM (replicate 5 mockBlock2) (forgeNext interpreter)
      atomically $ forM_ blks'' $ addBlock mockServer
      assertBlockNoBackoff 205

      atomically $ rollback mockServer (blockPoint $ last blks')
      assertBlockNoBackoff 200
  where
    testDir = rootTestDir </> "temp/bigChain"


bigChainRestart :: IOManager -> [(Text, Text)] -> IO ()
bigChainRestart =
    withFullConfig configDir testDir $ \interpreter mockServer asyncDBSync -> do
      blks <- forM (replicate 101 mockBlock0) (forgeNext interpreter)
      atomically $ forM_ blks $ addBlock mockServer
      dbSync <- asyncDBSync
      assertBlockNoBackoff 100

      blks' <- forM (replicate 10 mockBlock0) (forgeNext interpreter)
      atomically $ forM_ blks' $ addBlock mockServer
      assertBlockNoBackoff 200

      blks'' <- forM (replicate 5 mockBlock2) (forgeNext interpreter)
      atomically $ forM_ blks'' $ addBlock mockServer
      assertBlockNoBackoff 205

      cancel dbSync
      atomically $ rollback mockServer (blockPoint $ last blks')
      _ <- asyncDBSync
      assertBlockNoBackoff 200
  where
    testDir = rootTestDir </> "temp/bigChainRestart"

configNoPools :: IOManager -> [(Text, Text)] -> IO ()
configNoPools =
    withFullConfig (rootTestDir </> "config2") testDir $ \_ _ asyncDBSync -> do
      dbSync <- asyncDBSync
      assertBlocksCount 2
      assertTxCount 6
      cancel dbSync
      _ <- asyncDBSync
      -- Nothing changes, so polling assertions don't help here
      -- We have to pause and check if anything crashed.
      threadDelay 3_000_000
      assertBlocksCount 2
      assertTxCount 6
  where
    testDir = rootTestDir </> "temp/configNoPools"


configNoStakes :: IOManager -> [(Text, Text)] -> IO ()
configNoStakes =
    withFullConfig (rootTestDir </> "config3") testDir $ \interpreter _ asyncDBSync -> do
      dbSync <- asyncDBSync
      assertBlocksCount 2
      assertTxCount 7
      cancel dbSync
      _ <- asyncDBSync
      -- Nothing changes, so polling assertions don't help here
      -- We have to pause and check if anything crashed.
      threadDelay 3_000_000
      assertBlocksCount 2
      assertTxCount 7
      -- A pool with no stakes can't create a block.
      eblk <- try $ forgeNext interpreter mockBlock0
      case eblk of
        Right _ -> assertFailure "should fail"
        Left WentTooFar -> pure ()
        Left err -> assertFailure $ "got " <> show err <> " instead of WentTooFar"
  where
    testDir = rootTestDir </> "temp/configNoStakes"

addSimpleTx :: IOManager -> [(Text, Text)] -> IO ()
addSimpleTx =
    withFullConfig configDir testDir $ \interpreter mockServer asyncDBSync -> do
      -- translate the block to a real Cardano block.
      tx0 <- withAlonzoLedgerState interpreter $  Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000
      blk <- forgeNext interpreter $ MockBlock [TxAlonzo tx0] (NodeId 0)

      atomically $ addBlock mockServer blk
      -- start db-sync and let it sync
      _ <- asyncDBSync
      assertBlockNoBackoff 0
  where
    testDir = rootTestDir </> "temp/addSimpleTx"

addSimpleTxShelley :: IOManager -> [(Text, Text)] -> IO ()
addSimpleTxShelley =
    withFullConfig (rootTestDir </> "config-shelley") testDir $ \interpreter mockServer asyncDBSync -> do
      -- translate the block to a real Cardano block.
      tx0 <- withShelleyLedgerState interpreter $ Shelley.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000
      blk <- forgeNext interpreter $ MockBlock [TxShelley tx0] (NodeId 0)
      print blk

      atomically $ addBlock mockServer blk
      -- start db-sync and let it sync
      _ <- asyncDBSync
      assertBlockNoBackoff 0
  where
    testDir = rootTestDir </> "temp/addSimpleTxShelley"

testSimpleRewards :: IOManager -> [(Text, Text)] -> IO ()
testSimpleRewards =
    withFullConfig configDir testDir $ \interpreter mockServer asyncDBSync -> do
      _ <- asyncDBSync
      blk <- registerAllStakeCreds interpreter (NodeId 0)
      atomically $ addBlock mockServer blk
      -- Pools are not registered yet, this takes 2 epochs. So fees of this tx
      -- should not create any rewards.
      tx0 <- withAlonzoLedgerState interpreter $  Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000
      blk0 <- forgeNext interpreter $ MockBlock [TxAlonzo tx0] (NodeId 1)
      atomically $ addBlock mockServer blk0

      fillEpochEqually interpreter mockServer
      fillEpochEqually interpreter mockServer
      fillEpochEqually interpreter mockServer

      -- Now that pools are registered, we add a tx to fill the fees pot.
      -- Rewards will be distributed.
      tx1 <- withAlonzoLedgerState interpreter $  Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000
      blk1 <- forgeNext interpreter $ MockBlock [TxAlonzo tx1] (NodeId 1)
      atomically $ addBlock mockServer blk1

      fillEpochEqually interpreter mockServer
      fillEpochEqually interpreter mockServer
      fillEpochEqually interpreter mockServer

      assertBlockNoBackoff (6 * 720 + 2)
      assertRewardCount 17

  where
    testDir = rootTestDir </> "temp/addSimpleRewards"

-- This test is the same as the previous, but in Shelley era. Rewards result
-- should be different because of the old Shelley bug.
-- https://github.com/input-output-hk/cardano-db-sync/issues/959
--
-- The differenece in rewards is triggered when a reward address of a pool A
-- delegates to a pool B and is not an owner of pool B. In this case it receives
-- leader rewards from pool A and member rewards from pool B. In this test, we
-- have 2 instances of this case, one where A = B and one where A /= B.
testRewardsShelley :: IOManager -> [(Text, Text)] -> IO ()
testRewardsShelley =
    withFullConfig (rootTestDir </> "config-shelley") testDir $ \interpreter mockServer asyncDBSync -> do
      _ <- asyncDBSync
      blk <- registerAllStakeCreds interpreter (NodeId 0)
      atomically $ addBlock mockServer blk
      tx0 <- withShelleyLedgerState interpreter $  Shelley.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000
      blk0 <- forgeNext interpreter $ MockBlock [TxShelley tx0] (NodeId 1)
      atomically $ addBlock mockServer blk0

      fillEpochEqually interpreter mockServer
      fillEpochEqually interpreter mockServer
      fillEpochEqually interpreter mockServer

      tx1 <- withShelleyLedgerState interpreter $  Shelley.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000
      blk1 <- forgeNext interpreter $ MockBlock [TxShelley tx1] (NodeId 1)
      atomically $ addBlock mockServer blk1

      fillEpochEqually interpreter mockServer
      fillEpochEqually interpreter mockServer
      fillEpochEqually interpreter mockServer

      assertBlockNoBackoff (6 * 720 + 2)
      -- Note we have 2 rewards less compared to Alonzo era
      assertRewardCount 15

  where
    testDir = rootTestDir </> "temp/testRewardsShelley"

fillEpochEqually :: Interpreter -> ServerHandle IO CardanoBlock -> IO ()
fillEpochEqually interpreter mockServer = do
    blks0 <- forM (replicate thirdEpoch mockBlock0) (forgeNext interpreter)
    atomically $ forM_ blks0 $ addBlock mockServer

    blks1 <- forM (replicate thirdEpoch mockBlock1) (forgeNext interpreter)
    atomically $ forM_ blks1 $ addBlock mockServer

    blks2 <- forM (replicate thirdEpoch mockBlock2) (forgeNext interpreter)
    atomically $ forM_ blks2 $ addBlock mockServer
  where
    thirdEpoch = div 720 3
