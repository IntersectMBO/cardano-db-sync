{-# LANGUAGE NumericUnderscores #-}

module Test.Cardano.Db.Mock.Unit where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Class.MonadSTM.Strict
import           Data.Text (Text)

import           Cardano.Ledger.Slot (BlockNo (..))

import           Ouroboros.Network.Block (blockNo, blockPoint)

import           Cardano.Mock.ChainSync.Server
import           Cardano.Mock.Forging.Interpreter
import           Cardano.Mock.Forging.Types
import qualified Cardano.Mock.Forging.Tx.Alonzo as Alonzo
import qualified Cardano.Mock.Forging.Tx.Shelley as Shelley

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase)

import           Test.Cardano.Db.Mock.Config
import           Test.Cardano.Db.Mock.Examples
import           Test.Cardano.Db.Mock.Validate

unitTests :: IOManager -> [(Text, Text)] -> TestTree
unitTests iom knownMigrations =
    testGroup "unit tests"
      [ test "simple forge blocks" forgeBlocks
      , test "sync one block" addSimple
      , test "sync small chain" addSimpleChain
      , test "restart db-sync" restartDBSync
      , test "simple rollback" simpleRollback
      , test "sync bigger chain" bigChain
      , test "rollback while db-sync is off" restartAndRollback
      , test "genesis config without pool" configNoPools
      , test "genesis config without stakes" configNoStakes
      , test "simple tx" addSimpleTx
      , test "simple tx in Shelley era" addSimpleTxShelley
      , test "rewards" simpleRewards
      , test "shelley rewards from multiple sources" rewardsShelley
      ]
  where
    test :: String -> (IOManager -> [(Text, Text)] -> Assertion) -> TestTree
    test str action = testCase str (action iom knownMigrations)

defaultConfigDir ::  FilePath
defaultConfigDir = "config"

forgeBlocks :: IOManager -> [(Text, Text)] -> Assertion
forgeBlocks = do
    withFullConfig defaultConfigDir testLabel $ \interpreter _mockServer _dbSync -> do
      _block0 <- forgeNext interpreter mockBlock0
      _block1 <- forgeNext interpreter mockBlock1
      block2 <- forgeNext interpreter mockBlock2
      let blkNo = blockNo block2
      assertBool (show blkNo <> " /= " <> "2")
        $ blkNo == BlockNo 2
  where
    testLabel = "forgeBlocks"

addSimple :: IOManager -> [(Text, Text)] -> Assertion
addSimple =
    withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
      -- translate the block to a real Cardano block.
      blk <- forgeNext interpreter mockBlock0
      atomically $ addBlock mockServer blk
      -- start db-sync and let it sync
      startDBSync dbSync
      assertBlockNoBackoff dbSync 0
  where
    testLabel = "addSimple"

addSimpleChain :: IOManager -> [(Text, Text)] -> Assertion
addSimpleChain =
    withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
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
      assertBlockNoBackoff dbSync 2
  where
    testLabel = "addSimpleChain"

restartDBSync :: IOManager -> [(Text, Text)] -> Assertion
restartDBSync =
    withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
      -- translate the block to a real Cardano block.
      blk <- forgeNext interpreter mockBlock0
      atomically $ addBlock mockServer blk
      -- start db-sync and let it sync
      startDBSync dbSync
      assertBlockNoBackoff dbSync 0

      stopDBSync dbSync
      -- The server sees a separate client here
      startDBSync dbSync
      assertBlockNoBackoff dbSync 0
  where
    testLabel = "restartDBSync"

simpleRollback :: IOManager -> [(Text, Text)] -> Assertion
simpleRollback = do
    withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
      blk0 <- forgeNext interpreter mockBlock0
      blk1 <- forgeNext interpreter mockBlock1
      blk2 <- forgeNext interpreter mockBlock2
      atomically $ addBlock mockServer blk0
      startDBSync dbSync
      atomically $ addBlock mockServer blk1
      atomically $ addBlock mockServer blk2
      assertBlockNoBackoff dbSync 2

      atomically $ rollback mockServer (blockPoint blk1)
      assertBlockNoBackoff dbSync 1
  where
    testLabel = "simpleRollback"

bigChain :: IOManager -> [(Text, Text)] -> Assertion
bigChain =
    withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
      blks <- forM (replicate 101 mockBlock0) (forgeNext interpreter)
      atomically $ forM_ blks $ addBlock mockServer
      startDBSync dbSync
      assertBlockNoBackoff dbSync 100

      blks' <- forM (replicate 100 mockBlock1) (forgeNext interpreter)
      atomically $ forM_ blks' $ addBlock mockServer
      assertBlockNoBackoff dbSync 200

      blks'' <- forM (replicate 5 mockBlock2) (forgeNext interpreter)
      atomically $ forM_ blks'' $ addBlock mockServer
      assertBlockNoBackoff dbSync 205

      atomically $ rollback mockServer (blockPoint $ last blks')
      assertBlockNoBackoff dbSync 200
  where
    testLabel = "bigChain"


restartAndRollback :: IOManager -> [(Text, Text)] -> Assertion
restartAndRollback =
    withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
      blks <- forM (replicate 101 mockBlock0) (forgeNext interpreter)
      atomically $ forM_ blks $ addBlock mockServer
      startDBSync dbSync
      assertBlockNoBackoff dbSync 100

      blks' <- forM (replicate 100 mockBlock0) (forgeNext interpreter)
      atomically $ forM_ blks' $ addBlock mockServer
      assertBlockNoBackoff dbSync 200

      blks'' <- forM (replicate 5 mockBlock2) (forgeNext interpreter)
      atomically $ forM_ blks'' $ addBlock mockServer
      assertBlockNoBackoff dbSync 205

      stopDBSync dbSync
      atomically $ rollback mockServer (blockPoint $ last blks')
      startDBSync dbSync
      assertBlockNoBackoff dbSync 200
  where
    testLabel = "restartAndRollback"

configNoPools :: IOManager -> [(Text, Text)] -> Assertion
configNoPools =
    withFullConfig "config2" testLabel $ \_ _ dbSync -> do
      startDBSync  dbSync
      assertBlocksCount dbSync 2
      assertTxCount dbSync 6
      stopDBSync dbSync
      -- Nothing changes, so polling assertions don't help here
      -- We have to pause and check if anything crashed.
      threadDelay 3_000_000
      assertBlocksCount dbSync 2 -- 2 genesis blocks
      assertTxCount dbSync 6
  where
    testLabel = "configNoPools"


configNoStakes :: IOManager -> [(Text, Text)] -> Assertion
configNoStakes =
    withFullConfig "config3" testLabel $ \interpreter _ dbSync -> do
      startDBSync  dbSync
      assertBlocksCount dbSync 2
      assertTxCount dbSync 7
      stopDBSync dbSync
      startDBSync dbSync
      -- Nothing changes, so polling assertions don't help here
      -- We have to pause and check if anything crashed.
      threadDelay 3_000_000
      assertBlocksCount dbSync 2
      assertTxCount dbSync 7
      -- A pool with no stakes can't create a block.
      eblk <- try $ forgeNext interpreter mockBlock0
      case eblk of
        Right _ -> assertFailure "should fail"
        Left WentTooFar -> pure ()
        -- TODO add an option to disable fingerprint validation for tests like this.
        Left (EmptyFingerprint _) -> pure ()
        Left err -> assertFailure $ "got " <> show err <> " instead of WentTooFar"
  where
    testLabel = "configNoStakes"

addSimpleTx :: IOManager -> [(Text, Text)] -> Assertion
addSimpleTx =
    withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
      -- translate the block to a real Cardano block.
      tx0 <- withAlonzoLedgerState interpreter $  Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000
      blk <- forgeNext interpreter $ MockBlock [TxAlonzo tx0] (NodeId 0)

      atomically $ addBlock mockServer blk
      -- start db-sync and let it sync
      startDBSync  dbSync
      assertBlockNoBackoff dbSync 0
  where
    testLabel = "addSimpleTx"

addSimpleTxShelley :: IOManager -> [(Text, Text)] -> Assertion
addSimpleTxShelley =
    withFullConfig "config-shelley" testLabel $ \interpreter mockServer dbSync -> do
      -- translate the block to a real Cardano block.
      tx0 <- withShelleyLedgerState interpreter $ Shelley.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000
      blk <- forgeNext interpreter $ MockBlock [TxShelley tx0] (NodeId 0)

      atomically $ addBlock mockServer blk
      -- start db-sync and let it sync
      startDBSync  dbSync
      assertBlockNoBackoff dbSync 0
  where
    testLabel = "addSimpleTxShelley"

simpleRewards :: IOManager -> [(Text, Text)] -> Assertion
simpleRewards =
    withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
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

      assertBlockNoBackoff dbSync (6 * 150 + 2)
      assertRewardCount dbSync 17

  where
    testLabel = "simpleRewards"

-- This test is the same as the previous, but in Shelley era. Rewards result
-- should be different because of the old Shelley bug.
-- https://github.com/input-output-hk/cardano-db-sync/issues/959
--
-- The differenece in rewards is triggered when a reward address of a pool A
-- delegates to a pool B and is not an owner of pool B. In this case it receives
-- leader rewards from pool A and member rewards from pool B. In this test, we
-- have 2 instances of this case, one where A = B and one where A /= B.
rewardsShelley :: IOManager -> [(Text, Text)] -> Assertion
rewardsShelley =
    withFullConfig "config-shelley" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
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

      assertBlockNoBackoff dbSync (6 * 150 + 2)
      -- Note we have 2 rewards less compared to Alonzo era
      assertRewardCount dbSync 15

  where
    testLabel = "rewardsShelley"

-- 43200 slots per epoch
-- It takes on average 3 * 20 = 60 slots to create a block from a predefined leader
-- If we skip additionally 156 blocks, we get on average 43200/(60 + 228) = 150 blocks/epoch
fillEpochEqually :: Interpreter -> ServerHandle IO CardanoBlock -> Assertion
fillEpochEqually interpreter mockServer = do
    blks0 <- forM (replicate thirdEpoch mockBlock0) (forgeNextAfter interpreter skipSlots)
    atomically $ forM_ blks0 $ addBlock mockServer

    blks1 <- forM (replicate thirdEpoch mockBlock1) (forgeNextAfter interpreter skipSlots)
    atomically $ forM_ blks1 $ addBlock mockServer

    blks2 <- forM (replicate thirdEpoch mockBlock2) (forgeNextAfter interpreter skipSlots)
    atomically $ forM_ blks2 $ addBlock mockServer
  where
    thirdEpoch = div 150 3
    skipSlots = 228
