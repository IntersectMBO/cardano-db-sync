{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Db.Mock.Unit where

import           Cardano.Prelude (throwIO)


import           Control.Tracer (nullTracer)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Class.MonadSTM.Strict
import           Data.Text (Text)
import qualified Data.Text as Text
import           System.Directory
import           System.FilePath hiding (isValid)

import           Cardano.Ledger.Slot (BlockNo (..))

import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus

import           Ouroboros.Network.Block (blockNo, blockPoint)
import           Ouroboros.Network.Magic

import           Cardano.DbSync (runDbSyncNode)
import           Cardano.DbSync.Config.Types hiding (CardanoBlock)

import           Cardano.Mock.ChainSync.Server
import           Cardano.Mock.Forging.Interpreter

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (assertBool, testCase)

import           Test.Cardano.Db.Mock.Config
import           Test.Cardano.Db.Mock.Examples
import           Test.Cardano.Db.Mock.Validate

unitTests :: IOManager -> [(Text, Text)] -> TestTree
unitTests iom knownMigrations =
  testGroup "unit tests"
    [ testCase "Forge some blocks" forgeBlocks
    , testCase "Add one Simple block" (simpleRollback iom knownMigrations)
    ]

rootTestDir :: FilePath
rootTestDir = "test/testfiles"

configDir ::  FilePath
configDir = rootTestDir </> "config"

forgeBlocks :: IO ()
forgeBlocks = do
    let testDir = rootTestDir </> "temp/forgeBlocks"
    recreateDir testDir
    cfg <- mkConfig configDir testDir
    interpreter <- initInterpreter (protocolInfoForging cfg) nullTracer
    _block0 <- forgeNext interpreter mockBlock0
    _block1 <- forgeNext interpreter mockBlock1
    block2 <- forgeNext interpreter mockBlock2
    let blkNo = blockNo block2
    assertBool (show blkNo <> " /= " <> "2")
      $ blkNo == BlockNo 2

addSimple :: IOManager -> [(Text, Text)] -> IO ()
addSimple iom knownMigrations = do
    let testDir = rootTestDir </> "temp/addSimple"
    recreateDir testDir
      -- create all keys, configs and genesis files from a template
      -- Normally these will be already hard-coded.
    when False $
      setupTestsDir testDir
    -- create the configuration for dbsync and the interpreter
    cfg <- mkConfig configDir testDir
    -- initiate the interpreter
    interpreter <- initInterpreter (protocolInfoForging cfg) nullTracer
    -- translate the block to a real Cardano block.
    blk <- forgeNext interpreter mockBlock0
    let initSt = Consensus.pInfoInitLedger $ protocolInfo cfg
    -- fork the mocked chainsync server
    mockServer <- liftIO $ forkServerThread @CardanoBlock iom (topLevelConfig cfg) initSt (NetworkMagic 42) $ unSocketPath (enpSocketPath $ syncNodeParams cfg)
    atomically $ addBlock mockServer blk
    -- start db-sync and let it sync
    dbSync <- liftIO $ async $ runDbSyncNode emptyMetricsSetters True knownMigrations (syncNodeParams cfg)
    assertBlockNoBackoff 0

addSimpleChain :: IOManager -> [(Text, Text)] -> IO ()
addSimpleChain iom knownMigrations = do
    let testDir = rootTestDir </> "temp/addSimpleChain"
    recreateDir testDir
      -- create all keys, configs and genesis files from a template
      -- Normally these will be already hard-coded.
    when False $
      setupTestsDir testDir
    -- create the configuration for dbsync and the interpreter
    cfg <- mkConfig configDir testDir
    -- initiate the interpreter
    interpreter <- initInterpreter (protocolInfoForging cfg) nullTracer
    -- translate the block to a real Cardano block.
    blk0 <- forgeNext interpreter mockBlock0
    blk1 <- forgeNext interpreter mockBlock1
    blk2 <- forgeNext interpreter mockBlock2
    let initSt = Consensus.pInfoInitLedger $ protocolInfo cfg
    -- fork the mocked chainsync server
    mockServer <- liftIO $ forkServerThread @CardanoBlock iom (topLevelConfig cfg) initSt (NetworkMagic 42) $ unSocketPath (enpSocketPath $ syncNodeParams cfg)
    atomically $ addBlock mockServer blk0
    -- start db-sync and let it sync
    dbSync <- liftIO $ async $ runDbSyncNode emptyMetricsSetters True knownMigrations (syncNodeParams cfg)
    atomically $ addBlock mockServer blk1
    atomically $ addBlock mockServer blk2
    assertBlockNoBackoff 2

restartDBSync :: IOManager -> [(Text, Text)] -> IO ()
restartDBSync iom knownMigrations = do
    let testDir = rootTestDir </> "temp/restartDBSync"
    recreateDir testDir
      -- create all keys, configs and genesis files from a template
      -- Normally these will be already hard-coded.
    when False $
      setupTestsDir testDir
    -- create the configuration for dbsync and the interpreter
    cfg <- mkConfig configDir testDir
    -- initiate the interpreter
    interpreter <- initInterpreter (protocolInfoForging cfg) nullTracer
    -- translate the block to a real Cardano block.
    blk <- forgeNext interpreter mockBlock0
    -- fork the mocked chainsync server
    let initSt = Consensus.pInfoInitLedger $ protocolInfo cfg
    mockServer <- liftIO $ forkServerThread @CardanoBlock iom (topLevelConfig cfg) initSt (NetworkMagic 42) $ unSocketPath (enpSocketPath $ syncNodeParams cfg)
    atomically $ addBlock mockServer blk
    -- start db-sync and let it sync
    dbSync <- async $ runDbSyncNode emptyMetricsSetters True knownMigrations (syncNodeParams cfg)
    assertBlockNoBackoff 0
    cancel dbSync
    _dbSync' <- async $ runDbSyncNode emptyMetricsSetters True knownMigrations (syncNodeParams cfg)
    assertBlockNoBackoff 0

simpleRollback :: IOManager -> [(Text, Text)] -> IO ()
simpleRollback iom knownMigrations = do
    let testDir = rootTestDir </> "temp/simpleRollback"
    recreateDir testDir
      -- create all keys, configs and genesis files from a template
      -- Normally these will be already hard-coded.
    when False $
      setupTestsDir testDir
    -- create the configuration for dbsync and the interpreter
    cfg <- mkConfig configDir testDir
    -- initiate the interpreter
    interpreter <- initInterpreter (protocolInfoForging cfg) nullTracer
    -- translate the block to a real Cardano block.
    blk0 <- forgeNext interpreter mockBlock0
    blk1 <- forgeNext interpreter mockBlock1
    blk2 <- forgeNext interpreter mockBlock2
    let initSt = Consensus.pInfoInitLedger $ protocolInfo cfg
    -- fork the mocked chainsync server
    mockServer <- forkServerThread @CardanoBlock iom (topLevelConfig cfg) initSt (NetworkMagic 42) $ unSocketPath (enpSocketPath $ syncNodeParams cfg)
    atomically $ addBlock mockServer blk0
    -- start db-sync and let it sync
    _ <- async $ runDbSyncNode emptyMetricsSetters True knownMigrations (syncNodeParams cfg)
    atomically $ addBlock mockServer blk1
    atomically $ addBlock mockServer blk2
    assertBlockNoBackoff 2
    atomically $ rollback mockServer (blockPoint blk1)
    assertBlockNoBackoff 1

bigChain :: IOManager -> [(Text, Text)] -> IO ()
bigChain iom knownMigrations = do
    let testDir = rootTestDir </> "temp/bigChain"
    recreateDir testDir
    cfg <- mkConfig configDir testDir
    interpreter <- initInterpreter (protocolInfoForging cfg) nullTracer
    let initSt = Consensus.pInfoInitLedger $ protocolInfo cfg
    mockServer <- forkServerThread @CardanoBlock iom (topLevelConfig cfg) initSt (NetworkMagic 42) $ unSocketPath (enpSocketPath $ syncNodeParams cfg)
    blks <- forM (take 101 $ repeat mockBlock0) (forgeNext interpreter)
    atomically $ forM_ blks $ addBlock mockServer
    _ <- async $ runDbSyncNode emptyMetricsSetters True knownMigrations (syncNodeParams cfg)
    assertBlockNoBackoff 100
    blks' <- forM (take 100 $ repeat mockBlock1) (forgeNext interpreter)
    atomically $ forM_ blks' $ addBlock mockServer
    assertBlockNoBackoff 200
    blks'' <- forM (take 5 $ repeat mockBlock2) (forgeNext interpreter)
    atomically $ forM_ blks'' $ addBlock mockServer
    assertBlockNoBackoff 205
    atomically $ rollback mockServer (blockPoint $ last blks')
    assertBlockNoBackoff 200


bigChainRestart :: IOManager -> [(Text, Text)] -> IO ()
bigChainRestart iom knownMigrations = do
    let testDir = rootTestDir </> "temp/bigChainRestart"
    recreateDir testDir
    cfg <- mkConfig configDir testDir
    interpreter <- initInterpreter (protocolInfoForging cfg) nullTracer
    let initSt = Consensus.pInfoInitLedger $ protocolInfo cfg
    mockServer <- forkServerThread @CardanoBlock iom (topLevelConfig cfg) initSt (NetworkMagic 42) $ unSocketPath (enpSocketPath $ syncNodeParams cfg)
    blks <- forM (take 101 $ repeat mockBlock0) (forgeNext interpreter)
    atomically $ forM_ blks $ addBlock mockServer
    dbSync <- async $ runDbSyncNode emptyMetricsSetters True knownMigrations (syncNodeParams cfg)
    assertBlockNoBackoff 100
    blks' <- forM (take 100 $ repeat mockBlock1) (forgeNext interpreter)
    atomically $ forM_ blks' $ addBlock mockServer
    assertBlockNoBackoff 200
    blks'' <- forM (take 5 $ repeat mockBlock2) (forgeNext interpreter)
    atomically $ forM_ blks'' $ addBlock mockServer
    assertBlockNoBackoff 205
    cancel dbSync
    atomically $ rollback mockServer (blockPoint $ last blks')
    _ <- async $ runDbSyncNode emptyMetricsSetters True knownMigrations (syncNodeParams cfg)
    assertBlockNoBackoff 200

recreateDir :: FilePath -> IO ()
recreateDir path = do
  removePathForcibly path
  createDirectoryIfMissing True path
