{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Mock.Db.Config where

import           Cardano.Prelude (ReaderT, panic, stderr)

import           Control.Concurrent.Async
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TMVar
import           Control.Exception (SomeException, bracket)
import           Control.Monad (void)
import           Control.Monad.Extra (eitherM)
import           Control.Monad.Logger (NoLoggingT)
import           Control.Monad.Trans.Except.Exit (orDie)
import           Control.Monad.Trans.Except.Extra (newExceptT, runExceptT)

import           Control.Tracer (nullTracer)
import           Data.Text (Text)
import qualified Data.Text as Text
import           System.Directory
import           System.FilePath.Posix ((</>))
import           System.IO.Silently

import           Database.Persist.Sql (SqlBackend)

import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import           Ouroboros.Consensus.Shelley.Node (TPraosLeaderCredentials)

import           Cardano.Api
import           Cardano.CLI.Shelley.Commands as CLI
import           Cardano.CLI.Shelley.Run.Genesis as CLI
import           Cardano.Node.Protocol.Shelley (readLeaderCredentials)
import           Cardano.Node.Types (ProtocolFilepaths (..))

import qualified Cardano.Db as Db

import           Cardano.DbSync
import           Cardano.DbSync.Config
import           Cardano.DbSync.Config.Cardano
import           Cardano.DbSync.Config.Types
import           Cardano.DbSync.Error
import           Cardano.DbSync.Types (MetricSetters (..))

import           Cardano.SMASH.Server.PoolDataLayer

import           Cardano.Mock.ChainSync.Server
import           Cardano.Mock.Forging.Interpreter hiding (CardanoBlock)

mkMutableDir :: FilePath -> FilePath -> FilePath
mkMutableDir rootTestDir testLabel = rootTestDir </> "temp" </> testLabel

mkConfigDir :: FilePath -> FilePath -> FilePath
mkConfigDir rootTestDir config = rootTestDir </> config

fingerprintRoot :: FilePath -> FilePath
fingerprintRoot rootTestDir = rootTestDir </> "fingerprint"

mkFingerPrint :: FilePath -> FilePath -> FilePath
mkFingerPrint rootTestDir testLabel = fingerprintRoot rootTestDir </> testLabel

data Config = Config
    { topLevelConfig :: TopLevelConfig CardanoBlock
    , protocolInfo :: Consensus.ProtocolInfo IO CardanoBlock
    , protocolInfoForging :: Consensus.ProtocolInfo IO CardanoBlock
    , syncNodeParams :: SyncNodeParams
    }

data DBSyncEnv = DBSyncEnv
    { dbSyncParams :: SyncNodeParams
    , dbSyncForkDB :: IO (Async ())
    , dbSyncThreadVar :: TMVar (Async ())
    }

mkDBSyncEnv :: SyncNodeParams -> IO () -> IO DBSyncEnv
mkDBSyncEnv params runDBSync = do
  runningVar <- newEmptyTMVarIO
  pure $ DBSyncEnv
    { dbSyncParams = params
    , dbSyncForkDB = async runDBSync
    , dbSyncThreadVar = runningVar
    }

stopDBSync :: DBSyncEnv -> IO ()
stopDBSync env = do
  thr <- atomically $ tryReadTMVar (dbSyncThreadVar env)
  case thr of
    Nothing -> error "Could not cancel db-sync when it's not running"
    Just a -> do
      cancel a
      -- make it empty
      _ <- atomically $ takeTMVar (dbSyncThreadVar env)
      pure ()

stopDBSyncIfRunning :: DBSyncEnv -> IO ()
stopDBSyncIfRunning env = do
  thr <- atomically $ tryReadTMVar (dbSyncThreadVar env)
  case thr of
    Nothing -> pure ()
    Just a -> do
      cancel a
      -- make it empty
      void . atomically $ takeTMVar (dbSyncThreadVar env)
      pure ()

startDBSync :: DBSyncEnv -> IO ()
startDBSync env = do
    thr <- atomically $ tryReadTMVar $ dbSyncThreadVar env
    case thr of
      Just _a -> error "db-sync already running"
      Nothing -> do
        a <- dbSyncForkDB env
        _ <- atomically $ tryPutTMVar (dbSyncThreadVar env) a
        pure ()

pollDBSync :: DBSyncEnv -> IO (Maybe (Either SomeException ()))
pollDBSync env = do
  thr <- atomically $ tryReadTMVar (dbSyncThreadVar env)
  case thr of
    Nothing -> error "Could not poll db-sync when it's not running"
    Just a -> poll a

withDBSyncEnv :: IO DBSyncEnv -> (DBSyncEnv -> IO a) -> IO a
withDBSyncEnv mkEnv action = do
  bracket mkEnv stopDBSyncIfRunning action

getDBSyncPGPass :: DBSyncEnv -> Db.PGPassSource
getDBSyncPGPass = enpPGPassSource . dbSyncParams

queryDBSync :: DBSyncEnv -> ReaderT SqlBackend (NoLoggingT IO) a -> IO a
queryDBSync env = Db.runWithConnectionNoLogging (getDBSyncPGPass env)

getPoolLayer :: DBSyncEnv -> PoolDataLayer
getPoolLayer env = postgresqlPoolDataLayer
                    nullTracer
                    (enpPGPassSource $ dbSyncParams env)

setupTestsDir :: FilePath -> IO ()
setupTestsDir dir = do
    eitherM (panic . textShow) pure $ runExceptT $
      CLI.runGenesisCmd $ GenesisCreateStaked
        (CLI.GenesisDir dir) 3 3 3 3 Nothing (Just 3000000) 3000000 (Testnet $ NetworkMagic 42) 1 3 0

mkConfig :: FilePath -> FilePath -> IO Config
mkConfig staticDir mutableDir = do
    config <- readSyncNodeConfig $ ConfigFile ( staticDir </> "test-db-sync-config.json")
    genCfg <- either (error . Text.unpack . renderSyncNodeError) id <$> runExceptT (readCardanoGenesisConfig config)
    let pInfoDbSync = mkProtocolInfoCardano genCfg []
    creds <- mkShelleyCredentials $ staticDir </> "pools" </> "bulk1.creds"
    let pInfoForger = mkProtocolInfoCardano genCfg creds
    syncPars <- mkSyncNodeParams staticDir mutableDir
    pure $ Config (Consensus.pInfoConfig pInfoDbSync) pInfoDbSync pInfoForger syncPars

mkShelleyCredentials :: FilePath -> IO [TPraosLeaderCredentials StandardCrypto]
mkShelleyCredentials bulkFile = do
    eitherM (panic . textShow) pure $ runExceptT $ readLeaderCredentials (Just protFiles)
  where
    protFiles = ProtocolFilepaths
      { byronCertFile = Nothing
      , byronKeyFile = Nothing
      , shelleyKESFile = Nothing
      , shelleyVRFFile = Nothing
      , shelleyCertFile = Nothing
      , shelleyBulkCredsFile = Just bulkFile
      }

-- | staticDir can be shared by tests running in parallel. mutableDir not.
mkSyncNodeParams :: FilePath -> FilePath -> IO SyncNodeParams
mkSyncNodeParams staticDir mutableDir = do
  pgconfig <- orDie Db.renderPGPassError $ newExceptT Db.readPGPassDefault
  pure $ SyncNodeParams
    { enpConfigFile = ConfigFile $ staticDir </> "test-db-sync-config.json"
    , enpSocketPath = SocketPath $ mutableDir </> ".socket"
    , enpLedgerStateDir = LedgerStateDir $ mutableDir </> "ledger-states"
    , enpMigrationDir = MigrationDir "../schema"
    , enpPGPassSource = Db.PGPassCached pgconfig
    , enpExtended = True
    , enpMaybeRollback = Nothing
    }

emptyMetricsSetters :: MetricSetters
emptyMetricsSetters = MetricSetters
  { metricsSetNodeBlockHeight = \_ -> pure ()
  , metricsSetDbQueueLength = \_ -> pure ()
  , metricsSetDbBlockHeight = \_ -> pure ()
  , metricsSetDbSlotHeight = \_ -> pure ()
  }

withFullConfig :: FilePath -> FilePath -> FilePath
               -> (Interpreter -> ServerHandle IO CardanoBlock -> DBSyncEnv -> IO ())
               -> IOManager -> [(Text, Text)] -> IO ()
withFullConfig rootTestDir config testLabel action iom migr = do
    recreateDir mutableDir
    cfg <- mkConfig configDir mutableDir
    fingerFile <- prepareFingerprintFile rootTestDir testLabel
    let dbsyncParams = syncNodeParams cfg
    -- let trce = nullTracer
    -- Replace with this for better debugging of tests
    trce <- configureLogging dbsyncParams "db-sync-node"
    let dbsyncRun = runDbSync emptyMetricsSetters migr iom trce dbsyncParams True 35 35
    let initSt = Consensus.pInfoInitLedger $ protocolInfo cfg
    withInterpreter (protocolInfoForging cfg) nullTracer fingerFile $ \interpreter -> do
      -- TODO: get 42 from config
      withServerHandle @CardanoBlock iom (topLevelConfig cfg) initSt (NetworkMagic 42)
        (unSocketPath (enpSocketPath $ syncNodeParams cfg))
        $ \mockServer ->
          -- we dont fork dbsync here. Just prepare it as an action
          withDBSyncEnv (mkDBSyncEnv dbsyncParams dbsyncRun) $ \dbSync -> do
            _ <- hSilence [stderr] $ Db.recreateDB (getDBSyncPGPass dbSync)
            action interpreter mockServer dbSync
  where
    configDir = mkConfigDir rootTestDir config
    mutableDir =  mkMutableDir rootTestDir testLabel

-- Same as 'withFullConfig' but doesn't use the bracket style. Can be used with
-- 'cleanFullConfig' to cleanup the env.
mkFullConfig :: FilePath -> FilePath -> FilePath
             -> IOManager -> [(Text, Text)]
             -> IO (Interpreter, ServerHandle IO CardanoBlock, DBSyncEnv)
mkFullConfig rootTestDir config testLabel iom migr = do
    recreateDir mutableDir
    cfg <- mkConfig configDir mutableDir
    fingerFile <- prepareFingerprintFile rootTestDir testLabel
    let dbsyncParams = syncNodeParams cfg
    let trce = nullTracer
    -- Replace with this for better debugging of tests
    -- trce <- configureLogging dbsyncParams "db-sync-node"
    let dbsyncRun = runDbSync emptyMetricsSetters migr iom trce dbsyncParams True 35 35
    let initSt = Consensus.pInfoInitLedger $ protocolInfo cfg
    interpreter <- initInterpreter (protocolInfoForging cfg) nullTracer fingerFile
    serverHandle <- forkServerThread @CardanoBlock iom (topLevelConfig cfg) initSt (NetworkMagic 42)
                      (unSocketPath (enpSocketPath $ syncNodeParams cfg))
    dbSync <- mkDBSyncEnv dbsyncParams dbsyncRun
    _ <- hSilence [stderr] $ Db.recreateDB (getDBSyncPGPass dbSync)
    pure (interpreter, serverHandle, dbSync)
  where
    configDir = mkConfigDir rootTestDir config
    mutableDir =  mkMutableDir rootTestDir testLabel

cleanFullConfig :: (Interpreter, ServerHandle IO CardanoBlock, DBSyncEnv)
                -> IO ()
cleanFullConfig (interpreter, serverHandle, dbSync) = do
    stopDBSyncIfRunning dbSync
    stopServer serverHandle
    finalizeFingerprint interpreter

prepareFingerprintFile :: FilePath -> FilePath -> IO FilePath
prepareFingerprintFile rootTestDir testLabel = do
    createDirectoryIfMissing True (fingerprintRoot rootTestDir)
    pure fingerprintFile
  where
    fingerprintFile = mkFingerPrint rootTestDir testLabel

recreateDir :: FilePath -> IO ()
recreateDir path = do
  removePathForcibly path
  createDirectoryIfMissing True path

textShow :: Show a => a -> Text
textShow = Text.pack . show
