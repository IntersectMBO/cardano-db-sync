{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Db.Mock.Config
  ( Config (..)
  , DBSyncEnv (..)
  , emptyMetricsSetters
  , fingerprintRoot
  , getDBSyncPGPass
  , getPoolLayer
  , mkConfig
  , mkConfigDir
  , mkFingerPrint
  , mkMutableDir
  , mkDBSyncEnv
  , mkShelleyCredentials
  , mkSyncNodeParams
  , pollDBSync
  , prepareFingerprintFile
  , queryDBSync
  , recreateDir
  , rootTestDir
  , setupTestsDir
  , stopDBSync
  , stopDBSyncIfRunning
  , startDBSync
  , withDBSyncEnv
  , withFullConfig
  ) where

import           Cardano.Prelude (ReaderT, panic, stderr)

import qualified Cardano.Db as Db

import           Cardano.DbSync
import           Cardano.DbSync.Config
import           Cardano.DbSync.Config.Cardano
import           Cardano.DbSync.Error
import           Cardano.DbSync.Types (CardanoBlock, MetricSetters (..))

import           Cardano.Mock.ChainSync.Server
import           Cardano.Mock.Forging.Interpreter

import           Cardano.SMASH.Server.PoolDataLayer

import           Control.Concurrent.Async (Async, async, cancel, poll)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, takeTMVar, tryPutTMVar,
                   tryReadTMVar)
import           Control.Exception (SomeException, bracket)
import           Control.Monad (void)
import           Control.Monad.Extra (eitherM)
import           Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import           Control.Monad.Trans.Except.Exit (orDie)
import           Control.Monad.Trans.Except.Extra (newExceptT, runExceptT)

import           Control.Tracer (nullTracer)
import           Data.Text (Text)
import qualified Data.Text as Text
import           System.Directory (createDirectoryIfMissing, removePathForcibly)
import           System.FilePath.Posix ((</>))
import           System.IO.Silently (hSilence)

import           Database.Persist.Postgresql (createPostgresqlPool)
import           Database.Persist.Sql (SqlBackend)

import           Ouroboros.Consensus.Config (TopLevelConfig)
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import           Ouroboros.Consensus.Shelley.Node (ShelleyLeaderCredentials)

import           Cardano.Api (NetworkId (..), NetworkMagic (..))
import           Cardano.CLI.Shelley.Commands (GenesisCmd (..))
import qualified Cardano.CLI.Shelley.Commands as CLI
import qualified Cardano.CLI.Shelley.Run.Genesis as CLI
import           Cardano.Node.Protocol.Shelley (readLeaderCredentials)
import           Cardano.Node.Types (ProtocolFilepaths (..))


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

rootTestDir :: FilePath
rootTestDir = "test/testfiles"

mkMutableDir :: FilePath -> FilePath
mkMutableDir testLabel = rootTestDir </> "temp" </> testLabel

mkConfigDir :: FilePath -> FilePath
mkConfigDir config = rootTestDir </> config

fingerprintRoot :: FilePath
fingerprintRoot = rootTestDir </> "fingerprint"

mkFingerPrint :: FilePath -> FilePath
mkFingerPrint testLabel = fingerprintRoot </> testLabel

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
      void . atomically $ takeTMVar (dbSyncThreadVar env)
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

startDBSync :: DBSyncEnv -> IO ()
startDBSync env = do
  thr <- atomically $ tryReadTMVar (dbSyncThreadVar env)
  case thr of
    Just _a -> error "db-sync already running"
    Nothing -> do
      a <- dbSyncForkDB env
      void . atomically $ tryPutTMVar (dbSyncThreadVar env) a

pollDBSync :: DBSyncEnv -> IO (Maybe (Either SomeException ()))
pollDBSync env = do
  thr <- atomically $ tryReadTMVar (dbSyncThreadVar env)
  case thr of
    Nothing -> error "Could not poll db-sync when it's not running"
    Just a -> poll a

withDBSyncEnv :: IO DBSyncEnv -> (DBSyncEnv -> IO a) -> IO a
withDBSyncEnv mkEnv = bracket mkEnv stopDBSyncIfRunning

getDBSyncPGPass :: DBSyncEnv -> Db.PGPassSource
getDBSyncPGPass = enpPGPassSource . dbSyncParams

queryDBSync :: DBSyncEnv -> ReaderT SqlBackend (NoLoggingT IO) a -> IO a
queryDBSync env = Db.runWithConnectionNoLogging (getDBSyncPGPass env)

getPoolLayer :: DBSyncEnv -> IO PoolDataLayer
getPoolLayer env = do
  pgconfig <- orDie Db.renderPGPassError $ newExceptT $ Db.readPGPass (enpPGPassSource $ dbSyncParams env)
  pool <- runNoLoggingT $ createPostgresqlPool (Db.toConnectionString pgconfig) 1 -- Pool size of 1 for tests
  pure $ postgresqlPoolDataLayer
                    nullTracer
                    pool

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

mkShelleyCredentials :: FilePath -> IO [ShelleyLeaderCredentials StandardCrypto]
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
    , enpHasCache = True
    , enpHasLedger = True
    , enpMaybeRollback = Nothing
    }

emptyMetricsSetters :: MetricSetters
emptyMetricsSetters =
  MetricSetters
    { metricsSetNodeBlockHeight = \_ -> pure ()
    , metricsSetDbQueueLength = \_ -> pure ()
    , metricsSetDbBlockHeight = \_ -> pure ()
    , metricsSetDbSlotHeight = \_ -> pure ()
    }

withFullConfig
    :: FilePath -> FilePath
    -> (Interpreter -> ServerHandle IO CardanoBlock -> DBSyncEnv -> IO ())
    -> IOManager -> [(Text, Text)] -> IO ()
withFullConfig config testLabel action iom migr = do
    recreateDir mutableDir
    cfg <- mkConfig configDir mutableDir
    fingerFile <- prepareFingerprintFile testLabel
    let dbsyncParams = syncNodeParams cfg
    -- Set to True to disable logging, False to enable it.
    trce <- if True
              then pure nullTracer
              else configureLogging dbsyncParams "db-sync-node"
    let dbsyncRun = runDbSync emptyMetricsSetters migr iom trce dbsyncParams True 35 35
    let initSt = Consensus.pInfoInitLedger $ protocolInfo cfg
    withInterpreter (protocolInfoForging cfg) nullTracer fingerFile $ \interpreter -> do
      -- TODO: get 42 from config
      withServerHandle @CardanoBlock iom (topLevelConfig cfg) initSt (NetworkMagic 42)
        (unSocketPath (enpSocketPath $ syncNodeParams cfg))
        $ \mockServer ->
          -- we dont fork dbsync here. Just prepare it as an action
          withDBSyncEnv (mkDBSyncEnv dbsyncParams dbsyncRun) $ \dbSync -> do
            void . hSilence [stderr] $ Db.recreateDB (getDBSyncPGPass dbSync)
            action interpreter mockServer dbSync
  where
    configDir = mkConfigDir config
    mutableDir =  mkMutableDir testLabel

prepareFingerprintFile :: FilePath -> IO FilePath
prepareFingerprintFile testLabel = do
    createDirectoryIfMissing True fingerprintRoot
    pure fingerprintFile
  where
    fingerprintFile = mkFingerPrint testLabel

recreateDir :: FilePath -> IO ()
recreateDir path = do
  removePathForcibly path
  createDirectoryIfMissing True path

textShow :: Show a => a -> Text
textShow = Text.pack . show
