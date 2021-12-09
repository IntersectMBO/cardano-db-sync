{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Db.Mock.Config where

import           Cardano.Prelude (ReaderT, stderr, panic)

import           Control.Concurrent.Async
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TMVar
import           Control.Exception (SomeException)
import           Control.Monad.Extra (eitherM)
import           Control.Monad.Logger (NoLoggingT)
import           Control.Monad.Trans.Except (runExceptT)
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

import qualified Cardano.Db as DB

import           Cardano.DbSync
import           Cardano.DbSync.Config
import           Cardano.DbSync.Config.Cardano
import           Cardano.DbSync.Config.Types
import           Cardano.DbSync.Error
import           Cardano.DbSync.Types (MetricSetters (..))

import           Cardano.Mock.ChainSync.Server
import           Cardano.Mock.Forging.Interpreter hiding (CardanoBlock)

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
  runningVar <- atomically newEmptyTMVar
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

getDBSyncPGPass :: DBSyncEnv -> DB.PGPassSource
getDBSyncPGPass = enpPGPassSource . dbSyncParams

queryDBSync :: DBSyncEnv -> ReaderT SqlBackend (NoLoggingT IO) a -> IO a
queryDBSync env q = DB.runWithConnectionNoLogging (getDBSyncPGPass env) q

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
  Just pgconfig <- DB.parsePGConfig $ "/var/run/postgresql:5432:" <> dbname <> ":*:*"
  pure $ SyncNodeParams
    { enpConfigFile = ConfigFile $ staticDir </> "test-db-sync-config.json"
    , enpSocketPath = SocketPath $ mutableDir </> ".socket"
    , enpLedgerStateDir = LedgerStateDir $ mutableDir </> "ledger-states"
    , enpMigrationDir = MigrationDir "../schema"
    , enpPGPassSource = DB.PGPassCached pgconfig
    , enpExtended = True
    , enpMaybeRollback = Nothing
    }
  where
    -- TODO: Use this to have parallem tests
    -- <> (Text.encodeUtf8 $ Text.pack testLabel)
    dbname = "testing"

emptyMetricsSetters :: MetricSetters
emptyMetricsSetters = MetricSetters
  { metricsSetNodeBlockHeight = \_ -> pure ()
  , metricsSetDbQueueLength = \_ -> pure ()
  , metricsSetDbBlockHeight = \_ -> pure ()
  , metricsSetDbSlotHeight = \_ -> pure ()
  }

withFullConfig :: FilePath -> FilePath
               -> (Interpreter -> ServerHandle IO CardanoBlock -> DBSyncEnv -> IO ())
               -> IOManager -> [(Text, Text)] -> IO ()
withFullConfig config testLabel action iom migr = do
    recreateDir mutableDir
    cfg <- mkConfig configDir mutableDir
    fingerFile <- prepareFingerprintFile testLabel
    withInterpreter (protocolInfoForging cfg) nullTracer fingerFile $ \interpreter -> do
      let initSt = Consensus.pInfoInitLedger $ protocolInfo cfg
      -- TODO: get 42 from config
      mockServer <- forkServerThread @CardanoBlock iom (topLevelConfig cfg) initSt (NetworkMagic 42) $ unSocketPath (enpSocketPath $ syncNodeParams cfg)
      -- we dont fork dbsync here. Just prepare it as an action
      let dbsyncParams = syncNodeParams cfg
          dbsyncRun = runDbSync emptyMetricsSetters migr iom nullTracer dbsyncParams True 100 100
      dbSync <- mkDBSyncEnv dbsyncParams dbsyncRun
      _ <- hSilence [stderr] $ DB.recreateDB (getDBSyncPGPass dbSync)
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
