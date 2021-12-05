{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Db.Mock.Config where

import           Cardano.Prelude (panic)

import           Control.Concurrent.Async
import           Control.Monad.Extra (eitherM)
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Tracer (nullTracer)
import           Data.Text (Text)
import qualified Data.Text as Text
import           System.Directory
import           System.FilePath.Posix (takeFileName , (</>))

import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import           Ouroboros.Consensus.Shelley.Node (TPraosLeaderCredentials)

import           Cardano.Api
import           Cardano.CLI.Shelley.Commands as CLI
import           Cardano.CLI.Shelley.Run.Genesis as CLI
import           Cardano.Node.Protocol.Shelley (readLeaderCredentials)
import           Cardano.Node.Types (ProtocolFilepaths (..))

import           Cardano.DbSync
import           Cardano.DbSync.Config
import           Cardano.DbSync.Config.Cardano
import           Cardano.DbSync.Config.Types
import           Cardano.DbSync.Error
import           Cardano.DbSync.Types (MetricSetters (..))

import           Cardano.Mock.ChainSync.Server
import           Cardano.Mock.Forging.Interpreter hiding (CardanoBlock)

data Config = Config
    { topLevelConfig :: TopLevelConfig CardanoBlock
    , protocolInfo :: Consensus.ProtocolInfo IO CardanoBlock
    , protocolInfoForging :: Consensus.ProtocolInfo IO CardanoBlock
    , syncNodeParams :: SyncNodeParams
    }

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
    let syncParams = mkSyncNodeParams staticDir mutableDir
    pure $ Config (Consensus.pInfoConfig pInfoDbSync) pInfoDbSync pInfoForger syncParams

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
mkSyncNodeParams :: FilePath -> FilePath -> SyncNodeParams
mkSyncNodeParams staticDir mutableDir = SyncNodeParams
  { enpConfigFile = ConfigFile $ staticDir </> "test-db-sync-config.json"
  , enpSocketPath = SocketPath $ mutableDir </> ".socket"
  , enpLedgerStateDir = LedgerStateDir $ mutableDir </> "ledger-states"
  , enpMigrationDir = MigrationDir "../schema"
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

withFullConfig :: FilePath -> FilePath
               -> (Interpreter -> ServerHandle IO CardanoBlock -> IO (Async ()) -> IO ())
               -> IOManager -> [(Text, Text)] -> IO ()
withFullConfig staticDir mutableDir action iom migr = do
  recreateDir mutableDir
  cfg <- mkConfig staticDir mutableDir
  fingerFile <- prepareFingerprintFile staticDir mutableDir
  withInterpreter (protocolInfoForging cfg) nullTracer fingerFile $ \interpreter -> do
    let initSt = Consensus.pInfoInitLedger $ protocolInfo cfg
    -- TODO: get 42 from config
    mockServer <- forkServerThread @CardanoBlock iom (topLevelConfig cfg) initSt (NetworkMagic 42) $ unSocketPath (enpSocketPath $ syncNodeParams cfg)
    -- we dont fork dbsync here. Just prepare it as an action
    let dbSync = async $ runDbSyncNode emptyMetricsSetters True migr (syncNodeParams cfg)
    action interpreter mockServer dbSync

prepareFingerprintFile :: FilePath -> FilePath -> IO FilePath
prepareFingerprintFile staticDir mutableDir = do
  let testLabel = takeFileName mutableDir
  let dir = staticDir </> "fingerprints"
  createDirectoryIfMissing True dir
  pure $ dir </> testLabel

recreateDir :: FilePath -> IO ()
recreateDir path = do
  removePathForcibly path
  createDirectoryIfMissing True path

textShow :: Show a => a -> Text
textShow = Text.pack . show
