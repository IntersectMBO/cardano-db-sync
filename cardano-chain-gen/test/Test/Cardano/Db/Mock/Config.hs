{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Db.Mock.Config (
  Config (..),
  DBSyncEnv (..),
  CommandLineArgs (..),
  WithConfigArgs (..),
  initCommandLineArgs,
  babbageConfigDir,
  conwayConfigDir,
  alonzoConfigDir,
  emptyMetricsSetters,
  fingerprintRoot,
  getDBSyncPGPass,
  getPoolLayer,
  mkConfig,
  mkSyncNodeConfig,
  mkConfigDir,
  mkFingerPrint,
  mkMutableDir,
  mkDBSyncEnv,
  mkShelleyCredentials,
  mkSyncNodeParams,
  pollDBSync,
  prepareFingerprintFile,
  queryDBSync,
  recreateDir,
  rootTestDir,
  stopDBSync,
  stopDBSyncIfRunning,
  startDBSync,
  withDBSyncEnv,
  withFullConfig,
  withFullConfigAndDropDB,
  withFullConfigAndLogs,
  withCustomConfig,
  withCustomConfigAndDropDB,
  withCustomConfigAndLogs,
  withFullConfig',
) where

import Cardano.Api (NetworkMagic (..))
import qualified Cardano.Db as Db
import Cardano.DbSync
import Cardano.DbSync.Config
import Cardano.DbSync.Config.Cardano
import Cardano.DbSync.Error (runOrThrowIO)
import Cardano.DbSync.Types (CardanoBlock, MetricSetters (..))
import Cardano.Mock.ChainSync.Server
import Cardano.Mock.Forging.Interpreter
import Cardano.Node.Protocol.Shelley (readLeaderCredentials)
import Cardano.Node.Types (ProtocolFilepaths (..))
import Cardano.Prelude (ReaderT, panic, stderr)
import Cardano.SMASH.Server.PoolDataLayer
import Control.Concurrent.Async (Async, async, cancel, poll)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (
  TMVar,
  newEmptyTMVarIO,
  takeTMVar,
  tryPutTMVar,
  tryReadTMVar,
 )
import Control.Exception (SomeException, bracket)
import Control.Monad (void)
import Control.Monad.Extra (eitherM)
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Control.Monad.Trans.Except.Extra (runExceptT)
import Control.Tracer (nullTracer)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (SqlBackend)
import Ouroboros.Consensus.Block.Forging
import Ouroboros.Consensus.Config (TopLevelConfig)
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import Ouroboros.Consensus.Shelley.Node (ShelleyLeaderCredentials)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath.Posix ((</>))
import System.IO.Silently (hSilence)

data Config = Config
  { topLevelConfig :: TopLevelConfig CardanoBlock
  , protocolInfo :: Consensus.ProtocolInfo CardanoBlock
  , protocolInfoForging :: Consensus.ProtocolInfo CardanoBlock
  , protocolInfoForger :: [BlockForging IO CardanoBlock]
  , syncNodeParams :: SyncNodeParams
  }

data DBSyncEnv = DBSyncEnv
  { dbSyncParams :: SyncNodeParams
  , partialRunDbSync :: SyncNodeParams -> IO ()
  , dbSyncThreadVar :: TMVar (Async ())
  }

data CommandLineArgs = CommandLineArgs
  { claHasConfigFile :: Bool
  , claEpochDisabled :: Bool
  , claHasCache :: Bool
  , claShouldUseLedger :: Bool
  , claSkipFix :: Bool
  , claOnlyFix :: Bool
  , claForceIndexes :: Bool
  , claHasMultiAssets :: Bool
  , claHasMetadata :: Bool
  , claHasPlutusExtra :: Bool
  , claHasOffChainData :: Bool
  , claTurboMode :: Bool
  , claFullMode :: Bool
  , claMigrateConsumed :: Bool
  , claPruneTxOut :: Bool
  }

data WithConfigArgs = WithConfigArgs
  { hasFingerprint :: Bool
  , shouldLog :: Bool
  , shouldDropDB :: Bool
  }

babbageConfigDir :: FilePath
babbageConfigDir = "config"

conwayConfigDir :: FilePath
conwayConfigDir = "config-conway"

alonzoConfigDir :: FilePath
alonzoConfigDir = "config-alonzo"

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

mkDBSyncEnv :: SyncNodeParams -> (SyncNodeParams -> IO ()) -> IO DBSyncEnv
mkDBSyncEnv params pRunDbSync = do
  runningVar <- newEmptyTMVarIO
  pure $
    DBSyncEnv
      { dbSyncParams = params
      , partialRunDbSync = pRunDbSync
      , dbSyncThreadVar = runningVar
      }

stopDBSync :: DBSyncEnv -> IO ()
stopDBSync env = do
  thr <- atomically $ tryReadTMVar (dbSyncThreadVar env)
  case thr of
    Nothing -> error "Could not cancel db-sync when it's not running"
    Just tmVar -> do
      cancel tmVar
      -- make it empty
      void . atomically $ takeTMVar (dbSyncThreadVar env)
      pure ()

stopDBSyncIfRunning :: DBSyncEnv -> IO ()
stopDBSyncIfRunning env = do
  thr <- atomically $ tryReadTMVar (dbSyncThreadVar env)
  case thr of
    Nothing -> pure ()
    Just tmVar -> do
      cancel tmVar
      -- make it empty
      void . atomically $ takeTMVar (dbSyncThreadVar env)

startDBSync :: DBSyncEnv -> IO ()
startDBSync env = do
  thr <- atomically $ tryReadTMVar (dbSyncThreadVar env)
  case thr of
    Just _a -> error "db-sync already running"
    Nothing -> do
      let appliedRunDbSync = partialRunDbSync env $ dbSyncParams env
      -- we async the fully applied runDbSync here ad put it into the thread
      asyncApplied <- async appliedRunDbSync
      void . atomically $ tryPutTMVar (dbSyncThreadVar env) asyncApplied

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
  pgconfig <- runOrThrowIO $ Db.readPGPass (enpPGPassSource $ dbSyncParams env)
  pool <- runNoLoggingT $ createPostgresqlPool (Db.toConnectionString pgconfig) 1 -- Pool size of 1 for tests
  pure $
    postgresqlPoolDataLayer
      nullTracer
      pool

mkConfig :: FilePath -> FilePath -> CommandLineArgs -> SyncNodeConfig -> IO Config
mkConfig staticDir mutableDir cmdLineArgs config = do
  genCfg <- runOrThrowIO $ runExceptT (readCardanoGenesisConfig config)
  let (pInfoDbSync, _) = mkProtocolInfoCardano genCfg []
  creds <- mkShelleyCredentials $ staticDir </> "pools" </> "bulk1.creds"
  let (pInfoForger, forging) = mkProtocolInfoCardano genCfg creds
  forging' <- forging
  syncPars <- mkSyncNodeParams staticDir mutableDir cmdLineArgs
  pure $ Config (Consensus.pInfoConfig pInfoDbSync) pInfoDbSync pInfoForger forging' syncPars

mkSyncNodeConfig :: FilePath -> IO SyncNodeConfig
mkSyncNodeConfig staticDir =
  readSyncNodeConfig $ ConfigFile (staticDir </> "test-db-sync-config.json")

mkShelleyCredentials :: FilePath -> IO [ShelleyLeaderCredentials StandardCrypto]
mkShelleyCredentials bulkFile = do
  eitherM (panic . textShow) pure $ runExceptT $ readLeaderCredentials (Just protFiles)
  where
    protFiles =
      ProtocolFilepaths
        { byronCertFile = Nothing
        , byronKeyFile = Nothing
        , shelleyKESFile = Nothing
        , shelleyVRFFile = Nothing
        , shelleyCertFile = Nothing
        , shelleyBulkCredsFile = Just bulkFile
        }

-- | staticDir can be shared by tests running in parallel. mutableDir not.
mkSyncNodeParams :: FilePath -> FilePath -> CommandLineArgs -> IO SyncNodeParams
mkSyncNodeParams staticDir mutableDir CommandLineArgs {..} = do
  pgconfig <- runOrThrowIO Db.readPGPassDefault
  pure $
    SyncNodeParams
      { enpConfigFile = ConfigFile $ staticDir </> (if claHasConfigFile then "test-db-sync-config.json" else "")
      , enpSocketPath = SocketPath $ mutableDir </> ".socket"
      , enpMaybeLedgerStateDir = Just $ LedgerStateDir $ mutableDir </> "ledger-states"
      , enpMigrationDir = MigrationDir "../schema"
      , enpPGPassSource = Db.PGPassCached pgconfig
      , enpEpochDisabled = claEpochDisabled
      , enpHasCache = claHasCache
      , enpShouldUseLedger = claShouldUseLedger
      , enpSkipFix = claSkipFix
      , enpOnlyFix = claOnlyFix
      , enpForceIndexes = claForceIndexes
      , enpHasMultiAssets = claHasMultiAssets
      , enpHasMetadata = claHasMetadata
      , enpHasPlutusExtra = True
      , enpHasOffChainPoolData = True
      , enpTurboMode = False
      , enpFullMode = True
      , enpMigrateConsumed = claMigrateConsumed
      , enpPruneTxOut = claPruneTxOut
      , enpSnEveryFollowing = 35
      , enpSnEveryLagging = 35
      , enpMaybeRollback = Nothing
      }

initCommandLineArgs :: CommandLineArgs
initCommandLineArgs =
  CommandLineArgs
    { claHasConfigFile = True
    , claEpochDisabled = True
    , claHasCache = True
    , claShouldUseLedger = True
    , claSkipFix = True
    , claOnlyFix = False
    , claForceIndexes = False
    , claHasMultiAssets = True
    , claHasMetadata = True
    , claHasPlutusExtra = True
    , claHasOffChainData = True
    , claTurboMode = False
    , claFullMode = True
    , claMigrateConsumed = False
    , claPruneTxOut = False
    }

emptyMetricsSetters :: MetricSetters
emptyMetricsSetters =
  MetricSetters
    { metricsSetNodeBlockHeight = \_ -> pure ()
    , metricsSetDbQueueLength = \_ -> pure ()
    , metricsSetDbBlockHeight = \_ -> pure ()
    , metricsSetDbSlotHeight = \_ -> pure ()
    }

withFullConfig ::
  -- | config filepath
  FilePath ->
  -- | test label
  FilePath ->
  (Interpreter -> ServerHandle IO CardanoBlock -> DBSyncEnv -> IO a) ->
  IOManager ->
  [(Text, Text)] ->
  IO a
withFullConfig = withFullConfig' (WithConfigArgs True False False) initCommandLineArgs

-- this function needs to be used where the schema needs to be rebuilt
withFullConfigAndDropDB ::
  -- | config filepath
  FilePath ->
  -- | test label
  FilePath ->
  (Interpreter -> ServerHandle IO CardanoBlock -> DBSyncEnv -> IO a) ->
  IOManager ->
  [(Text, Text)] ->
  IO a
withFullConfigAndDropDB = withFullConfig' (WithConfigArgs True False True) initCommandLineArgs

withFullConfigAndLogs ::
  FilePath ->
  FilePath ->
  (Interpreter -> ServerHandle IO CardanoBlock -> DBSyncEnv -> IO a) ->
  IOManager ->
  [(Text, Text)] ->
  IO a
withFullConfigAndLogs = withFullConfig' (WithConfigArgs True True False) initCommandLineArgs

withCustomConfig ::
  CommandLineArgs ->
  -- | config filepath
  FilePath ->
  -- | test label
  FilePath ->
  (Interpreter -> ServerHandle IO CardanoBlock -> DBSyncEnv -> IO a) ->
  IOManager ->
  [(Text, Text)] ->
  IO a
withCustomConfig = withFullConfig' (WithConfigArgs True False False)

withCustomConfigAndDropDB ::
  CommandLineArgs ->
  -- | config filepath
  FilePath ->
  -- | test label
  FilePath ->
  (Interpreter -> ServerHandle IO CardanoBlock -> DBSyncEnv -> IO a) ->
  IOManager ->
  [(Text, Text)] ->
  IO a
withCustomConfigAndDropDB = withFullConfig' (WithConfigArgs True False True)

-- This is a usefull function to be able to see logs from DBSync when writing/debuging tests
withCustomConfigAndLogs ::
  CommandLineArgs ->
  -- | config filepath
  FilePath ->
  -- | test label
  FilePath ->
  (Interpreter -> ServerHandle IO CardanoBlock -> DBSyncEnv -> IO a) ->
  IOManager ->
  [(Text, Text)] ->
  IO a
withCustomConfigAndLogs = withFullConfig' (WithConfigArgs True True False)

withFullConfig' ::
  WithConfigArgs ->
  CommandLineArgs ->
  -- | config filepath
  FilePath ->
  -- | test label
  FilePath ->
  (Interpreter -> ServerHandle IO CardanoBlock -> DBSyncEnv -> IO a) ->
  IOManager ->
  [(Text, Text)] ->
  IO a
withFullConfig' WithConfigArgs {..} cmdLineArgs configFilePath testLabelFilePath action iom migr = do
  recreateDir mutableDir
  cfg <- mkConfig configDir mutableDir cmdLineArgs =<< mkSyncNodeConfig configDir
  fingerFile <- if hasFingerprint then Just <$> prepareFingerprintFile testLabelFilePath else pure Nothing
  let dbsyncParams = syncNodeParams cfg
  trce <-
    if shouldLog
      then configureLogging dbsyncParams "db-sync-node"
      else pure nullTracer
  -- runDbSync is partially applied so we can pass in syncNodeParams at call site / within tests
  let partialDbSyncRun params = runDbSync emptyMetricsSetters migr iom trce params True
      initSt = Consensus.pInfoInitLedger $ protocolInfo cfg

  withInterpreter (protocolInfoForging cfg) (protocolInfoForger cfg) nullTracer fingerFile $ \interpreter -> do
    -- TODO: get 42 from config
    withServerHandle @CardanoBlock
      iom
      (topLevelConfig cfg)
      initSt
      (NetworkMagic 42)
      (unSocketPath (enpSocketPath $ syncNodeParams cfg))
      $ \mockServer ->
        -- we dont fork dbsync here. Just prepare it as an action
        withDBSyncEnv (mkDBSyncEnv dbsyncParams partialDbSyncRun) $ \dbSyncEnv -> do
          let pgPass = getDBSyncPGPass dbSyncEnv
          tableNames <- Db.getAllTablleNames pgPass
          -- We only want to create the table schema once for the tests so here we check
          -- if there are any table names.
          if null tableNames || shouldDropDB
            then void . hSilence [stderr] $ Db.recreateDB pgPass
            else void . hSilence [stderr] $ Db.truncateTables pgPass tableNames
          action interpreter mockServer dbSyncEnv
  where
    configDir = mkConfigDir configFilePath
    mutableDir = mkMutableDir testLabelFilePath

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

textShow :: (Show a) => a -> Text
textShow = Text.pack . show
