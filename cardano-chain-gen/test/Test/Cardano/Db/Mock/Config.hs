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
  shelleyConfigDir,
  emptyMetricsSetters,
  fingerprintRoot,
  getDBSyncPGPass,
  getPoolLayer,

  -- * Configs
  withConfig,
  mkSyncNodeConfig,
  mkConfigDir,
  configPruneForceTxIn,
  configPrune,
  configConsume,
  configBootstrap,
  configPlutusDisable,
  configMultiAssetsDisable,
  configShelleyDisable,
  configRemoveJsonFromSchema,
  configRemoveJsonFromSchemaFalse,
  configLedgerIgnore,
  configMetadataEnable,
  configMetadataDisable,
  configMetadataKeys,
  configPoolStats,
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
  withFullConfigDropDB,
  withFullConfigDropDBLog,
  withFullConfigLog,
  withCustomConfigDropDBLog,
  withCustomConfig,
  withCustomConfigDropDB,
  withCustomConfigLog,
  withFullConfig',
  replaceConfigFile,
  txOutVariantTypeFromConfig,
) where

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
import Control.Monad.Trans.Except.Extra (runExceptT)
import Control.Tracer (nullTracer)
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath.Posix (takeDirectory, (</>))
import System.IO.Silently (hSilence)

import Cardano.Api (NetworkMagic (..))
import Ouroboros.Consensus.Block.Forging
import Ouroboros.Consensus.Byron.Ledger.Mempool ()
import Ouroboros.Consensus.Config (TopLevelConfig)
import Ouroboros.Consensus.HardFork.Combinator.Mempool ()
import Ouroboros.Consensus.Ledger.Abstract (projectLedgerTables)
import Ouroboros.Consensus.Ledger.Tables.Utils (forgetLedgerTables)
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import Ouroboros.Consensus.Shelley.Ledger.Mempool ()
import Ouroboros.Consensus.Shelley.Node (ShelleyLeaderCredentials)

import qualified Cardano.Db as DB
import Cardano.DbSync
import Cardano.DbSync.Config
import Cardano.DbSync.Config.Cardano
import Cardano.DbSync.Config.Types
import Cardano.DbSync.Error (runOrThrowIO)
import Cardano.DbSync.Types (CardanoBlock, MetricSetters (..))
import Cardano.Mock.ChainSync.Server
import Cardano.Mock.Forging.Interpreter
import Cardano.Node.Protocol.Shelley (readLeaderCredentials)
import Cardano.Node.Types (ProtocolFilepaths (..))
import Cardano.Prelude (NonEmpty ((:|)), panic, stderr, textShow, throwIO)
import Cardano.SMASH.Server.PoolDataLayer

data Config = Config
  { topLevelConfig :: TopLevelConfig CardanoBlock
  , protocolInfo :: Consensus.ProtocolInfo CardanoBlock
  , protocolInfoForging :: Consensus.ProtocolInfo CardanoBlock
  , protocolInfoForger :: [BlockForging IO CardanoBlock]
  , syncNodeParams :: SyncNodeParams
  }

data DBSyncEnv = DBSyncEnv
  { dbSyncParams :: SyncNodeParams
  , dbSyncConfig :: SyncNodeConfig
  , partialRunDbSync :: SyncNodeParams -> SyncNodeConfig -> IO ()
  , dbSyncThreadVar :: TMVar (Async ())
  }

data CommandLineArgs = CommandLineArgs
  { claConfigFilename :: FilePath
  , claEpochDisabled :: Bool
  , claHasCache :: Bool
  , claHasLedger :: Bool
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

shelleyConfigDir :: FilePath
shelleyConfigDir = "config-shelley"

rootTestDir :: FilePath
rootTestDir = "test/testfiles"

mkMutableDir :: FilePath -> FilePath
mkMutableDir testLabel = rootTestDir </> "temp" </> testLabel

mkConfigDir :: FilePath -> FilePath
mkConfigDir config = "cardano-chain-gen" </> rootTestDir </> config

fingerprintRoot :: FilePath
fingerprintRoot = rootTestDir </> "fingerprint"

mkFingerPrint :: FilePath -> FilePath
mkFingerPrint testLabel = fingerprintRoot </> testLabel

mkDBSyncEnv ::
  SyncNodeParams ->
  SyncNodeConfig ->
  (SyncNodeParams -> SyncNodeConfig -> IO ()) ->
  IO DBSyncEnv
mkDBSyncEnv params cfg pRunDbSync = do
  runningVar <- newEmptyTMVarIO
  pure $
    DBSyncEnv
      { dbSyncParams = params
      , dbSyncConfig = cfg
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
      let appliedRunDbSync = partialRunDbSync env (dbSyncParams env) (dbSyncConfig env)
      -- we async the fully applied runDbSync here and put it into the thread
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

getDBSyncPGPass :: DBSyncEnv -> DB.PGPassSource
getDBSyncPGPass = enpPGPassSource . dbSyncParams

queryDBSync :: DBSyncEnv -> DB.DbM a -> IO a
queryDBSync env = do
  DB.runDbStandaloneDirectSilent (getDBSyncPGPass env)

getPoolLayer :: DBSyncEnv -> IO PoolDataLayer
getPoolLayer env = do
  pgconfig <- runOrThrowIO $ DB.readPGPass (enpPGPassSource $ dbSyncParams env)
  connSetting <- case DB.toConnectionSetting pgconfig of
    Left err -> throwIO $ userError err
    Right setting -> pure setting

  -- Create the Hasql connection pool, using port as pool identifier
  pool <- DB.createHasqlConnectionPool [connSetting] 1 -- Pool size of 1 for tests
  pure $
    postgresqlPoolDataLayer
      nullTracer
      pool

withConfig :: FilePath -> FilePath -> CommandLineArgs -> SyncNodeConfig -> (Config -> IO a) -> IO a
withConfig staticDir mutableDir cmdLineArgs config action = do
  let cfgDir = mkConfigDir staticDir
  genCfg <- runOrThrowIO $ runExceptT (readCardanoGenesisConfig config)
  let (pInfoDbSync, _) = mkProtocolInfoCardano genCfg []
  creds <- mkShelleyCredentials $ cfgDir </> "pools" </> "bulk1.creds"
  let (pInfoForger, mkForgings) = mkProtocolInfoCardano genCfg [(head creds)]
  bracket
    (allocateRes mkForgings)
    (mapM finalize)
    ( \forgings -> do
        syncPars <- mkSyncNodeParams staticDir mutableDir cmdLineArgs
        let cfg = Config (Consensus.pInfoConfig pInfoDbSync) pInfoDbSync pInfoForger forgings syncPars
        action cfg
    )
  where
    allocateRes mkForgings = do
      forgings <- mkForgings
      -- _ <- throwIO $ userError "A"
      forgings' <- mapM mkBlockForging forgings
      -- _ <- throwIO $ userError "B"
      pure forgings'

mkSyncNodeConfig :: FilePath -> CommandLineArgs -> IO SyncNodeConfig
mkSyncNodeConfig configFilePath cmdLineArgs =
  readSyncNodeConfig $ mkConfigFile configDir configFilename
  where
    configFilename = claConfigFilename cmdLineArgs
    configDir = mkConfigDir configFilePath

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
  pgconfig <- runOrThrowIO DB.readPGPassDefault

  pure $
    SyncNodeParams
      { enpConfigFile = mkConfigFile staticDir claConfigFilename
      , enpSocketPath = SocketPath $ mutableDir </> ".socket"
      , enpMaybeLedgerStateDir = Just $ LedgerStateDir $ mutableDir </> "ledger-states"
      , enpMigrationDir = MigrationDir "../schema"
      , enpPGPassSource = DB.PGPassCached pgconfig
      , enpEpochDisabled = claEpochDisabled
      , enpHasCache = claHasCache
      , enpForceIndexes = claForceIndexes
      , enpHasInOut = True
      , enpMaybeRollback = Nothing
      }

------------------------------------------------------------------------------
-- Custom Configs
------------------------------------------------------------------------------
mkConfigFile :: FilePath -> FilePath -> ConfigFile
mkConfigFile staticDir cliConfigFilename =
  ConfigFile $ staticDir </> cliConfigFilename

configPruneForceTxIn :: Bool -> SyncNodeConfig -> SyncNodeConfig
configPruneForceTxIn useTxOutAddress cfg = do
  cfg {dncInsertOptions = (dncInsertOptions cfg) {sioTxOut = TxOutConsumedPrune (ForceTxIn True) (UseTxOutAddress useTxOutAddress)}}

configPrune :: Bool -> SyncNodeConfig -> SyncNodeConfig
configPrune useTxOutAddress cfg = do
  cfg {dncInsertOptions = (dncInsertOptions cfg) {sioTxOut = TxOutConsumedPrune (ForceTxIn False) (UseTxOutAddress useTxOutAddress)}}

configConsume :: Bool -> SyncNodeConfig -> SyncNodeConfig
configConsume useTxOutAddress cfg = do
  cfg {dncInsertOptions = (dncInsertOptions cfg) {sioTxOut = TxOutConsumed (ForceTxIn False) (UseTxOutAddress useTxOutAddress)}}

configBootstrap :: Bool -> SyncNodeConfig -> SyncNodeConfig
configBootstrap useTxOutAddress cfg = do
  cfg {dncInsertOptions = (dncInsertOptions cfg) {sioTxOut = TxOutConsumedBootstrap (ForceTxIn False) (UseTxOutAddress useTxOutAddress)}}

configPlutusDisable :: SyncNodeConfig -> SyncNodeConfig
configPlutusDisable cfg = do
  cfg {dncInsertOptions = (dncInsertOptions cfg) {sioPlutus = PlutusDisable}}

configMultiAssetsDisable :: SyncNodeConfig -> SyncNodeConfig
configMultiAssetsDisable cfg = do
  cfg {dncInsertOptions = (dncInsertOptions cfg) {sioMultiAsset = MultiAssetDisable}}

configShelleyDisable :: SyncNodeConfig -> SyncNodeConfig
configShelleyDisable cfg = do
  cfg {dncInsertOptions = (dncInsertOptions cfg) {sioShelley = ShelleyDisable}}

configRemoveJsonFromSchema :: SyncNodeConfig -> SyncNodeConfig
configRemoveJsonFromSchema cfg = do
  cfg {dncInsertOptions = (dncInsertOptions cfg) {sioRemoveJsonbFromSchema = RemoveJsonbFromSchemaConfig True}}

configRemoveJsonFromSchemaFalse :: SyncNodeConfig -> SyncNodeConfig
configRemoveJsonFromSchemaFalse cfg = do
  cfg {dncInsertOptions = (dncInsertOptions cfg) {sioRemoveJsonbFromSchema = RemoveJsonbFromSchemaConfig False}}

configLedgerIgnore :: SyncNodeConfig -> SyncNodeConfig
configLedgerIgnore cfg = do
  cfg {dncInsertOptions = (dncInsertOptions cfg) {sioLedger = LedgerIgnore}}

configMetadataEnable :: SyncNodeConfig -> SyncNodeConfig
configMetadataEnable cfg = do
  cfg {dncInsertOptions = (dncInsertOptions cfg) {sioMetadata = MetadataEnable}}

configMetadataDisable :: SyncNodeConfig -> SyncNodeConfig
configMetadataDisable cfg = do
  cfg {dncInsertOptions = (dncInsertOptions cfg) {sioMetadata = MetadataDisable}}

configMetadataKeys :: SyncNodeConfig -> SyncNodeConfig
configMetadataKeys cfg = do
  cfg {dncInsertOptions = (dncInsertOptions cfg) {sioMetadata = MetadataKeys $ 1 :| []}}

configPoolStats :: SyncNodeConfig -> SyncNodeConfig
configPoolStats cfg = do
  cfg {dncInsertOptions = (dncInsertOptions cfg) {sioPoolStats = PoolStatsConfig True}}

initCommandLineArgs :: CommandLineArgs
initCommandLineArgs =
  CommandLineArgs
    { claConfigFilename = "test-db-sync-config.json"
    , claEpochDisabled = True
    , claHasCache = True
    , claHasLedger = True
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
    , metricsSetDbEpochSyncDuration = \_ -> pure ()
    , metricsSetDbEpochSyncNumber = \_ -> pure ()
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
withFullConfig =
  withFullConfig'
    ( WithConfigArgs
        { hasFingerprint = True
        , shouldLog = False
        , shouldDropDB = False
        }
    )
    initCommandLineArgs
    Nothing

-- this function needs to be used where the schema needs to be rebuilt
withFullConfigDropDB ::
  -- | config filepath
  FilePath ->
  -- | test label
  FilePath ->
  (Interpreter -> ServerHandle IO CardanoBlock -> DBSyncEnv -> IO a) ->
  IOManager ->
  [(Text, Text)] ->
  IO a
withFullConfigDropDB =
  withFullConfig'
    ( WithConfigArgs
        { hasFingerprint = True
        , shouldLog = False
        , shouldDropDB = True
        }
    )
    initCommandLineArgs
    Nothing

withFullConfigDropDBLog ::
  -- | config filepath
  FilePath ->
  -- | test label
  FilePath ->
  (Interpreter -> ServerHandle IO CardanoBlock -> DBSyncEnv -> IO a) ->
  IOManager ->
  [(Text, Text)] ->
  IO a
withFullConfigDropDBLog =
  withFullConfig'
    ( WithConfigArgs
        { hasFingerprint = True
        , shouldLog = True
        , shouldDropDB = True
        }
    )
    initCommandLineArgs
    Nothing

withFullConfigLog ::
  -- | config filepath
  FilePath ->
  -- | test label
  FilePath ->
  (Interpreter -> ServerHandle IO CardanoBlock -> DBSyncEnv -> IO a) ->
  IOManager ->
  [(Text, Text)] ->
  IO a
withFullConfigLog =
  withFullConfig'
    ( WithConfigArgs
        { hasFingerprint = True
        , shouldLog = True
        , shouldDropDB = False
        }
    )
    initCommandLineArgs
    Nothing

withCustomConfig ::
  CommandLineArgs ->
  -- | custom SyncNodeConfig
  Maybe (SyncNodeConfig -> SyncNodeConfig) ->
  -- | config filepath
  FilePath ->
  -- | test label
  FilePath ->
  (Interpreter -> ServerHandle IO CardanoBlock -> DBSyncEnv -> IO a) ->
  IOManager ->
  [(Text, Text)] ->
  IO a
withCustomConfig =
  withFullConfig'
    ( WithConfigArgs
        { hasFingerprint = True
        , shouldLog = False
        , shouldDropDB = False
        }
    )

withCustomConfigDropDB ::
  CommandLineArgs ->
  -- | custom SyncNodeConfig
  Maybe (SyncNodeConfig -> SyncNodeConfig) ->
  -- | config filepath
  FilePath ->
  -- | test label
  FilePath ->
  (Interpreter -> ServerHandle IO CardanoBlock -> DBSyncEnv -> IO a) ->
  IOManager ->
  [(Text, Text)] ->
  IO a
withCustomConfigDropDB =
  withFullConfig'
    ( WithConfigArgs
        { hasFingerprint = True
        , shouldLog = False
        , shouldDropDB = True
        }
    )

-- This is a usefull function to be able to see logs from DBSync when writing/debuging tests
withCustomConfigLog ::
  CommandLineArgs ->
  -- | custom SyncNodeConfig
  Maybe (SyncNodeConfig -> SyncNodeConfig) ->
  -- | config filepath
  FilePath ->
  -- | test label
  FilePath ->
  (Interpreter -> ServerHandle IO CardanoBlock -> DBSyncEnv -> IO a) ->
  IOManager ->
  [(Text, Text)] ->
  IO a
withCustomConfigLog =
  withFullConfig'
    ( WithConfigArgs
        { hasFingerprint = True
        , shouldLog = True
        , shouldDropDB = False
        }
    )

withCustomConfigDropDBLog ::
  CommandLineArgs ->
  -- | custom SyncNodeConfig
  Maybe (SyncNodeConfig -> SyncNodeConfig) ->
  -- | config filepath
  FilePath ->
  -- | test label
  FilePath ->
  (Interpreter -> ServerHandle IO CardanoBlock -> DBSyncEnv -> IO a) ->
  IOManager ->
  [(Text, Text)] ->
  IO a
withCustomConfigDropDBLog =
  withFullConfig'
    ( WithConfigArgs
        { hasFingerprint = True
        , shouldLog = True
        , shouldDropDB = True
        }
    )

withFullConfig' ::
  WithConfigArgs ->
  CommandLineArgs ->
  -- | custom SyncNodeConfig
  Maybe (SyncNodeConfig -> SyncNodeConfig) ->
  -- | config filepath
  FilePath ->
  -- | test label
  FilePath ->
  (Interpreter -> ServerHandle IO CardanoBlock -> DBSyncEnv -> IO a) ->
  IOManager ->
  [(Text, Text)] ->
  IO a
withFullConfig' WithConfigArgs {..} cmdLineArgs mSyncNodeConfig configFilePath testLabelFilePath action iom migr = do
  recreateDir mutableDir
  -- check if custom syncNodeConfigs have been passed or not
  syncNodeConfig <-
    case mSyncNodeConfig of
      Just updateFn -> do
        initConfigFile <- mkSyncNodeConfig configFilePath cmdLineArgs
        pure $ updateFn initConfigFile
      Nothing -> mkSyncNodeConfig configFilePath cmdLineArgs

  withConfig configFilePath mutableDir cmdLineArgs syncNodeConfig $ \cfg -> do
    fingerFile <- if hasFingerprint then Just <$> prepareFingerprintFile testLabelFilePath else pure Nothing
    let dbsyncParams = syncNodeParams cfg
    trce <-
      if shouldLog
        then configureLogging syncNodeConfig "db-sync-node"
        else pure nullTracer
    -- runDbSync is partially applied so we can pass in syncNodeParams at call site / within tests
    let partialDbSyncRun params cfg' = runDbSync emptyMetricsSetters iom trce params cfg' True
        initSt = Consensus.pInfoInitLedger $ protocolInfo cfg

    withInterpreter (protocolInfoForging cfg) (protocolInfoForger cfg) nullTracer fingerFile $ \interpreter -> do
      -- TODO: get 42 from config
      withServerHandle @CardanoBlock
        iom
        (topLevelConfig cfg)
        (forgetLedgerTables initSt, projectLedgerTables initSt)
        (NetworkMagic 42)
        (unSocketPath (enpSocketPath $ syncNodeParams cfg))
        $ \mockServer ->
          -- we dont fork dbsync here. Just prepare it as an action
          withDBSyncEnv (mkDBSyncEnv dbsyncParams syncNodeConfig partialDbSyncRun) $ \dbSyncEnv -> do
            let pgPass = getDBSyncPGPass dbSyncEnv
            tableNames <- DB.getAllTableNames pgPass
            -- We only want to create the table schema once for the tests so here we check
            -- if there are any table names.
            if null tableNames || shouldDropDB
              then void . hSilence [stderr] $ DB.recreateDB pgPass
              else void . hSilence [stderr] $ DB.truncateTables pgPass tableNames

            -- Run migrations synchronously first
            runMigrationsOnly
              migr
              trce
              (syncNodeParams cfg)
              syncNodeConfig

            action interpreter mockServer dbSyncEnv
  where
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

replaceConfigFile :: FilePath -> DBSyncEnv -> IO DBSyncEnv
replaceConfigFile newFilename dbSync@DBSyncEnv {..} = do
  cfg <- readSyncNodeConfig $ mkConfigFile configDir newFilename

  pure $ dbSync {dbSyncParams = newParams, dbSyncConfig = cfg}
  where
    configDir = mkConfigDir . takeDirectory . unConfigFile . enpConfigFile $ dbSyncParams
    newParams =
      dbSyncParams {enpConfigFile = ConfigFile $ configDir </> newFilename}

txOutVariantTypeFromConfig :: DBSyncEnv -> DB.TxOutVariantType
txOutVariantTypeFromConfig dbSyncEnv =
  case sioTxOut $ dncInsertOptions $ dbSyncConfig dbSyncEnv of
    TxOutDisable -> DB.TxOutVariantCore
    TxOutEnable useTxOutAddress -> getTxOutTT useTxOutAddress
    TxOutConsumed _ useTxOutAddress -> getTxOutTT useTxOutAddress
    TxOutConsumedPrune _ useTxOutAddress -> getTxOutTT useTxOutAddress
    TxOutConsumedBootstrap _ useTxOutAddress -> getTxOutTT useTxOutAddress
  where
    getTxOutTT :: UseTxOutAddress -> DB.TxOutVariantType
    getTxOutTT value = if unUseTxOutAddress value then DB.TxOutVariantAddress else DB.TxOutVariantCore
