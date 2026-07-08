{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync (
  ConfigFile (..),
  SyncCommand (..),
  SyncNodeParams (..),
  GenesisFile (..),
  LedgerStateDir (..),
  NetworkName (..),
  SocketPath (..),
  DB.MigrationDir (..),
  runDbSyncNode,
  runMigrationsOnly,
  runDbSync,
  -- For testing and debugging
  OffChainFetchError (..),
  SimplifiedOffChainPoolData (..),
  extractSyncOptions,
) where

import Cardano.Network.NodeToClient (IOManager, withIOManager)
import Control.Concurrent.Async
import Control.Monad.Extra (whenJust)
import qualified Data.Strict.Maybe as Strict
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Version (showVersion)
import qualified Hasql.Connection as HsqlC
import qualified Hasql.Connection.Setting as HsqlSet
import Ouroboros.Consensus.Cardano (CardanoHardForkTrigger (..))
import Ouroboros.Network.Magic (NetworkMagic (..))
import Paths_cardano_db_sync (version)
import System.Directory (createDirectoryIfMissing)
import Prelude (id)

import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Crypto as Crypto
import Cardano.Db.Log (LogMessage, logError, logInfo, logWarning)
import Cardano.Logging (Trace)
import Cardano.Prelude hiding (Nat, (%))
import Cardano.Slotting.Slot (EpochNo (..))

import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (InsertOptions (..), LedgerEnv (..), RunMigration, SyncEnv (..), SyncOptions (..), envLedgerEnv)
import Cardano.DbSync.Config.Cardano
import Cardano.DbSync.Config.Types
import Cardano.DbSync.Database
import Cardano.DbSync.DbEvent
import Cardano.DbSync.Era
import Cardano.DbSync.Error
import Cardano.DbSync.Ledger.State
import Cardano.DbSync.Ledger.Types (HasLedgerEnv (..))
import Cardano.DbSync.OffChain (runFetchOffChainPoolThread, runFetchOffChainVoteThread)
import Cardano.DbSync.Rollback (handlePostRollbackSnapshots, unsafeRollback)
import Cardano.DbSync.Sync (runSyncNodeClient)
import Cardano.DbSync.Tracing.Setup (DbSyncTracers (..), mkDbSyncTracers)
import Cardano.DbSync.Types

runDbSyncNode :: MetricSetters -> [(Text, Text)] -> SyncNodeParams -> SyncNodeConfig -> IO ()
runDbSyncNode metricsSetters knownMigrations params syncNodeConfigFromFile =
  withIOManager $ \iomgr -> do
    -- The genesis config is read before the tracers are created, because the
    -- network magic and system start time are needed for forwarding to
    -- cardano-tracer.
    eGenCfg <- runExceptT $ readCardanoGenesisConfig syncNodeConfigFromFile
    now <- getCurrentTime
    let networkMagic = either (const $ NetworkMagic 0) genesisNetworkMagic eGenCfg
        systemStart = either (const now) genesisSystemStart eGenCfg

    tracers <-
      mkDbSyncTracers
        iomgr
        (dncEnableLogging syncNodeConfigFromFile)
        (dncTraceConfig syncNodeConfigFromFile)
        (enpTracerSocket params)
        networkMagic
        (unNetworkName $ dncNetworkName syncNodeConfigFromFile)
        systemStart
    let trce = dstTracer tracers

    abortOnPanic <- hasAbortOnPanicEnv
    startupReport trce abortOnPanic params

    genCfg <- case eGenCfg of
      Left err -> do
        logError trce $ "readCardanoGenesisConfig: " <> show err
        throwIO err
      Right genCfg -> pure genCfg

    -- Run initial migrations synchronously first
    runMigrationsOnly knownMigrations trce params syncNodeConfigFromFile

    runDbSync metricsSetters iomgr tracers params syncNodeConfigFromFile abortOnPanic genCfg

-- Extract just the initial migration logic (no indexes)
runMigrationsOnly ::
  [(Text, Text)] ->
  Trace IO LogMessage ->
  SyncNodeParams ->
  SyncNodeConfig ->
  IO ()
runMigrationsOnly knownMigrations trce params syncNodeConfigFromFile = do
  logInfo trce $ textShow syncOpts

  -- Read the PG connection info
  pgConfig <- runOrThrowIO (DB.readPGPass $ enpPGPassSource params)

  mErrors <- liftIO $ DB.validateMigrations dbMigrationDir knownMigrations
  whenJust mErrors $ \(unknown, stage4orNewStage3) ->
    if stage4orNewStage3
      then logWarning trce $ DB.renderMigrationValidateError unknown
      else logError trce $ DB.renderMigrationValidateError unknown

  logInfo trce "Schema migration files validated"

  let runMigration mode = do
        msg <- DB.getMaintenancePsqlConf pgConfig
        logInfo trce $ "Running database migrations in mode " <> textShow mode
        logInfo trce msg
        DB.runMigrations (Just trce) pgConfig True dbMigrationDir (Just $ DB.LogFileDir "/tmp") mode (txOutConfigToTableType txOutConfig)

  -- Always run Initial mode only - never indexes
  (ranMigrations, unofficial) <- runMigration DB.Initial
  unless (null unofficial) $
    logWarning trce $
      "Unofficial migration scripts found: "
        <> textShow unofficial

  if ranMigrations
    then logInfo trce "All migrations were executed"
    else logInfo trce "Some migrations were not executed. They need to run when syncing has started."

  logInfo trce "New user indexes were not created. They may be created later if necessary."
  where
    dbMigrationDir :: DB.MigrationDir
    dbMigrationDir = enpMigrationDir params
    syncOpts = extractSyncOptions params False syncNodeConfigFromFile
    txOutConfig = sioTxOut $ dncInsertOptions syncNodeConfigFromFile

runDbSync ::
  MetricSetters ->
  IOManager ->
  DbSyncTracers ->
  SyncNodeParams ->
  SyncNodeConfig ->
  -- Should abort on panic
  Bool ->
  GenesisConfig ->
  IO ()
runDbSync metricsSetters iomgr tracers params syncNodeConfigFromFile abortOnPanic genCfg = do
  logInfo trce $ textShow syncOpts

  -- Read the PG connection info
  pgConfig <- runOrThrowIO (DB.readPGPass $ enpPGPassSource params)

  dbConnectionSetting <- case DB.toConnectionSetting pgConfig of
    Left err -> do
      let syncNodeErr = SNErrPGConfig ("Invalid database connection setting: " <> err)
      logError trce $ show syncNodeErr
      throwIO syncNodeErr
    Right setting -> pure setting

  -- For testing and debugging.
  whenJust (enpMaybeRollback params) $ \slotNo ->
    void $ unsafeRollback trce (txOutConfigToTableType txOutConfig) pgConfig slotNo

  -- These migrations will be ran when near the tip of the chain eg: indexes.
  let runNearTipMigration mode = do
        msg <- DB.getMaintenancePsqlConf pgConfig
        logInfo trce $ "Running NearTip database migrations in mode " <> textShow mode
        logInfo trce msg
        when (mode `elem` [DB.NearTip, DB.Full]) $ logWarning trce indexesMsg
        DB.runMigrations (Just trce) pgConfig True dbMigrationDir (Just $ DB.LogFileDir "/tmp") mode (txOutConfigToTableType txOutConfig)

  runSyncNode
    metricsSetters
    tracers
    iomgr
    dbConnectionSetting
    (void . runNearTipMigration)
    syncNodeConfigFromFile
    params
    syncOpts
    genCfg
  where
    trce = dstTracer tracers
    dbMigrationDir :: DB.MigrationDir
    dbMigrationDir = enpMigrationDir params
    syncOpts = extractSyncOptions params abortOnPanic syncNodeConfigFromFile
    txOutConfig = sioTxOut $ dncInsertOptions syncNodeConfigFromFile

    indexesMsg :: Text
    indexesMsg =
      mconcat
        [ "Creating Indexes. This may require an extended period of time to perform."
        , " Setting a higher maintenance_work_mem from Postgres usually speeds up this process."
        , " These indexes are not used by db-sync but are meant for clients. If you want to skip"
        , " some of these indexes, you can stop db-sync, delete or modify any migration-4-* files"
        , " in the schema directory and restart it."
        ]

runSyncNode ::
  MetricSetters ->
  DbSyncTracers ->
  IOManager ->
  -- | Database connection settings
  HsqlSet.Setting ->
  -- | run migration function
  RunMigration ->
  SyncNodeConfig ->
  SyncNodeParams ->
  SyncOptions ->
  GenesisConfig ->
  IO ()
runSyncNode metricsSetters tracers iomgr dbConnSetting runNearTipMigrationFnc syncNodeConfigFromFile syncNodeParams syncOptions genCfg = do
  whenJust maybeLedgerDir $
    \enpLedgerStateDir -> do
      createDirectoryIfMissing True (unLedgerStateDir enpLedgerStateDir)
  logInfo trce $ "Using byron genesis file from: " <> (show . unGenesisFile $ dncByronGenesisFile syncNodeConfigFromFile)
  logInfo trce $ "Using shelley genesis file from: " <> (show . unGenesisFile $ dncShelleyGenesisFile syncNodeConfigFromFile)
  logInfo trce $ "Using alonzo genesis file from: " <> (show . unGenesisFile $ dncAlonzoGenesisFile syncNodeConfigFromFile)

  let useLedger = shouldUseLedger (sioLedger $ dncInsertOptions syncNodeConfigFromFile)
  -- The main thread
  bracket
    (DB.acquireConnection [dbConnSetting])
    HsqlC.release
    ( \dbConn -> do
        runOrThrowIO $ runExceptT $ do
          -- Create connection pool for parallel operations
          pool <- liftIO $ DB.createHasqlConnectionPool [dbConnSetting] 4 -- 4 connections for reasonable parallelism
          let dbEnv = DB.createDbEnv dbConn (Just pool) (Just trce)
          isJsonbInSchema <- liftSessionIO mkSyncNodeCallStack $ DB.queryJsonbInSchemaExists dbConn
          logProtocolMagicId trce $ genesisProtocolMagicId genCfg

          -- Determine the final JSONB state after any schema migrations
          let finalJsonbInSchema = case (isJsonbInSchema, removeJsonbFromSchemaConfig) of
                (True, True) -> False -- Will be removed
                (False, False) -> True -- Will be added
                (s, _) -> s -- No change
          syncEnv <-
            ExceptT $
              mkSyncEnvFromConfig
                metricsSetters
                trce
                dbEnv
                syncOptions
                genCfg
                syncNodeConfigFromFile
                syncNodeParams
                runNearTipMigrationFnc
                finalJsonbInSchema

          -- Warn the user that jsonb datatypes are being removed from the database schema.
          when (isJsonbInSchema && removeJsonbFromSchemaConfig) $ do
            liftIO $ logWarning trce "Removing jsonb datatypes from the database. This can take time."
            liftIO $ runRemoveJsonbFromSchema syncEnv

          -- Warn the user that jsonb datatypes are being added to the database schema.
          when (not isJsonbInSchema && not removeJsonbFromSchemaConfig) $ do
            liftIO $ logWarning trce "Adding jsonb datatypes back to the database. This can take time."
            liftIO $ runAddJsonbToSchema syncEnv
          liftIO $ runConsumedTxOutMigrationsMaybe syncEnv
          liftIO $ syncEpochViewConfig syncEnv
          unless useLedger $ liftIO $ do
            logInfo trce "Migrating to a no ledger schema"
            DB.noLedgerMigrations dbEnv trce
          insertValidateGenesisDist syncEnv (dncNetworkName syncNodeConfigFromFile) genCfg (useShelleyInit syncNodeConfigFromFile)

          -- Handle ledger snapshots after rollback to ensure consistency
          liftIO $ handlePostRollbackSnapshots syncEnv (enpMaybeRollback syncNodeParams)

          -- communication channel between datalayer thread and chainsync-client thread
          threadChannels <- liftIO newThreadChannels
          -- 'finally' on the worker pool ensures the LSM session (and any other
          -- backend resources) are closed even when db-sync is cancelled or
          -- crashes — important for tests that restart db-sync in the same
          -- process and need the OS file lock to be released.
          liftIO $
            mapConcurrently_
              id
              [ runDbThread syncEnv threadChannels
              , runSyncNodeClient metricsSetters syncEnv iomgr tracers threadChannels (enpSocketPath syncNodeParams)
              , runFetchOffChainPoolThread syncEnv
              , runFetchOffChainVoteThread syncEnv
              , runLedgerStateWriteThread (getTrace syncEnv) (envLedgerEnv syncEnv)
              ]
              `finally` closeLedgerEnv syncEnv
    )
  where
    trce = dstTracer tracers

    useShelleyInit :: SyncNodeConfig -> Bool
    useShelleyInit cfg =
      case dncShelleyHardFork cfg of
        CardanoTriggerHardForkAtEpoch (EpochNo 0) -> True
        _ -> False

    removeJsonbFromSchemaConfig = ioRemoveJsonbFromSchema $ soptInsertOptions syncOptions
    maybeLedgerDir = enpMaybeLedgerStateDir syncNodeParams

logProtocolMagicId :: Trace IO LogMessage -> Crypto.ProtocolMagicId -> ExceptT SyncNodeError IO ()
logProtocolMagicId tracer pm =
  liftIO
    . logInfo tracer
    $ mconcat
      [ "NetworkMagic: "
      , textShow (Crypto.unProtocolMagicId pm)
      ]

-- | The network magic of the network this genesis config is for.
genesisNetworkMagic :: GenesisConfig -> NetworkMagic
genesisNetworkMagic =
  NetworkMagic . Crypto.unProtocolMagicId . genesisProtocolMagicId

-- | The chain start time from the (Byron) genesis config.
genesisSystemStart :: GenesisConfig -> UTCTime
genesisSystemStart genCfg =
  case genCfg of
    GenesisCardano _ bCfg _ _ _ ->
      Byron.gdStartTime (Byron.configGenesisData bCfg)

-- -------------------------------------------------------------------------------------------------

extractSyncOptions :: SyncNodeParams -> Bool -> SyncNodeConfig -> SyncOptions
extractSyncOptions snp aop snc =
  SyncOptions
    { soptEpochViewEnabled =
        not isTxOutConsumedBootstrap'
          && ioInOut iopts
          && isEpochEnabled (sioEpoch (dncInsertOptions snc))
    , soptAbortOnInvalid = aop
    , soptCache = enpHasCache snp
    , soptPruneConsumeMigration =
        initPruneConsumeMigration
          isTxOutConsumed'
          isTxOutConsumedPrune'
          isTxOutConsumedBootstrap'
          forceTxIn'
    , soptInsertOptions = iopts
    , soptSnapshotInterval = dncSnapshotInterval snc
    , soptAllowPrivateOffChainUrls = enpAllowPrivateOffChainUrls snp
    }
  where
    maybeKeepMNames =
      case sioMetadata (dncInsertOptions snc) of
        MetadataKeys ks -> Strict.Just (map fromIntegral $ toList ks)
        MetadataEnable -> Strict.Nothing
        MetadataDisable -> Strict.Nothing

    iopts =
      InsertOptions
        { ioInOut = isTxOutEnabled'
        , ioTxCBOR = isTxCBOREnabled (sioTxCBOR (dncInsertOptions snc))
        , ioUseLedger = useLedger
        , ioShelley = isShelleyEnabled (sioShelley (dncInsertOptions snc))
        , -- Rewards are only disabled on "disable_all" and "only_gov" presets
          ioRewards = True
        , ioMultiAssets = isMultiAssetEnabled (sioMultiAsset (dncInsertOptions snc))
        , ioMetadata = isMetadataEnabled (sioMetadata (dncInsertOptions snc))
        , ioKeepMetadataNames = maybeKeepMNames
        , ioPlutusExtra = isPlutusEnabled (sioPlutus (dncInsertOptions snc))
        , ioOffChainPoolData = useOffchainPoolData
        , ioOffChainVoteData = useOffchainVoteData
        , ioPoolStats = isPoolStatsEnabled (sioPoolStats (dncInsertOptions snc))
        , ioGov = useGovernance
        , ioRemoveJsonbFromSchema = isRemoveJsonbFromSchemaEnabled (sioRemoveJsonbFromSchema (dncInsertOptions snc))
        , ioTxOutVariantType = ioTxOutVariantType'
        }

    useLedger = sioLedger (dncInsertOptions snc) == LedgerEnable
    useOffchainPoolData =
      isOffchainPoolDataEnabled (sioOffchainPoolData (dncInsertOptions snc))
    useOffchainVoteData =
      isOffchainVoteDataEnabled (sioOffchainVoteData (dncInsertOptions snc))
    useGovernance =
      isGovernanceEnabled (sioGovernance (dncInsertOptions snc))

    isTxOutConsumed' = isTxOutConsumed . sioTxOut . dncInsertOptions $ snc
    isTxOutConsumedPrune' = isTxOutConsumedPrune . sioTxOut . dncInsertOptions $ snc
    isTxOutConsumedBootstrap' = isTxOutConsumedBootstrap . sioTxOut . dncInsertOptions $ snc
    isTxOutEnabled' = isTxOutEnabled . sioTxOut . dncInsertOptions $ snc
    forceTxIn' = forceTxIn . sioTxOut . dncInsertOptions $ snc
    ioTxOutVariantType' = txOutConfigToTableType $ sioTxOut $ dncInsertOptions snc

startupReport :: Trace IO LogMessage -> Bool -> SyncNodeParams -> IO ()
startupReport trce aop params = do
  logInfo trce $ mconcat ["Version number: ", Text.pack (showVersion version)]
  logInfo trce $ mconcat ["Git hash: ", DB.gitRev]
  logInfo trce $ mconcat ["Enviroment variable DbSyncAbortOnPanic: ", textShow aop]
  logInfo trce $ textShow params

txOutConfigToTableType :: TxOutConfig -> DB.TxOutVariantType
txOutConfigToTableType config = case config of
  TxOutEnable (UseTxOutAddress flag) -> if flag then DB.TxOutVariantAddress else DB.TxOutVariantCore
  TxOutDisable -> DB.TxOutVariantCore
  TxOutConsumed _ (UseTxOutAddress flag) -> if flag then DB.TxOutVariantAddress else DB.TxOutVariantCore
  TxOutConsumedPrune _ (UseTxOutAddress flag) -> if flag then DB.TxOutVariantAddress else DB.TxOutVariantCore
  TxOutConsumedBootstrap _ (UseTxOutAddress flag) -> if flag then DB.TxOutVariantAddress else DB.TxOutVariantCore

-- | Release backend resources held by the ledger environment.
-- Currently this closes the LSM session (no-op for InMemory and NoLedger).
closeLedgerEnv :: SyncEnv -> IO ()
closeLedgerEnv syncEnv = case envLedgerEnv syncEnv of
  HasLedger le -> do
    let trce = leTrace le
    logInfo trce "closeLedgerEnv: closing LSM session..."
    leClose le
    logInfo trce "closeLedgerEnv: closed."
  NoLedger _ -> pure ()
