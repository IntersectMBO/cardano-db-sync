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

import Control.Concurrent.Async
import Control.Monad.Extra (whenJust)
import qualified Data.Strict.Maybe as Strict
import qualified Data.Text as Text
import Data.Version (showVersion)
import qualified Hasql.Connection as HsqlC
import qualified Hasql.Connection.Setting as HsqlSet
import Ouroboros.Consensus.Cardano (CardanoHardForkTrigger (..))
import Ouroboros.Network.NodeToClient (IOManager, withIOManager)
import Paths_cardano_db_sync (version)
import System.Directory (createDirectoryIfMissing)
import Prelude (id)

import Cardano.BM.Trace (Trace, logError, logInfo, logWarning)
import qualified Cardano.Crypto as Crypto
import Cardano.Prelude hiding (Nat, (%))
import Cardano.Slotting.Slot (EpochNo (..))

import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (InsertOptions (..), RunMigration, SyncEnv (..), SyncOptions (..), envLedgerEnv)
import Cardano.DbSync.Config (configureLogging)
import Cardano.DbSync.Config.Cardano
import Cardano.DbSync.Config.Types
import Cardano.DbSync.Database
import Cardano.DbSync.DbEvent
import Cardano.DbSync.Era
import Cardano.DbSync.Error
import Cardano.DbSync.Ledger.State
import Cardano.DbSync.OffChain (runFetchOffChainPoolThread, runFetchOffChainVoteThread)
import Cardano.DbSync.Rollback (unsafeRollback)
import Cardano.DbSync.Sync (runSyncNodeClient)
import Cardano.DbSync.Tracing.ToObjectOrphans ()
import Cardano.DbSync.Types

runDbSyncNode :: MetricSetters -> [(Text, Text)] -> SyncNodeParams -> SyncNodeConfig -> IO ()
runDbSyncNode metricsSetters knownMigrations params syncNodeConfigFromFile =
  withIOManager $ \iomgr -> do
    trce <- configureLogging syncNodeConfigFromFile "db-sync-node"

    abortOnPanic <- hasAbortOnPanicEnv
    startupReport trce abortOnPanic params

    -- Run initial migrations synchronously first
    runMigrationsOnly knownMigrations trce params syncNodeConfigFromFile

    runDbSync metricsSetters iomgr trce params syncNodeConfigFromFile abortOnPanic

-- Extract just the initial migration logic (no indexes)
runMigrationsOnly ::
  [(Text, Text)] ->
  Trace IO Text ->
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
        DB.runMigrations pgConfig True dbMigrationDir (Just $ DB.LogFileDir "/tmp") mode (txOutConfigToTableType txOutConfig)

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
  Trace IO Text ->
  SyncNodeParams ->
  SyncNodeConfig ->
  -- Should abort on panic
  Bool ->
  IO ()
runDbSync metricsSetters iomgr trce params syncNodeConfigFromFile abortOnPanic = do
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

  -- This runMigration is ONLY for delayed migrations during sync (like indexes)
  let runIndexesMigration mode = do
        msg <- DB.getMaintenancePsqlConf pgConfig
        logInfo trce $ "Running database migrations in mode " <> textShow mode
        logInfo trce msg
        when (mode `elem` [DB.Indexes, DB.Full]) $ logWarning trce indexesMsg
        DB.runMigrations pgConfig True dbMigrationDir (Just $ DB.LogFileDir "/tmp") mode (txOutConfigToTableType txOutConfig)

  runSyncNode
    metricsSetters
    trce
    iomgr
    dbConnectionSetting
    (void . runIndexesMigration)
    syncNodeConfigFromFile
    params
    syncOpts
  where
    dbMigrationDir :: DB.MigrationDir
    dbMigrationDir = enpMigrationDir params
    syncOpts = extractSyncOptions params abortOnPanic syncNodeConfigFromFile
    txOutConfig = sioTxOut $ dncInsertOptions syncNodeConfigFromFile

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
  Trace IO Text ->
  IOManager ->
  -- | Database connection settings
  HsqlSet.Setting ->
  -- | run migration function
  RunMigration ->
  SyncNodeConfig ->
  SyncNodeParams ->
  SyncOptions ->
  IO ()
runSyncNode metricsSetters trce iomgr dbConnSetting runIndexesMigrationFnc syncNodeConfigFromFile syncNodeParams syncOptions = do
  whenJust maybeLedgerDir $
    \enpLedgerStateDir -> do
      createDirectoryIfMissing True (unLedgerStateDir enpLedgerStateDir)
  logInfo trce $ "Using byron genesis file from: " <> (show . unGenesisFile $ dncByronGenesisFile syncNodeConfigFromFile)
  logInfo trce $ "Using shelley genesis file from: " <> (show . unGenesisFile $ dncShelleyGenesisFile syncNodeConfigFromFile)
  logInfo trce $ "Using alonzo genesis file from: " <> (show . unGenesisFile $ dncAlonzoGenesisFile syncNodeConfigFromFile)

  let useLedger = shouldUseLedger (sioLedger $ dncInsertOptions syncNodeConfigFromFile)
  -- The main thread
  bracket
    (acquireDbConnection [dbConnSetting])
    HsqlC.release
    ( \dbConn -> do
        runOrThrowIO $ runExceptT $ do
          let isLogingEnabled = dncEnableDbLogging syncNodeConfigFromFile
          -- Create connection pool for parallel operations
          pool <- liftIO $ DB.createHasqlConnectionPool [dbConnSetting] 4 -- 4 connections for reasonable parallelism
          let dbEnv =
                if isLogingEnabled
                  then DB.createDbEnv dbConn pool (Just trce)
                  else DB.createDbEnv dbConn pool Nothing
          genCfg <- readCardanoGenesisConfig syncNodeConfigFromFile
          isJsonbInSchema <- liftDbError $ DB.queryJsonbInSchemaExists dbConn
          logProtocolMagicId trce $ genesisProtocolMagicId genCfg
          syncEnv <-
            ExceptT $
              mkSyncEnvFromConfig
                trce
                dbEnv
                syncOptions
                genCfg
                syncNodeConfigFromFile
                syncNodeParams
                runIndexesMigrationFnc

          -- Warn the user that jsonb datatypes are being removed from the database schema.
          when (isJsonbInSchema && removeJsonbFromSchemaConfig) $ do
            liftIO $ logWarning trce "Removing jsonb datatypes from the database. This can take time."
            liftIO $ runRemoveJsonbFromSchema syncEnv

          -- Warn the user that jsonb datatypes are being added to the database schema.
          when (not isJsonbInSchema && not removeJsonbFromSchemaConfig) $ do
            liftIO $ logWarning trce "Adding jsonb datatypes back to the database. This can take time."
            liftIO $ runAddJsonbToSchema syncEnv
          liftIO $ runConsumedTxOutMigrationsMaybe syncEnv
          unless useLedger $ liftIO $ do
            logInfo trce "Migrating to a no ledger schema"
            DB.noLedgerMigrations dbEnv trce
          insertValidateGenesisDist syncEnv (dncNetworkName syncNodeConfigFromFile) genCfg (useShelleyInit syncNodeConfigFromFile)

          -- communication channel between datalayer thread and chainsync-client thread
          threadChannels <- liftIO newThreadChannels
          liftIO $
            race_
              -- We split the main thread into two parts to allow for graceful shutdown of the main App db thread.
              (runDbThread syncEnv metricsSetters threadChannels)
              ( mapConcurrently_
                  id
                  [ runSyncNodeClient metricsSetters syncEnv iomgr trce threadChannels (enpSocketPath syncNodeParams)
                  , runFetchOffChainPoolThread syncEnv syncNodeConfigFromFile
                  , runFetchOffChainVoteThread syncEnv syncNodeConfigFromFile
                  , runLedgerStateWriteThread (getTrace syncEnv) (envLedgerEnv syncEnv)
                  ]
              )
    )
  where
    useShelleyInit :: SyncNodeConfig -> Bool
    useShelleyInit cfg =
      case dncShelleyHardFork cfg of
        CardanoTriggerHardForkAtEpoch (EpochNo 0) -> True
        _other -> False

    removeJsonbFromSchemaConfig = ioRemoveJsonbFromSchema $ soptInsertOptions syncOptions
    maybeLedgerDir = enpMaybeLedgerStateDir syncNodeParams

logProtocolMagicId :: Trace IO Text -> Crypto.ProtocolMagicId -> ExceptT SyncNodeError IO ()
logProtocolMagicId tracer pm =
  liftIO
    . logInfo tracer
    $ mconcat
      [ "NetworkMagic: "
      , textShow (Crypto.unProtocolMagicId pm)
      ]

-- -------------------------------------------------------------------------------------------------

extractSyncOptions :: SyncNodeParams -> Bool -> SyncNodeConfig -> SyncOptions
extractSyncOptions snp aop snc =
  SyncOptions
    { soptEpochAndCacheEnabled =
        not isTxOutConsumedBootstrap'
          && ioInOut iopts
          && not (enpEpochDisabled snp || not (enpHasCache snp))
    , soptAbortOnInvalid = aop
    , soptCache = enpHasCache snp
    , soptPruneConsumeMigration =
        initPruneConsumeMigration
          isTxOutConsumed'
          isTxOutConsumedPrune'
          isTxOutConsumedBootstrap'
          forceTxIn'
    , soptInsertOptions = iopts
    , snapshotEveryFollowing = enpSnEveryFollowing snp
    , snapshotEveryLagging = enpSnEveryLagging snp
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
        , ioPoolStats = isPoolStatsEnabled (sioPoolStats (dncInsertOptions snc))
        , ioGov = useGovernance
        , ioRemoveJsonbFromSchema = isRemoveJsonbFromSchemaEnabled (sioRemoveJsonbFromSchema (dncInsertOptions snc))
        , ioTxOutVariantType = ioTxOutVariantType'
        }

    useLedger = sioLedger (dncInsertOptions snc) == LedgerEnable
    useOffchainPoolData =
      isOffchainPoolDataEnabled (sioOffchainPoolData (dncInsertOptions snc))
    useGovernance =
      isGovernanceEnabled (sioGovernance (dncInsertOptions snc))

    isTxOutConsumed' = isTxOutConsumed . sioTxOut . dncInsertOptions $ snc
    isTxOutConsumedPrune' = isTxOutConsumedPrune . sioTxOut . dncInsertOptions $ snc
    isTxOutConsumedBootstrap' = isTxOutConsumedBootstrap . sioTxOut . dncInsertOptions $ snc
    isTxOutEnabled' = isTxOutEnabled . sioTxOut . dncInsertOptions $ snc
    forceTxIn' = forceTxIn . sioTxOut . dncInsertOptions $ snc
    ioTxOutVariantType' = txOutConfigToTableType $ sioTxOut $ dncInsertOptions snc

startupReport :: Trace IO Text -> Bool -> SyncNodeParams -> IO ()
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
