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
  Db.MigrationDir (..),
  runDbSyncNode,
  runDbSync,
  -- For testing and debugging
  OffChainFetchError (..),
  SimplifiedOffChainPoolData (..),
  extractSyncOptions,
) where

import qualified Cardano.BM.Configuration as BM
import qualified Cardano.BM.Data.Severity as BM
import Cardano.BM.Trace (Trace)
import qualified Cardano.Crypto as Crypto
import qualified Cardano.Db as DB
import qualified Cardano.Db as Db
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (InsertOptions (..), RunMigration, SyncEnv (..), SyncOptions (..), envLedgerEnv)
import Cardano.DbSync.Config (configureLogging)
import Cardano.DbSync.Config.Cardano
import Cardano.DbSync.Config.Types
import Cardano.DbSync.Database
import Cardano.DbSync.DbAction
import Cardano.DbSync.Era
import Cardano.DbSync.Error
import Cardano.DbSync.Ledger.State
import Cardano.DbSync.OffChain (runFetchOffChainPoolThread, runFetchOffChainVoteThread)
import Cardano.DbSync.Rollback (unsafeRollback)
import Cardano.DbSync.Sync (runSyncNodeClient)
import Cardano.DbSync.Tracing.ToObjectOrphans ()
import Cardano.DbSync.Types
import Cardano.DbSync.Util.Constraint (queryIsJsonbInSchema)
import Cardano.DbSync.Util.Logging (LogContext (..), initLogCtx, logErrorCtx, logInfoCtx, logWarningCtx)
import Cardano.Prelude hiding (Nat, (%))
import Cardano.Slotting.Slot (EpochNo (..))
import Control.Concurrent.Async
import Control.Monad.Extra (whenJust)
import qualified Data.Strict.Maybe as Strict
import qualified Data.Text as Text
import Data.Version (showVersion)
import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn)
import qualified Ouroboros.Consensus.HardFork.Simple as HardFork
import Ouroboros.Network.NodeToClient (IOManager, withIOManager)
import Paths_cardano_db_sync (version)
import System.Directory (createDirectoryIfMissing)
import Prelude (id)

runDbSyncNode :: MetricSetters -> [(Text, Text)] -> SyncNodeParams -> SyncNodeConfig -> IO ()
runDbSyncNode metricsSetters knownMigrations params syncNodeConfigFromFile =
  withIOManager $ \iomgr -> do
    severity <- BM.minSeverity . dncLoggingConfig $ syncNodeConfigFromFile
    trce <- configureLogging syncNodeConfigFromFile "db-sync-node"

    abortOnPanic <- hasAbortOnPanicEnv
    startupReport trce severity abortOnPanic params

    runDbSync metricsSetters knownMigrations iomgr trce severity params syncNodeConfigFromFile abortOnPanic

runDbSync ::
  MetricSetters ->
  [(Text, Text)] ->
  IOManager ->
  Trace IO Text ->
  BM.Severity ->
  SyncNodeParams ->
  SyncNodeConfig ->
  -- Should abort on panic
  Bool ->
  IO ()
runDbSync metricsSetters knownMigrations iomgr trce severity params syncNodeConfigFromFile abortOnPanic = do
  let logCtx = initLogCtx severity "runDbSync" "Cardano.DbSync"
  logInfoCtx trce $ logCtx {lcMessage = "Current sync options: " <> textShow syncOpts}

  -- Read the PG connection info
  pgConfig <- runOrThrowIO (Db.readPGPass $ enpPGPassSource params)

  mErrors <- liftIO $ Db.validateMigrations dbMigrationDir knownMigrations
  whenJust mErrors $ \(unknown, stage4orNewStage3) ->
    if stage4orNewStage3
      then logWarningCtx trce $ logCtx {lcMessage = Db.renderMigrationValidateError unknown}
      else do
        let msg = Db.renderMigrationValidateError unknown
        logErrorCtx trce $ logCtx {lcMessage = msg}
        throwIO unknown

  logInfoCtx trce $ logCtx {lcMessage = "Schema migration files validated"}

  let runMigration mode = do
        msg <- Db.getMaintenancePsqlConf pgConfig
        logInfoCtx trce $ logCtx {lcMessage = "Running database migrations in mode " <> textShow mode}
        logInfoCtx trce $ logCtx {lcMessage = msg}
        when (mode `elem` [Db.Indexes, Db.Full]) $ logWarningCtx trce $ logCtx {lcMessage = indexesMsg}
        Db.runMigrations pgConfig True dbMigrationDir (Just $ Db.LogFileDir "/tmp") mode (txOutConfigToTableType txOutConfig)
  (ranMigrations, unofficial) <- if enpForceIndexes params then runMigration Db.Full else runMigration Db.Initial
  unless (null unofficial) $
    logWarningCtx trce $
      logCtx {lcMessage = "Unofficial migration scripts found: " <> textShow unofficial}

  logInfoCtx trce $
    logCtx
      { lcMessage =
          if ranMigrations
            then "All migrations were executed"
            else "Some migrations were not executed. They need to run when syncing has started."
      }

  logInfoCtx trce $
    logCtx
      { lcMessage =
          if enpForceIndexes params
            then "All user indexes were created"
            else "New user indexes were not created. They may be created later if necessary."
      }

  let connectionString = Db.toConnectionString pgConfig

  -- For testing and debugging.
  whenJust (enpMaybeRollback params) $ \slotNo ->
    void $ unsafeRollback trce severity (txOutConfigToTableType txOutConfig) pgConfig slotNo
  runSyncNode
    metricsSetters
    trce
    severity
    iomgr
    connectionString
    ranMigrations
    (void . runMigration)
    syncNodeConfigFromFile
    params
    syncOpts
  where
    dbMigrationDir :: Db.MigrationDir
    dbMigrationDir = enpMigrationDir params

    indexesMsg :: Text
    indexesMsg =
      mconcat
        [ "Creating Indexes. This may require an extended period of time to perform."
        , " Setting a higher maintenance_work_mem from Postgres usually speeds up this process."
        , " These indexes are not used by db-sync but are meant for clients. If you want to skip"
        , " some of these indexes, you can stop db-sync, delete or modify any migration-4-* files"
        , " in the schema directory and restart it."
        ]

    syncOpts = extractSyncOptions params abortOnPanic syncNodeConfigFromFile

    txOutConfig = sioTxOut $ dncInsertOptions syncNodeConfigFromFile

runSyncNode ::
  MetricSetters ->
  Trace IO Text ->
  BM.Severity ->
  IOManager ->
  ConnectionString ->
  -- | migrations were ran on startup
  Bool ->
  -- | run migration function
  RunMigration ->
  SyncNodeConfig ->
  SyncNodeParams ->
  SyncOptions ->
  IO ()
runSyncNode metricsSetters trce severity iomgr dbConnString ranMigrations runMigrationFnc syncNodeConfigFromFile syncNodeParams syncOptions = do
  let logCtx = initLogCtx severity "runSyncNode" "Cardano.DbSync"
  whenJust maybeLedgerDir $
    \enpLedgerStateDir -> do
      createDirectoryIfMissing True (unLedgerStateDir enpLedgerStateDir)
  logInfoCtx trce $
    logCtx {lcMessage = "Using byron genesis file from: " <> (show . unGenesisFile $ dncByronGenesisFile syncNodeConfigFromFile)}
  logInfoCtx trce $
    logCtx {lcMessage = "Using shelley genesis file from: " <> (show . unGenesisFile $ dncShelleyGenesisFile syncNodeConfigFromFile)}
  logInfoCtx trce $
    logCtx {lcMessage = "Using alonzo genesis file from: " <> (show . unGenesisFile $ dncAlonzoGenesisFile syncNodeConfigFromFile)}

  let useLedger = shouldUseLedger (sioLedger $ dncInsertOptions syncNodeConfigFromFile)

  Db.runIohkLogging trce $
    withPostgresqlConn dbConnString $
      \backend -> liftIO $ do
        runOrThrowIO $ runExceptT $ do
          genCfg <- readCardanoGenesisConfig syncNodeConfigFromFile
          isJsonbInSchema <- queryIsJsonbInSchema backend
          logProtocolMagicId trce severity $ genesisProtocolMagicId genCfg
          syncEnv <-
            ExceptT $
              mkSyncEnvFromConfig
                trce
                backend
                dbConnString
                syncOptions
                genCfg
                syncNodeConfigFromFile
                syncNodeParams
                ranMigrations
                runMigrationFnc

          -- Warn the user that jsonb datatypes are being removed from the database schema.
          when (isJsonbInSchema && removeJsonbFromSchemaConfig) $ do
            liftIO $ logWarningCtx trce $ logCtx {lcMessage = "Removing jsonb datatypes from the database. This can take time."}
            liftIO $ runRemoveJsonbFromSchema syncEnv

          -- Warn the user that jsonb datatypes are being added to the database schema.
          when (not isJsonbInSchema && not removeJsonbFromSchemaConfig) $ do
            liftIO $ logWarningCtx trce $ logCtx {lcMessage = "Adding jsonb datatypes back to the database. This can take time."}
            liftIO $ runAddJsonbToSchema syncEnv
          liftIO $ runExtraMigrationsMaybe syncEnv
          unless useLedger $ liftIO $ do
            logInfoCtx trce $ logCtx {lcMessage = "Migrating to a no ledger schema"}
            Db.noLedgerMigrations backend trce
          insertValidateGenesisDist syncEnv (dncNetworkName syncNodeConfigFromFile) genCfg (useShelleyInit syncNodeConfigFromFile)

          -- communication channel between datalayer thread and chainsync-client thread
          threadChannels <- liftIO newThreadChannels
          liftIO $
            mapConcurrently_
              id
              [ runDbThread syncEnv metricsSetters threadChannels
              , runSyncNodeClient metricsSetters syncEnv iomgr trce threadChannels (enpSocketPath syncNodeParams)
              , runFetchOffChainPoolThread syncEnv
              , runFetchOffChainVoteThread syncEnv
              , runLedgerStateWriteThread (getTrace syncEnv) severity (envLedgerEnv syncEnv)
              ]
  where
    useShelleyInit :: SyncNodeConfig -> Bool
    useShelleyInit cfg =
      case dncShelleyHardFork cfg of
        HardFork.TriggerHardForkAtEpoch (EpochNo 0) -> True
        _other -> False
    removeJsonbFromSchemaConfig = ioRemoveJsonbFromSchema $ soptInsertOptions syncOptions
    maybeLedgerDir = enpMaybeLedgerStateDir syncNodeParams

logProtocolMagicId :: Trace IO Text -> BM.Severity -> Crypto.ProtocolMagicId -> ExceptT SyncNodeError IO ()
logProtocolMagicId tracer severity pm = do
  let logCtx = initLogCtx severity "logProtocolMagicId" "Cardano.DbSync"
  liftIO
    . logInfoCtx tracer
    $ logCtx
      { lcMessage =
          mconcat
            [ "NetworkMagic: "
            , textShow (Crypto.unProtocolMagicId pm)
            ]
      }

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
    , soptSkipFix = enpSkipFix snp
    , soptOnlyFix = enpOnlyFix snp
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
        , ioTxOutTableType = ioTxOutTableType'
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
    ioTxOutTableType' = txOutConfigToTableType $ sioTxOut $ dncInsertOptions snc

startupReport :: Trace IO Text -> BM.Severity -> Bool -> SyncNodeParams -> IO ()
startupReport trce severity aop params = do
  let logCtx = initLogCtx severity "runSyncNode" "Cardano.DbSync"
  logInfoCtx trce $ logCtx {lcMessage = mconcat ["Version number: ", Text.pack (showVersion version)]}
  logInfoCtx trce $ logCtx {lcMessage = mconcat ["Git hash: ", Db.gitRev]}
  logInfoCtx trce $ logCtx {lcMessage = mconcat ["Enviroment variable DbSyncAbortOnPanic: ", textShow aop]}
  logInfoCtx trce $ logCtx {lcMessage = textShow params}

txOutConfigToTableType :: TxOutConfig -> DB.TxOutTableType
txOutConfigToTableType config = case config of
  TxOutEnable (UseTxOutAddress flag) -> if flag then DB.TxOutVariantAddress else DB.TxOutCore
  TxOutDisable -> DB.TxOutCore
  TxOutConsumed _ (UseTxOutAddress flag) -> if flag then DB.TxOutVariantAddress else DB.TxOutCore
  TxOutConsumedPrune _ (UseTxOutAddress flag) -> if flag then DB.TxOutVariantAddress else DB.TxOutCore
  TxOutConsumedBootstrap _ (UseTxOutAddress flag) -> if flag then DB.TxOutVariantAddress else DB.TxOutCore
