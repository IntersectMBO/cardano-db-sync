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

import Cardano.BM.Trace (Trace, logError, logInfo, logWarning)
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
    trce <- configureLogging syncNodeConfigFromFile "db-sync-node"

    abortOnPanic <- hasAbortOnPanicEnv
    startupReport trce abortOnPanic params

    runDbSync metricsSetters knownMigrations iomgr trce params syncNodeConfigFromFile abortOnPanic

runDbSync ::
  MetricSetters ->
  [(Text, Text)] ->
  IOManager ->
  Trace IO Text ->
  SyncNodeParams ->
  SyncNodeConfig ->
  -- Should abort on panic
  Bool ->
  IO ()
runDbSync metricsSetters knownMigrations iomgr trce params syncNodeConfigFromFile abortOnPanic = do
  logInfo trce $ textShow syncOpts

  -- Read the PG connection info
  pgConfig <- runOrThrowIO (Db.readPGPass $ enpPGPassSource params)

  mErrors <- liftIO $ Db.validateMigrations dbMigrationDir knownMigrations
  whenJust mErrors $ \(unknown, stage4orNewStage3) ->
    if stage4orNewStage3
      then logWarning trce $ Db.renderMigrationValidateError unknown
      else do
        let msg = Db.renderMigrationValidateError unknown
        logError trce msg
        throwIO unknown

  logInfo trce "Schema migration files validated"

  let runMigration mode = do
        msg <- Db.getMaintenancePsqlConf pgConfig
        logInfo trce $ "Running database migrations in mode " <> textShow mode
        logInfo trce msg
        when (mode `elem` [Db.Indexes, Db.Full]) $ logWarning trce indexesMsg
        Db.runMigrations pgConfig True dbMigrationDir (Just $ Db.LogFileDir "/tmp") mode (txOutConfigToTableType txOutConfig)
  (ranMigrations, unofficial) <- if enpForceIndexes params then runMigration Db.Full else runMigration Db.Initial
  unless (null unofficial) $
    logWarning trce $
      "Unofficial migration scripts found: "
        <> textShow unofficial

  if ranMigrations
    then logInfo trce "All migrations were executed"
    else logInfo trce "Some migrations were not executed. They need to run when syncing has started."

  if enpForceIndexes params
    then logInfo trce "All user indexes were created"
    else logInfo trce "New user indexes were not created. They may be created later if necessary."

  let connectionString = Db.toConnectionString pgConfig

  -- For testing and debugging.
  whenJust (enpMaybeRollback params) $ \slotNo ->
    void $ unsafeRollback trce (txOutConfigToTableType txOutConfig) pgConfig slotNo
  runSyncNode
    metricsSetters
    trce
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
runSyncNode metricsSetters trce iomgr dbConnString ranMigrations runMigrationFnc syncNodeConfigFromFile syncNodeParams syncOptions = do
  whenJust maybeLedgerDir $
    \enpLedgerStateDir -> do
      createDirectoryIfMissing True (unLedgerStateDir enpLedgerStateDir)
  logInfo trce $ "Using byron genesis file from: " <> (show . unGenesisFile $ dncByronGenesisFile syncNodeConfigFromFile)
  logInfo trce $ "Using shelley genesis file from: " <> (show . unGenesisFile $ dncShelleyGenesisFile syncNodeConfigFromFile)
  logInfo trce $ "Using alonzo genesis file from: " <> (show . unGenesisFile $ dncAlonzoGenesisFile syncNodeConfigFromFile)

  let useLedger = shouldUseLedger (sioLedger $ dncInsertOptions syncNodeConfigFromFile)

  Db.runIohkLogging trce $
    withPostgresqlConn dbConnString $
      \backend -> liftIO $ do
        runOrThrowIO $ runExceptT $ do
          genCfg <- readCardanoGenesisConfig syncNodeConfigFromFile
          isJsonbInSchema <- queryIsJsonbInSchema backend
          logProtocolMagicId trce $ genesisProtocolMagicId genCfg
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
            liftIO $ logWarning trce "Removing jsonb datatypes from the database. This can take time."
            liftIO $ runRemoveJsonbFromSchema syncEnv

          -- Warn the user that jsonb datatypes are being added to the database schema.
          when (not isJsonbInSchema && not removeJsonbFromSchemaConfig) $ do
            liftIO $ logWarning trce "Adding jsonb datatypes back to the database. This can take time."
            liftIO $ runAddJsonbToSchema syncEnv
          liftIO $ runExtraMigrationsMaybe syncEnv
          unless useLedger $ liftIO $ do
            logInfo trce "Migrating to a no ledger schema"
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
              , runLedgerStateWriteThread (getTrace syncEnv) (envLedgerEnv syncEnv)
              ]
  where
    useShelleyInit :: SyncNodeConfig -> Bool
    useShelleyInit cfg =
      case dncShelleyHardFork cfg of
        HardFork.TriggerHardForkAtEpoch (EpochNo 0) -> True
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

startupReport :: Trace IO Text -> Bool -> SyncNodeParams -> IO ()
startupReport trce aop params = do
  logInfo trce $ mconcat ["Version number: ", Text.pack (showVersion version)]
  logInfo trce $ mconcat ["Git hash: ", Db.gitRev]
  logInfo trce $ mconcat ["Enviroment variable DbSyncAbortOnPanic: ", textShow aop]
  logInfo trce $ textShow params

txOutConfigToTableType :: TxOutConfig -> DB.TxOutTableType
txOutConfigToTableType config = case config of
  TxOutEnable (UseTxOutAddress flag) -> if flag then DB.TxOutVariantAddress else DB.TxOutCore
  TxOutDisable -> DB.TxOutCore
  TxOutConsumed _ (UseTxOutAddress flag) -> if flag then DB.TxOutVariantAddress else DB.TxOutCore
  TxOutConsumedPrune _ (UseTxOutAddress flag) -> if flag then DB.TxOutVariantAddress else DB.TxOutCore
  TxOutConsumedBootstrap _ (UseTxOutAddress flag) -> if flag then DB.TxOutVariantAddress else DB.TxOutCore
