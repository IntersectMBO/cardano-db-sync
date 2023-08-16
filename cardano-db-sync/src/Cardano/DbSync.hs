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
  FetchError (..),
  SimplifiedPoolOfflineData (..),
  httpGetPoolOfflineData,
  parsePoolUrl,
) where

import Cardano.BM.Trace (Trace, logError, logInfo, logWarning)
import qualified Cardano.Crypto as Crypto
import Cardano.Db (textShow)
import qualified Cardano.Db as Db
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (InsertOptions (..), RunMigration, SyncOptions (..), envLedgerEnv)
import Cardano.DbSync.Config (configureLogging, readSyncNodeConfig)
import Cardano.DbSync.Config.Cardano
import Cardano.DbSync.Config.Types (
  ConfigFile (..),
  GenesisFile (..),
  LedgerStateDir (..),
  NetworkName (..),
  SocketPath (..),
  SyncCommand (..),
  SyncNodeConfig (..),
  SyncNodeParams (..),
 )
import Cardano.DbSync.Database
import Cardano.DbSync.DbAction
import Cardano.DbSync.Era
import Cardano.DbSync.Era.Shelley.Offline.Http (
  FetchError (..),
  SimplifiedPoolOfflineData (..),
  httpGetPoolOfflineData,
  parsePoolUrl,
  spodJson,
 )
import Cardano.DbSync.Error (SyncNodeError, hasAbortOnPanicEnv, runOrThrowIO)
import Cardano.DbSync.Ledger.State
import Cardano.DbSync.Rollback (unsafeRollback)
import Cardano.DbSync.Sync (runSyncNodeClient)
import Cardano.DbSync.Tracing.ToObjectOrphans ()
import Cardano.DbSync.Types
import Cardano.Prelude hiding (Nat, (%))
import Cardano.Slotting.Slot (EpochNo (..))
import Control.Concurrent.Async
import Control.Monad.Extra (whenJust)
import qualified Data.Text as Text
import Data.Version (showVersion)
import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn)
import qualified Ouroboros.Consensus.HardFork.Simple as HardFork
import Ouroboros.Network.NodeToClient (IOManager, withIOManager)
import Paths_cardano_db_sync (version)
import System.Directory (createDirectoryIfMissing)
import Prelude (id)

runDbSyncNode :: MetricSetters -> [(Text, Text)] -> SyncNodeParams -> IO ()
runDbSyncNode metricsSetters knownMigrations params =
  withIOManager $ \iomgr -> do
    trce <- configureLogging params "db-sync-node"

    aop <- hasAbortOnPanicEnv
    startupReport trce aop params

    runDbSync metricsSetters knownMigrations iomgr trce params aop

runDbSync ::
  MetricSetters ->
  [(Text, Text)] ->
  IOManager ->
  Trace IO Text ->
  SyncNodeParams ->
  Bool ->
  IO ()
runDbSync metricsSetters knownMigrations iomgr trce params aop = do
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
        Db.runMigrations pgConfig True dbMigrationDir (Just $ Db.LogFileDir "/tmp") mode
  (ranMigrations, unofficial) <- if enpForceIndexes params then runMigration Db.Full else runMigration Db.Initial
  unless (null unofficial) $
    logWarning trce $
      "Unofficial migration scripts found: " <> textShow unofficial

  if ranMigrations
    then logInfo trce "All migrations were executed"
    else logInfo trce "Some migrations were not executed. They need to run when syncing has started."

  if enpForceIndexes params
    then logInfo trce "All user indexes were created"
    else logInfo trce "New user indexes were not created. They may be created later if necessary."

  let connectionString = Db.toConnectionString pgConfig

  -- For testing and debugging.
  whenJust (enpMaybeRollback params) $ \slotNo ->
    void $ unsafeRollback trce pgConfig slotNo
  runSyncNode metricsSetters trce iomgr connectionString ranMigrations (void . runMigration) params syncOpts
  where
    dbMigrationDir :: Db.MigrationDir
    dbMigrationDir = enpMigrationDir params

    indexesMsg :: Text
    indexesMsg =
      mconcat
        [ "Creating Indexes. This may take a while."
        , " Setting a higher maintenance_work_mem from Postgres usually speeds up this process."
        , " These indexes are not used by db-sync but are meant for clients. If you want to skip"
        , " some of these indexes, you can stop db-sync, delete or modify any migration-4-* files"
        , " in the schema directory and restart it."
        ]

    syncOpts = extractSyncOptions params aop

runSyncNode ::
  MetricSetters ->
  Trace IO Text ->
  IOManager ->
  ConnectionString ->
  -- | migrations were ran on startup
  Bool ->
  -- | run migration function
  RunMigration ->
  SyncNodeParams ->
  SyncOptions ->
  IO ()
runSyncNode metricsSetters trce iomgr dbConnString ranMigrations runMigrationFnc syncNodeParams syncOptions = do
  syncNodeConfig <- readSyncNodeConfig configFile
  whenJust maybeLedgerDir $
    \enpLedgerStateDir -> do
      createDirectoryIfMissing True (unLedgerStateDir enpLedgerStateDir)

  logInfo trce $ "Using byron genesis file from: " <> (show . unGenesisFile $ dncByronGenesisFile syncNodeConfig)
  logInfo trce $ "Using shelley genesis file from: " <> (show . unGenesisFile $ dncShelleyGenesisFile syncNodeConfig)
  logInfo trce $ "Using alonzo genesis file from: " <> (show . unGenesisFile $ dncAlonzoGenesisFile syncNodeConfig)
  Db.runIohkLogging trce $
    withPostgresqlConn dbConnString $ \backend -> liftIO $ do
      runOrThrowIO $ runExceptT $ do
        genCfg <- readCardanoGenesisConfig syncNodeConfig
        logProtocolMagicId trce $ genesisProtocolMagicId genCfg

        syncEnv <-
          ExceptT $
            mkSyncEnvFromConfig
              trce
              dbConnString
              backend
              syncOptions
              genCfg
              syncNodeParams
              ranMigrations
              runMigrationFnc
        liftIO $ runExtraMigrationsMaybe syncEnv
        unless (enpShouldUseLedger syncNodeParams) $ liftIO $ do
          logInfo trce "Migrating to a no ledger schema"
          Db.noLedgerMigrations backend trce
        insertValidateGenesisDist syncEnv (dncNetworkName syncNodeConfig) genCfg (useShelleyInit syncNodeConfig)

        -- communication channel between datalayer thread and chainsync-client thread
        threadChannels <- liftIO newThreadChannels
        liftIO $
          mapConcurrently_
            id
            [ runDbThread syncEnv metricsSetters threadChannels
            , runSyncNodeClient metricsSetters syncEnv iomgr trce threadChannels (enpSocketPath syncNodeParams)
            , runOfflineFetchThread syncEnv
            , runLedgerStateWriteThread (getTrace syncEnv) (envLedgerEnv syncEnv)
            ]
  where
    useShelleyInit :: SyncNodeConfig -> Bool
    useShelleyInit cfg =
      case dncShelleyHardFork cfg of
        HardFork.TriggerHardForkAtEpoch (EpochNo 0) -> True
        _ -> False

    configFile = enpConfigFile syncNodeParams
    maybeLedgerDir = enpMaybeLedgerStateDir syncNodeParams

logProtocolMagicId :: Trace IO Text -> Crypto.ProtocolMagicId -> ExceptT SyncNodeError IO ()
logProtocolMagicId tracer pm =
  liftIO . logInfo tracer $
    mconcat
      [ "NetworkMagic: "
      , textShow (Crypto.unProtocolMagicId pm)
      ]

-- -------------------------------------------------------------------------------------------------

extractSyncOptions :: SyncNodeParams -> Bool -> SyncOptions
extractSyncOptions snp aop =
  SyncOptions
    { soptEpochAndCacheEnabled = not $ enpEpochDisabled snp && enpHasCache snp
    , soptAbortOnInvalid = aop
    , soptCache = enpHasCache snp
    , soptSkipFix = enpSkipFix snp
    , soptOnlyFix = enpOnlyFix snp
    , soptInsertOptions = iopts
    , snapshotEveryFollowing = enpSnEveryFollowing snp
    , snapshotEveryLagging = enpSnEveryLagging snp
    }
  where
    iopts
      | enpFullMode snp = fullInsertOptions
      | enpTurboMode snp = turboInsertOptions
      | otherwise =
          InsertOptions
            { ioMultiAssets = enpHasMultiAssets snp
            , ioMetadata = enpHasMetadata snp
            , ioPlutusExtra = enpHasPlutusExtra snp
            , ioOfflineData = enpHasOfflineData snp
            }

startupReport :: Trace IO Text -> Bool -> SyncNodeParams -> IO ()
startupReport trce aop params = do
  logInfo trce $ mconcat ["Version number: ", Text.pack (showVersion version)]
  logInfo trce $ mconcat ["Git hash: ", Db.gitRev]
  logInfo trce $ mconcat ["Option disable-ledger: ", textShow (not $ enpShouldUseLedger params)]
  logInfo trce $ mconcat ["Option disable-cache: ", textShow (not $ enpHasCache params)]
  logInfo trce $ mconcat ["Option disable-epoch: ", textShow (enpEpochDisabled params)]
  logInfo trce $ mconcat ["Option skip-fix: ", textShow (enpSkipFix params)]
  logInfo trce $ mconcat ["Option fix-only: ", textShow (enpOnlyFix params)]
  logInfo trce $ mconcat ["Option force-indexes: ", textShow (enpForceIndexes params)]
  logInfo trce $ mconcat ["Option consumed-tx-out: ", textShow (enpMigrateConsumed params)]
  logInfo trce $ mconcat ["Option prune-tx-out: ", textShow (enpPruneTxOut params)]
  logInfo trce $ mconcat ["Option disable-multiassets: ", textShow (not $ enpHasMultiAssets params)]
  logInfo trce $ mconcat ["Option disable-metadata: ", textShow (not $ enpHasMetadata params)]
  logInfo trce $ mconcat ["Option disable-plutus-extra: ", textShow (not $ enpHasPlutusExtra params)]
  logInfo trce $ mconcat ["Option disable-offline-data: ", textShow (not $ enpHasOfflineData params)]
  logInfo trce $ mconcat ["Option turbo: ", textShow (enpTurboMode params)]
  logInfo trce $ mconcat ["Option full: ", textShow (enpFullMode params)]
  logInfo trce $ mconcat ["Enviroment variable DbSyncAbortOnPanic: ", textShow aop]
