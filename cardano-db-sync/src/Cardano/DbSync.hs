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
  renderFetchError,
) where

import Cardano.BM.Trace (Trace, logError, logInfo, logWarning)
import Cardano.Db (textShow)
import qualified Cardano.Db as Db
import Cardano.DbSync.Api
import Cardano.DbSync.Config (configureLogging)
import Cardano.DbSync.Config.Types (
  ConfigFile (..),
  GenesisFile (..),
  LedgerStateDir (..),
  NetworkName (..),
  SocketPath (..),
  SyncCommand (..),
  SyncNodeParams (..),
 )
import Cardano.DbSync.Era.Shelley.Offline.Http (
  FetchError (..),
  SimplifiedPoolOfflineData (..),
  httpGetPoolOfflineData,
  parsePoolUrl,
  renderFetchError,
  spodJson,
 )
import Cardano.DbSync.Rollback (unsafeRollback)
import Cardano.DbSync.Sync (runSyncNode)
import Cardano.DbSync.Tracing.ToObjectOrphans ()
import Cardano.DbSync.Types
import Cardano.DbSync.Util (readAbortOnPanic)
import Cardano.Prelude hiding (Nat, (%))
import Control.Monad.Extra (whenJust)
import Control.Monad.Trans.Except.Exit (orDie)
import Control.Monad.Trans.Except.Extra (newExceptT)
import qualified Data.Text as Text
import Data.Version (showVersion)
import Ouroboros.Network.NodeToClient (IOManager, withIOManager)
import Paths_cardano_db_sync (version)

runDbSyncNode :: MetricSetters -> [(Text, Text)] -> SyncNodeParams -> IO ()
runDbSyncNode metricsSetters knownMigrations params =
  withIOManager $ \iomgr -> do
    trce <- configureLogging params "db-sync-node"

    aop <- readAbortOnPanic
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
  pgConfig <- orDie Db.renderPGPassError $ newExceptT (Db.readPGPass $ enpPGPassSource params)

  mErrors <- liftIO $ Db.validateMigrations dbMigrationDir knownMigrations
  whenJust mErrors $ \(unknown, stage4orNewStage3) ->
    if stage4orNewStage3
      then logWarning trce $ Db.renderMigrationValidateError unknown
      else do
        let msg = Db.renderMigrationValidateError unknown
        logError trce msg
        panic msg

  logInfo trce "Schema migration files validated"

  let runMigration mode = do
        msg <- Db.getMaintenancePsqlConf pgConfig
        logInfo trce $ "Running database migrations in mode " <> textShow mode
        logInfo trce msg
        when (mode `elem` [Db.Indexes, Db.Full]) $ logWarning trce indexesMsg
        Db.runMigrations pgConfig True dbMigrationDir (Just $ Db.LogFileDir "/tmp") mode
  (ranAll, unofficial) <- if enpForceIndexes params then runMigration Db.Full else runMigration Db.Initial
  unless (null unofficial) $
    logWarning trce $
      "Unofficial migration scripts found: " <> textShow unofficial

  if ranAll
    then logInfo trce "All migrations were executed"
    else logInfo trce "Some migrations were not executed. They need to run when syncing has started."

  if enpForceIndexes params
    then logInfo trce "All user indexes were created"
    else logInfo trce "New user indexes were not created. They may be created later if necessary."

  let connectionString = Db.toConnectionString pgConfig

  -- For testing and debugging.
  whenJust (enpMaybeRollback params) $ \slotNo ->
    void $ unsafeRollback trce pgConfig slotNo
  runSyncNode metricsSetters trce iomgr connectionString ranAll (void . runMigration) params syncOpts
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

-- -------------------------------------------------------------------------------------------------

extractSyncOptions :: SyncNodeParams -> Bool -> SyncOptions
extractSyncOptions snp aop =
  SyncOptions
    { soptExtended = enpExtended snp
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
  logInfo trce $ mconcat ["Option disable-epoch: ", textShow (not $ enpExtended params)]
  logInfo trce $ mconcat ["Option skip-fix: ", textShow (enpSkipFix params)]
  logInfo trce $ mconcat ["Option fix-only: ", textShow (enpOnlyFix params)]
  logInfo trce $ mconcat ["Option force-indexes: ", textShow (enpForceIndexes params)]
  logInfo trce $ mconcat ["Option disable-multiassets: ", textShow (not $ enpHasMultiAssets params)]
  logInfo trce $ mconcat ["Option disable-metadata: ", textShow (not $ enpHasMetadata params)]
  logInfo trce $ mconcat ["Option disable-plutus-extra: ", textShow (not $ enpHasPlutusExtra params)]
  logInfo trce $ mconcat ["Option disable-offline-data: ", textShow (not $ enpHasOfflineData params)]
  logInfo trce $ mconcat ["Option turbo: ", textShow (enpTurboMode params)]
  logInfo trce $ mconcat ["Option full: ", textShow (enpFullMode params)]
  logInfo trce $ mconcat ["Enviroment variable DbSyncAbortOnPanic: ", textShow aop]
