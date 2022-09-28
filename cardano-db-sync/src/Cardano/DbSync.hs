{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync
  ( ConfigFile (..)
  , SyncCommand (..)
  , SyncNodeParams (..)
  , GenesisFile (..)
  , LedgerStateDir (..)
  , NetworkName (..)
  , SocketPath (..)
  , Db.MigrationDir (..)

  , runDbSyncNode
  , runDbSync

  -- For testing and debugging
  , FetchError (..)
  , SimplifiedPoolOfflineData (..)
  , httpGetPoolOfflineData
  , parsePoolUrl
  , renderFetchError
  ) where

import           Cardano.Prelude hiding (Nat, option, (%))

import           Cardano.BM.Trace (Trace, logError, logInfo, logWarning)

import           Cardano.Db (textShow)
import qualified Cardano.Db as Db

import           Cardano.DbSync.Era.Shelley.Offline.Http (FetchError (..),
                   SimplifiedPoolOfflineData (..), httpGetPoolOfflineData, parsePoolUrl,
                   renderFetchError, spodJson)
import           Cardano.DbSync.Rollback (unsafeRollback)
import           Cardano.DbSync.Util (readAbortOnPanic)

import           Cardano.DbSync.Config (configureLogging)
import           Cardano.DbSync.Config.Types (ConfigFile (..), GenesisFile (..),
                   LedgerStateDir (..), NetworkName (..), SocketPath (..), SyncCommand (..),
                   SyncNodeParams (..))
import           Cardano.DbSync.Sync (runSyncNode)
import           Cardano.DbSync.Tracing.ToObjectOrphans ()
import           Cardano.DbSync.Types

import           Control.Monad.Extra (whenJust)
import           Control.Monad.Trans.Except.Exit (orDie)
import           Control.Monad.Trans.Except.Extra (newExceptT)

import qualified Data.Text as Text
import           Data.Version (showVersion)

import           Ouroboros.Network.NodeToClient (IOManager, withIOManager)

import           Paths_cardano_db_sync (version)

runDbSyncNode :: MetricSetters -> [(Text, Text)] -> SyncNodeParams -> IO ()
runDbSyncNode metricsSetters knownMigrations params =
  withIOManager $ \iomgr -> do
    trce <- configureLogging params "db-sync-node"

    aop <- readAbortOnPanic
    startupReport trce aop params

    runDbSync metricsSetters knownMigrations iomgr trce params aop 500 10000

runDbSync
    :: MetricSetters -> [(Text, Text)] -> IOManager -> Trace IO Text
    -> SyncNodeParams -> Bool -> Word64 -> Word64 -> IO ()
runDbSync metricsSetters knownMigrations iomgr trce params aop snEveryFollowing snEveryLagging = do
    -- Read the PG connection info
    pgConfig <- orDie Db.renderPGPassError $ newExceptT (Db.readPGPass $ enpPGPassSource params)

    orDieWithLog Db.renderMigrationValidateError trce $
      Db.validateMigrations dbMigrationDir knownMigrations

    logInfo trce "Schema migration files validated"
    logInfo trce "Running database migrations"

    unofficial <- Db.runMigrations pgConfig True dbMigrationDir (Just $ Db.LogFileDir "/tmp")
    unless (null unofficial) $
      logWarning trce $ "Unofficial migration scripts found: " <> textShow unofficial

    let connectionString = Db.toConnectionString pgConfig

    -- For testing and debugging.
    whenJust (enpMaybeRollback params) $ \ slotNo ->
      void $ unsafeRollback trce pgConfig slotNo
    runSyncNode metricsSetters trce iomgr aop snEveryFollowing snEveryLagging connectionString params

  where
    dbMigrationDir :: Db.MigrationDir
    dbMigrationDir = enpMigrationDir params

-- -------------------------------------------------------------------------------------------------

-- Log error to Trace and panic.
orDieWithLog :: (t -> Text) -> Trace IO Text -> ExceptT t IO () -> IO ()
orDieWithLog render trce e = do
  runExceptT e >>= \case
    Left errors -> do
      let errorStr = render errors
      liftIO $ logError trce errorStr
      panic errorStr
    Right () -> pure ()

startupReport :: Trace IO Text -> Bool -> SyncNodeParams -> IO ()
startupReport trce aop params = do
  logWarning trce $ mconcat ["Version number: ", Text.pack (showVersion version)]
  logWarning trce $ mconcat ["Git hash: ", Db.gitRev]
  logWarning trce $ mconcat ["Option disable-ledger: ", textShow (not $ enpHasLedger params)]
  logWarning trce $ mconcat ["Option disable-cache: ", textShow (not $ enpHasCache params)]
  logWarning trce $ mconcat ["Option disable-epoch: ", textShow (not $ enpExtended params)]
  logWarning trce $ mconcat ["Enviroment variable DbSyncAbortOnPanic: ", textShow aop]
