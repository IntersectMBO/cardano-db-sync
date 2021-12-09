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
  ) where

import           Cardano.Prelude hiding (Nat, option, (%))

import           Cardano.BM.Trace (Trace, logError, logInfo, logWarning)

import qualified Cardano.Db as Db

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

import           Ouroboros.Network.NodeToClient (IOManager, withIOManager)

import           Database.Persist.Postgresql (withPostgresqlConn)

runDbSyncNode :: MetricSetters -> [(Text, Text)] -> SyncNodeParams -> IO ()
runDbSyncNode metricsSetters knownMigrations params =
  withIOManager $ \iomgr -> do
    trce <- configureLogging params "db-sync-node"

    aop <- readAbortOnPanic
    if aop
      then logWarning trce "Enviroment variable DbSyncAbortOnPanic: True"
      else logInfo trce "Enviroment variable DbSyncAbortOnPanic: False"

    runDbSync metricsSetters knownMigrations iomgr trce params aop 500 10000

runDbSync :: MetricSetters -> [(Text, Text)] -> IOManager -> Trace IO Text
          -> SyncNodeParams -> Bool -> Word64 -> Word64 -> IO ()
runDbSync metricsSetters knownMigrations iomgr trce params aop snEveryFollowing snEveryLagging = do

    -- Read the PG connection info
    pgConfig <- Db.readPGPass (enpPGPassSource params)

    orDieWithLog Db.renderMigrationValidateError trce $
      Db.validateMigrations dbMigrationDir knownMigrations

    logInfo trce "Schema migration files validated"
    logInfo trce "Running database migrations"

    Db.runMigrations pgConfig True dbMigrationDir (Just $ Db.LogFileDir "/tmp")

    let connectionString = Db.toConnectionString pgConfig

    Db.runIohkLogging trce $ withPostgresqlConn connectionString $ \backend ->
      lift $ do
        -- For testing and debugging.
        whenJust (enpMaybeRollback params) $ \ slotNo ->
          void $ unsafeRollback trce pgConfig slotNo

        runSyncNode metricsSetters trce backend iomgr aop snEveryFollowing snEveryLagging params

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

