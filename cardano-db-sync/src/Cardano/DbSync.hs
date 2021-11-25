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

import           Database.Persist.Postgresql (withPostgresqlConn)

runDbSyncNode :: MetricSetters -> Bool -> [(Text, Text)] -> SyncNodeParams -> IO ()
runDbSyncNode metricsSetters extended knownMigrations params = do

    -- Read the PG connection info
    pgConfig <- Db.readPGPassFileEnv Nothing

    trce <- configureLogging params "db-sync-node"

    orDieWithLog Db.renderMigrationValidateError trce $
      Db.validateMigrations dbMigrationDir knownMigrations

    logInfo trce "Schema migration files validated"
    logInfo trce "Running database migrations"

    aop <- readAbortOnPanic
    if aop
      then logWarning trce "Enviroment variable DbSyncAbortOnPanic: True"
      else logInfo trce "Enviroment variable DbSyncAbortOnPanic: False"

    Db.runMigrations pgConfig True dbMigrationDir (Just $ Db.LogFileDir "/tmp")

    let connectionString = Db.toConnectionString pgConfig

    Db.runIohkLogging trce $ withPostgresqlConn connectionString $ \backend ->
      lift $ do
        -- For testing and debugging.
        whenJust (enpMaybeRollback params) $ \ slotNo ->
          void $ unsafeRollback trce slotNo

        runSyncNode metricsSetters trce backend extended params

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

