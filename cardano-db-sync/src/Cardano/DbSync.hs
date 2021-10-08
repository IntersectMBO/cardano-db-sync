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
  , DB.MigrationDir (..)

  , defDbSyncNodePlugin
  , runDbSyncNode
  ) where

import           Cardano.Prelude hiding (Nat, option, (%))

import           Control.Monad.Trans.Maybe (MaybeT (..))

import           Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))

import           Cardano.BM.Trace (Trace, logError, logInfo, logWarning)

import qualified Cardano.Db as DB

import           Cardano.DbSync.Database (runDbThread)
import           Cardano.DbSync.Era (insertValidateGenesisDist)
import           Cardano.DbSync.Plugin.Default (defDbSyncNodePlugin)
import           Cardano.DbSync.Rollback (unsafeRollback)
import           Cardano.DbSync.Util (readAbortOnPanic)

import           Cardano.DbSync.Api (SyncDataLayer (..))
import           Cardano.DbSync.Types (Block (..), MetricSetters)
import           Cardano.DbSync.Plugin (SyncNodePlugin)
import           Cardano.DbSync.Sync (runSyncNode)
import           Cardano.DbSync.Config.Types (ConfigFile (..), GenesisFile (..), LedgerStateDir (..),
                   MigrationDir (..), NetworkName (..), SocketPath (..), SyncCommand (..),
                   SyncNodeParams (..))
import           Cardano.DbSync.Config (configureLogging)
import           Cardano.DbSync.Tracing.ToObjectOrphans ()

import           Control.Monad.Extra (whenJust)

import           Database.Persist.Postgresql (withPostgresqlConn)

import           Database.Persist.Sql (SqlBackend)

import           Ouroboros.Network.Block (BlockNo (..))

runDbSyncNode :: MetricSetters -> (SqlBackend -> SyncNodePlugin) -> [(Text, Text)] -> SyncNodeParams -> IO ()
runDbSyncNode metricsSetters mkPlugin knownMigrations params = do

    -- Read the PG connection info
    pgConfig <- DB.readPGPassFileEnv Nothing

    trce <- configureLogging params "db-sync-node"

    orDieWithLog DB.renderMigrationValidateError trce $ DB.validateMigrations dbMigrationDir knownMigrations

    logInfo trce "Schema migration files validated"

    logInfo trce "Running database migrations"

    aop <- readAbortOnPanic
    if aop
      then logWarning trce "Enviroment variable DbSyncAbortOnPanic: True"
      else logInfo trce "Enviroment variable DbSyncAbortOnPanic: False"

    DB.runMigrations pgConfig True dbMigrationDir (Just $ DB.LogFileDir "/tmp")

    let connectionString = DB.toConnectionString pgConfig

    DB.runIohkLogging trce $ withPostgresqlConn connectionString $ \backend ->
      lift $ do
        -- For testing and debugging.
        whenJust (enpMaybeRollback params) $ \ slotNo ->
          void $ unsafeRollback trce slotNo

        -- The separation of `cardano-db` and `cardano-sync` is such a *HUGE* pain in the neck.
        runSyncNode (mkSyncDataLayer trce backend) backend metricsSetters trce (mkPlugin backend)
              params (insertValidateGenesisDist backend) runDbThread

  where
    -- This is only necessary because `cardano-db` and `cardano-sync` both define
    -- this newtype, but the later does not depend on the former.
    dbMigrationDir :: DB.MigrationDir
    dbMigrationDir = DB.MigrationDir $ unMigrationDir (enpMigrationDir params)

-- -------------------------------------------------------------------------------------------------

-- The base @DataLayer@.
mkSyncDataLayer :: Trace IO Text -> SqlBackend -> SyncDataLayer
mkSyncDataLayer trce backend =
  SyncDataLayer
    { sdlGetSlotHash = DB.runDbIohkLogging backend trce . DB.querySlotHash
    , sdlGetLatestBlock =
        runMaybeT $ do
          block <- MaybeT $ DB.runDbNoLogging DB.queryLatestBlock
          -- The EpochNo, SlotNo and BlockNo can only be zero for the Byron
          -- era, but we need to make the types match, hence `fromMaybe`.
          pure $ Block
                  { bHash = DB.blockHash block
                  , bEpochNo = EpochNo . fromMaybe 0 $ DB.blockEpochNo block
                  , bSlotNo = SlotNo . fromMaybe 0 $ DB.blockSlotNo block
                  , bBlockNo = BlockNo . fromMaybe 0 $ DB.blockBlockNo block
                  }
    , sdlGetLatestSlotNo = SlotNo <$> DB.runDbNoLogging DB.queryLatestSlotNo
    }

-- Log error to Trace and panic.
orDieWithLog :: (t -> Text) -> Trace IO Text -> ExceptT t IO () -> IO ()
orDieWithLog render trce e = do
  runExceptT e >>= \case
    Left errors -> do
      let errorStr = render errors
      liftIO $ logError trce errorStr
      panic errorStr
    Right () -> pure ()

