{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync
  ( ConfigFile (..)
  , SyncCommand (..)
  , SyncNodeParams (..)
  , SyncNodePlugin (..)
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

import           Cardano.Api (SlotNo (..))

import           Cardano.BM.Trace (Trace)

import qualified Cardano.Db as DB

import           Cardano.DbSync.Era (insertValidateGenesisDist)
import           Cardano.DbSync.Plugin.Default (defDbSyncNodePlugin)
import           Cardano.DbSync.Rollback (unsafeRollback)
import           Cardano.Sync.Database (runDbThread)

import           Cardano.Sync (Block (..), SyncDataLayer (..), SyncNodePlugin (..),
                   configureLogging, runSyncNode)
import           Cardano.Sync.Config.Types (ConfigFile (..), GenesisFile (..), LedgerStateDir (..),
                   MigrationDir (..), NetworkName (..), SocketPath (..), SyncCommand (..),
                   SyncNodeParams (..))
import           Cardano.Sync.Tracing.ToObjectOrphans ()

import           Database.Persist.Postgresql (withPostgresqlConn)

import           Database.Persist.Sql (SqlBackend)


runDbSyncNode :: (SqlBackend -> SyncNodePlugin) -> SyncNodeParams -> IO ()
runDbSyncNode mkPlugin params = do

    -- Read the PG connection info
    pgConfig <- DB.readPGPassFileEnv Nothing

    let MigrationDir migrationDir = enpMigrationDir params
    DB.runMigrations pgConfig True (DB.MigrationDir migrationDir) (Just $ DB.LogFileDir "/tmp")

    trce <- configureLogging params "db-sync-node"

    let connectionString = DB.toConnectionString pgConfig

    DB.runIohkLogging trce $ withPostgresqlConn connectionString $ \backend ->
      lift $ do
        -- For testing and debugging.
        case enpMaybeRollback params of
          Just slotNo -> void $ unsafeRollback trce slotNo
          Nothing -> pure ()

        runSyncNode (mkSyncDataLayer trce backend) trce (mkPlugin backend)
            params (insertValidateGenesisDist backend) runDbThread

-- -------------------------------------------------------------------------------------------------

-- The base @DataLayer@.
mkSyncDataLayer :: Trace IO Text -> SqlBackend -> SyncDataLayer
mkSyncDataLayer trce backend =
  SyncDataLayer
    { sdlGetSlotHash = DB.runDbIohkLogging backend trce . DB.querySlotHash
    , sdlGetLatestBlock =
        runMaybeT $ do
          block <- MaybeT $ DB.runDbNoLogging DB.queryLatestBlock
          pure $ Block
                  { bHash = DB.blockHash block
                  , bEpochNo = DB.blockEpochNo block
                  , bSlotNo = DB.blockSlotNo block
                  , bBlockNo = DB.blockBlockNo block
                  }
    , sdlGetLatestSlotNo = SlotNo <$> DB.runDbNoLogging DB.queryLatestSlotNo
    }
