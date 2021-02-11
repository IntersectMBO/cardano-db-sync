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
  , DbSyncCommand (..)
  , DbSyncNodeParams (..)
  , DbSyncNodePlugin (..)
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

import           Cardano.DbSync.Era
import           Cardano.DbSync.Plugin.Default (defDbSyncNodePlugin)
import           Cardano.DbSync.Rollback (unsafeRollback)

import           Cardano.Sync
import           Cardano.Sync.Config.Types
import           Cardano.Sync.Tracing.ToObjectOrphans ()

import           Database.Persist.Postgresql (withPostgresqlConn)

import           Database.Persist.Sql (SqlBackend)


runDbSyncNode :: (SqlBackend -> DbSyncNodePlugin) -> DbSyncNodeParams -> IO ()
runDbSyncNode mkPlugin params = do
    let MigrationDir migrationDir = enpMigrationDir params
    DB.runMigrations identity True (DB.MigrationDir migrationDir) (Just $ DB.LogFileDir "/tmp")

    trce <- configureLogging params

    -- Open up a connection and use it
    connectionString <- DB.toConnectionString <$> DB.readPGPassFileEnv

    DB.runIohkLogging trce $ withPostgresqlConn connectionString $ \backend ->
      lift $ do
        -- For testing and debugging.
        case enpMaybeRollback params of
          Just slotNo -> void $ unsafeRollback trce slotNo
          Nothing -> pure ()

        runSyncNode (mkSyncDataLayer trce backend) (mkPlugin backend)
            params (insertValidateGenesisDist backend)

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
