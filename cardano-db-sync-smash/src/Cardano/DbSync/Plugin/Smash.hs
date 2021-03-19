module Cardano.DbSync.Plugin.Smash
  ( smashExtendedDbSyncNodePlugin
  ) where

import           Cardano.DbSync.Plugin.Default (defDbSyncNodePlugin)
import           Cardano.DbSync.Plugin.Epoch (epochPluginInsertBlock, epochPluginOnStartup,
                   epochPluginRollbackBlock)

import           Cardano.SMASH.DB (DataLayer)
import           Cardano.SMASH.DBSyncPlugin (poolMetadataDbSyncNodePlugin)

import           Cardano.Sync (SyncNodePlugin (..))

import           Database.Persist.Sql (SqlBackend)

smashExtendedDbSyncNodePlugin :: DataLayer -> SqlBackend -> SyncNodePlugin
smashExtendedDbSyncNodePlugin dataLayer backend =
  let defPlugin = defDbSyncNodePlugin backend
      smashPlugin = poolMetadataDbSyncNodePlugin dataLayer
  in  defPlugin
        { plugOnStartup =
            plugOnStartup defPlugin
              ++ [epochPluginOnStartup backend] ++ plugOnStartup smashPlugin
        , plugInsertBlock =
            plugInsertBlock defPlugin
                ++ [epochPluginInsertBlock backend] ++ plugInsertBlock smashPlugin
        , plugRollbackBlock =
            plugRollbackBlock defPlugin
              ++ [epochPluginRollbackBlock] ++ plugRollbackBlock smashPlugin
        }
