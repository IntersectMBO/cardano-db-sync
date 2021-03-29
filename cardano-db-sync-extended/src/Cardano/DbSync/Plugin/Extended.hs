module Cardano.DbSync.Plugin.Extended
  ( extendedDbSyncNodePlugin
  ) where

import           Cardano.DbSync.Plugin.Default (defDbSyncNodePlugin)
import           Cardano.DbSync.Plugin.Epoch (epochPluginInsertBlockDetails, epochPluginOnStartup,
                   epochPluginRollbackBlock)

import           Cardano.Sync (SyncNodePlugin (..))

import           Database.Persist.Sql (SqlBackend)

extendedDbSyncNodePlugin :: SqlBackend -> SyncNodePlugin
extendedDbSyncNodePlugin backend =
  let defPlugin = defDbSyncNodePlugin backend
  in  defPlugin
        { plugOnStartup =
            plugOnStartup defPlugin
              ++ [epochPluginOnStartup backend]
        , plugInsertBlock =
            plugInsertBlock defPlugin
        , plugInsertBlockDetails =
            plugInsertBlockDetails defPlugin
                ++ [epochPluginInsertBlockDetails backend]
        , plugRollbackBlock =
            plugRollbackBlock defPlugin
              ++ [epochPluginRollbackBlock]
        }
