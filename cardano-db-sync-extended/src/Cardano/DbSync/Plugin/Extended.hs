module Cardano.DbSync.Plugin.Extended
  ( extendedDbSyncNodePlugin
  ) where

import           Cardano.DbSync.Plugin.Default (defDbSyncNodePlugin)
import           Cardano.DbSync.Plugin.Epoch (epochPluginInsertBlock, epochPluginOnStartup,
                   epochPluginRollbackBlock)

import           Cardano.DbSync.Plugin (SyncNodePlugin (..))

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
                ++ [epochPluginInsertBlock backend]
        , plugRollbackBlock =
            plugRollbackBlock defPlugin
              ++ [epochPluginRollbackBlock]
        }
