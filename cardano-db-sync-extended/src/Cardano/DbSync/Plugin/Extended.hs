module Cardano.DbSync.Plugin.Extended
  ( extendedDbSyncNodePlugin
  ) where


import           Cardano.DbSync (DbSyncNodePlugin (..), defDbSyncNodePlugin)
import           Cardano.DbSync.Plugin.Epoch (epochPluginOnStartup, epochPluginInsertBlock,
                    epochPluginRollbackBlock)

extendedDbSyncNodePlugin :: DbSyncNodePlugin
extendedDbSyncNodePlugin =
  defDbSyncNodePlugin
    { plugOnStartup =
        plugOnStartup defDbSyncNodePlugin
          ++ [epochPluginOnStartup]
    , plugInsertBlock =
        plugInsertBlock defDbSyncNodePlugin
          ++ [epochPluginInsertBlock]
    , plugRollbackBlock =
        plugRollbackBlock defDbSyncNodePlugin
          ++ [epochPluginRollbackBlock]
    }

