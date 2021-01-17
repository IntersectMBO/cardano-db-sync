module Cardano.DbSync.Plugin.Extended
  ( mkExtendedDbSyncNodePlugin
  ) where


import           Cardano.DbSync (DbSyncNodePlugin (..), mkDefDbSyncNodePlugin)
import           Cardano.DbSync.Plugin.Epoch (epochPluginInsertBlock, epochPluginOnStartup,
                   epochPluginRollbackBlock)

mkExtendedDbSyncNodePlugin :: IO DbSyncNodePlugin
mkExtendedDbSyncNodePlugin = do
  defDbSyncNodePlugin <- mkDefDbSyncNodePlugin
  return $ defDbSyncNodePlugin
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

