module Cardano.DbSync.Plugin.Extended
  ( extendedDbSyncNodePlugin
  ) where


import           Cardano.DbSync (DbSyncNodePlugin (..), defDbSyncNodePlugin)
import           Cardano.DbSync.Plugin.Epoch (epochPluginOnStartup, epochPluginInsertBlock,
                    epochPluginRollbackBlock)
import           Cardano.DbSync.Plugin.TxBody (insertTxBody)

extendedDbSyncNodePlugin :: DbSyncNodePlugin
extendedDbSyncNodePlugin =
  defDbSyncNodePlugin
    { plugOnStartup =
        plugOnStartup defDbSyncNodePlugin
          ++ [epochPluginOnStartup]
    , plugInsertBlock =
        plugInsertBlock defDbSyncNodePlugin
          ++ [epochPluginInsertBlock, insertTxBody]
    , plugRollbackBlock =
        plugRollbackBlock defDbSyncNodePlugin
          ++ [epochPluginRollbackBlock]
    }

