module Cardano.DbSync.Plugin.Default
  ( defDbSyncNodePlugin
  ) where

import           Cardano.DbSync.Plugin
import           Cardano.DbSync.Plugin.Default.Rollback (rollbackToPoint)
import           Cardano.DbSync.Plugin.Default.Insert (insertByronBlock)

-- | The default DbSyncNodePlugin.
-- Does exactly what the cardano-db-sync node did before the plugin system was added.
defDbSyncNodePlugin :: DbSyncNodePlugin
defDbSyncNodePlugin =
  DbSyncNodePlugin
    { plugOnStartup = []
    , plugInsertBlock = [insertByronBlock]
    , plugRollbackBlock = [rollbackToPoint]
    }
