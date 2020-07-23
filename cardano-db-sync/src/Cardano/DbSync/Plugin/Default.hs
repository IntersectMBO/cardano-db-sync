module Cardano.DbSync.Plugin.Default
  ( defDbSyncNodePlugin
  , insertDefaultBlock
  , rollbackToSlot
  ) where

import           Cardano.DbSync.Plugin
import           Cardano.DbSync.Plugin.Default.Rollback (rollbackToSlot)
import           Cardano.DbSync.Plugin.Default.Insert (insertDefaultBlock)

-- | The default DbSyncNodePlugin.
-- Does exactly what the cardano-db-sync node did before the plugin system was added.
-- The non-default node takes this structure and extends the lists.
defDbSyncNodePlugin :: DbSyncNodePlugin
defDbSyncNodePlugin =
  DbSyncNodePlugin
    { plugOnStartup = []
    , plugInsertBlock = [insertDefaultBlock]
    , plugRollbackBlock = [rollbackToSlot]
    }
