module Explorer.Node.Plugin.Default
  ( defExplorerNodePlugin
  ) where

import           Explorer.Node.Plugin
import           Explorer.Node.Plugin.Default.Rollback (rollbackToPoint)
import           Explorer.Node.Plugin.Default.Insert (insertByronBlock)

-- | The default ExplorerNodePlugin.
-- Does exactly what the explorer node did before the plugin system was added.
defExplorerNodePlugin :: ExplorerNodePlugin
defExplorerNodePlugin =
  ExplorerNodePlugin
    { plugInsertBlock = [insertByronBlock]
    , plugRollbackBlock = [rollbackToPoint]
    }
