module Explorer.Plugin.Extended
  ( extendedExplorerNodePlugin
  ) where


import           Explorer.Node (ExplorerNodePlugin (..), defExplorerNodePlugin)
import           Explorer.Node.Plugin.Epoch (epochPluginOnStartup, epochPluginInsertBlock,
                    epochPluginRollbackBlock)

extendedExplorerNodePlugin :: ExplorerNodePlugin
extendedExplorerNodePlugin =
  defExplorerNodePlugin
    { plugOnStartup =
        plugOnStartup defExplorerNodePlugin
          ++ [epochPluginOnStartup]
    , plugInsertBlock =
        plugInsertBlock defExplorerNodePlugin
          ++ [epochPluginInsertBlock]
    , plugRollbackBlock =
        plugRollbackBlock defExplorerNodePlugin
          ++ [epochPluginRollbackBlock]
    }

