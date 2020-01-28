module Explorer.Plugin.Extended
  ( extendedExplorerNodePlugin
  ) where


import           Explorer.Node (ExplorerNodePlugin (..), defExplorerNodePlugin)

extendedExplorerNodePlugin :: ExplorerNodePlugin
extendedExplorerNodePlugin =
  defExplorerNodePlugin
    { plugInsertBlock =
        plugInsertBlock defExplorerNodePlugin
    , plugRollbackBlock =
        plugRollbackBlock defExplorerNodePlugin
    }

