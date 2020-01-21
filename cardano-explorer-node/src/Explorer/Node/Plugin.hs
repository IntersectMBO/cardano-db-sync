module Explorer.Node.Plugin
  ( ExplorerNodePlugin (..)
  ) where

import           Cardano.BM.Trace (Trace)

import           Cardano.Prelude

import           Control.Monad.Logger (NoLoggingT)
import           Control.Monad.Trans.Reader (ReaderT)

import           Database.Persist.Sql (SqlBackend)

import           Explorer.Node.Error

import           Ouroboros.Network.Block (BlockNo (..), Point (..))
import           Ouroboros.Consensus.Ledger.Byron (ByronBlock)


-- | This plugin system allows access to the database to be extended by running one or more
-- actions on a block insert or rollback.
--
-- The insert and rollback actions are applied from the head of the list to the tail.
-- THe default ExplorerNodePlugin is 'Explorer.Node.Plugin.Default.defExplorerNodePlugin'. This
-- allows clients to insert db actions both before and after the default action.

-- Plugins are free to read from the existing tables but should not modify them. Plugins however
-- are free to operation on their own tables.

-- Usually, insert plugins would be added after the default (so that the default action happens
-- first, and then the added ones) whereas for rollback plugins, the rollback actions would
-- normally happen *before* the default rollback.
-- That is why the following data type does not have a Semigroup/Monoid instances.

data ExplorerNodePlugin = ExplorerNodePlugin
  { -- Called for each block recieved from the network.
    -- This will not be called for the original genesis block, but will be called for
    -- all subsequent blocks.
    -- Blocks (including epoch boundary blocks) are called in sequence from the oldest to the newest.
    plugInsertBlock
        :: [Trace IO Text -> ByronBlock -> BlockNo -> ReaderT SqlBackend (NoLoggingT IO) (Either ExplorerNodeError ())]

    -- Rollback to the specified SlotNumber/HeaderHash.
  , plugRollbackBlock
        :: [Trace IO Text -> Point ByronBlock -> IO (Either ExplorerNodeError ())]
  }
