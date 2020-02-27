module Cardano.DbSync.Plugin
  ( DbSyncNodePlugin (..)
  ) where

import           Cardano.BM.Trace (Trace)

import           Cardano.Prelude

import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Trans.Reader (ReaderT)

import           Database.Persist.Sql (SqlBackend)

import           Cardano.DbSync.Error

import           Ouroboros.Network.Block (Point (..), Tip)
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)


-- | This plugin system allows access to the database to be extended by running one or more
-- actions on a block insert or rollback.
--
-- The insert and rollback actions are applied from the head of the list to the tail.
-- THe default DbSyncNodePlugin is 'Cardano.DbSync.Plugin.Default.defDbSyncNodePlugin'. This
-- allows clients to insert db actions both before and after the default action.

-- Plugins are free to read from the existing tables but should not modify them. Plugins however
-- are free to operation on their own tables.

-- Usually, insert plugins would be added after the default (so that the default action happens
-- first, and then the added ones) whereas for rollback plugins, the rollback actions would
-- normally happen *before* the default rollback.
-- That is why the following data type does not have a Semigroup/Monoid instances.

data DbSyncNodePlugin = DbSyncNodePlugin
  { -- A function run each time the application starts. Can be used to do a one time update/setup
    -- of a table.
    plugOnStartup
        :: [Trace IO Text -> ReaderT SqlBackend (LoggingT IO) ()]
    -- Called for each block recieved from the network.
    -- This will not be called for the original genesis block, but will be called for
    -- all subsequent blocks.
    -- Blocks (including epoch boundary blocks) are called in sequence from the oldest to the newest.
  , plugInsertBlock
        :: [Trace IO Text -> ByronBlock -> Tip ByronBlock -> ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ())]

    -- Rollback to the specified SlotNumber/HeaderHash.
  , plugRollbackBlock
        :: [Trace IO Text -> Point ByronBlock -> IO (Either DbSyncNodeError ())]
  }
