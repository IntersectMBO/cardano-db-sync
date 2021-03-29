module Cardano.Sync.Plugin
  ( SyncNodePlugin (..)
  ) where

import           Control.Monad.Except

import           Cardano.BM.Trace (Trace)

import           Cardano.Prelude

import           Cardano.Sync.Api
import           Cardano.Sync.Error
import           Cardano.Sync.Types


-- | This plugin system allows access to the database to be extended by running one or more
-- actions on a block insert or rollback.
--
-- The insert and rollback actions are applied from the head of the list to the tail.
-- The default SyncNodePlugin is 'defDbSyncNodePlugin'. This
-- allows clients to insert db actions both before and after the default action.

-- Plugins are free to read from the existing tables but should not modify them. Plugins however
-- are free to operation on their own tables.

-- Usually, insert plugins would be added after the default (so that the default action happens
-- first, and then the added ones) whereas for rollback plugins, the rollback actions would
-- normally happen *before* the default rollback.
-- That is why the following data type does not have a Semigroup/Monoid instances.

-- TODO(KS): If required, we can unfiy the plugin system to be sequence based, rather then list based.
-- We can switch the types to be like the `plugInsertBlock`, not lists.
data SyncNodePlugin = SyncNodePlugin
  { -- A function run each time the application starts. Can be used to do a one time update/setup
    -- of a table.
    plugOnStartup :: [Trace IO Text -> IO (Either SyncNodeError ())]
    -- Called for each block recieved from the network.
    -- This will not be called for the original genesis block, but will be called for
    -- all subsequent blocks.
    -- Blocks (including epoch boundary blocks) are called in sequence from the oldest to the newest.
  , plugInsertBlock :: Trace IO Text -> SyncEnv -> [CardanoBlock] -> ExceptT SyncNodeError IO [BlockDetails]
  , plugInsertBlockDetails :: [Trace IO Text -> SyncEnv -> [BlockDetails] -> ExceptT SyncNodeError IO ()]
    -- Rollback to the specified Point.
  , plugRollbackBlock :: [Trace IO Text -> CardanoPoint -> IO (Either SyncNodeError ())]
  }
