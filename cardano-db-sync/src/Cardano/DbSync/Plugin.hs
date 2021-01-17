module Cardano.DbSync.Plugin
  ( DbSyncNodePlugin (..)
  ) where

import           Cardano.BM.Trace (Trace)

import           Cardano.Prelude

import           Cardano.DbSync.LedgerState
import           Cardano.Slotting.Slot (SlotNo (..))

import           Control.Monad.Logger (LoggingT)

import           Database.Persist.Sql (SqlBackend)

import           Cardano.DbSync.Config.Types
import           Cardano.DbSync.Error
import           Cardano.DbSync.Types


-- | This plugin system allows access to the database to be extended by running one or more
-- actions on a block insert or rollback.
--
-- The insert and rollback actions are applied from the head of the list to the tail.
-- THe default DbSyncNodePlugin is created in 'Cardano.DbSync.Plugin.Default.mkDefDbSyncNodePlugin'.
-- This allows clients to insert db actions both before and after the default action.

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
        :: [Trace IO Text -> DbSyncEnv -> LedgerStateVar -> BlockDetails -> ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ())]

    -- Rollback to the specified absolute SlotNo.
  , plugRollbackBlock
        :: [Trace IO Text -> SlotNo -> IO (Either DbSyncNodeError ())]
  }
