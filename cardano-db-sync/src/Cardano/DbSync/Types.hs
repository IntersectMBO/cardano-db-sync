module Cardano.DbSync.Types
  ( ConfigFile (..)
  , DbSyncNodeParams (..)
  , GenesisFile (..)
  , SocketPath (..)
  ) where

import Cardano.Db (MigrationDir (..))

import Ouroboros.Network.Block (SlotNo (..))

-- | The product type of all command line arguments
data DbSyncNodeParams = DbSyncNodeParams
  { enpConfigFile :: !ConfigFile
  , enpGenesisFile :: !GenesisFile
  , enpSocketPath :: !SocketPath
  , enpMigrationDir :: !MigrationDir
  , enpMaybeRollback :: !(Maybe SlotNo)
  }

newtype ConfigFile = ConfigFile
  { unConfigFile :: FilePath
  }

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath
  }

newtype SocketPath = SocketPath
  { unSocketPath :: FilePath
  }
