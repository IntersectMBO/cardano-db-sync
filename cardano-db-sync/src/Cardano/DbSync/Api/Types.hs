{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Api.Types (
  SyncEnv (..),
  SyncOptions (..),
  InsertOptions (..),
  LedgerEnv (..),
  RunMigration,
  FixesRan (..),
  ConsistentLevel (..),
  EpochState (..),
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Cache.Types (Cache)
import Cardano.DbSync.Config.Types (SyncProtocol)
import Cardano.DbSync.Ledger.Types (HasLedgerEnv)
import Cardano.DbSync.LocalStateQuery (NoLedgerEnv)
import Cardano.DbSync.Types (FetchResult, PoolFetchRetry)
import Cardano.Prelude (Bool, Eq, IO, Show, Word64)
import Cardano.Slotting.Slot (EpochNo (..))
import Control.Concurrent.Class.MonadSTM.Strict (
  StrictTVar,
 )
import Control.Concurrent.Class.MonadSTM.Strict.TBQueue (StrictTBQueue)
import qualified Data.Strict.Maybe as Strict
import Data.Time.Clock (UTCTime)
import Database.Persist.Postgresql (ConnectionString)
import Database.Persist.Sql (SqlBackend)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import Ouroboros.Network.Magic (NetworkMagic (..))

data SyncEnv = SyncEnv
  { envBackend :: !SqlBackend
  , envCache :: !Cache
  , envConnString :: ConnectionString
  , envConsistentLevel :: !(StrictTVar IO ConsistentLevel)
  , envEpochState :: !(StrictTVar IO EpochState)
  , envEpochSyncTime :: !(StrictTVar IO UTCTime)
  , envIndexes :: !(StrictTVar IO Bool)
  , envIsFixed :: !(StrictTVar IO FixesRan)
  , envLedgerEnv :: !LedgerEnv
  , envNetworkMagic :: !NetworkMagic
  , envOfflineResultQueue :: !(StrictTBQueue IO FetchResult)
  , envOfflineWorkQueue :: !(StrictTBQueue IO PoolFetchRetry)
  , envOptions :: !SyncOptions
  , envProtocol :: !SyncProtocol
  , envPruneConsumeMigration :: !(StrictTVar IO DB.PruneConsumeMigration)
  , envRunDelayedMigration :: RunMigration
  , envSystemStart :: !SystemStart
  }

data SyncOptions = SyncOptions
  { soptEpochAndCacheEnabled :: !Bool
  , soptAbortOnInvalid :: !Bool
  , soptCache :: !Bool
  , soptSkipFix :: !Bool
  , soptOnlyFix :: !Bool
  , soptInsertOptions :: !InsertOptions
  , snapshotEveryFollowing :: !Word64
  , snapshotEveryLagging :: !Word64
  }

data InsertOptions = InsertOptions
  { ioMultiAssets :: !Bool
  , ioMetadata :: !Bool
  , ioPlutusExtra :: !Bool
  , ioOfflineData :: !Bool
  }

-- A representation of if we are using a ledger or not given CLI options
data LedgerEnv where
  HasLedger :: HasLedgerEnv -> LedgerEnv
  NoLedger :: NoLedgerEnv -> LedgerEnv

type RunMigration = DB.MigrationToRun -> IO ()

data FixesRan = NoneFixRan | DataFixRan | AllFixRan

data ConsistentLevel = Consistent | DBAheadOfLedger | Unchecked
  deriving (Show, Eq)

data EpochState = EpochState
  { esInitialized :: !Bool
  , esEpochNo :: !(Strict.Maybe EpochNo)
  }
