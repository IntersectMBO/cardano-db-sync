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
  ExtraMigrations (..),
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
  { envProtocol :: !SyncProtocol
  , envNetworkMagic :: !NetworkMagic
  , envSystemStart :: !SystemStart
  , envConnString :: ConnectionString
  , envRunDelayedMigration :: RunMigration
  , envBackend :: !(StrictTVar IO (Strict.Maybe SqlBackend))
  , envConsistentLevel :: !(StrictTVar IO ConsistentLevel)
  , envIsFixed :: !(StrictTVar IO FixesRan)
  , envIndexes :: !(StrictTVar IO Bool)
  , envOptions :: !SyncOptions
  , envCache :: !Cache
  , envExtraMigrations :: !(StrictTVar IO ExtraMigrations)
  , envOfflineWorkQueue :: !(StrictTBQueue IO PoolFetchRetry)
  , envOfflineResultQueue :: !(StrictTBQueue IO FetchResult)
  , envEpochState :: !(StrictTVar IO EpochState)
  , envEpochSyncTime :: !(StrictTVar IO UTCTime)
  , envLedgerEnv :: !LedgerEnv
  }

data SyncOptions = SyncOptions
  { soptExtended :: !Bool
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

data ExtraMigrations = ExtraMigrations
  { emRan :: Bool
  , emConsume :: Bool
  , emPrune :: Bool
  }
  deriving (Show)

data EpochState = EpochState
  { esInitialized :: !Bool
  , esEpochNo :: !(Strict.Maybe EpochNo)
  }
