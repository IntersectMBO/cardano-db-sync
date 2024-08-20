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
  CurrentEpochNo (..),
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Cache.Types (CacheStatus)
import Cardano.DbSync.Config.Types (SyncNodeConfig)
import Cardano.DbSync.Ledger.Types (HasLedgerEnv)
import Cardano.DbSync.LocalStateQuery (NoLedgerEnv)
import Cardano.DbSync.Types (
  OffChainPoolResult,
  OffChainPoolWorkQueue,
  OffChainVoteResult,
  OffChainVoteWorkQueue,
 )
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
  , envCache :: !CacheStatus
  , envConnectionString :: !ConnectionString
  , envConsistentLevel :: !(StrictTVar IO ConsistentLevel)
  , envDbConstraints :: !(StrictTVar IO DB.ManualDbConstraints)
  , envCurrentEpochNo :: !(StrictTVar IO CurrentEpochNo)
  , envEpochSyncTime :: !(StrictTVar IO UTCTime)
  , envIndexes :: !(StrictTVar IO Bool)
  , envIsFixed :: !(StrictTVar IO FixesRan)
  , envBootstrap :: !(StrictTVar IO Bool)
  , envLedgerEnv :: !LedgerEnv
  , envNetworkMagic :: !NetworkMagic
  , envOffChainPoolResultQueue :: !(StrictTBQueue IO OffChainPoolResult)
  , envOffChainPoolWorkQueue :: !(StrictTBQueue IO OffChainPoolWorkQueue)
  , envOffChainVoteResultQueue :: !(StrictTBQueue IO OffChainVoteResult)
  , envOffChainVoteWorkQueue :: !(StrictTBQueue IO OffChainVoteWorkQueue)
  , envOptions :: !SyncOptions
  , envSyncNodeConfig :: !SyncNodeConfig
  , envRunDelayedMigration :: RunMigration
  , envSystemStart :: !SystemStart
  }

data SyncOptions = SyncOptions
  { soptEpochAndCacheEnabled :: !Bool
  , soptAbortOnInvalid :: !Bool
  , soptCache :: !Bool
  , soptSkipFix :: !Bool
  , soptOnlyFix :: !Bool
  , soptPruneConsumeMigration :: !DB.PruneConsumeMigration
  , soptInsertOptions :: !InsertOptions
  , snapshotEveryFollowing :: !Word64
  , snapshotEveryLagging :: !Word64
  }
  deriving (Show)

data InsertOptions = InsertOptions
  { ioTxCBOR :: !Bool
  , ioInOut :: !Bool
  , ioUseLedger :: !Bool
  , ioShelley :: !Bool
  , ioRewards :: !Bool
  , ioMultiAssets :: !Bool
  , ioMetadata :: !Bool
  , ioKeepMetadataNames :: Strict.Maybe [Word64]
  , ioPlutusExtra :: !Bool
  , ioOffChainPoolData :: !Bool
  , ioPoolStats :: !Bool
  , ioGov :: !Bool
  , ioRemoveJsonbFromSchema :: !Bool
  , ioTxOutTableType :: !DB.TxOutTableType
  }
  deriving (Show)

-- A representation of if we are using a ledger or not given CLI options
data LedgerEnv where
  HasLedger :: HasLedgerEnv -> LedgerEnv
  NoLedger :: NoLedgerEnv -> LedgerEnv

type RunMigration = DB.MigrationToRun -> IO ()

data FixesRan = NoneFixRan | DataFixRan | AllFixRan

data ConsistentLevel = Consistent | DBAheadOfLedger | Unchecked
  deriving (Show, Eq)

newtype CurrentEpochNo = CurrentEpochNo
  { cenEpochNo :: Strict.Maybe EpochNo
  }
