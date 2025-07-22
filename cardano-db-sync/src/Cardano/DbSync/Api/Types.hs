{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Api.Types (
  SyncEnv (..),
  SyncOptions (..),
  InsertOptions (..),
  LedgerEnv (..),
  RunMigration,
  ConsistentLevel (..),
  CurrentEpochNo (..),
  UnicodeNullSource (..),
  EpochStatistics (..),
  formatUnicodeNullSource,
) where

import Control.Concurrent.Class.MonadSTM.Strict (StrictTVar)
import Control.Concurrent.Class.MonadSTM.Strict.TBQueue (StrictTBQueue)
import qualified Data.Map.Strict as Map
import qualified Data.Strict.Maybe as Strict
import Data.Time.Clock (UTCTime)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import Ouroboros.Network.Magic (NetworkMagic (..))

import Cardano.Prelude (Bool, Eq, IO, Ord, Show, Text, Word64)
import Cardano.Slotting.Slot (EpochNo (..))

import qualified Cardano.Db as DB
import Cardano.DbSync.Cache.Types (CacheStatistics, CacheStatus)
import Cardano.DbSync.Config.Types (SyncNodeConfig)
import Cardano.DbSync.Ledger.Types (HasLedgerEnv)
import Cardano.DbSync.LocalStateQuery (NoLedgerEnv)
import Cardano.DbSync.Types (
  OffChainPoolResult,
  OffChainPoolWorkQueue,
  OffChainVoteResult,
  OffChainVoteWorkQueue,
 )

-- | SyncEnv is the main environment for the whole application.
data SyncEnv = SyncEnv
  { envDbEnv :: !DB.DbEnv
  , envCache :: !CacheStatus
  , envEpochStatistics :: !(StrictTVar IO EpochStatistics)
  , envConsistentLevel :: !(StrictTVar IO ConsistentLevel)
  , envDbConstraints :: !(StrictTVar IO DB.ManualDbConstraints)
  , envCurrentEpochNo :: !(StrictTVar IO CurrentEpochNo)
  , envIndexes :: !(StrictTVar IO Bool)
  , envBootstrap :: !(StrictTVar IO Bool)
  , envLedgerEnv :: !LedgerEnv
  , envNetworkMagic :: !NetworkMagic
  , envOffChainPoolResultQueue :: !(StrictTBQueue IO OffChainPoolResult)
  , envOffChainPoolWorkQueue :: !(StrictTBQueue IO OffChainPoolWorkQueue)
  , envOffChainVoteResultQueue :: !(StrictTBQueue IO OffChainVoteResult)
  , envOffChainVoteWorkQueue :: !(StrictTBQueue IO OffChainVoteWorkQueue)
  , envOptions :: !SyncOptions
  , envSyncNodeConfig :: !SyncNodeConfig
  , envRunIndexesMigration :: RunMigration
  , envSystemStart :: !SystemStart
  }

data SyncOptions = SyncOptions
  { soptEpochAndCacheEnabled :: !Bool
  , soptAbortOnInvalid :: !Bool
  , soptCache :: !Bool
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
  , ioTxOutVariantType :: !DB.TxOutVariantType
  }
  deriving (Show)

-- A representation of if we are using a ledger or not given CLI options
data LedgerEnv where
  HasLedger :: HasLedgerEnv -> LedgerEnv
  NoLedger :: NoLedgerEnv -> LedgerEnv

type RunMigration = DB.MigrationToRun -> IO ()

data ConsistentLevel = Consistent | DBAheadOfLedger | Unchecked
  deriving (Show, Eq)

newtype CurrentEpochNo = CurrentEpochNo
  { cenEpochNo :: Strict.Maybe EpochNo
  }

data UnicodeNullSource
  = InsertDatum
  | InsertRedeemerData
  | InsertScript
  | PrepareTxMetadata
  deriving (Eq, Ord, Show)

formatUnicodeNullSource :: UnicodeNullSource -> Text
formatUnicodeNullSource source = case source of
  InsertDatum -> "insertDatum: Column 'value' in table 'datum'"
  InsertRedeemerData -> "insertRedeemerData: Column 'value' in table 'redeemer'"
  InsertScript -> "insertScript: Column 'json' in table 'script'"
  PrepareTxMetadata -> "prepareTxMetadata: Column 'json' in table 'tx_metadata'"

data EpochStatistics = EpochStatistics
  { elsStartTime :: !UTCTime
  , elsCaches :: !CacheStatistics
  , elsUnicodeNull :: !(Map.Map UnicodeNullSource [DB.TxId])
  }
