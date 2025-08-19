{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Cache.Types (
  CacheStatus (..),
  CacheCapacity (..),
  CacheAction (..),
  CacheEpoch (..),
  CacheInternal (..),
  EpochBlockDiff (..),
  StakeCache (..),
  StakePoolCache,

  -- * Inits
  useNoCache,
  initCacheStatistics,
  newEmptyCache,

  -- * Utils
  shouldCache,

  -- * CacheStatistics
  CacheStatistics (..),
  textShowCacheStats,
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Cache.FIFO (FIFOCache)
import qualified Cardano.DbSync.Cache.FIFO as FIFO
import Cardano.DbSync.Cache.LRU (LRUCache)
import qualified Cardano.DbSync.Cache.LRU as LRU
import Cardano.DbSync.Types (DataHash, PoolKeyHash, StakeCred)
import Cardano.Ledger.Mary.Value (AssetName, PolicyID)
import qualified Cardano.Ledger.TxIn as Ledger
import Cardano.Prelude
import Control.Concurrent.Class.MonadSTM.Strict (
  StrictTVar,
  newTVarIO,
  readTVarIO,
 )
import qualified Data.Map.Strict as Map
import Data.Time.Clock (UTCTime)
import Data.WideWord.Word128 (Word128)

type StakePoolCache = Map PoolKeyHash DB.PoolHashId

-- | We use a stable cache for entries that are expected to be reused frequentyl.
-- These are stake addresses that have rewards, delegations etc.
-- They are never removed unless manually eg when it's deregistered
-- The LRU cache is much smaller for the rest stake addresses.
data StakeCache = StakeCache
  { scStableCache :: !(Map StakeCred DB.StakeAddressId)
  , scLruCache :: !(LRUCache StakeCred DB.StakeAddressId)
  }

-- | 'CacheStatus' enables functions in this module to be called even if the cache has not been initialized.
-- This is used during genesis insertions, where the cache is not yet initiated, and when the user has disabled the cache functionality.
data CacheStatus
  = NoCache
  | ActiveCache !CacheInternal

data CacheAction
  = UpdateCache
  | UpdateCacheStrong
  | DoNotUpdateCache
  | EvictAndUpdateCache
  deriving (Eq)

data CacheInternal = CacheInternal
  { cIsCacheCleanedForTip :: !(StrictTVar IO Bool)
  , cStake :: !(StrictTVar IO StakeCache)
  , cPools :: !(StrictTVar IO StakePoolCache)
  , cDatum :: !(StrictTVar IO (LRUCache DataHash DB.DatumId))
  , cMultiAssets :: !(StrictTVar IO (LRUCache (PolicyID, AssetName) DB.MultiAssetId))
  , cPrevBlock :: !(StrictTVar IO (Maybe (DB.BlockId, ByteString)))
  , cEpoch :: !(StrictTVar IO CacheEpoch)
  , cAddress :: !(StrictTVar IO (LRUCache ByteString DB.AddressId))
  , cTxIds :: !(StrictTVar IO (FIFOCache Ledger.TxId DB.TxId))
  , -- Optimisation target sizes for Map-based caches
    cOptimisePools :: !Word64
  , cOptimiseStake :: !Word64
  }

data CacheStatistics = CacheStatistics
  { credsHits :: !Word64
  , credsQueries :: !Word64
  , poolsHits :: !Word64
  , poolsQueries :: !Word64
  , datumHits :: !Word64
  , datumQueries :: !Word64
  , multiAssetsHits :: !Word64
  , multiAssetsQueries :: !Word64
  , prevBlockHits :: !Word64
  , prevBlockQueries :: !Word64
  , addressHits :: !Word64
  , addressQueries :: !Word64
  , txIdsHits :: !Word64
  , txIdsQueries :: !Word64
  }

-- CacheCapacity is used to define capacities for different types of cache entries.
data CacheCapacity = CacheCapacity
  { cacheCapacityAddress :: !Word64
  , cacheCapacityStake :: !Word64
  , cacheCapacityDatum :: !Word64
  , cacheCapacityMultiAsset :: !Word64
  , cacheCapacityTx :: !Word64
  , -- Optimisation target sizes for Map-based caches (used every 100k blocks)
    cacheOptimisePools :: !Word64
  , cacheOptimiseStake :: !Word64
  }

-- When inserting Txs and Blocks we also caculate values which can later be used when calculating a Epochs.
-- For this reason we store these values in cache.
data EpochBlockDiff = EpochBlockDiff
  { ebdBlockId :: !DB.BlockId
  -- ^ The blockId of the current block, this is used as the key for MapEpoch.
  , ebdFees :: !Word64
  , ebdOutSum :: !Word128
  , ebdTxCount :: !Word64
  , ebdEpochNo :: !Word64
  , ebdTime :: !UTCTime
  }
  deriving (Show)

data CacheEpoch = CacheEpoch
  { ceMapEpoch :: !(Map DB.BlockId DB.Epoch)
  , ceEpochBlockDiff :: !(Maybe EpochBlockDiff)
  }
  deriving (Show)

textShowCacheStats :: CacheStatistics -> CacheStatus -> IO Text
textShowCacheStats _ NoCache = pure "No Caches"
textShowCacheStats stats (ActiveCache ic) = do
  isCacheCleanedForTip <- readTVarIO $ cIsCacheCleanedForTip ic
  stakeHashRaws <- readTVarIO (cStake ic)
  pools <- readTVarIO (cPools ic)
  datums <- readTVarIO (cDatum ic)
  mAssets <- readTVarIO (cMultiAssets ic)
  txIds <- readTVarIO (cTxIds ic)
  address <- readTVarIO (cAddress ic)
  pure $
    mconcat
      [ "\n\nEpoch Cache Statistics: "
      , "\n  Caches Cleaned For Tip: " <> textShow isCacheCleanedForTip
      , textCacheSection "  Stake Addresses" (scLruCache stakeHashRaws) (scStableCache stakeHashRaws) (credsHits stats) (credsQueries stats)
      , textMapSection "  Pools" pools (poolsHits stats) (poolsQueries stats)
      , textLruSection "  Datums" datums (datumHits stats) (datumQueries stats)
      , textLruSection "  Addresses" address (addressHits stats) (addressQueries stats)
      , textLruSection "  Multi Assets" mAssets (multiAssetsHits stats) (multiAssetsQueries stats)
      , textPrevBlockSection
      , textFifoSection "  TxId" txIds (txIdsHits stats) (txIdsQueries stats)
      ]
  where
    textCacheSection title cacheLru cacheStable hits queries =
      mconcat
        [ "\n" <> title <> ": "
        , "cache sizes: "
        , textShow (Map.size cacheStable)
        , " and "
        , textShow (LRU.getSize cacheLru)
        , hitMissStats hits queries
        ]

    textMapSection title cache hits queries =
      mconcat
        [ "\n" <> title <> ": "
        , "cache size: "
        , textShow (Map.size cache)
        , hitMissStats hits queries
        ]

    textLruSection title cache hits queries =
      mconcat
        [ "\n" <> title <> ": "
        , "cache capacity: "
        , textShow (LRU.getCapacity cache)
        , ", cache size: "
        , textShow (LRU.getSize cache)
        , hitMissStats hits queries
        ]

    textFifoSection title cache hits queries =
      mconcat
        [ "\n" <> title <> ": "
        , "cache size: "
        , textShow (FIFO.getSize cache)
        , ", cache capacity: "
        , textShow (FIFO.getCapacity cache)
        , hitMissStats hits queries
        ]

    textPrevBlockSection =
      mconcat
        [ "\nPrevious Block: "
        , hitMissStats (prevBlockHits stats) (prevBlockQueries stats)
        ]

    hitMissStats hits queries =
      mconcat
        [ hitRate hits queries
        , ", hits: "
        , textShow hits
        , ", misses: "
        , textShow (queries - hits)
        ]

    hitRate hits queries =
      if queries == 0
        then ""
        else ", hit rate: " <> textShow (100 * hits `div` queries) <> "%"

useNoCache :: CacheStatus
useNoCache = NoCache

newEmptyCache :: MonadIO m => CacheCapacity -> m CacheStatus
newEmptyCache CacheCapacity {..} = liftIO $ do
  cIsCacheCleanedForTip <- newTVarIO False
  cStake <- newTVarIO (StakeCache Map.empty (LRU.empty cacheCapacityStake))
  cPools <- newTVarIO Map.empty
  cDatum <- newTVarIO (LRU.empty cacheCapacityDatum)
  cAddress <- newTVarIO (LRU.empty cacheCapacityAddress)
  cMultiAssets <- newTVarIO (LRU.empty cacheCapacityMultiAsset)
  cPrevBlock <- newTVarIO Nothing
  cEpoch <- newTVarIO initCacheEpoch
  cTxIds <- newTVarIO (FIFO.empty cacheCapacityTx)

  pure . ActiveCache $
    CacheInternal
      { cIsCacheCleanedForTip = cIsCacheCleanedForTip
      , cStake = cStake
      , cPools = cPools
      , cDatum = cDatum
      , cMultiAssets = cMultiAssets
      , cPrevBlock = cPrevBlock
      , cEpoch = cEpoch
      , cAddress = cAddress
      , cTxIds = cTxIds
      , cOptimisePools = cacheOptimisePools
      , cOptimiseStake = cacheOptimiseStake
      }

initCacheStatistics :: CacheStatistics
initCacheStatistics = CacheStatistics 0 0 0 0 0 0 0 0 0 0 0 0 0 0

initCacheEpoch :: CacheEpoch
initCacheEpoch = CacheEpoch mempty Nothing

shouldCache :: CacheAction -> Bool
shouldCache = \case
  UpdateCache -> True
  UpdateCacheStrong -> True
  _ -> False
