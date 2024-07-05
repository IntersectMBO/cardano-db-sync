{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Cache.Types (
  CacheStatus (..),
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

  -- * CacheStatistics
  CacheStatistics (..),
  textShowStats,
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Cache.LRU (LRUCache)
import qualified Cardano.DbSync.Cache.LRU as LRU
import Cardano.DbSync.Types (DataHash, PoolKeyHash, StakeCred)
import Cardano.Ledger.Mary.Value (AssetName, PolicyID)
import Cardano.Prelude
import Control.Concurrent.Class.MonadSTM.Strict (
  StrictTVar,
  newTVarIO,
  readTVarIO,
 )
import qualified Data.Map.Strict as Map
import Data.Time.Clock (UTCTime)
import Data.WideWord.Word128 (Word128)
import Ouroboros.Consensus.Cardano.Block (StandardCrypto)

type StakePoolCache = Map PoolKeyHash DB.PoolHashId

data StakeCache = StakeCache
  { scStableCache :: !(Map StakeCred DB.StakeAddressId)
  , scLruCache :: !(LRUCache StakeCred DB.StakeAddressId)
  }

-- 'CacheStatus' enables functions in this module to be called even if the cache has not been initialized.
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
  { cStakeRawHashes :: !(StrictTVar IO StakeCache)
  , cPools :: !(StrictTVar IO StakePoolCache)
  , cDatum :: !(StrictTVar IO (LRUCache DataHash DB.DatumId))
  , cMultiAssets :: !(StrictTVar IO (LRUCache (PolicyID StandardCrypto, AssetName) DB.MultiAssetId))
  , cPrevBlock :: !(StrictTVar IO (Maybe (DB.BlockId, ByteString)))
  , cStats :: !(StrictTVar IO CacheStatistics)
  , cEpoch :: !(StrictTVar IO CacheEpoch)
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

textShowStats :: CacheStatus -> IO Text
textShowStats NoCache = pure "NoCache"
textShowStats (ActiveCache ic) = do
  stats <- readTVarIO $ cStats ic
  stakeHashRaws <- readTVarIO (cStakeRawHashes ic)
  pools <- readTVarIO (cPools ic)
  datums <- readTVarIO (cDatum ic)
  mAssets <- readTVarIO (cMultiAssets ic)
  pure $
    mconcat
      [ "\nCache Statistics:"
      , "\n  Stake Addresses: "
      , "cache sizes: "
      , textShow (Map.size $ scStableCache stakeHashRaws)
      , " and "
      , textShow (LRU.getSize $ scLruCache stakeHashRaws)
      , if credsQueries stats == 0
          then ""
          else ", hit rate: " <> textShow (100 * credsHits stats `div` credsQueries stats) <> "%"
      , ", hits: "
      , textShow (credsHits stats)
      , ", misses: "
      , textShow (credsQueries stats - credsHits stats)
      , "\n  Pools: "
      , "cache size: "
      , textShow (Map.size pools)
      , if poolsQueries stats == 0
          then ""
          else ", hit rate: " <> textShow (100 * poolsHits stats `div` poolsQueries stats) <> "%"
      , ", hits: "
      , textShow (poolsHits stats)
      , ", misses: "
      , textShow (poolsQueries stats - poolsHits stats)
      , "\n  Datums: "
      , "cache capacity: "
      , textShow (LRU.getCapacity datums)
      , ", cache size: "
      , textShow (LRU.getSize datums)
      , if datumQueries stats == 0
          then ""
          else ", hit rate: " <> textShow (100 * datumHits stats `div` datumQueries stats) <> "%"
      , ", hits: "
      , textShow (datumHits stats)
      , ", misses: "
      , textShow (datumQueries stats - datumHits stats)
      , "\n  Multi Assets: "
      , "cache capacity: "
      , textShow (LRU.getCapacity mAssets)
      , ", cache size: "
      , textShow (LRU.getSize mAssets)
      , if multiAssetsQueries stats == 0
          then ""
          else ", hit rate: " <> textShow (100 * multiAssetsHits stats `div` multiAssetsQueries stats) <> "%"
      , ", hits: "
      , textShow (multiAssetsHits stats)
      , ", misses: "
      , textShow (multiAssetsQueries stats - multiAssetsHits stats)
      , "\n  Previous Block: "
      , if prevBlockQueries stats == 0
          then ""
          else "hit rate: " <> textShow (100 * prevBlockHits stats `div` prevBlockQueries stats) <> "%"
      , ", hits: "
      , textShow (prevBlockHits stats)
      , ", misses: "
      , textShow (prevBlockQueries stats - prevBlockHits stats)
      ]

useNoCache :: CacheStatus
useNoCache = NoCache

newEmptyCache :: MonadIO m => LRU.LRUCacheCapacity -> m CacheStatus
newEmptyCache LRU.LRUCacheCapacity {..} =
  liftIO . fmap ActiveCache $
    CacheInternal
      <$> newTVarIO (StakeCache Map.empty (LRU.empty lirCapacityStakeHashRaw))
      <*> newTVarIO Map.empty
      <*> newTVarIO (LRU.empty lruCapacityDatum)
      <*> newTVarIO (LRU.empty lruCapacityMultiAsset)
      <*> newTVarIO Nothing
      <*> newTVarIO initCacheStatistics
      <*> newTVarIO initCacheEpoch

initCacheStatistics :: CacheStatistics
initCacheStatistics = CacheStatistics 0 0 0 0 0 0 0 0 0 0

initCacheEpoch :: CacheEpoch
initCacheEpoch = CacheEpoch mempty Nothing
