{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Cache.FIFO (
  FIFOCache (..),
  empty,
  insert,
  lookup,
  getSize,
  getCapacity,
  cleanupCache,
) where

import Cardano.Prelude hiding (empty)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as Seq

{-
FIFOCache: A First-In-First-Out cache with fixed capacity, using Map.Strict
for key-value storage and Sequence.Strict for maintaining insertion order.

Key operations and their complexities:
- Insertion: O(log n)
- Lookup: O(log n)
- Removal of oldest element: O(1)
- Size query: O(1)
-}
data FIFOCache k v = FIFOCache
  { maxCapacity :: !Word64
  , cacheMap :: !(Map.Map k v)
  , keyOrder :: !(StrictSeq k)
  }

empty :: Word64 -> FIFOCache k v
empty capacity = FIFOCache capacity Map.empty Seq.empty

insert :: Ord k => k -> v -> FIFOCache k v -> FIFOCache k v
insert key value cache@FIFOCache {..}
  | Map.size cacheMap >= fromIntegral maxCapacity =
      case Seq.lookup 0 keyOrder of
        Nothing -> cache -- This should never happen if invariants are maintained
        Just oldestKey ->
          let !newKeyOrder = key Seq.<| Seq.drop 1 keyOrder
              !newMap = Map.insert key value $ Map.delete oldestKey cacheMap
           in cache {cacheMap = newMap, keyOrder = newKeyOrder}
  | otherwise =
      cache
        { cacheMap = Map.insert key value cacheMap
        , keyOrder = keyOrder Seq.|> key
        }

lookup :: Ord k => k -> FIFOCache k v -> Maybe v
lookup key FIFOCache {..} = Map.lookup key cacheMap

getSize :: FIFOCache k v -> Int
getSize = Map.size . cacheMap

getCapacity :: FIFOCache k v -> Word64
getCapacity = maxCapacity

cleanupCache :: FIFOCache k v -> FIFOCache k v
cleanupCache cache = cache {cacheMap = Map.empty, keyOrder = Seq.empty}
