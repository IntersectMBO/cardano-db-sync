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
import qualified Data.Sequence as Seq

{-
FIFOCache: A First-In-First-Out cache with fixed capacity.

Efficiently stores and retrieves key-value pairs, automatically removing
oldest entries when full.
Main operations: (insert, lookup, remove oldest) perform in O(log n) time, with O(1) size queries.
-}

data FIFOCache k v = FIFOCache
  { maxCapacity :: !Word64
  , cacheMap :: !(Map.Map k v)
  , keyOrder :: !(Seq.Seq k) -- To track insertion order
  }

empty :: Word64 -> FIFOCache k v
empty capacity = FIFOCache capacity Map.empty Seq.empty

insert :: Ord k => k -> v -> FIFOCache k v -> FIFOCache k v
insert key value cache@FIFOCache {..}
  | Map.size cacheMap >= fromIntegral maxCapacity =
      let oldestKey = Seq.index keyOrder 0
          !newKeyOrder = Seq.drop 1 keyOrder Seq.|> key
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
