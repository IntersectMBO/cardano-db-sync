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

{-
FIFOCache: A First-Ins-First-Outs cache with fixed capacity, using Map.Strict
for key-value storage. It keeps 3 rolling Maps and only the first one is inserted.
When the first one is full, the last Map is removed.

Key operations and their complexities:
- Insertion: O(log mapCapacity)
- Lookup: O(log (mapCapacity * n))
- Removal of oldest elements: O(1)
- Size query: O(1)
-}
data FIFOCache k v = FIFOCache
  { mapCapacity :: !Word64
  , cacheMap0 :: !(Map.Map k v)
  , cacheMap1 :: !(Map.Map k v)
  , cacheMap2 :: !(Map.Map k v)
  }

empty :: Word64 -> FIFOCache k v
empty capacity = FIFOCache capacity Map.empty Map.empty Map.empty

insert :: Ord k => k -> v -> FIFOCache k v -> FIFOCache k v
insert key value cache@FIFOCache {..}
  | Map.size cacheMap0 >= fromIntegral mapCapacity =
      cache
        { cacheMap0 = Map.singleton key value
        , cacheMap1 = cacheMap0
        , cacheMap2 = cacheMap1
        }
  | otherwise =
      cache
        { cacheMap0 = Map.insert key value cacheMap0
        }

lookup :: Ord k => k -> FIFOCache k v -> Maybe v
lookup key FIFOCache {..} =
  Map.lookup key cacheMap0 <|> Map.lookup key cacheMap1 <|> Map.lookup key cacheMap2

getSize :: FIFOCache k v -> Int
getSize FIFOCache {..} = Map.size cacheMap0 + Map.size cacheMap1 + Map.size cacheMap2

getCapacity :: FIFOCache k v -> Word64
getCapacity FIFOCache {..} = 3 * mapCapacity

cleanupCache :: FIFOCache k v -> FIFOCache k v
cleanupCache cache =
  cache
    { cacheMap0 = Map.empty
    , cacheMap1 = Map.empty
    , cacheMap2 = Map.empty
    }
