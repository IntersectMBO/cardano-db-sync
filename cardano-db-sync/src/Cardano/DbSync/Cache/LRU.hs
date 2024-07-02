{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Cache.LRU (
  LRUCache (..),
  empty,
  cleanup,
  trim,
  insert,
  fromList,
  delete,
  lookup,
  getSize,
  getCapacity,
) where

import Cardano.Prelude hiding (empty)
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as OrdPSQ

-- Inspired by https://jaspervdj.be/posts/2015-02-24-lru-cache.html
-- We use Maps based on Ord instead of Hash, to avoid hash collision attacks.

-- LRUCache represents a Least Recently Used (LRU) Cache.
-- It uses an OrdPSQ to maintain the order of access.
data LRUCache k v = LRUCache
  { cCapacity :: !Word64 -- The maximum capacity of the cache
  , cTick :: !Word64 -- A counter used to track the order of access
  , cQueue :: !(OrdPSQ k Word64 v) -- The priority search queue storing the cache entries
  }

-- empty creates an empty LRUCache with the specified capacity.
empty :: Word64 -> LRUCache k v
empty capacity =
  LRUCache
    { cCapacity = capacity
    , cTick = 0
    , cQueue = OrdPSQ.empty
    }

-- cleanup resets the cache, emptying all entries and resetting the tick counter.
cleanup :: LRUCache k v -> LRUCache k v
cleanup cache =
  cache
    { cTick = 0
    , cQueue = OrdPSQ.empty
    }

-- trim ensures the cache size does not exceed its capacity.
-- It removes the least recently used item if the cache is over capacity.
trim :: Ord k => LRUCache k v -> LRUCache k v
trim cache
  | cTick cache == maxBound = empty (cCapacity cache) -- Reset the cache if the tick counter overflows
  | fromIntegral (OrdPSQ.size $ cQueue cache) > cCapacity cache =
      cache {cQueue = OrdPSQ.deleteMin (cQueue cache)} -- Remove the least recently used item
  | otherwise = cache

-- insert adds a new key-value pair to the cache, updating the access order.
-- It trims the cache if necessary to maintain the capacity.
insert :: Ord k => k -> v -> LRUCache k v -> LRUCache k v
insert k v cache =
  trim $!
    cache
      { cTick = cTick cache + 1 -- Increment the tick counter
      , cQueue = queue
      }
  where
    (_mbOldVal, queue) = OrdPSQ.insertView k (cTick cache) v (cQueue cache) -- Insert the new entry

-- fromList inserts into a cache from a list of key-value pairs.
fromList :: Ord k => [(k, v)] -> LRUCache k v -> LRUCache k v
fromList kvs cache = foldl' (\c (k, v) -> insert k v c) cache kvs

delete :: Ord k => k -> LRUCache k v -> LRUCache k v
delete key cache =
  cache {cQueue = OrdPSQ.delete key (cQueue cache)}

-- lookup retrieves a value from the cache by its key, updating the access order.
-- It returns the value and the updated cache.
lookup :: Ord k => k -> LRUCache k v -> Maybe (v, LRUCache k v)
lookup key cache =
  case OrdPSQ.alter lookupAndUpdate key (cQueue cache) of
    (Nothing, _) -> Nothing
    (Just value, updatedQueue) ->
      let !updatedCache = trim $ cache {cTick = cTick cache + 1, cQueue = updatedQueue} -- Update the tick counter and trim the cache
       in Just (value, updatedCache)
  where
    lookupAndUpdate Nothing = (Nothing, Nothing)
    lookupAndUpdate (Just (_, value)) = (Just value, Just (cTick cache, value)) -- Update the access order

-- getSize returns the number of entries currently in the cache.
getSize :: LRUCache k v -> Int
getSize = OrdPSQ.size . cQueue

-- getCapacity returns the maximum capacity of the cache.
getCapacity :: LRUCache k v -> Word64
getCapacity = cCapacity
