{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Cache.LRU (
  LRUCache (..),
  empty,
  cleanup,
  trim,
  insert,
  lookup,
  getSize,
  getCapacity,
) where

import Cardano.Prelude hiding (empty)
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as OrdPSQ

-- Inspired by https://jaspervdj.be/posts/2015-02-24-lru-cache.html
-- We use Maps based on Ord instead of Hash, to avoid hash collision attacks.

data LRUCache k v = LRUCache
  { cCapacity :: !Word64
  , cTick :: !Word64
  , cQueue :: !(OrdPSQ k Word64 v)
  }

empty :: Word64 -> LRUCache k v
empty capacity =
  LRUCache
    { cCapacity = capacity
    , cTick = 0
    , cQueue = OrdPSQ.empty
    }

cleanup :: LRUCache k v -> LRUCache k v
cleanup cache =
  cache
    { cTick = 0
    , cQueue = OrdPSQ.empty
    }

trim :: Ord k => LRUCache k v -> LRUCache k v
trim cache
  | cTick cache == maxBound = empty (cCapacity cache)
  | fromIntegral (OrdPSQ.size $ cQueue cache) > cCapacity cache =
      cache {cQueue = OrdPSQ.deleteMin (cQueue cache)}
  | otherwise = cache

insert :: Ord k => k -> v -> LRUCache k v -> LRUCache k v
insert k v cache =
  trim $!
    cache
      { cTick = cTick cache + 1
      , cQueue = queue
      }
  where
    (_mbOldVal, queue) = OrdPSQ.insertView k (cTick cache) v (cQueue cache)

lookup :: Ord k => k -> LRUCache k v -> Maybe (v, LRUCache k v)
lookup k c =
  case OrdPSQ.alter lookupAndBump k (cQueue c) of
    (Nothing, _) -> Nothing
    (Just x, q) ->
      let !c' = trim $ c {cTick = cTick c + 1, cQueue = q}
       in Just (x, c')
  where
    lookupAndBump Nothing = (Nothing, Nothing)
    lookupAndBump (Just (_, x)) = (Just x, Just (cTick c, x))

getSize :: LRUCache k v -> Int
getSize = OrdPSQ.size . cQueue

getCapacity :: LRUCache k v -> Word64
getCapacity = cCapacity
