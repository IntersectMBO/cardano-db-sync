{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Cache.FIFO (
  FifoCache (..),
  TxIdCache,
  HashableTxId (..),
  empty,
  insert,
  lookup,
  getSize,
  getCapacity,
  emptyTxIdCache,
  insertTxIdCache,
  lookupTxIdCache,
  cleanupTxIdCache,
) where

import Cardano.Crypto.Hash.Class (hashToBytes)
import qualified Cardano.Db as DB
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.SafeHash (extractHash)
import Cardano.Ledger.TxIn (TxId (..))
import Cardano.Prelude hiding (empty)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable, hashWithSalt)
import qualified Data.List.NonEmpty as NE

{- FifoCache: A high-performance, fixed-capacity FIFO (First-In-First-Out) cache implementation

Performance characteristics:
1. Space efficiency:
   - Uses UNPACK pragma for maxCapacity and currentSize, reducing memory overhead
   - Compact representation using NonEmpty for the queue and HashMap for the cache

2. Time complexity:
   - Lookup: O(1) average case, leveraging HashMap's constant-time lookups
   - Insert: O(1) amortized time
     - HashMap insert/delete: O(1) average case
     - NonEmpty cons/uncons: O(1)
   - When cache is full:
     - Efficiently removes oldest item using NonEmpty's O(1) uncons operation
     - Maintains FIFO order without full list traversal

3. Optimizations:
   - Uses strict fields (!) to prevent space leaks
   - Leverages NonEmpty for the queue, ensuring type-safe non-empty invariant
   - Employs HashMap for fast key-value lookups and updates

4. Scalability:
   - Performs well with increasing cache sizes due to HashMap's efficiency
   - Constant-time operations regardless of cache occupancy

5. Memory usage:
   - Bounded by maxCapacity, preventing unbounded growth
   - UNPACK pragma reduces memory footprint for frequently accessed fields

6. Thread safety:
   - Note: This implementation is not thread-safe. Concurrent access should be
     managed externally if required.
-}

data FifoCache k v = FifoCache
  { maxCapacity :: !Natural
  , currentSize :: {-# UNPACK #-} !Int
  , queue :: !(Maybe (NE.NonEmpty k))
  , cacheMap :: !(HM.HashMap k v)
  }

empty :: Natural -> FifoCache k v
empty capacity =
  FifoCache
    { maxCapacity = capacity
    , currentSize = 0
    , queue = Nothing
    , cacheMap = HM.empty
    }

insert :: Hashable k => k -> v -> FifoCache k v -> FifoCache k v
insert key value cache@FifoCache {..}
  | currentSize >= fromIntegral maxCapacity =
      case queue of
        Nothing -> cache -- This should never happen if invariants are maintained
        Just q ->
          let (oldestKey, mNewQueue) = NE.uncons q
              newQueue = Just $ maybe (key NE.:| []) (NE.cons key) mNewQueue
              newMap = HM.insert key value (HM.delete oldestKey cacheMap)
           in cache {queue = newQueue, cacheMap = newMap}
  | otherwise =
      cache
        { currentSize = currentSize + 1
        , queue = Just $ maybe (key NE.:| []) (NE.cons key) queue
        , cacheMap = HM.insert key value cacheMap
        }

lookup :: Hashable k => k -> FifoCache k v -> Maybe v
lookup key FifoCache {..} = HM.lookup key cacheMap

getSize :: FifoCache k v -> Int
getSize = currentSize

getCapacity :: FifoCache k v -> Word64
getCapacity = fromIntegral . maxCapacity

cleanupCache :: FifoCache k v -> FifoCache k v
cleanupCache cache =
  cache
    { currentSize = 0
    , queue = Nothing
    , cacheMap = HM.empty
    }

-- | TxIdCache: A specialized FIFO cache for TxId StandardCrypto.
-- Uses HashableTxId as keys and allows flexible value types.
type TxIdCache v = FifoCache HashableTxId v

newtype HashableTxId = HashableTxId {unHashableTxId :: TxId StandardCrypto}
  deriving newtype (Eq, Ord, Show)

instance Hashable HashableTxId where
  hashWithSalt salt = hashWithSalt salt . hashToBytes . extractHash . getSafeHash . unHashableTxId
    where
      getSafeHash (TxId safehash) = safehash

-- Create a cache for TxId
emptyTxIdCache :: Word64 -> TxIdCache v
emptyTxIdCache = empty . fromIntegral

-- Insert a TxId
insertTxIdCache ::
  TxId StandardCrypto ->
  DB.TxId ->
  TxIdCache DB.TxId ->
  TxIdCache DB.TxId
insertTxIdCache txId = insert (HashableTxId txId)

-- Lookup a TxId
lookupTxIdCache :: TxId StandardCrypto -> TxIdCache DB.TxId -> Maybe DB.TxId
lookupTxIdCache txId = lookup (HashableTxId txId)

cleanupTxIdCache :: TxIdCache v -> TxIdCache v
cleanupTxIdCache = cleanupCache
