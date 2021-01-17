{-# LANGUAGE DataKinds #-}
module Cardano.DbSync.Types
  ( BlockDetails (..)
  , CardanoBlock
  , CardanoProtocol
  , EpochSlot (..)
  , SlotDetails (..)
  , SyncState (..)
  , Cache
  , PrevInfo (..)
  , newCache
  , readPrevInfo
  , writePrevInfo
  ) where

import           Cardano.DbSync.Config.Types (CardanoBlock, CardanoProtocol)
import           Cardano.Db

import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..))

import           Control.Concurrent.STM
import           Data.ByteString
import           Data.Time.Clock (UTCTime)
import           Data.Word (Word64)

data BlockDetails = BlockDetails
  { bdBlock :: !CardanoBlock
  , bdSlot :: !SlotDetails
  }

newtype EpochSlot = EpochSlot
  { unEpochSlot :: Word64
  } deriving (Eq, Show)

data SlotDetails = SlotDetails
  { sdSlotTime :: !UTCTime
  , sdCurrentTime :: !UTCTime
  , sdEpochNo :: !EpochNo
  , sdEpochSlot :: !EpochSlot
  , sdEpochSize :: !EpochSize
  } deriving (Eq, Show)

data SyncState
  = SyncLagging         -- Local tip is lagging the global chain tip.
  | SyncFollowing       -- Local tip is following global chain tip.
  deriving (Eq, Show)

newtype Cache = Cache
  { _getCache :: TVar (Maybe PrevInfo)
  }

data PrevInfo = PrevInfo
  { cachePrevBlockId :: BlockId
  , cachePrevHash :: ByteString
  }

readPrevInfo :: Cache -> IO (Maybe PrevInfo)
readPrevInfo (Cache cache) = readTVarIO cache

writePrevInfo :: Cache -> Maybe PrevInfo -> IO ()
writePrevInfo (Cache cache) = atomically . writeTVar cache

newCache :: IO Cache
newCache = Cache <$> newTVarIO Nothing
