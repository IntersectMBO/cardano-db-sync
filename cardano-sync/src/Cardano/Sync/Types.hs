{-# LANGUAGE DataKinds #-}
module Cardano.Sync.Types
  ( BlockDetails (..)
  , CardanoBlock
  , CardanoPoint
  , CardanoProtocol
  , EpochSlot (..)
  , PoolKeyHash
  , SlotDetails (..)
  , SyncState (..)
  , Block (..)
  , MetricSetters (..)
  ) where

import           Cardano.Prelude hiding (Meta)

import           Cardano.Sync.Config.Types (CardanoBlock, CardanoProtocol)

import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))

import           Data.Time.Clock (UTCTime)

import           Ouroboros.Consensus.Cardano.Block (StandardCrypto)
import           Ouroboros.Network.Block (BlockNo, Point)

import qualified Shelley.Spec.Ledger.Keys as Shelley


type CardanoPoint = Point CardanoBlock

type PoolKeyHash = Shelley.KeyHash 'Shelley.StakePool StandardCrypto

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

-- The hash must be unique!
data Block = Block
  { bHash :: !ByteString
  , bEpochNo :: !EpochNo
  , bSlotNo  :: !SlotNo
  , bBlockNo :: !BlockNo
  } deriving (Eq, Show)

-- The metrics we use.
-- Kept as a separate struct and do not put into environment because
-- when we need to test functions using this we need to initialize the
-- whole environment and not just pass in the layer. This shows clearly
-- that it needs to remain a separate parameter passed around where needed.
data MetricSetters = MetricSetters
  { metricsSetNodeBlockHeight :: BlockNo -> IO ()
  , metricsSetDbQueueLength :: Natural -> IO ()
  , metricsSetDbBlockHeight :: BlockNo -> IO ()
  , metricsSetDbSlotHeight :: SlotNo -> IO ()
  }

