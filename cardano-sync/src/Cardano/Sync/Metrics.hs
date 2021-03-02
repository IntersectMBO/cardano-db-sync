module Cardano.Sync.Metrics
  ( setNodeBlockHeight
  , setDbQueueLength
  , setDbBlockHeight
  , setDbSlotHeight
  ) where

import           Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..), fromWithOrigin)

import           Cardano.Sync.Types

import           Numeric.Natural (Natural)

import           Ouroboros.Network.Block (BlockNo (..))


setNodeBlockHeight :: MetricSetters -> WithOrigin BlockNo -> IO ()
setNodeBlockHeight setters woBlkNo =
  metricsSetNodeBlockHeight setters (fromWithOrigin (BlockNo 0) woBlkNo)

setDbQueueLength :: MetricSetters -> Natural -> IO ()
setDbQueueLength = metricsSetDbQueueLength

setDbBlockHeight :: MetricSetters -> BlockNo -> IO ()
setDbBlockHeight = metricsSetDbBlockHeight

setDbSlotHeight :: MetricSetters -> SlotNo -> IO ()
setDbSlotHeight = metricsSetDbSlotHeight
