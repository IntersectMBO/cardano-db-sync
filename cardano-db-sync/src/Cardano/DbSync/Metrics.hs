{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Metrics (
  Metrics (..),
  setNodeBlockHeight,
  setDbQueueLength,
  setDbBlockHeight,
  setDbSlotHeight,
  setDbEpochSyncDuration,
  setDbEpochSyncNumber,
  makeMetrics,
  withMetricSetters,
  withMetricsServer,
) where

import Cardano.DbSync.Types (MetricSetters (..))
import Cardano.Prelude
import Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..), fromWithOrigin)
import Ouroboros.Network.Block (BlockNo (..))
import System.Metrics.Prometheus.Concurrent.RegistryT (
  RegistryT (..),
  registerGauge,
  runRegistryT,
  unRegistryT,
 )
import System.Metrics.Prometheus.Http.Scrape (serveMetricsT)
import System.Metrics.Prometheus.Metric.Gauge (Gauge)
import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge

data Metrics = Metrics
  { mNodeBlockHeight :: !Gauge
  -- ^ The block tip number of the remote node.
  , mDbQueueLength :: !Gauge
  -- ^ The number of @DbEvent@ remaining for the database.
  , mDbBlockHeight :: !Gauge
  -- ^ The block tip number in the database.
  , mDbSlotHeight :: !Gauge
  -- ^ The slot tip number in the database.
  , mDbEpochSyncDuration :: !Gauge
  -- ^ The duration of the last epoch sync in seconds.
  , mDbEpochSyncNumber :: !Gauge
  -- ^ The number of the last epoch that was synced.
  }

-- This enables us to be much more flexibile with what we actually measure.
withMetricSetters :: Int -> (MetricSetters -> IO a) -> IO a
withMetricSetters prometheusPort action =
  withMetricsServer prometheusPort $ \metrics -> do
    action $
      MetricSetters
        { metricsSetNodeBlockHeight = \(BlockNo nodeHeight) ->
            Gauge.set (fromIntegral nodeHeight) $ mNodeBlockHeight metrics
        , metricsSetDbQueueLength = \queuePostWrite ->
            Gauge.set (fromIntegral queuePostWrite) $ mDbQueueLength metrics
        , metricsSetDbBlockHeight = \(BlockNo blockNo) ->
            Gauge.set (fromIntegral blockNo) $ mDbBlockHeight metrics
        , metricsSetDbSlotHeight = \(SlotNo slotNo) ->
            Gauge.set (fromIntegral slotNo) $ mDbSlotHeight metrics
        , metricsSetDbEpochSyncDuration = \duration ->
            Gauge.set duration $ mDbEpochSyncDuration metrics
        , metricsSetDbEpochSyncNumber = \epochNo ->
            Gauge.set (fromIntegral epochNo) $ mDbEpochSyncNumber metrics
        }

withMetricsServer :: Int -> (Metrics -> IO a) -> IO a
withMetricsServer port action = do
  -- Using both `RegistryT` and `bracket` here is overkill. Unfortunately the
  -- Prometheus API requires the use of a `Registry` and this seems to be the
  -- least sucky way of doing it.
  (metrics, registry) <- runRegistryT $ (,) <$> makeMetrics <*> RegistryT ask
  bracket
    (async $ runReaderT (unRegistryT $ serveMetricsT port []) registry)
    cancel
    (const $ action metrics)

makeMetrics :: RegistryT IO Metrics
makeMetrics =
  Metrics
    <$> registerGauge "cardano_db_sync_node_block_height" mempty
    <*> registerGauge "cardano_db_sync_db_queue_length" mempty
    <*> registerGauge "cardano_db_sync_db_block_height" mempty
    <*> registerGauge "cardano_db_sync_db_slot_height" mempty
    <*> registerGauge "cardano_db_sync_db_epoch_sync_duration_seconds" mempty
    <*> registerGauge "cardano_db_sync_db_epoch_sync_number" mempty

setNodeBlockHeight :: MetricSetters -> WithOrigin BlockNo -> IO ()
setNodeBlockHeight setters woBlkNo =
  metricsSetNodeBlockHeight setters (fromWithOrigin (BlockNo 0) woBlkNo)

setDbQueueLength :: MetricSetters -> Natural -> IO ()
setDbQueueLength = metricsSetDbQueueLength

setDbBlockHeight :: MetricSetters -> BlockNo -> IO ()
setDbBlockHeight = metricsSetDbBlockHeight

setDbSlotHeight :: MetricSetters -> SlotNo -> IO ()
setDbSlotHeight = metricsSetDbSlotHeight

setDbEpochSyncDuration :: MetricSetters -> Double -> IO ()
setDbEpochSyncDuration = metricsSetDbEpochSyncDuration

setDbEpochSyncNumber :: MetricSetters -> Word64 -> IO ()
setDbEpochSyncNumber = metricsSetDbEpochSyncNumber
