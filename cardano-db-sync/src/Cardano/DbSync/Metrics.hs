{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Metrics (
  Metrics (..),
  MetricSetters (..),
  setNodeBlockHeight,
  setDbQueueLength,
  setDbBlockHeight,
  setDbSlotHeight,
  makeMetrics,
  withMetricSetters,
  withMetricsServer,
  nullMetricSetters,
) where

import Cardano.DbSync.AppT (App, runApp)
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
  -- ^ The number of @DbAction@ remaining for the database.
  , mDbBlockHeight :: !Gauge
  -- ^ The block tip number in the database.
  , mDbSlotHeight :: !Gauge
  -- ^ The slot tip number in the database.
  }

-- The metrics we use.
-- Kept as a separate struct and do not put into environment because
-- when we need to test functions using this we need to initialize the
-- whole environment and not just pass in the layer. This shows clearly
-- that it needs to remain a separate parameter passed around where needed.
data MetricSetters = MetricSetters
  { metricsSetNodeBlockHeight :: BlockNo -> App ()
  , metricsSetDbQueueLength :: Natural -> App ()
  , metricsSetDbBlockHeight :: BlockNo -> App ()
  , metricsSetDbSlotHeight :: SlotNo -> App ()
  }

-- This enables us to be much more flexibile with what we actually measure.
withMetricSetters :: Int -> (MetricSetters -> App a) -> App a
withMetricSetters prometheusPort action =
  withMetricsServer prometheusPort $ \metrics -> do
    action $
      MetricSetters
        { metricsSetNodeBlockHeight = \(BlockNo nodeHeight) ->
            liftIO $ Gauge.set (fromIntegral nodeHeight) $ mNodeBlockHeight metrics
        , metricsSetDbQueueLength = \queuePostWrite ->
            liftIO $ Gauge.set (fromIntegral queuePostWrite) $ mDbQueueLength metrics
        , metricsSetDbBlockHeight = \(BlockNo blockNo) ->
            liftIO $ Gauge.set (fromIntegral blockNo) $ mDbBlockHeight metrics
        , metricsSetDbSlotHeight = \(SlotNo slotNo) ->
            liftIO $ Gauge.set (fromIntegral slotNo) $ mDbSlotHeight metrics
        }

-- | Eequired for testing or when disabling the metrics.
nullMetricSetters :: MetricSetters
nullMetricSetters =
  MetricSetters
    { metricsSetNodeBlockHeight = const $ pure ()
    , metricsSetDbQueueLength = const $ pure ()
    , metricsSetDbBlockHeight = const $ pure ()
    , metricsSetDbSlotHeight = const $ pure ()
    }

withMetricsServer :: Int -> (Metrics -> App a) -> App a
withMetricsServer port action = do
  syncEnv <- ask
  -- Using both `RegistryT` and `bracket` here is overkill. Unfortunately the
  -- Prometheus API requires the use of a `Registry` and this seems to be the
  -- least sucky way of doing it.
  (metrics, registry) <- liftIO $ runRegistryT $ (,) <$> makeMetrics <*> RegistryT ask
  result <-
    liftIO $
      bracket
        (async $ runReaderT (unRegistryT $ serveMetricsT port []) registry)
        cancel
        (\_ -> runApp syncEnv (action metrics))
  -- Handle the result of the action
  case result of
    Left err -> throwError err
    Right val -> pure val

makeMetrics :: RegistryT IO Metrics
makeMetrics =
  Metrics
    <$> registerGauge "cardano_db_sync_node_block_height" mempty
    <*> registerGauge "cardano_db_sync_db_queue_length" mempty
    <*> registerGauge "cardano_db_sync_db_block_height" mempty
    <*> registerGauge "cardano_db_sync_db_slot_height" mempty

setNodeBlockHeight :: MetricSetters -> WithOrigin BlockNo -> App ()
setNodeBlockHeight setters woBlkNo =
  metricsSetNodeBlockHeight setters (fromWithOrigin (BlockNo 0) woBlkNo)

setDbQueueLength :: MetricSetters -> Natural -> App ()
setDbQueueLength = metricsSetDbQueueLength

setDbBlockHeight :: MetricSetters -> BlockNo -> App ()
setDbBlockHeight = metricsSetDbBlockHeight

setDbSlotHeight :: MetricSetters -> SlotNo -> App ()
setDbSlotHeight = metricsSetDbSlotHeight
