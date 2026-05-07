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
  setDbBlocksPerSecond,
  setInsertDuration,
  setCacheHitRate,
  makeMetrics,
  withMetricSetters,
  withMetricsServer,
) where

import Cardano.DbSync.Types (MetricSetters (..), CacheType(..))
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
  , mDbBlocksPerSecond :: !Gauge
  -- ^ The number of blocks being processes per second.
  , mInsertDuration :: !Gauge
  -- ^ The duration of the last insert operation in seconds.
  , mCacheStakeHitRate :: !Gauge
  -- ^ Cache hit rate for stake cache.
  , mCachePoolsHitRate :: !Gauge
  -- ^ Cache hit rate for pools cache.
  , mCacheDatumHitRate :: !Gauge
  -- ^ Cache hit rate for datum cache.
  , mCacheMultiAssetsHitRate :: !Gauge
  -- ^ Cache hit rate for multi_assets cache.
  , mCachePrevBlockHitRate :: !Gauge
  -- ^ Cache hit rate for prev_block cache.
  , mCacheAddressHitRate :: !Gauge
  -- ^ Cache hit rate for address cache.
  , mCacheTxIdsHitRate :: !Gauge
  -- ^ Cache hit rate for tx_ids cache.
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
        , metricsSetDbBlocksPerSecond = \bps ->
            Gauge.set bps $ mDbBlocksPerSecond metrics
        , metricsSetInsertDuration = \duration ->
            Gauge.set duration $ mInsertDuration metrics
        , metricsSetCacheHitRate = \cacheName hitRate ->
            case cacheName of
              CacheStake -> Gauge.set hitRate $ mCacheStakeHitRate metrics
              CachePools -> Gauge.set hitRate $ mCachePoolsHitRate metrics
              CacheDatum -> Gauge.set hitRate $ mCacheDatumHitRate metrics
              CacheMultiAssets -> Gauge.set hitRate $ mCacheMultiAssetsHitRate metrics
              CachePrevBlock -> Gauge.set hitRate $ mCachePrevBlockHitRate metrics
              CacheAddress -> Gauge.set hitRate $ mCacheAddressHitRate metrics
              CacheTxIds -> Gauge.set hitRate $ mCacheTxIdsHitRate metrics
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
    <*> registerGauge "cardano_db_sync_blocks_per_second" mempty
    <*> registerGauge "cardano_db_sync_insert_duration_seconds" mempty
    <*> registerGauge "cardano_db_sync_cache_stake_hit_rate" mempty
    <*> registerGauge "cardano_db_sync_cache_pools_hit_rate" mempty
    <*> registerGauge "cardano_db_sync_cache_datum_hit_rate" mempty
    <*> registerGauge "cardano_db_sync_cache_multi_assets_hit_rate" mempty
    <*> registerGauge "cardano_db_sync_cache_prev_block_hit_rate" mempty
    <*> registerGauge "cardano_db_sync_cache_address_hit_rate" mempty
    <*> registerGauge "cardano_db_sync_cache_tx_ids_hit_rate" mempty

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

setDbBlocksPerSecond :: MetricSetters -> Double -> IO ()
setDbBlocksPerSecond = metricsSetDbBlocksPerSecond

setInsertDuration :: MetricSetters -> Double -> IO ()
setInsertDuration = metricsSetInsertDuration

setCacheHitRate :: MetricSetters -> CacheType -> Double -> IO ()
setCacheHitRate = metricsSetCacheHitRate
