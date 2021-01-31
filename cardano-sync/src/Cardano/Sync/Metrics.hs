{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Sync.Metrics
  ( Metrics (..)
  , makeMetrics
  , withMetricsServer
  ) where

import           Cardano.Prelude

import           System.Metrics.Prometheus.Concurrent.RegistryT (RegistryT (..), registerGauge,
                   runRegistryT, unRegistryT)
import           System.Metrics.Prometheus.Http.Scrape (serveMetricsT)
import           System.Metrics.Prometheus.Metric.Gauge (Gauge)


data Metrics = Metrics
  { mDbHeight :: !Gauge
  , mNodeHeight :: !Gauge
  , mQueuePre :: !Gauge
  , mQueuePost :: !Gauge
  , mQueuePostWrite :: !Gauge
  }

withMetricsServer :: Int -> (Metrics -> IO a) -> IO a
withMetricsServer port action = do
  (metrics, registry) <- runRegistryT $ (,) <$> makeMetrics <*> RegistryT ask
  bracket
     (async $ runReaderT (unRegistryT $ serveMetricsT port []) registry)
     cancel
     (const $ action metrics)

makeMetrics :: RegistryT IO Metrics
makeMetrics =
  Metrics
    <$> registerGauge "db_block_height" mempty
    <*> registerGauge "remote_tip_height" mempty
    <*> registerGauge "action_queue_length_pre" mempty
    <*> registerGauge "action_queue_length_post" mempty
    <*> registerGauge "action_queue_length_post_write" mempty

