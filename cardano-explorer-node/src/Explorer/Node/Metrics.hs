{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Explorer.Node.Metrics
  ( Metrics (..)
  , makeMetrics
  , registerMetricsServer
  ) where

import           Cardano.Prelude

import           System.Metrics.Prometheus.Concurrent.RegistryT (RegistryT (..), registerGauge,
                    runRegistryT, unRegistryT)
import           System.Metrics.Prometheus.Metric.Gauge (Gauge)
import           System.Metrics.Prometheus.Http.Scrape (serveHttpTextMetricsT)


data Metrics = Metrics
  { mNodeHeight :: !Gauge
  , mQueuePre :: !Gauge
  , mQueuePost :: !Gauge
  , mQueuePostWrite :: !Gauge
  }

registerMetricsServer :: IO (Metrics, Async ())
registerMetricsServer =
  runRegistryT $ do
    metrics <- makeMetrics
    registry <- RegistryT ask
    server <- liftIO . async $ runReaderT (unRegistryT $ serveHttpTextMetricsT 8080 []) registry
    pure (metrics, server)

makeMetrics :: RegistryT IO Metrics
makeMetrics =
  Metrics
    <$> registerGauge "remote_tip_height" mempty
    <*> registerGauge "action_queue_length_pre" mempty
    <*> registerGauge "action_queue_length_post" mempty
    <*> registerGauge "action_queue_length_post_write" mempty

