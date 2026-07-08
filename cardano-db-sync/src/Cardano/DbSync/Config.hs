{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Config (
  ConfigFile (..),
  GenesisConfig (..),
  GenesisFile (..),
  LedgerStateDir (..),
  NetworkName (..),
  ShelleyConfig (..),
  SocketPath (..),
  SyncCommand (..),
  SyncProtocol (..),
  SyncNodeConfig (..),
  SyncNodeParams (..),
  genesisProtocolMagicId,
  readCardanoGenesisConfig,
  readSyncNodeConfig,
  configureLogging,
  defaultTraceConfig,
) where

import Cardano.Db.Log (LogMessage)
import Cardano.DbSync.Api (extractInsertOptions)
import Cardano.DbSync.Config.Cardano
import Cardano.DbSync.Config.Node (NodeConfig (..), parseNodeConfig, parseSyncPreConfig, readByteStringFromFile)
import Cardano.DbSync.Config.Shelley
import Cardano.DbSync.Config.Types
import Cardano.DbSync.Tracing.Setup (mkStdoutTracer)
import Cardano.Logging (
  BackendConfig (..),
  DetailLevel (..),
  FormatLogging (..),
  SeverityS (..),
  Trace,
  TraceConfig,
  mkConfigurationWithFallback,
  readConfigurationWithFallback,
 )
import Cardano.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml
import System.FilePath (takeDirectory, (</>))

-- | Create a simple stdout tracer configured from the db-sync config file.
-- This is what the tests and simple tools use; @cardano-db-sync@ itself
-- creates its tracers (with optional forwarding to cardano-tracer) via
-- 'Cardano.DbSync.Tracing.Setup.mkDbSyncTracers'.
configureLogging :: SyncNodeConfig -> Text -> IO (Trace IO LogMessage)
configureLogging syncNodeConfig loggingName =
  mkStdoutTracer (dncEnableLogging syncNodeConfig) (dncTraceConfig syncNodeConfig) loggingName

readSyncNodeConfig :: ConfigFile -> IO SyncNodeConfig
readSyncNodeConfig (ConfigFile fp) = do
  configBytes <- readByteStringFromFile fp "DbSync"
  pcfg <- adjustNodeFilePath <$> parseSyncPreConfig configBytes
  ncfg <- parseNodeConfig =<< readByteStringFromFile (pcNodeConfigFilePath pcfg) "node"
  traceConfig <- readTraceConfig fp configBytes
  coalesceConfig traceConfig pcfg ncfg (mkAdjustPath pcfg)
  where
    adjustNodeFilePath :: SyncPreConfig -> SyncPreConfig
    adjustNodeFilePath cfg =
      cfg {pcNodeConfigFile = adjustNodeConfigFilePath (takeDirectory fp </>) (pcNodeConfigFile cfg)}

-- | The default trace-dispatcher configuration for db-sync: everything at
-- 'Info' severity to stdout (human readable).
defaultTraceConfig :: TraceConfig
defaultTraceConfig =
  mkConfigurationWithFallback Info DNormal (Stdout HumanFormatUncoloured)

-- | Read the trace-dispatcher configuration ('TraceOptions' et al.) from the
-- db-sync config file. When the file does not contain any tracing options
-- (for example an old config file with the legacy @iohk-monitoring@ keys,
-- which are silently ignored), the default configuration is used.
readTraceConfig :: FilePath -> ByteString -> IO TraceConfig
readTraceConfig fp configBytes =
  case Yaml.decodeEither' configBytes of
    Right (Aeson.Object o)
      | any (`KeyMap.member` o) ["TraceOptions", "HermodTracing", "Options"] ->
          readConfigurationWithFallback Info DNormal (Stdout HumanFormatUncoloured) fp
    _otherwise -> pure defaultTraceConfig

coalesceConfig ::
  TraceConfig ->
  SyncPreConfig ->
  NodeConfig ->
  (FilePath -> FilePath) ->
  IO SyncNodeConfig
coalesceConfig traceConfig pcfg ncfg adjustGenesisPath =
  pure $
    SyncNodeConfig
      { dncNetworkName = pcNetworkName pcfg
      , dncTraceConfig = traceConfig
      , dncNodeConfigFile = pcNodeConfigFile pcfg
      , dncProtocol = ncProtocol ncfg
      , dncRequiresNetworkMagic = ncRequiresNetworkMagic ncfg
      , dncEnableLogging = pcEnableLogging pcfg
      , dncEnableMetrics = pcEnableMetrics pcfg
      , dncPrometheusPort = pcPrometheusPort pcfg
      , dncPBftSignatureThreshold = ncPBftSignatureThreshold ncfg
      , dncByronGenesisFile = adjustGenesisFilePath adjustGenesisPath (ncByronGenesisFile ncfg)
      , dncByronGenesisHash = ncByronGenesisHash ncfg
      , dncShelleyGenesisFile = adjustGenesisFilePath adjustGenesisPath (ncShelleyGenesisFile ncfg)
      , dncShelleyGenesisHash = ncShelleyGenesisHash ncfg
      , dncAlonzoGenesisFile = adjustGenesisFilePath adjustGenesisPath (ncAlonzoGenesisFile ncfg)
      , dncAlonzoGenesisHash = ncAlonzoGenesisHash ncfg
      , dncConwayGenesisFile = adjustGenesisFilePath adjustGenesisPath (ncConwayGenesisFile ncfg)
      , dncConwayGenesisHash = ncConwayGenesisHash ncfg
      , dncByronProtocolVersion = ncByronProtocolVersion ncfg
      , dncShelleyHardFork = ncShelleyHardFork ncfg
      , dncAllegraHardFork = ncAllegraHardFork ncfg
      , dncMaryHardFork = ncMaryHardFork ncfg
      , dncAlonzoHardFork = ncAlonzoHardFork ncfg
      , dncBabbageHardFork = ncBabbageHardFork ncfg
      , dncConwayHardFork = ncConwayHardFork ncfg
      , dncInsertOptions = extractInsertOptions pcfg
      , dncIpfsGateway = endsInSlash <$> pcIpfsGateway pcfg
      , dncSnapshotInterval = pcSnapshotInterval pcfg
      , dncLedgerBackend = pcLedgerBackend pcfg
      }

mkAdjustPath :: SyncPreConfig -> (FilePath -> FilePath)
mkAdjustPath cfg fp = takeDirectory (pcNodeConfigFilePath cfg) </> fp

endsInSlash :: Text -> Text
endsInSlash txt = if Text.isSuffixOf "/" txt then txt else txt <> "/"
