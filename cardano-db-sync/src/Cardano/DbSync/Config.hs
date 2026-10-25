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
import Cardano.DbSync.Error (NodeConfigError (..), SyncNodeError (..))
import Cardano.DbSync.Tracing.Setup (mkStdoutTracer)
import Cardano.Logging (
  BackendConfig (..),
  ConfigSource (..),
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
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml
import System.FilePath (takeDirectory, (</>))
import qualified System.IO as IO

-- | Create a simple stdout tracer configured from the db-sync config file.
-- This is what the tests and simple tools use; @cardano-db-sync@ itself
-- creates its tracers (with optional forwarding to cardano-tracer) via
-- 'Cardano.DbSync.Tracing.Setup.mkDbSyncTracers'.
configureLogging :: SyncNodeConfig -> Text -> IO (Trace IO LogMessage)
configureLogging syncNodeConfig =
  mkStdoutTracer (dncEnableLogging syncNodeConfig) (dncTraceConfig syncNodeConfig)

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

-- | The keys that configured logging before the move to @trace-dispatcher@.
legacyLoggingKeys :: [Text]
legacyLoggingKeys =
  [ "defaultBackends"
  , "defaultScribes"
  , "minSeverity"
  , "options"
  , "rotation"
  , "setupBackends"
  , "setupScribes"
  ]

-- | Read the trace-dispatcher configuration ('TraceOptions' et al.) from the
-- db-sync config file. When the file does not contain any tracing options the
-- default configuration is used; a file that still carries the legacy
-- @iohk-monitoring@ keys gets a warning, because it asks for logging that is
-- not what it will get.
readTraceConfig :: FilePath -> ByteString -> IO TraceConfig
readTraceConfig fp configBytes =
  case Yaml.decodeEither' configBytes of
    Right (Aeson.Object o)
      | any (`KeyMap.member` o) ["TraceOptions", "HermodTracing", "Options"] -> parseTraceConfig
      | otherwise -> warnLegacyKeys (legacyKeysIn o) >> pure defaultTraceConfig
    _otherwise -> pure defaultTraceConfig
  where
    -- trace-dispatcher parses the tracing options itself, reading the file again,
    -- and throws on a malformed one. Left alone that reaches 'main' as a bare
    -- 'Yaml.ParseException' naming neither db-sync nor the file.
    parseTraceConfig :: IO TraceConfig
    parseTraceConfig =
      catch (readConfigurationWithFallback Info DNormal (Stdout HumanFormatUncoloured) (FromFile fp)) $
        \(err :: Yaml.ParseException) ->
          throwIO . SNErrNodeConfig . NodeConfigParseError $
            "Error parsing the tracing configuration in " <> fp <> ": " <> show err

    legacyKeysIn :: Aeson.Object -> [Text]
    legacyKeysIn o = filter (`elem` map Key.toText (KeyMap.keys o)) legacyLoggingKeys

    -- On stderr rather than through the tracer, which does not exist yet and is
    -- the very thing this part of the config failed to configure.
    warnLegacyKeys :: [Text] -> IO ()
    warnLegacyKeys [] = pure ()
    warnLegacyKeys keys =
      IO.hPutStrLn IO.stderr . Text.unpack $
        mconcat
          [ "WARNING: ignoring the logging keys "
          , Text.intercalate ", " keys
          , " in "
          , Text.pack fp
          , ": they configure iohk-monitoring, which db-sync no longer uses. Logging is "
          , "running on the default configuration, everything at Info severity to stdout. "
          , "See config/mainnet-config.yaml for the TraceOptions format that replaces them."
          ]

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
      , dncDijkstraGenesisFile = adjustGenesisFilePath adjustGenesisPath <$> ncDijkstraGenesisFile ncfg
      , dncDijkstraGenesisHash = ncDijkstraGenesisHash ncfg
      , dncByronProtocolVersion = ncByronProtocolVersion ncfg
      , dncExperimentalHardForksEnabled = ncExperimentalHardForksEnabled ncfg
      , dncShelleyHardFork = ncShelleyHardFork ncfg
      , dncAllegraHardFork = ncAllegraHardFork ncfg
      , dncMaryHardFork = ncMaryHardFork ncfg
      , dncAlonzoHardFork = ncAlonzoHardFork ncfg
      , dncBabbageHardFork = ncBabbageHardFork ncfg
      , dncConwayHardFork = ncConwayHardFork ncfg
      , dncDijkstraHardFork = ncDijkstraHardFork ncfg
      , dncInsertOptions = extractInsertOptions pcfg
      , dncIpfsGateway = endsInSlash <$> pcIpfsGateway pcfg
      , dncSnapshotInterval = pcSnapshotInterval pcfg
      , dncLedgerBackend = pcLedgerBackend pcfg
      }

mkAdjustPath :: SyncPreConfig -> (FilePath -> FilePath)
mkAdjustPath cfg fp = takeDirectory (pcNodeConfigFilePath cfg) </> fp

endsInSlash :: Text -> Text
endsInSlash txt = if Text.isSuffixOf "/" txt then txt else txt <> "/"
