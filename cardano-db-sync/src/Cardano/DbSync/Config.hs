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
  cardanoLedgerConfig,
  genesisProtocolMagicId,
  readCardanoGenesisConfig,
  readSyncNodeConfig,
  configureLogging,
) where

import qualified Cardano.BM.Configuration.Model as Logging
import qualified Cardano.BM.Setup as Logging
import Cardano.BM.Trace (Trace)
import qualified Cardano.BM.Trace as Logging
import Cardano.DbSync.Config.Cardano
import Cardano.DbSync.Config.Node (NodeConfig (..), parseNodeConfig, parseSyncPreConfig, readByteStringFromFile)
import Cardano.DbSync.Config.Shelley
import Cardano.DbSync.Config.Types
import Cardano.Prelude
import System.FilePath (takeDirectory, (</>))

configureLogging :: SyncNodeParams -> Text -> IO (Trace IO Text)
configureLogging params loggingName = do
  let configFile = enpConfigFile params
  enc <- readSyncNodeConfig configFile

  if not (dncEnableLogging enc)
    then pure Logging.nullTracer
    else liftIO $ Logging.setupTrace (Right $ dncLoggingConfig enc) loggingName

readSyncNodeConfig :: ConfigFile -> IO SyncNodeConfig
readSyncNodeConfig (ConfigFile fp) = do
  pcfg <- (adjustNodeFilePath . parseSyncPreConfig) =<< readByteStringFromFile fp "DbSync"
  ncfg <- parseNodeConfig =<< readByteStringFromFile (pcNodeConfigFilePath pcfg) "node"
  coalesceConfig pcfg ncfg (mkAdjustPath pcfg)
  where
    adjustNodeFilePath :: IO SyncPreConfig -> IO SyncPreConfig
    adjustNodeFilePath spc = do
      cfg <- spc
      pure $ cfg {pcNodeConfigFile = adjustNodeConfigFilePath (takeDirectory fp </>) (pcNodeConfigFile cfg)}

coalesceConfig ::
  SyncPreConfig ->
  NodeConfig ->
  (FilePath -> FilePath) ->
  IO SyncNodeConfig
coalesceConfig pcfg ncfg adjustGenesisPath = do
  lc <- Logging.setupFromRepresentation $ pcLoggingConfig pcfg
  pure $
    SyncNodeConfig
      { dncNetworkName = pcNetworkName pcfg
      , dncLoggingConfig = lc
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
      , dncByronProtocolVersion = ncByronProtocolVersion ncfg
      , dncShelleyHardFork = ncShelleyHardFork ncfg
      , dncAllegraHardFork = ncAllegraHardFork ncfg
      , dncMaryHardFork = ncMaryHardFork ncfg
      , dncAlonzoHardFork = ncAlonzoHardFork ncfg
      , dncBabbageHardFork = ncBabbageHardFork ncfg
      , dncConwayHardFork = ncConwayHardFork ncfg
      }

mkAdjustPath :: SyncPreConfig -> (FilePath -> FilePath)
mkAdjustPath cfg fp = takeDirectory (pcNodeConfigFilePath cfg) </> fp
