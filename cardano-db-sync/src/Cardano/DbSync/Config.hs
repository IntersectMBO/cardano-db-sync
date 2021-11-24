{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.DbSync.Config
  ( ConfigFile (..)
  , GenesisConfig (..)
  , GenesisFile (..)
  , LedgerStateDir (..)
  , NetworkName (..)
  , ShelleyConfig (..)
  , SocketPath (..)
  , SyncCommand (..)
  , SyncProtocol (..)
  , SyncNodeConfig (..)
  , SyncNodeParams (..)
  , cardanoLedgerConfig
  , genesisProtocolMagicId
  , readCardanoGenesisConfig
  , readSyncNodeConfig
  , configureLogging
  ) where

import           Cardano.Prelude

import qualified Cardano.BM.Setup as Logging
import           Cardano.BM.Trace (Trace)
import qualified Cardano.BM.Trace as Logging

import qualified Cardano.BM.Configuration.Model as Logging

import           Cardano.DbSync.Config.Cardano
import           Cardano.DbSync.Config.Node
import           Cardano.DbSync.Config.Shelley
import           Cardano.DbSync.Config.Types
import           Cardano.DbSync.Util

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml

import           System.FilePath (takeDirectory, (</>))

configureLogging :: SyncNodeParams -> Text -> IO (Trace IO Text)
configureLogging params loggingName = do
    let configFile = enpConfigFile params
    enc <- readSyncNodeConfig configFile

    if not (dncEnableLogging enc)
       then pure Logging.nullTracer
       else liftIO $ Logging.setupTrace (Right $ dncLoggingConfig enc) loggingName

readSyncNodeConfig :: ConfigFile -> IO SyncNodeConfig
readSyncNodeConfig (ConfigFile fp) = do
    pcfg <- adjustNodeFilePath . parseSyncPreConfig <$> readByteString fp "DbSync"
    ncfg <- parseNodeConfig <$> readByteString (pcNodeConfigFilePath pcfg) "node"
    coalesceConfig pcfg ncfg (mkAdjustPath pcfg)
  where
    parseSyncPreConfig :: ByteString -> SyncPreConfig
    parseSyncPreConfig bs =
      case Yaml.decodeEither' bs of
      Left err -> panic $ "readSyncNodeConfig: Error parsing config: " <> textShow err
      Right res -> res

    adjustNodeFilePath :: SyncPreConfig -> SyncPreConfig
    adjustNodeFilePath cfg =
      cfg { pcNodeConfigFile = adjustNodeConfigFilePath (takeDirectory fp </>) (pcNodeConfigFile cfg) }

coalesceConfig
    :: SyncPreConfig -> NodeConfig -> (FilePath -> FilePath)
    -> IO SyncNodeConfig
coalesceConfig pcfg ncfg adjustGenesisPath = do
  lc <- Logging.setupFromRepresentation $ pcLoggingConfig pcfg
  pure $ SyncNodeConfig
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
          , dncByronSoftwareVersion = ncByronSotfwareVersion ncfg
          , dncByronProtocolVersion = ncByronProtocolVersion ncfg

          , dncShelleyHardFork = ncShelleyHardFork ncfg
          , dncAllegraHardFork = ncAllegraHardFork ncfg
          , dncMaryHardFork = ncMaryHardFork ncfg
          , dncAlonzoHardFork = ncAlonzoHardFork ncfg
          }

mkAdjustPath :: SyncPreConfig -> (FilePath -> FilePath)
mkAdjustPath cfg fp = takeDirectory (pcNodeConfigFilePath cfg) </> fp

readByteString :: FilePath -> Text -> IO ByteString
readByteString fp cfgType =
  catch (BS.readFile fp) $ \(_ :: IOException) ->
    panic $ mconcat [ "Cannot find the ", cfgType, " configuration file at : ", Text.pack fp ]
