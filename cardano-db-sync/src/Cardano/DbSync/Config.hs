{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.DbSync.Config
  ( ConfigFile (..)
  , DbSyncCommand (..)
  , DbSyncProtocol (..)
  , DbSyncNodeConfig (..)
  , DbSyncNodeParams (..)
  , DbSyncEnv (..)
  , GenesisConfig (..)
  , GenesisFile (..)
  , LedgerStateDir (..)
  , NetworkName (..)
  , ShelleyConfig (..)
  , SocketPath (..)
  , cardanoLedgerConfig
  , genesisConfigToEnv
  , genesisProtocolMagicId
  , readDbSyncNodeConfig
  , readCardanoGenesisConfig
  ) where

import qualified Cardano.BM.Configuration.Model as Logging

import           Cardano.DbSync.Config.Cardano
import           Cardano.DbSync.Config.Node
import           Cardano.DbSync.Config.Shelley
import           Cardano.DbSync.Config.Types
import           Cardano.DbSync.Util

import           Cardano.Prelude

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml

import           System.FilePath (takeDirectory, (</>))


readDbSyncNodeConfig :: ConfigFile -> IO DbSyncNodeConfig
readDbSyncNodeConfig (ConfigFile fp) = do
    pcfg <- adjustNodeFilePath . parseDbSyncPreConfig <$> readByteString fp "DbSync"
    ncfg <- parseNodeConfig <$> readByteString (pcNodeConfigFilePath pcfg) "node"
    coalesceConfig pcfg ncfg (mkAdjustPath pcfg)
  where
    parseDbSyncPreConfig :: ByteString -> DbSyncPreConfig
    parseDbSyncPreConfig bs =
      case Yaml.decodeEither' bs of
      Left err -> panic $ "readDbSyncNodeConfig: Error parsing config: " <> textShow err
      Right res -> res

    adjustNodeFilePath :: DbSyncPreConfig -> DbSyncPreConfig
    adjustNodeFilePath cfg =
      cfg { pcNodeConfigFile = adjustNodeConfigFilePath (takeDirectory fp </>) (pcNodeConfigFile cfg) }

coalesceConfig
    :: DbSyncPreConfig -> NodeConfig -> (FilePath -> FilePath)
    -> IO DbSyncNodeConfig
coalesceConfig pcfg ncfg adjustGenesisPath = do
  lc <- Logging.setupFromRepresentation $ pcLoggingConfig pcfg
  pure $ DbSyncNodeConfig
          { dncNetworkName = pcNetworkName pcfg
          , dncLoggingConfig = lc
          , dncNodeConfigFile = pcNodeConfigFile pcfg
          , dncProtocol = ncProtocol ncfg
          , dncRequiresNetworkMagic = ncRequiresNetworkMagic ncfg
          , dncEnableLogging = pcEnableLogging pcfg
          , dncEnableMetrics = pcEnableMetrics pcfg
          , dncByronGenesisFile = adjustGenesisFilePath adjustGenesisPath (ncByronGenesisFile ncfg)
          , dncByronGenesisHash = ncByronGenesisHash ncfg
          , dncShelleyGenesisFile = adjustGenesisFilePath adjustGenesisPath (ncShelleyGenesisFile ncfg)
          , dncShelleyGenesisHash = ncShelleyGenesisHash ncfg
          , dncByronSoftwareVersion = ncByronSotfwareVersion ncfg
          , dncByronProtocolVersion = ncByronProtocolVersion ncfg
          , dncShelleyHardFork = ncShelleyHardFork ncfg
          , dncShelleyHardForkNotBeforeEpoch = ncShelleyHardForkAfterEpoch ncfg
          , dncShelleyMaxProtocolVersion = ncShelleyMaxProtocolVersion ncfg
          }

mkAdjustPath :: DbSyncPreConfig -> (FilePath -> FilePath)
mkAdjustPath cfg fp = takeDirectory (pcNodeConfigFilePath cfg) </> fp

readByteString :: FilePath -> Text -> IO ByteString
readByteString fp cfgType =
  catch (BS.readFile fp) $ \(_ :: IOException) ->
    panic $ mconcat [ "Cannot find the ", cfgType, " configuration file at : ", Text.pack fp ]
