{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.DbSync.Config
  ( ConfigFile (..)
  , DbSyncProtocol (..)
  , DbSyncNodeConfig
  , DbSyncNodeParams (..)
  , DbSyncEnv (..)
  , GenesisConfig (..)
  , GenesisFile (..)
  , GenDbSyncNodeConfig (..)
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
import qualified Cardano.BM.Data.Configuration as Logging

import           Cardano.DbSync.Config.Cardano
import           Cardano.DbSync.Config.Shelley
import           Cardano.DbSync.Config.Types
import           Cardano.DbSync.Util

import           Cardano.Prelude

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml


readDbSyncNodeConfig :: FilePath -> IO DbSyncNodeConfig
readDbSyncNodeConfig fp = do
    res <- Yaml.decodeEither' <$> readLoggingConfig
    case res of
      Left err -> panic $ "readDbSyncNodeConfig: Error parsing config: " <> textShow err
      Right icr -> convertLogging icr
  where
    readLoggingConfig :: IO ByteString
    readLoggingConfig =
      catch (BS.readFile fp) $ \(_ :: IOException) ->
        panic $ "Cannot find the logging configuration file at : " <> Text.pack fp

convertLogging :: GenDbSyncNodeConfig Logging.Representation -> IO DbSyncNodeConfig
convertLogging enp = do
  lc <- Logging.setupFromRepresentation $ encLoggingConfig enp
  pure $ enp { encLoggingConfig = lc }
