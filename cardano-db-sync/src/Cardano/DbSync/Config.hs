{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.DbSync.Config
  ( DbSyncNodeConfig
  , GenesisHash (..)
  , GenDbSyncNodeConfig (..)
  , NetworkName (..)
  , readDbSyncNodeConfig
  ) where

import qualified Cardano.BM.Configuration as Logging
import qualified Cardano.BM.Configuration.Model as Logging
import qualified Cardano.BM.Data.Configuration as Logging

import           Cardano.Crypto (RequiresNetworkMagic (..))

import           Cardano.Prelude

import           Data.Aeson (FromJSON (..), Object, Value (..), (.:))
import           Data.Aeson.Types (Parser)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml

import           Cardano.DbSync.Util

type DbSyncNodeConfig = GenDbSyncNodeConfig Logging.Configuration

data GenDbSyncNodeConfig a = GenDbSyncNodeConfig
  { encNetworkName :: !NetworkName
  , encLoggingConfig :: !a
  , encGenesisHash :: !GenesisHash
  , encEnableLogging :: !Bool
  , encEnableMetrics :: !Bool
  , encRequiresNetworkMagic :: !RequiresNetworkMagic
  }

newtype GenesisHash = GenesisHash
  { unGenesisHash :: Text
  }

newtype NetworkName = NetworkName
  { unNetworkName :: Text
  }


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

-- -------------------------------------------------------------------------------------------------

instance FromJSON (GenDbSyncNodeConfig Logging.Representation) where
  parseJSON o =
    Aeson.withObject "top-level" parseGenDbSyncNodeConfig o

parseGenDbSyncNodeConfig :: Object -> Parser (GenDbSyncNodeConfig Logging.Representation)
parseGenDbSyncNodeConfig o =
  GenDbSyncNodeConfig
    <$> fmap NetworkName (o .: "NetworkName")
    <*> parseJSON (Object o)
    <*> fmap GenesisHash (o .: "GenesisHash")
    <*> o .: "EnableLogging"
    <*> o .: "EnableLogMetrics"
    <*> o .: "RequiresNetworkMagic"
