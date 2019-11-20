{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Explorer.Node.Config
  ( ExplorerNodeConfig
  , GenExplorerNodeConfig (..)
  , readExplorerNodeConfig
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

type ExplorerNodeConfig = GenExplorerNodeConfig Logging.Configuration

data GenExplorerNodeConfig a = GenExplorerNodeConfig
  { encLoggingConfig :: a
  , encGenesisHash :: !Text
  , encEnableLogging :: !Bool
  , encEnableMetrics :: !Bool
  , encRequiresNetworkMagic :: !RequiresNetworkMagic
  }

readExplorerNodeConfig :: FilePath -> IO ExplorerNodeConfig
readExplorerNodeConfig fp = do
    res <- Yaml.decodeEither' <$> readLoggingConfig
    case res of
      Left _ -> panic "readExplorerNodeConfig: Error parsing config"
      Right icr -> convertLogging icr
  where
    readLoggingConfig :: IO ByteString
    readLoggingConfig =
      catch (BS.readFile fp) $ \(_ :: IOException) ->
        panic $ "Cannot find the logging configuration file at : " <> Text.pack fp

convertLogging :: GenExplorerNodeConfig Logging.Representation -> IO ExplorerNodeConfig
convertLogging enp = do
  lc <- Logging.setupFromRepresentation $ encLoggingConfig enp
  pure $ enp { encLoggingConfig = lc }

-- -------------------------------------------------------------------------------------------------

instance FromJSON (GenExplorerNodeConfig Logging.Representation) where
  parseJSON o =
    Aeson.withObject "top-level" parseGenExplorerNodeConfig o

parseGenExplorerNodeConfig :: Object -> Parser (GenExplorerNodeConfig Logging.Representation)
parseGenExplorerNodeConfig o =
  GenExplorerNodeConfig
    <$> parseJSON (Object o)
    <*> o .: "GenesisHash"
    <*> o .: "EnableLogging"
    <*> o .: "EnableLogMetrics"
    <*> o .: "RequiresNetworkMagic"
