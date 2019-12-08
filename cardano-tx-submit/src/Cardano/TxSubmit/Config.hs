{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.TxSubmit.Config
  ( TxSubmitNodeConfig
  , GenesisHash (..)
  , GenTxSubmitNodeConfig (..)
  , readTxSubmitNodeConfig
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

type TxSubmitNodeConfig = GenTxSubmitNodeConfig Logging.Configuration

data GenTxSubmitNodeConfig a = GenTxSubmitNodeConfig
  { tscLoggingConfig :: !a
  , tscGenesisHash :: !GenesisHash
  , tscEnableLogging :: !Bool
  , tscEnableMetrics :: !Bool
  , tscRequiresNetworkMagic :: !RequiresNetworkMagic
  }

newtype GenesisHash = GenesisHash
  { unGenesisHash :: Text
  }


readTxSubmitNodeConfig :: FilePath -> IO TxSubmitNodeConfig
readTxSubmitNodeConfig fp = do
    res <- Yaml.decodeEither' <$> readLoggingConfig
    case res of
      Left err -> panic $ "readTxSubmitNodeConfig: Error parsing config: " <> textShow err
      Right icr -> convertLogging icr
  where
    readLoggingConfig :: IO ByteString
    readLoggingConfig =
      catch (BS.readFile fp) $ \(_ :: IOException) ->
        panic $ "Cannot find the logging configuration file at : " <> Text.pack fp

convertLogging :: GenTxSubmitNodeConfig Logging.Representation -> IO TxSubmitNodeConfig
convertLogging tsc = do
  lc <- Logging.setupFromRepresentation $ tscLoggingConfig tsc
  pure $ tsc { tscLoggingConfig = lc }

-- -------------------------------------------------------------------------------------------------

instance FromJSON (GenTxSubmitNodeConfig Logging.Representation) where
  parseJSON o =
    Aeson.withObject "top-level" parseGenTxSubmitNodeConfig o

parseGenTxSubmitNodeConfig :: Object -> Parser (GenTxSubmitNodeConfig Logging.Representation)
parseGenTxSubmitNodeConfig o =
  GenTxSubmitNodeConfig
    <$> parseJSON (Object o)
    <*> fmap GenesisHash (o .: "GenesisHash")
    <*> o .: "EnableLogging"
    <*> o .: "EnableLogMetrics"
    <*> o .: "RequiresNetworkMagic"

textShow :: Show a => a -> Text
textShow = Text.pack . show
