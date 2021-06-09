{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Sync.Config.Node
  ( NodeConfig (..)
  , parseNodeConfig
  ) where

-- Node really should export something like this, but doesn't and it actually seemed to
-- be easier and faster to just parse out the bits we need here.

import qualified Cardano.Chain.Update as Byron

import           Cardano.Crypto (RequiresNetworkMagic (..))

import           Cardano.Sync.Config.Types
import           Cardano.Sync.Util

import           Cardano.Prelude

import           Data.Aeson (FromJSON (..), Object, (.:), (.:?))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.Yaml as Yaml

import qualified Ouroboros.Consensus.Cardano.CanHardFork as Shelley

data NodeConfig = NodeConfig
  { ncProtocol :: !SyncProtocol
  , ncPBftSignatureThreshold :: !(Maybe Double)
  , ncByronGenesisFile :: !GenesisFile
  , ncByronGenesisHash :: !GenesisHashByron
  , ncShelleyGenesisFile :: !GenesisFile
  , ncShelleyGenesisHash :: !GenesisHashShelley
  , ncAlonzoGenesisFile :: !GenesisFile
  , ncAlonzoGenesisHash :: !GenesisHashAlonzo
  , ncRequiresNetworkMagic :: !RequiresNetworkMagic
  , ncByronSotfwareVersion :: !Byron.SoftwareVersion
  , ncByronProtocolVersion :: !Byron.ProtocolVersion

  -- Shelley hardfok parameters
  , ncShelleyHardFork :: !Shelley.TriggerHardFork

  -- Allegra hardfok parameters
  , ncAllegraHardFork :: !Shelley.TriggerHardFork

  -- Mary hardfok parameters
  , ncMaryHardFork :: !Shelley.TriggerHardFork

  -- Alonzo hardfok parameters
  , ncAlonzoHardFork :: !Shelley.TriggerHardFork
  }

parseNodeConfig :: ByteString -> NodeConfig
parseNodeConfig bs =
  case Yaml.decodeEither' bs of
    Left err -> panic $ "Error parsing node config: " <> textShow err
    Right nc -> nc

-- -------------------------------------------------------------------------------------------------

instance FromJSON NodeConfig where
  parseJSON v =
      Aeson.withObject "NodeConfig" parse v
    where
      parse :: Object -> Parser NodeConfig
      parse o =
        NodeConfig
          <$> o .: "Protocol"
          <*> o .:? "PBftSignatureThreshold"
          <*> fmap GenesisFile (o .: "ByronGenesisFile")
          <*> fmap GenesisHashByron (o .: "ByronGenesisHash")
          <*> fmap GenesisFile (o .: "ShelleyGenesisFile")
          <*> fmap GenesisHashShelley (o .: "ShelleyGenesisHash")
          <*> fmap GenesisFile (o .: "AlonzoGenesisFile")
          <*> fmap GenesisHashAlonzo (o .: "AlonzoGenesisHash")
          <*> o .: "RequiresNetworkMagic"
          <*> parseByronSoftwareVersion o
          <*> parseByronProtocolVersion o

          <*> parseShelleyHardForkEpoch o

          <*> parseAllegraHardForkEpoch o

          <*> parseMaryHardForkEpoch o

          <*> parseAlonzoHardForkEpoch o

      parseByronProtocolVersion :: Object -> Parser Byron.ProtocolVersion
      parseByronProtocolVersion o =
        Byron.ProtocolVersion
          <$> o .: "LastKnownBlockVersion-Major"
          <*> o .: "LastKnownBlockVersion-Minor"
          <*> o .: "LastKnownBlockVersion-Alt"

      parseByronSoftwareVersion :: Object -> Parser Byron.SoftwareVersion
      parseByronSoftwareVersion o =
        Byron.SoftwareVersion
          <$> fmap Byron.ApplicationName (o .: "ApplicationName")
          <*> o .: "ApplicationVersion"

      parseShelleyHardForkEpoch :: Object -> Parser Shelley.TriggerHardFork
      parseShelleyHardForkEpoch o =
        asum
          [ Shelley.TriggerHardForkAtEpoch <$> o .: "TestShelleyHardForkAtEpoch"
          , pure $ Shelley.TriggerHardForkAtVersion 2 -- Mainnet default
          ]

      parseAllegraHardForkEpoch :: Object -> Parser Shelley.TriggerHardFork
      parseAllegraHardForkEpoch o =
        asum
          [ Shelley.TriggerHardForkAtEpoch <$> o .: "TestAllegraHardForkAtEpoch"
          , pure $ Shelley.TriggerHardForkAtVersion 3 -- Mainnet default
          ]

      parseMaryHardForkEpoch :: Object -> Parser Shelley.TriggerHardFork
      parseMaryHardForkEpoch o =
        asum
          [ Shelley.TriggerHardForkAtEpoch <$> o .: "TestMaryHardForkAtEpoch"
          , pure $ Shelley.TriggerHardForkAtVersion 4 -- Mainnet default
          ]

      parseAlonzoHardForkEpoch :: Object -> Parser Shelley.TriggerHardFork
      parseAlonzoHardForkEpoch o =
        asum
          [ Shelley.TriggerHardForkAtEpoch <$> o .: "TestAlonzoHardForkAtEpoch"
          , pure $ Shelley.TriggerHardForkAtVersion 5 -- Mainnet default
          ]
