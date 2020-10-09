{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.DbSync.Config.Node
  ( NodeConfig (..)
  , parseNodeConfig
  ) where

-- Node really should export something like this, but doesn't and it actually seemed to
-- be easier and faster to just parse out the bits we need here.

import qualified Cardano.Chain.Update as Byron

import           Cardano.Crypto (RequiresNetworkMagic (..))

import           Cardano.DbSync.Config.Types
import           Cardano.DbSync.Util

import           Cardano.Prelude

import           Cardano.Slotting.Slot (EpochNo (..))

import           Data.Aeson (FromJSON (..), Object, (.:), (.:?))
import           Data.Aeson.Types (Parser)
import qualified Data.Aeson as Aeson
import           Data.Foldable (asum)
import qualified Data.Yaml as Yaml

import qualified Ouroboros.Consensus.Cardano.CanHardFork as Shelley

data NodeConfig = NodeConfig
  { ncProtocol :: !DbSyncProtocol
  , ncByronGenesisFile :: !GenesisFile
  , ncByronGenesisHash :: !GenesisHashByron
  , ncShelleyGenesisFile :: !GenesisFile
  , ncShelleyGenesisHash :: !GenesisHashShelley
  , ncRequiresNetworkMagic :: !RequiresNetworkMagic
  , ncByronSotfwareVersion :: !Byron.SoftwareVersion
  , ncByronProtocolVersion :: !Byron.ProtocolVersion
  , ncShelleyHardFork :: !Shelley.TriggerHardFork
  , ncShelleyHardForkAfterEpoch :: !(Maybe EpochNo)
  , ncShelleyMaxProtocolVersion :: !Natural
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
          <*> fmap GenesisFile (o .: "ByronGenesisFile")
          <*> fmap GenesisHashByron (o .: "ByronGenesisHash")
          <*> fmap GenesisFile (o .: "ShelleyGenesisFile")
          <*> fmap GenesisHashShelley (o .: "ShelleyGenesisHash")
          <*> o .: "RequiresNetworkMagic"
          <*> parseByronSoftwareVersion o
          <*> parseByronProtocolVersion o
          <*> parseShelleyHardFork o
          <*> o .:? "ShelleyHardForkNotBeforeEpoch"
          <*> fmap (fromMaybe 1) (o .:? "MaxKnownMajorProtocolVersion")

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

      parseShelleyHardFork :: Object -> Parser Shelley.TriggerHardFork
      parseShelleyHardFork o =
        asum -- choice
          [ Shelley.TriggerHardForkAtEpoch <$> o .: "TestShelleyHardForkAtEpoch"
          , pure $ Shelley.TriggerHardForkAtVersion 2 -- Mainnet default
          ]
