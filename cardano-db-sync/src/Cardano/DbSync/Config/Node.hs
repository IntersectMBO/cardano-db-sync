{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Config.Node (
  NodeConfig (..),
  parseNodeConfig,
  parseSyncPreConfig,
  readByteStringFromFile,
) where

-- Node really should export something like this, but doesn't and it actually seemed to
-- be easier and faster to just parse out the bits we need here.

import qualified Cardano.Chain.Update as Byron
import Cardano.Crypto (RequiresNetworkMagic (..))
import Cardano.DbSync.Config.Types
import Cardano.DbSync.Error (NodeConfigError (..), SyncNodeError (..))
import Cardano.Prelude
import Data.Aeson (FromJSON (..), Object, (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Yaml
import Ouroboros.Consensus.Cardano (CardanoHardForkTrigger (..))
import Ouroboros.Consensus.Cardano.Block (AllegraEra, AlonzoEra, BabbageEra, ConwayEra, MaryEra, ShelleyEra, StandardCrypto)
import Ouroboros.Consensus.Protocol.Praos (Praos)
import Ouroboros.Consensus.Protocol.TPraos (TPraos)
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)

data NodeConfig = NodeConfig
  { ncProtocol :: !SyncProtocol
  , ncPBftSignatureThreshold :: !(Maybe Double)
  , ncByronGenesisFile :: !GenesisFile
  , ncByronGenesisHash :: !GenesisHashByron
  , ncShelleyGenesisFile :: !GenesisFile
  , ncShelleyGenesisHash :: !GenesisHashShelley
  , ncAlonzoGenesisFile :: !GenesisFile
  , ncAlonzoGenesisHash :: !GenesisHashAlonzo
  , ncConwayGenesisFile :: !(Maybe GenesisFile)
  , ncConwayGenesisHash :: !(Maybe GenesisHashConway)
  , ncRequiresNetworkMagic :: !RequiresNetworkMagic
  , ncByronProtocolVersion :: !Byron.ProtocolVersion
  , -- Shelley hardfok parameters
    ncShelleyHardFork :: !(CardanoHardForkTrigger (ShelleyBlock (TPraos StandardCrypto) ShelleyEra))
  , -- Allegra hardfok parameters
    ncAllegraHardFork :: !(CardanoHardForkTrigger (ShelleyBlock (TPraos StandardCrypto) AllegraEra))
  , -- Mary hardfok parameters
    ncMaryHardFork :: !(CardanoHardForkTrigger (ShelleyBlock (TPraos StandardCrypto) MaryEra))
  , -- Alonzo hardfok parameters
    ncAlonzoHardFork :: !(CardanoHardForkTrigger (ShelleyBlock (TPraos StandardCrypto) AlonzoEra))
  , -- Babbage hardfok parameters
    ncBabbageHardFork :: !(CardanoHardForkTrigger (ShelleyBlock (Praos StandardCrypto) BabbageEra))
  , -- Conway hardfok parameters
    ncConwayHardFork :: !(CardanoHardForkTrigger (ShelleyBlock (Praos StandardCrypto) ConwayEra))
  }

parseNodeConfig :: ByteString -> IO NodeConfig
parseNodeConfig bs =
  case Yaml.decodeEither' bs of
    Left err -> throwIO $ SNErrNodeConfig $ NodeConfigParseError (show err)
    Right nc -> pure nc

parseSyncPreConfig :: ByteString -> IO SyncPreConfig
parseSyncPreConfig bs =
  case Yaml.decodeEither' bs of
    Left err -> throwIO $ SNErrNodeConfig $ ParseSyncPreConfigError ("Error parsing config: " <> show err)
    Right res -> pure res

readByteStringFromFile :: FilePath -> Text -> IO ByteString
readByteStringFromFile fp cfgType =
  catch (BS.readFile fp) $ \(_ :: IOException) ->
    throwIO $ SNErrNodeConfig $ NodeConfigParseError ("Cannot find the " <> show cfgType <> " configuration file at : " <> show fp)

-- -------------------------------------------------------------------------------------------------

instance FromJSON NodeConfig where
  parseJSON =
    Aeson.withObject "NodeConfig" parse
    where
      parse :: Object -> Parser NodeConfig
      parse o =
        NodeConfig
          <$> (o .: "Protocol")
          <*> (o .:? "PBftSignatureThreshold")
          <*> (o .: "ByronGenesisFile")
          <*> (o .: "ByronGenesisHash")
          <*> (o .: "ShelleyGenesisFile")
          <*> (o .: "ShelleyGenesisHash")
          <*> (o .: "AlonzoGenesisFile")
          <*> (o .: "AlonzoGenesisHash")
          <*> (o .:? "ConwayGenesisFile")
          <*> (o .:? "ConwayGenesisHash")
          <*> (o .: "RequiresNetworkMagic")
          <*> parseByronProtocolVersion o
          <*> parseShelleyHardForkEpoch o
          <*> parseAllegraHardForkEpoch o
          <*> parseMaryHardForkEpoch o
          <*> parseAlonzoHardForkEpoch o
          <*> parseBabbageHardForkEpoch o
          <*> parseConwayHardForkEpoch o

      parseByronProtocolVersion :: Object -> Parser Byron.ProtocolVersion
      parseByronProtocolVersion o =
        Byron.ProtocolVersion
          <$> (o .: "LastKnownBlockVersion-Major")
          <*> (o .: "LastKnownBlockVersion-Minor")
          <*> (o .: "LastKnownBlockVersion-Alt")

      parseShelleyHardForkEpoch :: Object -> Parser (CardanoHardForkTrigger (ShelleyBlock (TPraos StandardCrypto) ShelleyEra))
      parseShelleyHardForkEpoch o =
        asum
          [ CardanoTriggerHardForkAtEpoch <$> o .: "TestShelleyHardForkAtEpoch"
          , pure CardanoTriggerHardForkAtDefaultVersion
          ]

      parseAllegraHardForkEpoch :: Object -> Parser (CardanoHardForkTrigger (ShelleyBlock (TPraos StandardCrypto) AllegraEra))
      parseAllegraHardForkEpoch o =
        asum
          [ CardanoTriggerHardForkAtEpoch <$> o .: "TestAllegraHardForkAtEpoch"
          , pure CardanoTriggerHardForkAtDefaultVersion
          ]

      parseMaryHardForkEpoch :: Object -> Parser (CardanoHardForkTrigger (ShelleyBlock (TPraos StandardCrypto) MaryEra))
      parseMaryHardForkEpoch o =
        asum
          [ CardanoTriggerHardForkAtEpoch <$> o .: "TestMaryHardForkAtEpoch"
          , pure CardanoTriggerHardForkAtDefaultVersion
          ]

      parseAlonzoHardForkEpoch :: Object -> Parser (CardanoHardForkTrigger (ShelleyBlock (TPraos StandardCrypto) AlonzoEra))
      parseAlonzoHardForkEpoch o =
        asum
          [ CardanoTriggerHardForkAtEpoch <$> o .: "TestAlonzoHardForkAtEpoch"
          , pure CardanoTriggerHardForkAtDefaultVersion
          ]

      parseBabbageHardForkEpoch :: Object -> Parser (CardanoHardForkTrigger (ShelleyBlock (Praos StandardCrypto) BabbageEra))
      parseBabbageHardForkEpoch o =
        asum
          [ CardanoTriggerHardForkAtEpoch <$> o .: "TestBabbageHardForkAtEpoch"
          , pure CardanoTriggerHardForkAtDefaultVersion
          ]

      parseConwayHardForkEpoch :: Object -> Parser (CardanoHardForkTrigger (ShelleyBlock (Praos StandardCrypto) ConwayEra))
      parseConwayHardForkEpoch o =
        asum
          [ CardanoTriggerHardForkAtEpoch <$> o .: "TestConwayHardForkAtEpoch"
          , pure CardanoTriggerHardForkAtDefaultVersion
          ]
