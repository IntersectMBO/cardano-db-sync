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
  flattenNodeConfig,
) where

-- db-sync reads only a handful of keys out of the node's configuration file, so it
-- parses them here rather than depending on cardano-config. That package would pin
-- db-sync's ledger floor to whatever it requires and pull in the node's dependency
-- closure, to decode about a dozen flat fields.

import qualified Cardano.Chain.Update as Byron
import Cardano.Crypto (RequiresNetworkMagic (..))
import Cardano.DbSync.Config.Types
import Cardano.DbSync.Error (NodeConfigError (..), SyncNodeError (..))
import Cardano.Prelude
import Data.Aeson (FromJSON (..), Object, (.!=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as List
import qualified Data.Yaml as Yaml
import Ouroboros.Consensus.Cardano (CardanoHardForkTrigger (..))
import Ouroboros.Consensus.Cardano.Block (AllegraEra, AlonzoEra, BabbageEra, ConwayEra, DijkstraEra, MaryEra, ShelleyEra, StandardCrypto)
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
  , ncConwayGenesisFile :: !GenesisFile
  , ncConwayGenesisHash :: !(Maybe GenesisHashConway)
  , ncDijkstraGenesisFile :: !(Maybe GenesisFile)
  , ncDijkstraGenesisHash :: !(Maybe GenesisHashDijkstra)
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
  , -- Dijkstra hardfok parameters
    ncDijkstraHardFork :: !(CardanoHardForkTrigger (ShelleyBlock (Praos StandardCrypto) DijkstraEra))
  }

parseNodeConfig :: ByteString -> IO NodeConfig
parseNodeConfig bs =
  case Yaml.decodeEither' bs of
    Left err -> throwIO $ SNErrNodeConfig $ NodeConfigParseError (show err)
    Right value ->
      let flattened = flattenNodeConfig value
       in case missingRequiredKeys flattened of
            missing@(_ : _) ->
              throwIO . SNErrNodeConfig . NodeConfigParseError $
                mconcat
                  [ "the "
                  , nodeConfigFormat value
                  , " node configuration is missing required "
                  , if length missing == 1 then "key " else "keys "
                  , List.intercalate ", " (map Key.toString missing)
                  ]
            [] ->
              case Aeson.fromJSON flattened of
                Aeson.Error err -> throwIO $ SNErrNodeConfig $ NodeConfigParseError err
                Aeson.Success nc -> pure nc

-- | The node's v2 configuration format nests settings under @Configuration@ in named
-- sections. db-sync reads keys only from @Protocol@ and @Testing@, and their names are
-- unchanged from the flat v1 format, so flattening those two sections back to the top
-- level recovers the shape 'FromJSON NodeConfig' already expects. A v1 config has no
-- @Configuration@ key and is returned untouched.
--
-- @Network@ and @HermodTracing@ are deliberately dropped: they configure the node, not
-- db-sync, and ignoring them keeps this parser's surface small.
flattenNodeConfig :: Aeson.Value -> Aeson.Value
flattenNodeConfig value =
  case value of
    Aeson.Object o
      | Just (Aeson.Object cfg) <- KeyMap.lookup "Configuration" o ->
          -- '<>' on KeyMap is left-biased. The two sections share no key name today,
          -- and Protocol wins if that ever changes.
          Aeson.Object (sectionOf "Protocol" cfg <> sectionOf "Testing" cfg)
    _otherwise -> value
  where
    sectionOf name cfg =
      case KeyMap.lookup name cfg of
        Just (Aeson.Object section) -> section
        _otherwise -> KeyMap.empty

-- | Keys db-sync cannot run without. They are checked before decoding so that a format
-- change moving one of them out of the sections 'flattenNodeConfig' knows about fails
-- here, naming the key and the layout, rather than surfacing as a bare aeson
-- \"key not found\" or being silently defaulted.
requiredNodeConfigKeys :: [Key.Key]
requiredNodeConfigKeys =
  [ "ByronGenesisFile"
  , "ByronGenesisHash"
  , "ShelleyGenesisFile"
  , "AlonzoGenesisFile"
  , "ConwayGenesisFile"
  , "RequiresNetworkMagic"
  ]

missingRequiredKeys :: Aeson.Value -> [Key.Key]
missingRequiredKeys value =
  case value of
    Aeson.Object o -> filter (\k -> not (KeyMap.member k o)) requiredNodeConfigKeys
    _otherwise -> requiredNodeConfigKeys

-- | Which of the node's two layouts a document uses, to make the error above easier to
-- act on.
nodeConfigFormat :: Aeson.Value -> [Char]
nodeConfigFormat value =
  case value of
    Aeson.Object o | KeyMap.member "Configuration" o -> "nested (v2)"
    _otherwise -> "flat (v1)"

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
          <$> (o .:? "Protocol" .!= SyncProtocolCardano)
          <*> (o .:? "PBftSignatureThreshold")
          <*> (o .: "ByronGenesisFile")
          <*> (o .: "ByronGenesisHash")
          <*> (o .: "ShelleyGenesisFile")
          <*> (o .: "ShelleyGenesisHash")
          <*> (o .: "AlonzoGenesisFile")
          <*> (o .: "AlonzoGenesisHash")
          <*> (o .: "ConwayGenesisFile")
          <*> (o .:? "ConwayGenesisHash")
          <*> (o .:? "DijkstraGenesisFile")
          <*> (o .:? "DijkstraGenesisHash")
          <*> (o .: "RequiresNetworkMagic")
          <*> parseByronProtocolVersion o
          <*> parseShelleyHardForkEpoch o
          <*> parseAllegraHardForkEpoch o
          <*> parseMaryHardForkEpoch o
          <*> parseAlonzoHardForkEpoch o
          <*> parseBabbageHardForkEpoch o
          <*> parseConwayHardForkEpoch o
          <*> parseDijkstraHardForkEpoch o

      -- The v2 format dropped these three keys. Nothing in db-sync depends on the
      -- value: it is forwarded to Consensus.byronProtocolVersion, two lines above a
      -- hardcoded cardanoProtocolVersion. A config that omits them parses as zeroes
      -- rather than being rejected.
      parseByronProtocolVersion :: Object -> Parser Byron.ProtocolVersion
      parseByronProtocolVersion o =
        Byron.ProtocolVersion
          <$> (o .:? "LastKnownBlockVersion-Major" .!= 0)
          <*> (o .:? "LastKnownBlockVersion-Minor" .!= 0)
          <*> (o .:? "LastKnownBlockVersion-Alt" .!= 0)

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

      parseDijkstraHardForkEpoch :: Object -> Parser (CardanoHardForkTrigger (ShelleyBlock (Praos StandardCrypto) DijkstraEra))
      parseDijkstraHardForkEpoch o =
        asum
          [ CardanoTriggerHardForkAtEpoch <$> o .: "TestDijkstraHardForkAtEpoch"
          , pure CardanoTriggerHardForkAtDefaultVersion
          ]
