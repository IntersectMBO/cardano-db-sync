{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.DbSync.Config.NodeTest (tests) where

import qualified Cardano.Chain.Update as Byron
import Cardano.DbSync.Config.Node
import Cardano.DbSync.Config.Types
import Cardano.DbSync.Error (SyncNodeError)
import Cardano.Ledger.BaseTypes (EpochNo (..))
import Cardano.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Hedgehog
import Ouroboros.Consensus.Cardano (CardanoHardForkTrigger (..))
import Prelude ()

tests :: IO Bool
tests =
  checkParallel $
    Group
      "Cardano.DbSync.Config.Node"
      [ ("the flat (v1) node configuration parses", prop_parse_v1)
      , ("the nested (v2) node configuration parses", prop_parse_v2)
      , ("both layouts of the same configuration are read alike", prop_layouts_agree)
      , ("ByronProtocolVersion is the only setting the layouts differ on", prop_layouts_differ_only_in_protocol_version)
      , ("flattening leaves a v1 configuration untouched", prop_flatten_keeps_v1)
      , ("flattening drops the sections that configure the node", prop_flatten_drops_node_sections)
      , ("PBftSignatureThreshold is still read when present", prop_parse_pbft_threshold)
      , ("a missing required key names the key and the layout", prop_parse_missing_key)
      ]

-- | The node ships one configuration in each of its two dialects, for its own
-- @cardano-config-compare@ test: @node-config-v2.json@ is @node-config-v1.json@ put
-- through @cardano-node config migrate@. Both are copied verbatim, so these properties
-- run against the real documents rather than against our model of them.
v1Path, v2Path :: FilePath
v1Path = "test/testfiles/node-config-v1.json"
v2Path = "test/testfiles/node-config-v2.json"

valueOf :: FilePath -> PropertyT IO Aeson.Value
valueOf fp = do
  bs <- evalIO (BS.readFile fp)
  evalEither (Aeson.eitherDecodeStrict bs)

parseValue :: Aeson.Value -> PropertyT IO NodeConfig
parseValue value = do
  result <- evalIO (try (parseNodeConfig (LBS.toStrict (Aeson.encode value))))
  case result of
    Left (err :: SyncNodeError) -> annotateShow err >> failure
    Right nc -> pure nc

parseFile :: FilePath -> PropertyT IO NodeConfig
parseFile fp = valueOf fp >>= parseValue

-- | 'CardanoHardForkTrigger' has no 'Eq', and its 'Show' output is not something to
-- pin a test to.
triggerEpoch :: CardanoHardForkTrigger blk -> Maybe Word64
triggerEpoch trigger =
  case trigger of
    CardanoTriggerHardForkAtDefaultVersion -> Nothing
    CardanoTriggerHardForkAtEpoch epochNo -> Just (unEpochNo epochNo)

-- | Every setting db-sync reads out of the node configuration, rendered so that two
-- 'NodeConfig' values can be compared without an 'Eq' instance.
settings :: NodeConfig -> [(Text, Text)]
settings nc =
  [ ("Protocol", render (ncProtocol nc))
  , ("PBftSignatureThreshold", render (ncPBftSignatureThreshold nc))
  , ("ByronGenesisFile", render (ncByronGenesisFile nc))
  , ("ByronGenesisHash", render (ncByronGenesisHash nc))
  , ("ShelleyGenesisFile", render (ncShelleyGenesisFile nc))
  , ("ShelleyGenesisHash", render (ncShelleyGenesisHash nc))
  , ("AlonzoGenesisFile", render (ncAlonzoGenesisFile nc))
  , ("AlonzoGenesisHash", render (ncAlonzoGenesisHash nc))
  , ("ConwayGenesisFile", render (ncConwayGenesisFile nc))
  , ("ConwayGenesisHash", render (ncConwayGenesisHash nc))
  , ("RequiresNetworkMagic", render (ncRequiresNetworkMagic nc))
  , ("ShelleyHardFork", render (triggerEpoch (ncShelleyHardFork nc)))
  , ("AllegraHardFork", render (triggerEpoch (ncAllegraHardFork nc)))
  , ("MaryHardFork", render (triggerEpoch (ncMaryHardFork nc)))
  , ("AlonzoHardFork", render (triggerEpoch (ncAlonzoHardFork nc)))
  , ("BabbageHardFork", render (triggerEpoch (ncBabbageHardFork nc)))
  , ("ConwayHardFork", render (triggerEpoch (ncConwayHardFork nc)))
  ]
  where
    render :: Show a => a -> Text
    render = Text.pack . show

genesisFiles :: NodeConfig -> [FilePath]
genesisFiles nc =
  map
    unGenesisFile
    [ ncByronGenesisFile nc
    , ncShelleyGenesisFile nc
    , ncAlonzoGenesisFile nc
    , ncConwayGenesisFile nc
    ]

expectedGenesisFiles :: [FilePath]
expectedGenesisFiles =
  [ "byron-genesis.json"
  , "shelley-genesis.json"
  , "alonzo-genesis.json"
  , "conway-genesis.json"
  ]

prop_parse_v1 :: Property
prop_parse_v1 = withTests 1 $ property $ do
  cfg <- parseFile v1Path
  genesisFiles cfg === expectedGenesisFiles
  -- the fixture forks to every era at epoch 0, except Conway, which it leaves to the
  -- protocol version
  triggerEpoch (ncShelleyHardFork cfg) === Just 0
  triggerEpoch (ncAllegraHardFork cfg) === Just 0
  triggerEpoch (ncMaryHardFork cfg) === Just 0
  triggerEpoch (ncAlonzoHardFork cfg) === Just 0
  triggerEpoch (ncBabbageHardFork cfg) === Just 0
  triggerEpoch (ncConwayHardFork cfg) === Nothing

-- | The v2 layout keeps the keys db-sync reads under @Configuration.Protocol@ and
-- @Configuration.Testing@, and drops @Protocol@ and @LastKnownBlockVersion-*@
-- altogether. None of that may stop db-sync from reading it.
prop_parse_v2 :: Property
prop_parse_v2 = withTests 1 $ property $ do
  cfg <- parseFile v2Path
  genesisFiles cfg === expectedGenesisFiles
  triggerEpoch (ncShelleyHardFork cfg) === Just 0
  triggerEpoch (ncConwayHardFork cfg) === Nothing

prop_layouts_agree :: Property
prop_layouts_agree = withTests 1 $ property $ do
  v1 <- parseFile v1Path
  v2 <- parseFile v2Path
  settings v2 === settings v1

-- | The one setting the two layouts do not agree on. v2 has no
-- @LastKnownBlockVersion-*@, so it parses as zeroes. db-sync forwards the value to
-- @Consensus.byronProtocolVersion@, which only a Byron block producer announces, and
-- db-sync passes @byronLeaderCredentials = Nothing@. Pinned here so a future use for
-- the value is not quietly fed zeroes.
prop_layouts_differ_only_in_protocol_version :: Property
prop_layouts_differ_only_in_protocol_version = withTests 1 $ property $ do
  v1 <- parseFile v1Path
  v2 <- parseFile v2Path
  ncByronProtocolVersion v1 === Byron.ProtocolVersion 3 0 0
  ncByronProtocolVersion v2 === Byron.ProtocolVersion 0 0 0

prop_flatten_keeps_v1 :: Property
prop_flatten_keeps_v1 = withTests 1 $ property $ do
  value <- valueOf v1Path
  flattenNodeConfig value === value

prop_flatten_drops_node_sections :: Property
prop_flatten_drops_node_sections = withTests 1 $ property $ do
  value <- valueOf v2Path
  let flattened = decodeUtf8 (LBS.toStrict (Aeson.encode (flattenNodeConfig value)))
  annotate (Text.unpack flattened)
  -- Network and HermodTracing configure the node, not db-sync
  Text.isInfixOf "DeadlineTargetNumberOfActivePeers" flattened === False
  Text.isInfixOf "TraceOptionNodeName" flattened === False
  -- while the keys db-sync reads have been lifted to the top level
  Text.isInfixOf "ByronGenesisFile" flattened === True
  Text.isInfixOf "TestShelleyHardForkAtEpoch" flattened === True

-- | Byron-only, optional in both layouts and absent from both fixtures, so it is worth
-- a check of its own.
prop_parse_pbft_threshold :: Property
prop_parse_pbft_threshold = withTests 1 $ property $ do
  value <- valueOf v1Path
  cfg <- parseValue (insertKey "PBftSignatureThreshold" (Aeson.Number 1.1) value)
  ncPBftSignatureThreshold cfg === Just 1.1
  where
    insertKey key val value =
      case value of
        Aeson.Object o -> Aeson.Object (KeyMap.insert key val o)
        _otherwise -> value

-- | A layout change that moves a required key out of the two sections
-- 'flattenNodeConfig' knows about has to fail loudly, naming the key and the layout.
prop_parse_missing_key :: Property
prop_parse_missing_key = withTests 1 $ property $ do
  value <- valueOf v2Path
  result <- evalIO (try (parseNodeConfig (LBS.toStrict (Aeson.encode (withoutByronGenesisFile value)))))
  case result of
    Right (_ :: NodeConfig) -> annotate "expected a parse failure" >> failure
    Left (err :: SyncNodeError) -> do
      let msg = Text.pack (show err)
      annotate (Text.unpack msg)
      Text.isInfixOf "ByronGenesisFile" msg === True
      Text.isInfixOf "nested (v2)" msg === True
  where
    withoutByronGenesisFile value =
      case value of
        Aeson.Object o
          | Just (Aeson.Object cfg) <- KeyMap.lookup "Configuration" o
          , Just (Aeson.Object protocol) <- KeyMap.lookup "Protocol" cfg ->
              let protocol' = Aeson.Object (KeyMap.delete "ByronGenesisFile" protocol)
                  cfg' = Aeson.Object (KeyMap.insert "Protocol" protocol' cfg)
               in Aeson.Object (KeyMap.insert "Configuration" cfg' o)
        _otherwise -> value
