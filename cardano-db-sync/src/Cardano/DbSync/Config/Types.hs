{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Config.Types (
  ConfigFile (..),
  SyncCommand (..),
  SyncNodeParams (..),
  SyncProtocol (..),
  GenesisFile (..),
  GenesisHashShelley (..),
  GenesisHashByron (..),
  GenesisHashAlonzo (..),
  GenesisHashConway (..),
  SyncNodeConfig (..),
  SyncPreConfig (..),
  SyncInsertConfig (..),
  SyncInsertOptions (..),
  TxOutConfig (..),
  ForceTxIn (..),
  LedgerInsertConfig (..),
  ShelleyInsertConfig (..),
  RewardsConfig (..),
  MultiAssetConfig (..),
  MetadataConfig (..),
  PlutusConfig (..),
  GovernanceConfig (..),
  OffchainPoolDataConfig (..),
  JsonTypeConfig (..),
  LedgerStateDir (..),
  LogFileDir (..),
  NetworkName (..),
  NodeConfigFile (..),
  SocketPath (..),
  adjustGenesisFilePath,
  adjustNodeConfigFilePath,
  pcNodeConfigFilePath,
  isTxOutEnabled,
  hasLedger,
  shouldUseLedger,
  isShelleyEnabled,
  isMultiAssetEnabled,
  isMetadataEnabled,
  isPlutusEnabled,
  isTxOutBootstrap,
  isTxOutConsumed,
  isTxOutPrune,
  forceTxIn,
) where

import qualified Cardano.BM.Configuration as Logging
import qualified Cardano.BM.Data.Configuration as Logging
import qualified Cardano.Chain.Update as Byron
import Cardano.Crypto (RequiresNetworkMagic (..))
import qualified Cardano.Crypto.Hash as Crypto
import Cardano.Db (MigrationDir, PGPassSource (..))
import Cardano.Prelude
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (fail)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Data.ByteString.Short (ShortByteString (), fromShort, toShort)
import Data.Default.Class (Default (..))
import Ouroboros.Consensus.Cardano.CanHardFork (TriggerHardFork (..))

newtype LogFileDir = LogFileDir
  { unLogFileDir :: FilePath
  }

newtype ConfigFile = ConfigFile
  { unConfigFile :: FilePath
  }
  deriving (Show)

data SyncCommand
  = CmdRun !SyncNodeParams
  | CmdVersion

-- | The product type of all command line arguments
data SyncNodeParams = SyncNodeParams
  { enpConfigFile :: !ConfigFile
  , enpSocketPath :: !SocketPath
  , enpMaybeLedgerStateDir :: !(Maybe LedgerStateDir)
  , enpMigrationDir :: !MigrationDir
  , enpPGPassSource :: !PGPassSource
  , enpEpochDisabled :: !Bool
  , enpHasCache :: !Bool
  , enpSkipFix :: !Bool
  , enpOnlyFix :: !Bool
  , enpForceIndexes :: !Bool
  , enpHasInOut :: !Bool
  , enpSnEveryFollowing :: !Word64
  , enpSnEveryLagging :: !Word64
  , enpMaybeRollback :: !(Maybe SlotNo)
  }
  deriving (Show)

-- May have other constructors when we are preparing for a HFC event.
data SyncProtocol
  = SyncProtocolCardano
  deriving (Show)

data SyncNodeConfig = SyncNodeConfig
  { dncNetworkName :: !NetworkName
  , dncLoggingConfig :: !Logging.Configuration
  , dncNodeConfigFile :: !NodeConfigFile
  , dncProtocol :: !SyncProtocol
  , dncRequiresNetworkMagic :: !RequiresNetworkMagic
  , dncEnableLogging :: !Bool
  , dncEnableMetrics :: !Bool
  , dncPrometheusPort :: !Int
  , dncPBftSignatureThreshold :: !(Maybe Double)
  , dncByronGenesisFile :: !GenesisFile
  , dncByronGenesisHash :: !GenesisHashByron
  , dncShelleyGenesisFile :: !GenesisFile
  , dncShelleyGenesisHash :: !GenesisHashShelley
  , dncAlonzoGenesisFile :: !GenesisFile
  , dncAlonzoGenesisHash :: !GenesisHashAlonzo
  , dncConwayGenesisFile :: !(Maybe GenesisFile)
  , dncConwayGenesisHash :: !(Maybe GenesisHashConway)
  , dncByronProtocolVersion :: !Byron.ProtocolVersion
  , dncShelleyHardFork :: !TriggerHardFork
  , dncAllegraHardFork :: !TriggerHardFork
  , dncMaryHardFork :: !TriggerHardFork
  , dncAlonzoHardFork :: !TriggerHardFork
  , dncBabbageHardFork :: !TriggerHardFork
  , dncConwayHardFork :: !TriggerHardFork
  , dncInsertOptions :: !SyncInsertOptions
  }

data SyncPreConfig = SyncPreConfig
  { pcNetworkName :: !NetworkName
  , pcLoggingConfig :: !Logging.Representation
  , pcNodeConfigFile :: !NodeConfigFile
  , pcEnableLogging :: !Bool
  , pcEnableMetrics :: !Bool
  , pcPrometheusPort :: !Int
  , pcInsertConfig :: !SyncInsertConfig
  }
  deriving (Show)

data SyncInsertConfig
  = FullInsertOptions
  | OnlyUTxOInsertOptions
  | OnlyGovInsertOptions
  | DisableAllInsertOptions
  | SyncInsertConfig SyncInsertOptions
  deriving (Eq, Show)

data SyncInsertOptions = SyncInsertOptions
  { sioTxOut :: TxOutConfig
  , sioLedger :: LedgerInsertConfig
  , sioShelley :: ShelleyInsertConfig
  , sioRewards :: RewardsConfig
  , sioMultiAsset :: MultiAssetConfig
  , sioMetadata :: MetadataConfig
  , sioPlutus :: PlutusConfig
  , sioGovernance :: GovernanceConfig
  , sioOffchainPoolData :: OffchainPoolDataConfig
  , sioJsonType :: JsonTypeConfig
  }
  deriving (Eq, Show)

data TxOutConfig
  = TxOutEnable
  | TxOutDisable
  | TxOutConsumed ForceTxIn
  | TxOutPrune ForceTxIn
  | TxOutBootstrap ForceTxIn
  deriving (Eq, Show)

newtype ForceTxIn = ForceTxIn {unForceTxIn :: Bool}
  deriving (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

data LedgerInsertConfig
  = LedgerEnable
  | LedgerDisable
  | LedgerIgnore
  deriving (Eq, Show)

data ShelleyInsertConfig
  = ShelleyEnable
  | ShelleyDisable
  | ShelleyStakeAddrs (NonEmpty ShortByteString)
  deriving (Eq, Show)

newtype RewardsConfig = RewardsConfig
  {areRewardsEnabled :: Bool}
  deriving (Eq, Show)

data MultiAssetConfig
  = MultiAssetEnable
  | MultiAssetDisable
  | MultiAssetPolicies (NonEmpty ShortByteString)
  deriving (Eq, Show)

data MetadataConfig
  = MetadataEnable
  | MetadataDisable
  | MetadataKeys (NonEmpty Word)
  deriving (Eq, Show)

data PlutusConfig
  = PlutusEnable
  | PlutusDisable
  | PlutusScripts (NonEmpty ShortByteString)
  deriving (Eq, Show)

newtype GovernanceConfig = GovernanceConfig
  { isGovernanceEnabled :: Bool
  }
  deriving (Eq, Show)

newtype OffchainPoolDataConfig = OffchainPoolDataConfig
  { isOffchainPoolDataEnabled :: Bool
  }
  deriving (Eq, Show)

data JsonTypeConfig
  = JsonTypeText
  | JsonTypeJsonb
  | JsonTypeDisable
  deriving (Eq, Show)

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath
  }
  deriving (Show)
  deriving newtype (FromJSON, ToJSON)

newtype GenesisHashByron = GenesisHashByron
  { unGenesisHashByron :: Text
  }
  deriving newtype (Eq, Show, ToJSON, FromJSON)

newtype GenesisHashShelley = GenesisHashShelley
  { unGenesisHashShelley :: Crypto.Hash Crypto.Blake2b_256 ByteString
  }
  deriving newtype (Eq, Show, ToJSON, FromJSON)

newtype GenesisHashAlonzo = GenesisHashAlonzo
  { unGenesisHashAlonzo :: Crypto.Hash Crypto.Blake2b_256 ByteString
  }
  deriving newtype (Eq, Show, ToJSON, FromJSON)

newtype GenesisHashConway = GenesisHashConway
  {unGenesisHashConway :: Crypto.Hash Crypto.Blake2b_256 ByteString}
  deriving newtype (Eq, Show, ToJSON, FromJSON)

newtype LedgerStateDir = LedgerStateDir
  { unLedgerStateDir :: FilePath
  }
  deriving (Show)

newtype NetworkName = NetworkName
  { unNetworkName :: Text
  }
  deriving (Show)

newtype NodeConfigFile = NodeConfigFile
  { unNodeConfigFile :: FilePath
  }
  deriving (Show)

newtype SocketPath = SocketPath
  { unSocketPath :: FilePath
  }
  deriving (Show)

adjustGenesisFilePath :: (FilePath -> FilePath) -> GenesisFile -> GenesisFile
adjustGenesisFilePath f (GenesisFile p) = GenesisFile (f p)

adjustNodeConfigFilePath :: (FilePath -> FilePath) -> NodeConfigFile -> NodeConfigFile
adjustNodeConfigFilePath f (NodeConfigFile p) = NodeConfigFile (f p)

pcNodeConfigFilePath :: SyncPreConfig -> FilePath
pcNodeConfigFilePath = unNodeConfigFile . pcNodeConfigFile

isTxOutEnabled :: TxOutConfig -> Bool
isTxOutEnabled TxOutDisable = False
isTxOutEnabled TxOutEnable = True
isTxOutEnabled (TxOutConsumed _) = True
isTxOutEnabled (TxOutPrune _) = True
isTxOutEnabled (TxOutBootstrap _) = True

isTxOutBootstrap :: TxOutConfig -> Bool
isTxOutBootstrap (TxOutBootstrap _) = True
isTxOutBootstrap _ = False

isTxOutConsumed :: TxOutConfig -> Bool
isTxOutConsumed (TxOutConsumed _) = True
isTxOutConsumed _ = False

isTxOutPrune :: TxOutConfig -> Bool
isTxOutPrune (TxOutPrune _) = True
isTxOutPrune _ = False

forceTxIn :: TxOutConfig -> Bool
forceTxIn (TxOutConsumed f) = unForceTxIn f
forceTxIn (TxOutPrune f) = unForceTxIn f
forceTxIn (TxOutBootstrap f) = unForceTxIn f
forceTxIn TxOutEnable = False
forceTxIn TxOutDisable = False

hasLedger :: LedgerInsertConfig -> Bool
hasLedger LedgerDisable = False
hasLedger LedgerEnable = True
hasLedger LedgerIgnore = True

shouldUseLedger :: LedgerInsertConfig -> Bool
shouldUseLedger LedgerDisable = False
shouldUseLedger LedgerEnable = True
shouldUseLedger LedgerIgnore = False

isShelleyEnabled :: ShelleyInsertConfig -> Bool
isShelleyEnabled ShelleyDisable = False
isShelleyEnabled ShelleyEnable = True
isShelleyEnabled (ShelleyStakeAddrs _) = True

isMultiAssetEnabled :: MultiAssetConfig -> Bool
isMultiAssetEnabled MultiAssetDisable = False
isMultiAssetEnabled MultiAssetEnable = True
isMultiAssetEnabled (MultiAssetPolicies _) = True

isMetadataEnabled :: MetadataConfig -> Bool
isMetadataEnabled MetadataDisable = False
isMetadataEnabled MetadataEnable = True
isMetadataEnabled (MetadataKeys _) = True

isPlutusEnabled :: PlutusConfig -> Bool
isPlutusEnabled PlutusDisable = False
isPlutusEnabled PlutusEnable = True
isPlutusEnabled (PlutusScripts _) = True

-- -------------------------------------------------------------------------------------------------

instance FromJSON SyncPreConfig where
  parseJSON =
    Aeson.withObject "top-level" parseGenSyncNodeConfig

parseGenSyncNodeConfig :: Aeson.Object -> Parser SyncPreConfig
parseGenSyncNodeConfig o =
  SyncPreConfig
    <$> fmap NetworkName (o .: "NetworkName")
    <*> parseJSON (Object o)
    <*> fmap NodeConfigFile (o .: "NodeConfigFile")
    <*> o .: "EnableLogging"
    <*> o .: "EnableLogMetrics"
    <*> fmap (fromMaybe 8080) (o .:? "PrometheusPort")
    <*> o .:? "insert_options" .!= def

instance FromJSON SyncProtocol where
  parseJSON o =
    case o of
      String "Cardano" -> pure SyncProtocolCardano
      x -> typeMismatch "Protocol" x

instance FromJSON SyncInsertConfig where
  parseJSON = Aeson.withObject "SyncInsertConfig" $ \obj -> do
    preset <- obj .:? "preset"
    case preset :: Maybe Text of
      Nothing -> SyncInsertConfig <$> parseJSON (Aeson.Object obj)
      Just "full" -> pure FullInsertOptions
      Just "only_utxo" -> pure OnlyUTxOInsertOptions
      Just "only_gov" -> pure OnlyGovInsertOptions
      Just "disable_all" -> pure DisableAllInsertOptions
      Just other -> fail $ "unexpected preset: " <> show other

instance ToJSON SyncInsertConfig where
  toJSON (SyncInsertConfig opts) = toJSON opts
  toJSON FullInsertOptions = Aeson.object ["preset" .= ("full" :: Text)]
  toJSON OnlyUTxOInsertOptions = Aeson.object ["preset" .= ("only_utxo" :: Text)]
  toJSON OnlyGovInsertOptions = Aeson.object ["preset" .= ("only_gov" :: Text)]
  toJSON DisableAllInsertOptions = Aeson.object ["preset" .= ("disable_all" :: Text)]

instance FromJSON SyncInsertOptions where
  parseJSON = Aeson.withObject "SyncInsertOptions" $ \obj ->
    SyncInsertOptions
      <$> obj .:? "tx_out" .!= sioTxOut def
      <*> obj .:? "ledger" .!= sioLedger def
      <*> obj .:? "shelley" .!= sioShelley def
      <*> pure (sioRewards def)
      <*> obj .:? "multi_asset" .!= sioMultiAsset def
      <*> obj .:? "metadata" .!= sioMetadata def
      <*> obj .:? "plutus" .!= sioPlutus def
      <*> obj .:? "governance" .!= sioGovernance def
      <*> obj .:? "offchain_pool_data" .!= sioOffchainPoolData def
      <*> obj .:? "json_type" .!= sioJsonType def

instance ToJSON SyncInsertOptions where
  toJSON SyncInsertOptions {..} =
    Aeson.object
      [ "tx_out" .= sioTxOut
      , "ledger" .= sioLedger
      , "shelley" .= sioShelley
      , "multi_asset" .= sioMultiAsset
      , "metadata" .= sioMetadata
      , "plutus" .= sioPlutus
      , "governance" .= sioGovernance
      , "offchain_pool_data" .= sioOffchainPoolData
      , "json_type" .= sioJsonType
      ]

instance ToJSON TxOutConfig where
  toJSON cfg =
    Aeson.object
      [ "value" .= value cfg
      , "force_tx_in" .= forceTxIn' cfg
      ]
    where
      value :: TxOutConfig -> Text
      value TxOutEnable = "enable"
      value TxOutDisable = "disable"
      value (TxOutConsumed _) = "consumed"
      value (TxOutPrune _) = "prune"
      value (TxOutBootstrap _) = "bootstrap"

      forceTxIn' :: TxOutConfig -> Maybe Bool
      forceTxIn' TxOutEnable = Nothing
      forceTxIn' TxOutDisable = Nothing
      forceTxIn' (TxOutConsumed f) = Just (unForceTxIn f)
      forceTxIn' (TxOutPrune f) = Just (unForceTxIn f)
      forceTxIn' (TxOutBootstrap f) = Just (unForceTxIn f)

instance FromJSON TxOutConfig where
  parseJSON = Aeson.withObject "tx_out" $ \obj -> do
    val <- obj .: "value"
    forceTxIn' <- obj .:? "force_tx_in" .!= ForceTxIn False

    case val :: Text of
      "enable" -> pure TxOutEnable
      "disable" -> pure TxOutDisable
      "consumed" -> pure (TxOutConsumed forceTxIn')
      "prune" -> pure (TxOutPrune forceTxIn')
      "bootstrap" -> pure (TxOutBootstrap forceTxIn')
      other -> fail $ "unexpected tx_out: " <> show other

instance ToJSON LedgerInsertConfig where
  toJSON LedgerEnable = "enable"
  toJSON LedgerDisable = "disable"
  toJSON LedgerIgnore = "ignore"

instance FromJSON LedgerInsertConfig where
  parseJSON = Aeson.withText "ledger" $ \case
    "enable" -> pure LedgerEnable
    "disable" -> pure LedgerDisable
    "ignore" -> pure LedgerIgnore
    other -> fail $ "unexpected ledger: " <> show other

instance ToJSON ShelleyInsertConfig where
  toJSON cfg =
    Aeson.object
      [ "enable" .= isShelleyEnabled cfg
      , "stake_addresses" .= stakeAddrs cfg
      ]
    where
      stakeAddrs (ShelleyStakeAddrs addrs) = toJSON (map shortByteStringToJSON addrs)
      stakeAddrs _ = Aeson.Null

instance FromJSON ShelleyInsertConfig where
  parseJSON = Aeson.withObject "shelley" $ \obj -> do
    enable <- obj .: "enable"
    stakeAddrs <- obj .:? "stake_addresses"

    pure $
      case (enable, stakeAddrs) of
        (False, _) -> ShelleyDisable
        (True, Nothing) -> ShelleyEnable
        (True, Just addrs) -> ShelleyStakeAddrs (map parseShortByteString addrs)

instance ToJSON MultiAssetConfig where
  toJSON cfg =
    Aeson.object
      [ "enable" .= isMultiAssetEnabled cfg
      , "policies" .= policies cfg
      ]
    where
      policies (MultiAssetPolicies ps) = toJSON (map shortByteStringToJSON ps)
      policies _ = Aeson.Null

instance FromJSON MultiAssetConfig where
  parseJSON = Aeson.withObject "multi_asset" $ \obj -> do
    enable <- obj .: "enable"
    policies <- obj .:? "policies"

    pure $
      case (enable, policies) of
        (False, _) -> MultiAssetDisable
        (True, Nothing) -> MultiAssetEnable
        (True, Just ps) -> MultiAssetPolicies (map parseShortByteString ps)

instance ToJSON MetadataConfig where
  toJSON cfg =
    Aeson.object
      [ "enable" .= isMetadataEnabled cfg
      , "keys" .= keys cfg
      ]
    where
      keys (MetadataKeys ks) = toJSON ks
      keys _ = Aeson.Null

instance FromJSON MetadataConfig where
  parseJSON = Aeson.withObject "metadata" $ \obj -> do
    enable <- obj .: "enable"
    keys <- obj .:? "keys"

    pure $
      case (enable, keys) of
        (False, _) -> MetadataDisable
        (True, Nothing) -> MetadataEnable
        (True, Just ks) -> MetadataKeys ks

instance ToJSON PlutusConfig where
  toJSON cfg =
    Aeson.object
      [ "enable" .= isPlutusEnabled cfg
      , "script_hashes" .= scriptHashes cfg
      ]
    where
      scriptHashes (PlutusScripts ps) = toJSON (map shortByteStringToJSON ps)
      scriptHashes _ = Aeson.Null

instance FromJSON PlutusConfig where
  parseJSON = Aeson.withObject "plutus" $ \obj -> do
    enable <- obj .: "enable"
    scriptHashes <- obj .:? "script_hashes"

    pure $
      case (enable, scriptHashes) of
        (False, _) -> PlutusDisable
        (True, Nothing) -> PlutusEnable
        (True, Just hs) -> PlutusScripts (map parseShortByteString hs)

instance ToJSON GovernanceConfig where
  toJSON = boolToEnableDisable . isGovernanceEnabled

instance FromJSON GovernanceConfig where
  parseJSON = Aeson.withText "governance" $ \v ->
    case enableDisableToBool v of
      Just g -> pure (GovernanceConfig g)
      Nothing -> fail $ "unexpected governance: " <> show v

instance ToJSON OffchainPoolDataConfig where
  toJSON = boolToEnableDisable . isOffchainPoolDataEnabled

instance FromJSON OffchainPoolDataConfig where
  parseJSON = Aeson.withText "offchain_pool_data" $ \v ->
    case enableDisableToBool v of
      Just g -> pure (OffchainPoolDataConfig g)
      Nothing -> fail $ "unexpected offchain_pool_data: " <> show v

instance ToJSON JsonTypeConfig where
  toJSON JsonTypeText = "text"
  toJSON JsonTypeJsonb = "jsonb"
  toJSON JsonTypeDisable = "disable"

instance FromJSON JsonTypeConfig where
  parseJSON = Aeson.withText "json_type" $ \case
    "text" -> pure JsonTypeText
    "jsonb" -> pure JsonTypeJsonb
    "disable" -> pure JsonTypeDisable
    other -> fail $ "unexpected json_type: " <> show other

instance Default SyncInsertConfig where
  def = SyncInsertConfig def

instance Default SyncInsertOptions where
  def =
    SyncInsertOptions
      { sioTxOut = TxOutEnable
      , sioLedger = LedgerEnable
      , sioShelley = ShelleyEnable
      , sioRewards = RewardsConfig True
      , sioMultiAsset = MultiAssetEnable
      , sioMetadata = MetadataEnable
      , sioPlutus = PlutusEnable
      , sioGovernance = GovernanceConfig True
      , sioOffchainPoolData = OffchainPoolDataConfig True
      , sioJsonType = JsonTypeText
      }

boolToEnableDisable :: IsString s => Bool -> s
boolToEnableDisable True = "enable"
boolToEnableDisable False = "disable"

enableDisableToBool :: (Eq s, IsString s) => s -> Maybe Bool
enableDisableToBool = \case
  "enable" -> Just True
  "disable" -> Just False
  _ -> Nothing

parseShortByteString :: Text -> ShortByteString
parseShortByteString = toShort . encodeUtf8

shortByteStringToJSON :: ShortByteString -> Aeson.Value
shortByteStringToJSON = toJSON . decodeUtf8 . fromShort
