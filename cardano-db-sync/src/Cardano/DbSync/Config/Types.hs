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
  TxOutConfig (..),
  LedgerInsertConfig (..),
  ShelleyInsertConfig (..),
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
  isLedgerEnabled,
  isShelleyEnabled,
  isMultiAssetEnabled,
  isMetadataEnabled,
  isPlutusEnabled,
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
  , enpHasLedger :: !Bool
  , enpShouldUseLedger :: !Bool
  , enpSkipFix :: !Bool
  , enpOnlyFix :: !Bool
  , enpForceIndexes :: !Bool
  , enpHasInOut :: !Bool
  , enpHasShelley :: !Bool
  , enpHasMultiAssets :: !Bool
  , enpHasMetadata :: !Bool
  , enpKeepMetadataNames :: ![Word64]
  , enpHasPlutusExtra :: !Bool
  , enpHasGov :: !Bool
  , enpHasOffChainPoolData :: !Bool
  , enpForceTxIn :: !Bool
  , enpDisableAllMode :: !Bool
  , enpFullMode :: !Bool
  , enpOnlyUTxO :: !Bool
  , enpOnlyGov :: !Bool
  , enpMigrateConsumed :: !Bool
  , enpPruneTxOut :: !Bool
  , enpBootstrap :: !Bool
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
  , dncInsertConfig :: !SyncInsertConfig
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

data SyncInsertConfig = SyncInsertConfig
  { spcTxOut :: TxOutConfig
  , spcLedger :: LedgerInsertConfig
  , spcShelley :: ShelleyInsertConfig
  , spcMultiAsset :: MultiAssetConfig
  , spcMetadata :: MetadataConfig
  , spcPlutus :: PlutusConfig
  , spcGovernance :: GovernanceConfig
  , spcOffchainPoolData :: OffchainPoolDataConfig
  , spcJsonType :: JsonTypeConfig
  }
  deriving (Eq, Show)

data TxOutConfig
  = TxOutEnable
  | TxOutDisable
  | TxOutConsumed
  | TxOutPrune
  | TxOutBootstrap
  deriving (Eq, Show)

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
isTxOutEnabled TxOutConsumed = True
isTxOutEnabled TxOutPrune = True
isTxOutEnabled TxOutBootstrap = True

isLedgerEnabled :: LedgerInsertConfig -> Bool
isLedgerEnabled LedgerDisable = False
isLedgerEnabled LedgerEnable = True
isLedgerEnabled LedgerIgnore = True

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
  parseJSON = Aeson.withObject "SyncInsertConfig" $ \obj ->
    SyncInsertConfig
      <$> obj .:? "tx_out" .!= spcTxOut def
      <*> obj .:? "ledger" .!= spcLedger def
      <*> obj .:? "shelley" .!= spcShelley def
      <*> obj .:? "multi_asset" .!= spcMultiAsset def
      <*> obj .:? "metadata" .!= spcMetadata def
      <*> obj .:? "plutus" .!= spcPlutus def
      <*> obj .:? "governance" .!= spcGovernance def
      <*> obj .:? "offchain_pool_data" .!= spcOffchainPoolData def
      <*> obj .:? "json_type" .!= spcJsonType def

instance ToJSON SyncInsertConfig where
  toJSON SyncInsertConfig {..} =
    Aeson.object
      [ "tx_out" .= spcTxOut
      , "ledger" .= spcLedger
      , "shelley" .= spcShelley
      , "multi_asset" .= spcMultiAsset
      , "metadata" .= spcMetadata
      , "plutus" .= spcPlutus
      , "governance" .= spcGovernance
      , "offchain_pool_data" .= spcOffchainPoolData
      , "json_type" .= spcJsonType
      ]

instance ToJSON TxOutConfig where
  toJSON TxOutEnable = "enable"
  toJSON TxOutDisable = "disable"
  toJSON TxOutConsumed = "consumed"
  toJSON TxOutPrune = "prune"
  toJSON TxOutBootstrap = "bootstrap"

instance FromJSON TxOutConfig where
  parseJSON = Aeson.withText "tx_out" $ \case
    "enable" -> pure TxOutEnable
    "disable" -> pure TxOutDisable
    "consumed" -> pure TxOutConsumed
    "prune" -> pure TxOutPrune
    "bootstrap" -> pure TxOutBootstrap
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
  def =
    SyncInsertConfig
      { spcTxOut = TxOutEnable
      , spcLedger = LedgerEnable
      , spcShelley = ShelleyEnable
      , spcMultiAsset = MultiAssetEnable
      , spcMetadata = MetadataEnable
      , spcPlutus = PlutusEnable
      , spcGovernance = GovernanceConfig True
      , spcOffchainPoolData = OffchainPoolDataConfig True
      , spcJsonType = JsonTypeText
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
