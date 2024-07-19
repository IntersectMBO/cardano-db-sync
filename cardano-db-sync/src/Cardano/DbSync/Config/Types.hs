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
  RemoveJsonbFromSchemaConfig (..),
  SyncNodeConfig (..),
  SyncPreConfig (..),
  SyncInsertConfig (..),
  SyncInsertOptions (..),
  TxCBORConfig (..),
  PoolStatsConfig (..),
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
  isShelleyModeActive,
  isShelleyWhitelistModeActive,
  isMultiAssetModeActive,
  isMetadataModeActive,
  isPlutusModeActive,
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
import qualified Data.Text as Text
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
  , pcEnableFutureGenesis :: !Bool
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
  { sioTxCBOR :: TxCBORConfig
  , sioTxOut :: TxOutConfig
  , sioLedger :: LedgerInsertConfig
  , sioShelley :: ShelleyInsertConfig
  , sioRewards :: RewardsConfig
  , sioMultiAsset :: MultiAssetConfig
  , sioMetadata :: MetadataConfig
  , sioPlutus :: PlutusConfig
  , sioGovernance :: GovernanceConfig
  , sioOffchainPoolData :: OffchainPoolDataConfig
  , sioPoolStats :: PoolStatsConfig
  , sioJsonType :: JsonTypeConfig
  , sioRemoveJsonbFromSchema :: RemoveJsonbFromSchemaConfig
  }
  deriving (Eq, Show)

newtype TxCBORConfig = TxCBORConfig
  { isTxCBOREnabled :: Bool
  }
  deriving (Eq, Show)

newtype PoolStatsConfig = PoolStatsConfig
  { isPoolStatsEnabled :: Bool
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
  | -- | Whitelist of Shelley stake addresses
    ShelleyStakeAddrs (NonEmpty ShortByteString)
  deriving (Eq, Show)

newtype RewardsConfig = RewardsConfig
  {areRewardsEnabled :: Bool}
  deriving (Eq, Show)

data MultiAssetConfig
  = MultiAssetEnable
  | MultiAssetDisable
  | -- | Whitelist of multiAsset policy IDs
    MultiAssetPolicies (NonEmpty ShortByteString)
  deriving (Eq, Show)

data MetadataConfig
  = MetadataEnable
  | MetadataDisable
  | -- | Whitelist of metadata keys
    MetadataKeys (NonEmpty Word)
  deriving (Eq, Show)

data PlutusConfig
  = PlutusEnable
  | PlutusDisable
  | -- | Whitelist of plutus script hashes
    PlutusScripts (NonEmpty ShortByteString)
  deriving (Eq, Show)

newtype GovernanceConfig = GovernanceConfig
  { isGovernanceEnabled :: Bool
  }
  deriving (Eq, Show)

newtype OffchainPoolDataConfig = OffchainPoolDataConfig
  { isOffchainPoolDataEnabled :: Bool
  }
  deriving (Eq, Show)

newtype RemoveJsonbFromSchemaConfig = RemoveJsonbFromSchemaConfig
  { isRemoveJsonbFromSchemaEnabled :: Bool
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

isShelleyModeActive :: ShelleyInsertConfig -> Bool
isShelleyModeActive ShelleyDisable = False
isShelleyModeActive ShelleyEnable = True
isShelleyModeActive (ShelleyStakeAddrs _) = True

isShelleyWhitelistModeActive :: ShelleyInsertConfig -> Bool
isShelleyWhitelistModeActive (ShelleyStakeAddrs _) = True
isShelleyWhitelistModeActive _other = False

isMultiAssetModeActive :: MultiAssetConfig -> Bool
isMultiAssetModeActive MultiAssetDisable = False
isMultiAssetModeActive MultiAssetEnable = True
isMultiAssetModeActive (MultiAssetPolicies _) = True

isMetadataModeActive :: MetadataConfig -> Bool
isMetadataModeActive MetadataDisable = False
isMetadataModeActive MetadataEnable = True
isMetadataModeActive (MetadataKeys _) = True

isPlutusModeActive :: PlutusConfig -> Bool
isPlutusModeActive PlutusDisable = False
isPlutusModeActive PlutusEnable = True
isPlutusModeActive (PlutusScripts _) = True

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
    <*> fmap (fromMaybe True) (o .:? "EnableFutureGenesis")
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
      <$> obj .:? "tx_cbor" .!= sioTxCBOR def
      <*> obj .:? "tx_out" .!= sioTxOut def
      <*> obj .:? "ledger" .!= sioLedger def
      <*> obj .:? "shelley" .!= sioShelley def
      <*> pure (sioRewards def)
      <*> obj .:? "multi_asset" .!= sioMultiAsset def
      <*> obj .:? "metadata" .!= sioMetadata def
      <*> obj .:? "plutus" .!= sioPlutus def
      <*> obj .:? "governance" .!= sioGovernance def
      <*> obj .:? "offchain_pool_data" .!= sioOffchainPoolData def
      <*> obj .:? "pool_stat" .!= sioPoolStats def
      <*> obj .:? "json_type" .!= sioJsonType def
      <*> obj .:? "remove_jsonb_from_schema" .!= sioRemoveJsonbFromSchema def

instance ToJSON SyncInsertOptions where
  toJSON SyncInsertOptions {..} =
    Aeson.object
      [ "tx_cbor" .= sioTxCBOR
      , "tx_out" .= sioTxOut
      , "ledger" .= sioLedger
      , "shelley" .= sioShelley
      , "multi_asset" .= sioMultiAsset
      , "metadata" .= sioMetadata
      , "plutus" .= sioPlutus
      , "governance" .= sioGovernance
      , "offchain_pool_data" .= sioOffchainPoolData
      , "pool_stat" .= sioPoolStats
      , "json_type" .= sioJsonType
      , "remove_jsonb_from_schema" .= sioRemoveJsonbFromSchema
      ]

instance ToJSON TxCBORConfig where
  toJSON = boolToEnableDisable . isTxCBOREnabled

instance ToJSON PoolStatsConfig where
  toJSON = boolToEnableDisable . isPoolStatsEnabled

instance FromJSON TxCBORConfig where
  parseJSON = Aeson.withText "tx_cbor" $ \v ->
    case enableDisableToBool v of
      Just g -> pure (TxCBORConfig g)
      Nothing -> fail $ "unexpected tx_cbor: " <> show v

instance FromJSON PoolStatsConfig where
  parseJSON = Aeson.withText "pool_stat" $ \v ->
    case enableDisableToBool v of
      Just g -> pure (PoolStatsConfig g)
      Nothing -> fail $ "unexpected pool_stat: " <> show v

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
      [ "enable" .= isShelleyModeActive cfg
      , "stake_addresses" .= stakeAddrs cfg
      ]
    where
      stakeAddrs (ShelleyStakeAddrs addrs) = toJSON (map shortByteStringToJSON addrs)
      stakeAddrs _ = Aeson.Null

instance FromJSON ShelleyInsertConfig where
  parseJSON = Aeson.withObject "shelley" $ \obj -> do
    enable <- obj .: "enable"
    stakeAddrs <- obj .:? "stake_addresses"

    case (enable, stakeAddrs) of
      (False, _) -> pure ShelleyDisable
      (True, Nothing) -> pure ShelleyEnable
      (True, Just addrs) -> do
        addrsParsed <- traverse parseValidateHash addrs
        pure $ ShelleyStakeAddrs addrsParsed

instance ToJSON MultiAssetConfig where
  toJSON cfg =
    Aeson.object
      [ "enable" .= isMultiAssetModeActive cfg
      , "policies" .= policies cfg
      ]
    where
      policies (MultiAssetPolicies ps) = toJSON (map shortByteStringToJSON ps)
      policies _ = Aeson.Null

instance FromJSON MultiAssetConfig where
  parseJSON = Aeson.withObject "multi_asset" $ \obj -> do
    enable <- obj .: "enable"
    policies <- obj .:? "policies"

    case (enable, policies) of
      (False, _) -> pure MultiAssetDisable
      (True, Nothing) -> pure MultiAssetEnable
      (True, Just ps) -> do
        policiesParsed <- traverse parseValidateHash ps
        pure $ MultiAssetPolicies policiesParsed

instance ToJSON MetadataConfig where
  toJSON cfg =
    Aeson.object
      [ "enable" .= isMetadataModeActive cfg
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
      [ "enable" .= isPlutusModeActive cfg
      , "script_hashes" .= scriptHashes cfg
      ]
    where
      scriptHashes (PlutusScripts ps) = toJSON (map shortByteStringToJSON ps)
      scriptHashes _ = Aeson.Null

instance FromJSON PlutusConfig where
  parseJSON = Aeson.withObject "plutus" $ \obj -> do
    enable <- obj .: "enable"
    scriptHashes <- obj .:? "script_hashes"

    case (enable, scriptHashes) of
      (False, _) -> pure PlutusDisable
      (True, Nothing) -> pure PlutusEnable
      (True, Just hs) -> do
        hsParsed <- traverse parseValidateHash hs
        pure $ PlutusScripts hsParsed

instance ToJSON GovernanceConfig where
  toJSON = boolToEnableDisable . isGovernanceEnabled

instance FromJSON GovernanceConfig where
  parseJSON = Aeson.withText "governance" $ \v ->
    case enableDisableToBool v of
      Just g -> pure (GovernanceConfig g)
      Nothing -> fail $ "unexpected governance: " <> show v

instance ToJSON OffchainPoolDataConfig where
  toJSON = boolToEnableDisable . isOffchainPoolDataEnabled

instance FromJSON RemoveJsonbFromSchemaConfig where
  parseJSON = Aeson.withText "remove_jsonb_from_schema" $ \v ->
    case enableDisableToBool v of
      Just g -> pure (RemoveJsonbFromSchemaConfig g)
      Nothing -> fail $ "unexpected remove_jsonb_from_schema: " <> show v

instance ToJSON RemoveJsonbFromSchemaConfig where
  toJSON = boolToEnableDisable . isRemoveJsonbFromSchemaEnabled

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
      { sioTxCBOR = TxCBORConfig False
      , sioTxOut = TxOutEnable
      , sioLedger = LedgerEnable
      , sioShelley = ShelleyEnable
      , sioRewards = RewardsConfig True
      , sioMultiAsset = MultiAssetEnable
      , sioMetadata = MetadataEnable
      , sioPlutus = PlutusEnable
      , sioGovernance = GovernanceConfig True
      , sioOffchainPoolData = OffchainPoolDataConfig True
      , sioPoolStats = PoolStatsConfig False
      , sioJsonType = JsonTypeText
      , sioRemoveJsonbFromSchema = RemoveJsonbFromSchemaConfig False
      }

boolToEnableDisable :: IsString s => Bool -> s
boolToEnableDisable True = "enable"
boolToEnableDisable False = "disable"

enableDisableToBool :: (Eq s, IsString s) => s -> Maybe Bool
enableDisableToBool = \case
  "enable" -> Just True
  "disable" -> Just False
  _ -> Nothing

parseValidateHash :: Text -> Parser ShortByteString
parseValidateHash txt =
  if "\\x" `Text.isPrefixOf` txt
    then fail $ "Invalid Hash: starts with \\x please adjust it:  " <> show txt
    else pure $ toShort $ encodeUtf8 txt

shortByteStringToJSON :: ShortByteString -> Aeson.Value
shortByteStringToJSON = toJSON . decodeUtf8 . fromShort
