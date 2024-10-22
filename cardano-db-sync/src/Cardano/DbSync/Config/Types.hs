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
  TxOutTableTypeConfig (..),
  SyncNodeConfig (..),
  SyncPreConfig (..),
  SyncInsertConfig (..),
  SyncInsertPreset (..),
  SyncInsertOptions (..),
  TxCBORConfig (..),
  PoolStatsConfig (..),
  TxOutConfig (..),
  UseTxOutAddress (..),
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
  isTxOutConsumedBootstrap,
  isTxOutConsumed,
  isTxOutConsumedPrune,
  forceTxIn,
  fullInsertOptions,
  onlyUTxOInsertOptions,
  onlyGovInsertOptions,
  disableAllInsertOptions,
) where

import qualified Cardano.BM.Configuration as Logging
import qualified Cardano.BM.Data.Configuration as Logging
import qualified Cardano.Chain.Update as Byron
import Cardano.Crypto (RequiresNetworkMagic (..))
import qualified Cardano.Crypto.Hash as Crypto
import Cardano.Db (MigrationDir, PGPassSource (..), TxOutTableType (..))
import Cardano.Prelude
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Monad (fail)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (Pair, Parser, typeMismatch)
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
  , dncIpfsGateway :: [Text]
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
  , pcIpfsGateway :: ![Text]
  }
  deriving (Show)

data SyncInsertConfig = SyncInsertConfig
  { sicPreset :: Maybe SyncInsertPreset
  , sicOptions :: SyncInsertOptions
  }
  deriving (Eq, Show)

data SyncInsertPreset
  = FullInsertPreset
  | OnlyUTxOInsertPreset
  | OnlyGovInsertPreset
  | DisableAllInsertPreset
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
  = TxOutEnable UseTxOutAddress
  | TxOutDisable
  | TxOutConsumed ForceTxIn UseTxOutAddress
  | TxOutConsumedPrune ForceTxIn UseTxOutAddress
  | TxOutConsumedBootstrap ForceTxIn UseTxOutAddress
  deriving (Eq, Show)

newtype ForceTxIn = ForceTxIn {unForceTxIn :: Bool}
  deriving (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

newtype UseTxOutAddress = UseTxOutAddress {unUseTxOutAddress :: Bool}
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

newtype RemoveJsonbFromSchemaConfig = RemoveJsonbFromSchemaConfig
  { isRemoveJsonbFromSchemaEnabled :: Bool
  }
  deriving (Eq, Show)

newtype TxOutTableTypeConfig = TxOutTableTypeConfig
  { unTxOutTableTypeConfig :: TxOutTableType
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
isTxOutEnabled (TxOutEnable _) = True
isTxOutEnabled (TxOutConsumed _ _) = True
isTxOutEnabled (TxOutConsumedPrune _ _) = True
isTxOutEnabled (TxOutConsumedBootstrap _ _) = True

isTxOutConsumedBootstrap :: TxOutConfig -> Bool
isTxOutConsumedBootstrap (TxOutConsumedBootstrap _ _) = True
isTxOutConsumedBootstrap _ = False

isTxOutConsumed :: TxOutConfig -> Bool
isTxOutConsumed (TxOutConsumed _ _) = True
isTxOutConsumed _ = False

isTxOutConsumedPrune :: TxOutConfig -> Bool
isTxOutConsumedPrune (TxOutConsumedPrune _ _) = True
isTxOutConsumedPrune _ = False

forceTxIn :: TxOutConfig -> Bool
forceTxIn (TxOutConsumed f _) = unForceTxIn f
forceTxIn (TxOutConsumedPrune f _) = unForceTxIn f
forceTxIn (TxOutConsumedBootstrap f _) = unForceTxIn f
forceTxIn (TxOutEnable _) = False
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
    <*> fmap (fromMaybe True) (o .:? "EnableFutureGenesis")
    <*> o .: "EnableLogging"
    <*> o .: "EnableLogMetrics"
    <*> fmap (fromMaybe 8080) (o .:? "PrometheusPort")
    <*> o .:? "insert_options" .!= def
    <*> o .:? "ipfs_gateway" .!= ["https://ipfs.io/ipfs"]

instance FromJSON SyncProtocol where
  parseJSON o =
    case o of
      String "Cardano" -> pure SyncProtocolCardano
      x -> typeMismatch "Protocol" x

instance FromJSON SyncInsertPreset where
  parseJSON = Aeson.withText "SyncInsertPreset" $ \case
    "full" -> pure FullInsertPreset
    "only_utxo" -> pure OnlyUTxOInsertPreset
    "only_governance" -> pure OnlyGovInsertPreset
    "disable_all" -> pure DisableAllInsertPreset
    other -> fail $ "unexpected preset: " <> show other

instance ToJSON SyncInsertPreset where
  toJSON FullInsertPreset = "full"
  toJSON OnlyUTxOInsertPreset = "only_utxo"
  toJSON OnlyGovInsertPreset = "only_governance"
  toJSON DisableAllInsertPreset = "disable_all"

instance FromJSON SyncInsertConfig where
  parseJSON = Aeson.withObject "SyncInsertConfig" $ \obj -> do
    preset <- obj .:? "preset"
    baseOptions <- case preset of
      Just FullInsertPreset -> pure fullInsertOptions
      Just OnlyUTxOInsertPreset -> pure onlyUTxOInsertOptions
      Just OnlyGovInsertPreset -> pure onlyGovInsertOptions
      Just DisableAllInsertPreset -> pure disableAllInsertOptions
      Nothing -> pure def -- Default options
    options <- parseOverrides obj baseOptions
    pure $ SyncInsertConfig preset options

parseOverrides :: Aeson.Object -> SyncInsertOptions -> Parser SyncInsertOptions
parseOverrides obj baseOptions = do
  SyncInsertOptions
    <$> obj .:? "tx_cbor" .!= sioTxCBOR baseOptions
    <*> obj .:? "tx_out" .!= sioTxOut baseOptions
    <*> obj .:? "ledger" .!= sioLedger baseOptions
    <*> obj .:? "shelley" .!= sioShelley baseOptions
    <*> pure (sioRewards baseOptions)
    <*> obj .:? "multi_asset" .!= sioMultiAsset baseOptions
    <*> obj .:? "metadata" .!= sioMetadata baseOptions
    <*> obj .:? "plutus" .!= sioPlutus baseOptions
    <*> obj .:? "governance" .!= sioGovernance baseOptions
    <*> obj .:? "offchain_pool_data" .!= sioOffchainPoolData baseOptions
    <*> obj .:? "pool_stat" .!= sioPoolStats baseOptions
    <*> obj .:? "json_type" .!= sioJsonType baseOptions
    <*> obj .:? "remove_jsonb_from_schema" .!= sioRemoveJsonbFromSchema baseOptions

instance ToJSON SyncInsertConfig where
  toJSON (SyncInsertConfig preset options) =
    Aeson.object $ maybe [] (\p -> [fromText "preset" .= p]) preset ++ optionsToList options

optionsToList :: SyncInsertOptions -> [Pair]
optionsToList SyncInsertOptions {..} =
  catMaybes
    [ toJsonIfSet "tx_cbor" sioTxCBOR
    , toJsonIfSet "tx_out" sioTxOut
    , toJsonIfSet "ledger" sioLedger
    , toJsonIfSet "shelley" sioShelley
    , toJsonIfSet "rewards" sioRewards
    , toJsonIfSet "multi_asset" sioMultiAsset
    , toJsonIfSet "metadata" sioMetadata
    , toJsonIfSet "plutus" sioPlutus
    , toJsonIfSet "governance" sioGovernance
    , toJsonIfSet "offchain_pool_data" sioOffchainPoolData
    , toJsonIfSet "pool_stat" sioPoolStats
    , toJsonIfSet "json_type" sioJsonType
    , toJsonIfSet "remove_jsonb_from_schema" sioRemoveJsonbFromSchema
    ]

toJsonIfSet :: ToJSON a => Text -> a -> Maybe Pair
toJsonIfSet key value = Just $ fromText key .= value

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

instance ToJSON RewardsConfig where
  toJSON (RewardsConfig enabled) = Aeson.Bool enabled

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
      , "use_address_table" .= useTxOutAddress' cfg
      ]
    where
      value :: TxOutConfig -> Text
      value (TxOutEnable _) = "enable"
      value TxOutDisable = "disable"
      value (TxOutConsumed _ _) = "consumed"
      value (TxOutConsumedPrune _ _) = "prune"
      value (TxOutConsumedBootstrap _ _) = "bootstrap"

      forceTxIn' :: TxOutConfig -> Maybe Bool
      forceTxIn' (TxOutEnable _) = Nothing
      forceTxIn' TxOutDisable = Nothing
      forceTxIn' (TxOutConsumed f _) = Just (unForceTxIn f)
      forceTxIn' (TxOutConsumedPrune f _) = Just (unForceTxIn f)
      forceTxIn' (TxOutConsumedBootstrap f _) = Just (unForceTxIn f)

      useTxOutAddress' :: TxOutConfig -> Maybe Bool
      useTxOutAddress' (TxOutEnable u) = Just (unUseTxOutAddress u)
      useTxOutAddress' TxOutDisable = Nothing
      useTxOutAddress' (TxOutConsumed _ u) = Just (unUseTxOutAddress u)
      useTxOutAddress' (TxOutConsumedPrune _ u) = Just (unUseTxOutAddress u)
      useTxOutAddress' (TxOutConsumedBootstrap _ u) = Just (unUseTxOutAddress u)

instance FromJSON TxOutConfig where
  parseJSON = Aeson.withObject "tx_out" $ \obj -> do
    val <- obj .: "value"
    forceTxIn' <- obj .:? "force_tx_in" .!= ForceTxIn False
    useAddress' <- obj .:? "use_address_table" .!= UseTxOutAddress False

    case val :: Text of
      "enable" -> pure (TxOutEnable useAddress')
      "disable" -> pure TxOutDisable
      "consumed" -> pure (TxOutConsumed forceTxIn' useAddress')
      "prune" -> pure (TxOutConsumedPrune forceTxIn' useAddress')
      "bootstrap" -> pure (TxOutConsumedBootstrap forceTxIn' useAddress')
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

instance FromJSON RemoveJsonbFromSchemaConfig where
  parseJSON = Aeson.withText "remove_jsonb_from_schema" $ \v ->
    case enableDisableToBool v of
      Just g -> pure (RemoveJsonbFromSchemaConfig g)
      Nothing -> fail $ "unexpected remove_jsonb_from_schema: " <> show v

instance ToJSON RemoveJsonbFromSchemaConfig where
  toJSON = boolToEnableDisable . isRemoveJsonbFromSchemaEnabled

instance FromJSON TxOutTableTypeConfig where
  parseJSON = Aeson.withText "use_address_table" $ \v ->
    case enableDisableToTxOutTableType v of
      Just g -> pure (TxOutTableTypeConfig g)
      Nothing -> fail $ "unexpected use_address_table: " <> show v

instance ToJSON TxOutTableTypeConfig where
  toJSON = addressTypeToEnableDisable . unTxOutTableTypeConfig

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
  def = SyncInsertConfig Nothing def

instance Default SyncInsertOptions where
  def =
    SyncInsertOptions
      { sioTxCBOR = TxCBORConfig False
      , sioTxOut = TxOutEnable (UseTxOutAddress False)
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

fullInsertOptions :: SyncInsertOptions
fullInsertOptions =
  SyncInsertOptions
    { sioTxCBOR = TxCBORConfig False
    , sioTxOut = TxOutEnable (UseTxOutAddress False)
    , sioLedger = LedgerEnable
    , sioShelley = ShelleyEnable
    , sioRewards = RewardsConfig True
    , sioMultiAsset = MultiAssetEnable
    , sioMetadata = MetadataEnable
    , sioPlutus = PlutusEnable
    , sioGovernance = GovernanceConfig True
    , sioOffchainPoolData = OffchainPoolDataConfig True
    , sioPoolStats = PoolStatsConfig True
    , sioJsonType = JsonTypeText
    , sioRemoveJsonbFromSchema = RemoveJsonbFromSchemaConfig False
    }

onlyUTxOInsertOptions :: SyncInsertOptions
onlyUTxOInsertOptions =
  SyncInsertOptions
    { sioTxCBOR = TxCBORConfig False
    , sioTxOut = TxOutConsumedBootstrap (ForceTxIn False) (UseTxOutAddress False)
    , sioLedger = LedgerIgnore
    , sioShelley = ShelleyDisable
    , sioRewards = RewardsConfig True
    , sioMultiAsset = MultiAssetEnable
    , sioMetadata = MetadataDisable
    , sioPlutus = PlutusDisable
    , sioGovernance = GovernanceConfig False
    , sioOffchainPoolData = OffchainPoolDataConfig False
    , sioPoolStats = PoolStatsConfig False
    , sioJsonType = JsonTypeText
    , sioRemoveJsonbFromSchema = RemoveJsonbFromSchemaConfig False
    }

onlyGovInsertOptions :: SyncInsertOptions
onlyGovInsertOptions =
  disableAllInsertOptions
    { sioLedger = LedgerEnable
    , sioGovernance = GovernanceConfig True
    , sioPoolStats = PoolStatsConfig True
    }

disableAllInsertOptions :: SyncInsertOptions
disableAllInsertOptions =
  SyncInsertOptions
    { sioTxCBOR = TxCBORConfig False
    , sioTxOut = TxOutDisable
    , sioLedger = LedgerDisable
    , sioShelley = ShelleyDisable
    , sioRewards = RewardsConfig False
    , sioMultiAsset = MultiAssetDisable
    , sioMetadata = MetadataDisable
    , sioPlutus = PlutusDisable
    , sioOffchainPoolData = OffchainPoolDataConfig False
    , sioPoolStats = PoolStatsConfig False
    , sioGovernance = GovernanceConfig False
    , sioJsonType = JsonTypeText
    , sioRemoveJsonbFromSchema = RemoveJsonbFromSchemaConfig False
    }

addressTypeToEnableDisable :: IsString s => TxOutTableType -> s
addressTypeToEnableDisable TxOutVariantAddress = "enable"
addressTypeToEnableDisable TxOutCore = "disable"

enableDisableToTxOutTableType :: (Eq s, IsString s) => s -> Maybe TxOutTableType
enableDisableToTxOutTableType = \case
  "enable" -> Just TxOutVariantAddress
  "disable" -> Just TxOutCore
  _ -> Nothing

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
