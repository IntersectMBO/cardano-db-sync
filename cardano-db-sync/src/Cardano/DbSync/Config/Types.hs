{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
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
  SyncNodeConfig (..),
  SyncPreConfig (..),
  LedgerStateDir (..),
  LogFileDir (..),
  NetworkName (..),
  NodeConfigFile (..),
  SocketPath (..),
  adjustGenesisFilePath,
  adjustNodeConfigFilePath,
  pcNodeConfigFilePath,
) where

import qualified Cardano.BM.Configuration as Logging
import qualified Cardano.BM.Data.Configuration as Logging
import qualified Cardano.Chain.Update as Byron
import Cardano.Crypto (RequiresNetworkMagic (..))
import qualified Cardano.Crypto.Hash as Crypto
import Cardano.Db (MigrationDir, PGPassSource (..))
import Cardano.Prelude
import Cardano.Slotting.Slot (SlotNo (..))
import Data.Aeson (FromJSON (..), Object, Value (..), (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser, typeMismatch)
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
  , enpShouldUseLedger :: !Bool
  , enpSkipFix :: !Bool
  , enpOnlyFix :: !Bool
  , enpForceIndexes :: !Bool
  , enpHasMultiAssets :: !Bool
  , enpHasMetadata :: !Bool
  , enpHasPlutusExtra :: !Bool
  , enpHasOfflineData :: !Bool
  , enpTurboMode :: !Bool
  , enpFullMode :: !Bool
  , enpMigrateConsumed :: !Bool
  , enpPruneTxOut :: !Bool
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
  , dncByronProtocolVersion :: !Byron.ProtocolVersion
  , dncShelleyHardFork :: !TriggerHardFork
  , dncAllegraHardFork :: !TriggerHardFork
  , dncMaryHardFork :: !TriggerHardFork
  , dncAlonzoHardFork :: !TriggerHardFork
  , dncBabbageHardFork :: !TriggerHardFork
  , dncConwayHardFork :: !TriggerHardFork
  }

data SyncPreConfig = SyncPreConfig
  { pcNetworkName :: !NetworkName
  , pcLoggingConfig :: !Logging.Representation
  , pcNodeConfigFile :: !NodeConfigFile
  , pcEnableLogging :: !Bool
  , pcEnableMetrics :: !Bool
  , pcPrometheusPort :: !Int
  }

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath
  }
  deriving (Show)

newtype GenesisHashByron = GenesisHashByron
  { unGenesisHashByron :: Text
  }
  deriving newtype (Eq, Show)

newtype GenesisHashShelley = GenesisHashShelley
  { unGenesisHashShelley :: Crypto.Hash Crypto.Blake2b_256 ByteString
  }
  deriving newtype (Eq, Show)

newtype GenesisHashAlonzo = GenesisHashAlonzo
  { unGenesisHashAlonzo :: Crypto.Hash Crypto.Blake2b_256 ByteString
  }
  deriving newtype (Eq, Show)

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

-- -------------------------------------------------------------------------------------------------

instance FromJSON SyncPreConfig where
  parseJSON =
    Aeson.withObject "top-level" parseGenSyncNodeConfig

parseGenSyncNodeConfig :: Object -> Parser SyncPreConfig
parseGenSyncNodeConfig o =
  SyncPreConfig
    <$> fmap NetworkName (o .: "NetworkName")
    <*> parseJSON (Object o)
    <*> fmap NodeConfigFile (o .: "NodeConfigFile")
    <*> o .: "EnableLogging"
    <*> o .: "EnableLogMetrics"
    <*> fmap (fromMaybe 8080) (o .:? "PrometheusPort")

instance FromJSON SyncProtocol where
  parseJSON o =
    case o of
      String "Cardano" -> pure SyncProtocolCardano
      x -> typeMismatch "Protocol" x
