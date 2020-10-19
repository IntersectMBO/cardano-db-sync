{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.DbSync.Config.Types
  ( CardanoBlock
  , CardanoProtocol
  , ConfigFile (..)
  , DbSyncCommand (..)
  , DbSyncEnv (..)
  , DbSyncNodeParams (..)
  , DbSyncProtocol (..)
  , GenesisFile (..)
  , GenesisHashShelley (..)
  , GenesisHashByron (..)
  , DbSyncNodeConfig (..)
  , DbSyncPreConfig (..)
  , LedgerStateDir (..)
  , LogFileDir (..)
  , NetworkName (..)
  , NodeConfigFile (..)
  , SocketPath (..)
  , adjustGenesisFilePath
  , adjustNodeConfigFilePath
  , pcNodeConfigFilePath
  ) where

import qualified Cardano.BM.Configuration as Logging
import qualified Cardano.BM.Data.Configuration as Logging

import qualified Cardano.Chain.Update as Byron

import           Cardano.Crypto (RequiresNetworkMagic (..))
import qualified Cardano.Crypto.Hash as Crypto

import           Cardano.Db (MigrationDir (..))

import           Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))

import           Cardano.Prelude

import           Data.Aeson (FromJSON (..), Object, Value (..), (.:))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (Parser, typeMismatch)
import           Data.Text (Text)

import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))
import qualified Ouroboros.Consensus.Cardano.Block as Cardano
import qualified Ouroboros.Consensus.Cardano.CanHardFork as Shelley
import qualified Ouroboros.Consensus.HardFork.Combinator.Basics as Cardano
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Shelley
import qualified Ouroboros.Consensus.Shelley.Protocol as Shelley

import           Ouroboros.Network.Magic (NetworkMagic (..))

import qualified Shelley.Spec.Ledger.BaseTypes as Shelley


type CardanoBlock = Cardano.HardForkBlock (Cardano.CardanoEras Shelley.StandardCrypto)
type CardanoProtocol = Cardano.HardForkProtocol '[ByronBlock, Shelley.ShelleyBlock Shelley.StandardShelley]

newtype ConfigFile = ConfigFile
  { unConfigFile :: FilePath
  }

data DbSyncCommand
  = CmdRun !DbSyncNodeParams
  | CmdVersion

data DbSyncEnv = DbSyncEnv
  { envProtocol :: !DbSyncProtocol
  , envNetwork :: !Shelley.Network
  , envNetworkMagic :: !NetworkMagic
  , envSystemStart :: !SystemStart
  , envLedgerStateDir :: !LedgerStateDir
  }

-- | The product type of all command line arguments
data DbSyncNodeParams = DbSyncNodeParams
  { enpConfigFile :: !ConfigFile
  , enpSocketPath :: !SocketPath
  , enpLedgerStateDir :: !LedgerStateDir
  , enpMigrationDir :: !MigrationDir
  , enpMaybeRollback :: !(Maybe SlotNo)
  }

-- May have other constructors when we are preparing for a HFC event.
data DbSyncProtocol
  = DbSyncProtocolCardano
  deriving Show

data DbSyncNodeConfig = DbSyncNodeConfig
  { dncNetworkName :: !NetworkName
  , dncLoggingConfig :: !Logging.Configuration
  , dncNodeConfigFile :: !NodeConfigFile
  , dncProtocol :: !DbSyncProtocol
  , dncRequiresNetworkMagic :: !RequiresNetworkMagic
  , dncEnableLogging :: !Bool
  , dncEnableMetrics :: !Bool
  , dncByronGenesisFile :: !GenesisFile
  , dncByronGenesisHash :: !GenesisHashByron
  , dncShelleyGenesisFile :: !GenesisFile
  , dncShelleyGenesisHash :: !GenesisHashShelley
  , dncByronSoftwareVersion :: !Byron.SoftwareVersion
  , dncByronProtocolVersion :: !Byron.ProtocolVersion

  , dncShelleyHardFork :: !Shelley.TriggerHardFork
  , dncShelleyHardForkNotBeforeEpoch :: !(Maybe EpochNo)
  , dncShelleyMaxProtocolVersion :: !Natural
  }


data DbSyncPreConfig = DbSyncPreConfig
  { pcNetworkName :: !NetworkName
  , pcLoggingConfig :: !Logging.Representation
  , pcNodeConfigFile :: !NodeConfigFile
  , pcEnableLogging :: !Bool
  , pcEnableMetrics :: !Bool
  }

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath
  } deriving Show

newtype GenesisHashByron = GenesisHashByron
  { unGenesisHashByron :: Text
  } deriving newtype (Eq, Show)

newtype GenesisHashShelley = GenesisHashShelley
  { unGenesisHashShelley :: Crypto.Hash Crypto.Blake2b_256 ByteString
  } deriving newtype (Eq, Show)

newtype LedgerStateDir = LedgerStateDir
  {  unLedgerStateDir :: FilePath
  } deriving Show

newtype LogFileDir
  = LogFileDir FilePath

newtype NetworkName = NetworkName
  { unNetworkName :: Text
  } deriving Show

newtype NodeConfigFile = NodeConfigFile
  { unNodeConfigFile :: FilePath
  } deriving Show

newtype SocketPath = SocketPath
  { unSocketPath :: FilePath
  } deriving Show

adjustGenesisFilePath :: (FilePath -> FilePath) -> GenesisFile -> GenesisFile
adjustGenesisFilePath f (GenesisFile p) = GenesisFile (f p)

adjustNodeConfigFilePath :: (FilePath -> FilePath) -> NodeConfigFile -> NodeConfigFile
adjustNodeConfigFilePath f (NodeConfigFile p) = NodeConfigFile (f p)

pcNodeConfigFilePath :: DbSyncPreConfig -> FilePath
pcNodeConfigFilePath = unNodeConfigFile . pcNodeConfigFile

-- -------------------------------------------------------------------------------------------------

instance FromJSON DbSyncPreConfig where
  parseJSON o =
    Aeson.withObject "top-level" parseGenDbSyncNodeConfig o

parseGenDbSyncNodeConfig :: Object -> Parser DbSyncPreConfig
parseGenDbSyncNodeConfig o =
  DbSyncPreConfig
    <$> fmap NetworkName (o .: "NetworkName")
    <*> parseJSON (Object o)
    <*> fmap NodeConfigFile (o .: "NodeConfigFile")
    <*> o .: "EnableLogging"
    <*> o .: "EnableLogMetrics"

instance FromJSON DbSyncProtocol where
  parseJSON o =
    case o of
      String "Cardano" -> pure DbSyncProtocolCardano
      x -> typeMismatch "Protocol" x
