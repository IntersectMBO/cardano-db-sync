{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Sync.Config.Types
  ( AllegraToMary
  , ByronToShelley
  , CardanoBlock
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
  , MigrationDir (..)
  , LogFileDir (..)
  , NetworkName (..)
  , NodeConfigFile (..)
  , ShelleyToAllegra
  , SocketPath (..)
  , adjustGenesisFilePath
  , adjustNodeConfigFilePath
  , pcNodeConfigFilePath
  ) where

import           Cardano.Prelude

import qualified Cardano.BM.Configuration as Logging
import qualified Cardano.BM.Data.Configuration as Logging

import qualified Cardano.Chain.Update as Byron

import           Cardano.Crypto (RequiresNetworkMagic (..))
import qualified Cardano.Crypto.Hash as Crypto

import           Cardano.Slotting.Slot (SlotNo (..))

import           Data.Aeson (FromJSON (..), Object, Value (..), (.:), (.:?))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (Parser, typeMismatch)

import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))
import qualified Ouroboros.Consensus.Cardano.Block as Cardano
import qualified Ouroboros.Consensus.Cardano.CanHardFork as Shelley
import           Ouroboros.Consensus.Cardano.Node (ProtocolParamsTransition)
import qualified Ouroboros.Consensus.HardFork.Combinator.Basics as Cardano
import           Ouroboros.Consensus.Shelley.Eras (StandardShelley)
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Shelley
import qualified Ouroboros.Consensus.Shelley.Protocol as Shelley

import           Ouroboros.Network.Magic (NetworkMagic (..))

import qualified Shelley.Spec.Ledger.BaseTypes as Shelley


newtype MigrationDir = MigrationDir FilePath

newtype LogFileDir = LogFileDir FilePath

type CardanoBlock =
        Cardano.HardForkBlock
            (Cardano.CardanoEras Shelley.StandardCrypto)

type CardanoProtocol =
        Cardano.HardForkProtocol
            '[ ByronBlock
            , Shelley.ShelleyBlock StandardShelley
            , Shelley.ShelleyBlock Cardano.StandardAllegra
            , Shelley.ShelleyBlock Cardano.StandardMary
            ]

type ByronToShelley =
        ProtocolParamsTransition ByronBlock (Shelley.ShelleyBlock Cardano.StandardShelley)

type ShelleyToAllegra =
        ProtocolParamsTransition (Shelley.ShelleyBlock Cardano.StandardShelley) (Shelley.ShelleyBlock Cardano.StandardAllegra)

type AllegraToMary =
        ProtocolParamsTransition (Shelley.ShelleyBlock Cardano.StandardAllegra) (Shelley.ShelleyBlock Cardano.StandardMary)

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
  , dncPrometheusPort :: !Int
  , dncPBftSignatureThreshold :: !(Maybe Double)
  , dncByronGenesisFile :: !GenesisFile
  , dncByronGenesisHash :: !GenesisHashByron
  , dncShelleyGenesisFile :: !GenesisFile
  , dncShelleyGenesisHash :: !GenesisHashShelley
  , dncByronSoftwareVersion :: !Byron.SoftwareVersion
  , dncByronProtocolVersion :: !Byron.ProtocolVersion

  , dncShelleyHardFork :: !Shelley.TriggerHardFork
  , dncAllegraHardFork :: !Shelley.TriggerHardFork
  , dncMaryHardFork :: !Shelley.TriggerHardFork

  , dncByronToShelley :: !ByronToShelley
  , dncShelleyToAllegra :: !ShelleyToAllegra
  , dncAllegraToMary :: !AllegraToMary
  }

data DbSyncPreConfig = DbSyncPreConfig
  { pcNetworkName :: !NetworkName
  , pcLoggingConfig :: !Logging.Representation
  , pcNodeConfigFile :: !NodeConfigFile
  , pcEnableLogging :: !Bool
  , pcEnableMetrics :: !Bool
  , pcPrometheusPort :: !Int
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
    <*> fmap (fromMaybe 8080) (o .:? "PrometheusPort")

instance FromJSON DbSyncProtocol where
  parseJSON o =
    case o of
      String "Cardano" -> pure DbSyncProtocolCardano
      x -> typeMismatch "Protocol" x
