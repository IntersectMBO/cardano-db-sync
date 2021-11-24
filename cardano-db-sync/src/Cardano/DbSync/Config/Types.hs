{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.DbSync.Config.Types
  ( AllegraToMary
  , ByronToShelley
  , CardanoBlock
  , CardanoProtocol
  , ConfigFile (..)
  , SyncCommand (..)
  , SyncNodeParams (..)
  , SyncProtocol (..)
  , GenesisFile (..)
  , GenesisHashShelley (..)
  , GenesisHashByron (..)
  , GenesisHashAlonzo (..)
  , SyncNodeConfig (..)
  , SyncPreConfig (..)
  , LedgerStateDir (..)
  , MigrationDir (..)
  , MaryToAlonzo
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

import           Cardano.Ledger.Allegra (AllegraEra)

import           Cardano.Slotting.Slot (SlotNo (..))

import           Data.Aeson (FromJSON (..), Object, Value (..), (.:), (.:?))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (Parser, typeMismatch)

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))
import           Ouroboros.Consensus.Cardano.Block (AlonzoEra, MaryEra, ShelleyEra)
import qualified Ouroboros.Consensus.Cardano.Block as Cardano
import qualified Ouroboros.Consensus.Cardano.CanHardFork as Shelley
import           Ouroboros.Consensus.Cardano.Node (ProtocolTransitionParamsShelleyBased)
import qualified Ouroboros.Consensus.HardFork.Combinator.Basics as Cardano
import           Ouroboros.Consensus.Shelley.Eras (StandardShelley)
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Shelley
import           Ouroboros.Consensus.Shelley.Protocol (StandardCrypto)

newtype MigrationDir = MigrationDir
  { unMigrationDir :: FilePath
  }

newtype LogFileDir = LogFileDir
  { unLogFileDir :: FilePath
  }

type CardanoBlock =
        Cardano.HardForkBlock
            (Cardano.CardanoEras StandardCrypto)

type CardanoProtocol =
        Cardano.HardForkProtocol
            '[ ByronBlock
            , Shelley.ShelleyBlock StandardShelley
            , Shelley.ShelleyBlock Cardano.StandardAllegra
            , Shelley.ShelleyBlock Cardano.StandardMary
            ]

type ByronToShelley =
        ProtocolTransitionParamsShelleyBased (ShelleyEra StandardCrypto)

type ShelleyToAllegra =
        ProtocolTransitionParamsShelleyBased (AllegraEra StandardCrypto)

type AllegraToMary =
        ProtocolTransitionParamsShelleyBased (MaryEra StandardCrypto)

type MaryToAlonzo =
        ProtocolTransitionParamsShelleyBased (AlonzoEra StandardCrypto)

newtype ConfigFile = ConfigFile
  { unConfigFile :: FilePath
  }

data SyncCommand
  = CmdRun !SyncNodeParams
  | CmdVersion

-- | The product type of all command line arguments
data SyncNodeParams = SyncNodeParams
  { enpConfigFile :: !ConfigFile
  , enpSocketPath :: !SocketPath
  , enpLedgerStateDir :: !LedgerStateDir
  , enpMigrationDir :: !MigrationDir
  , enpExtended :: !Bool
  , enpMaybeRollback :: !(Maybe SlotNo)
  }

-- May have other constructors when we are preparing for a HFC event.
data SyncProtocol
  = SyncProtocolCardano
  deriving Show

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
  , dncByronSoftwareVersion :: !Byron.SoftwareVersion
  , dncByronProtocolVersion :: !Byron.ProtocolVersion

  , dncShelleyHardFork :: !Shelley.TriggerHardFork
  , dncAllegraHardFork :: !Shelley.TriggerHardFork
  , dncMaryHardFork :: !Shelley.TriggerHardFork
  , dncAlonzoHardFork :: !Shelley.TriggerHardFork
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
  } deriving Show

newtype GenesisHashByron = GenesisHashByron
  { unGenesisHashByron :: Text
  } deriving newtype (Eq, Show)

newtype GenesisHashShelley = GenesisHashShelley
  { unGenesisHashShelley :: Crypto.Hash Crypto.Blake2b_256 ByteString
  } deriving newtype (Eq, Show)

newtype GenesisHashAlonzo = GenesisHashAlonzo
  { unGenesisHashAlonzo :: Crypto.Hash Crypto.Blake2b_256 ByteString
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

pcNodeConfigFilePath :: SyncPreConfig -> FilePath
pcNodeConfigFilePath = unNodeConfigFile . pcNodeConfigFile

-- -------------------------------------------------------------------------------------------------

instance FromJSON SyncPreConfig where
  parseJSON o =
    Aeson.withObject "top-level" parseGenSyncNodeConfig o

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
