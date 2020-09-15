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
  , DbSyncEnv (..)
  , DbSyncNodeParams (..)
  , DbSyncNodeConfig
  , DbSyncProtocol (..)
  , GenesisFile (..)
  , GenesisHashShelley (..)
  , GenesisHashByron (..)
  , GenDbSyncNodeConfig (..)
  , LedgerStateDir (..)
  , LogFileDir (..)
  , NetworkName (..)
  , SocketPath (..)
  ) where

import qualified Cardano.BM.Configuration as Logging
import qualified Cardano.BM.Data.Configuration as Logging

import           Cardano.Crypto (RequiresNetworkMagic (..))
import qualified Cardano.Crypto.Hash as Crypto

import           Cardano.Db (MigrationDir (..))

import           Cardano.Slotting.Slot (SlotNo (..))

import           Cardano.Prelude

import           Data.Aeson (FromJSON (..), Object, Value (..), (.:))
import           Data.Aeson.Types (Parser, typeMismatch)
import qualified Data.Aeson as Aeson
import           Data.Text (Text)

import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))
import qualified Ouroboros.Consensus.Cardano.Block as Cardano
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

data DbSyncEnv = DbSyncEnv
  { envProtocol :: !DbSyncProtocol
  , envNetwork :: !Shelley.Network
  , envNetworkMagic :: !NetworkMagic
  , envSystemStart :: !SystemStart
  }

-- | The product type of all command line arguments
data DbSyncNodeParams = DbSyncNodeParams
  { enpConfigFile :: !ConfigFile
  , enpSocketPath :: !SocketPath
  , enpMigrationDir :: !MigrationDir
  , enpMaybeRollback :: !(Maybe SlotNo)
  }

-- May have other constructors when we are preparing for a HFC event.
data DbSyncProtocol
  = DbSyncProtocolCardano

type DbSyncNodeConfig = GenDbSyncNodeConfig Logging.Configuration

data GenDbSyncNodeConfig a = GenDbSyncNodeConfig
  { encNetworkName :: !NetworkName
  , encLoggingConfig :: !a
  , encProtocol :: !DbSyncProtocol
  , encByronGenesisFile :: !GenesisFile
  , encByronGenesisHash :: !GenesisHashByron
  , encShelleyGenesisFile :: !GenesisFile
  , encShelleyGenesisHash :: !GenesisHashShelley
  , encEnableLogging :: !Bool
  , encEnableMetrics :: !Bool
  , encRequiresNetworkMagic :: !RequiresNetworkMagic
  }

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath
  }

newtype GenesisHashByron = GenesisHashByron
  { unGenesisHashByron :: Text
  } deriving newtype (Eq, Show)

newtype GenesisHashShelley = GenesisHashShelley
  { unGenesisHashShelley :: Crypto.Hash Crypto.Blake2b_256 ByteString
  } deriving newtype (Eq, Show)

newtype LedgerStateDir = LedgerStateDir
  {  unLedgerStateDir :: FilePath
  }

newtype LogFileDir
  = LogFileDir FilePath

newtype NetworkName = NetworkName
  { unNetworkName :: Text
  }

newtype SocketPath = SocketPath
  { unSocketPath :: FilePath
  }

-- -------------------------------------------------------------------------------------------------

instance FromJSON (GenDbSyncNodeConfig Logging.Representation) where
  parseJSON o =
    Aeson.withObject "top-level" parseGenDbSyncNodeConfig o

parseGenDbSyncNodeConfig :: Object -> Parser (GenDbSyncNodeConfig Logging.Representation)
parseGenDbSyncNodeConfig o =
  GenDbSyncNodeConfig
    <$> fmap NetworkName (o .: "NetworkName")
    <*> parseJSON (Object o)
    <*> o .: "Protocol"
    <*> fmap GenesisFile (o .: "ByronGenesisFile")
    <*> fmap GenesisHashByron (o .: "ByronGenesisHash")
    <*> fmap GenesisFile (o .: "ShelleyGenesisFile")
    <*> fmap GenesisHashShelley (o .: "ShelleyGenesisHash")
    <*> o .: "EnableLogging"
    <*> o .: "EnableLogMetrics"
    <*> o .: "RequiresNetworkMagic"

instance FromJSON DbSyncProtocol where
  parseJSON o =
    case o of
      String "Cardano" -> pure DbSyncProtocolCardano
      x -> typeMismatch "Protocol" x
