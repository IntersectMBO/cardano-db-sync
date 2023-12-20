{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Cardano.DbSync.Types (
  BlockDetails (..),
  BlockEra (..),
  CardanoBlock,
  CardanoPoint,
  StakeCred,
  PoolKeyHash,
  DataHash,
  CardanoInterpreter,
  EpochSlot (..),
  OffChainDataVariant (..),
  OffChainFetchResult (..),
  OffChainMetadata (..),
  OffChainError (..),
  SlotDetails (..),
  TipInfo (..),
  SyncState (..),
  TPraosStandard,
  MetricSetters (..),
  OffChainPoolFetchRetry (..),
  OffChainAnchorFetchRetry (..),
  PraosStandard,
  Retry (..),
) where

import Cardano.Db (
  OffChainAnchorData,
  OffChainAnchorFetchError,
  OffChainPoolData,
  OffChainPoolFetchError,
  PoolHashId,
  PoolMetaHash,
  PoolMetadataRefId,
  PoolUrl,
  VoteUrl,
  VotingAnchorId,
 )
import qualified Cardano.Ledger.Credential as Ledger
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Hashes as Ledger
import Cardano.Ledger.Keys
import Cardano.Prelude hiding (Meta)
import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Ouroboros.Consensus.Cardano.Block as Cardano
import qualified Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.Protocol.Praos (Praos)
import Ouroboros.Consensus.Protocol.TPraos (TPraos)
import Ouroboros.Consensus.Shelley.Protocol.Praos ()
import Ouroboros.Consensus.Shelley.Protocol.TPraos ()
import Ouroboros.Network.Block (BlockNo, Point)

type TPraosStandard = TPraos StandardCrypto

type PraosStandard = Praos StandardCrypto

type CardanoBlock = Cardano.CardanoBlock StandardCrypto

type CardanoInterpreter =
  History.Interpreter (Cardano.CardanoEras StandardCrypto)

type CardanoPoint = Point CardanoBlock

type StakeCred = Ledger.StakeCredential StandardCrypto

type PoolKeyHash = KeyHash 'StakePool StandardCrypto

type DataHash = Ledger.DataHash StandardCrypto

data BlockDetails = BlockDetails
  { bdBlock :: !CardanoBlock
  , bdSlot :: !SlotDetails
  }

data BlockEra
  = Byron
  | Shelley
  | Allegra
  | Mary
  | Alonzo
  | Babbage
  | Conway
  deriving (Eq, Show)

-- | Slot within an Epoch.
newtype EpochSlot = EpochSlot
  { unEpochSlot :: Word64
  }
  deriving (Eq, Ord, Show)

-- offChain
data OffChainDataVariant
  = OffChainPoolDataVariant
  | OffChainAnchorDataVariant

data OffChainFetchResult
  = OffChainPoolFetchResult
  | OffChainAnchorFetchResult
  -- = OffChainFetchMetadata !OffChainMetadata
  -- | OffChainFetchResultError !OffChainError

data OffChainMetadata
  = OffChainPoolMetadata OffChainPoolData
  | OffChainAnchorMetadata OffChainAnchorData

data OffChainError
  = OffChainPoolError OffChainPoolFetchError
  | OffChainAnchorError OffChainAnchorFetchError

data SlotDetails = SlotDetails
  { sdSlotTime :: !UTCTime
  , sdCurrentTime :: !UTCTime
  , sdEpochNo :: !EpochNo
  , sdSlotNo :: !SlotNo
  , sdEpochSlot :: !EpochSlot
  , sdEpochSize :: !EpochSize
  }
  deriving (Eq, Show)

-- The hash must be unique!
data TipInfo = TipInfo
  { bHash :: !ByteString
  , bEpochNo :: !EpochNo
  , bSlotNo :: !SlotNo
  , bBlockNo :: !BlockNo
  }
  deriving (Eq, Show)

-- The metrics we use.
-- Kept as a separate struct and do not put into environment because
-- when we need to test functions using this we need to initialize the
-- whole environment and not just pass in the layer. This shows clearly
-- that it needs to remain a separate parameter passed around where needed.
data MetricSetters = MetricSetters
  { metricsSetNodeBlockHeight :: BlockNo -> IO ()
  , metricsSetDbQueueLength :: Natural -> IO ()
  , metricsSetDbBlockHeight :: BlockNo -> IO ()
  , metricsSetDbSlotHeight :: SlotNo -> IO ()
  }

data SyncState = SyncLagging | SyncFollowing
  deriving (Eq, Show)

data OffChainPoolFetchRetry = OffChainPoolFetchRetry
  { opfrPoolHashId :: !PoolHashId
  , opfrReferenceId :: !PoolMetadataRefId
  , opfrPoolUrl :: !PoolUrl
  , opfrPoolMDHash :: !(Maybe PoolMetaHash)
  , opfrRetry :: !Retry
  }
  deriving (Show)

data OffChainAnchorFetchRetry = OffChainAnchorFetchRetry
  { oprfUrl :: VoteUrl
  , oprfHash :: ByteString
  , oprfReferenceId :: VotingAnchorId
  , oprfRetry :: !Retry
  }
  deriving (Show)

data Retry = Retry
  { retryFetchTime :: !POSIXTime -- Time last time time
  , retryRetryTime :: !POSIXTime -- Time to retry
  , retryCount :: !Word
  }
  deriving (Eq, Show, Generic)
