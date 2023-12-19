{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

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
  OffChainPoolResult (..),
  OffChainVoteResult (..),
  OffChainHashType (..),
  OffChainUrlType (..),
  OffChainFetchError (..),
  SlotDetails (..),
  TipInfo (..),
  SyncState (..),
  TPraosStandard,
  MetricSetters (..),
  OffChainWorkQueueType (..),
  OffChainPoolWorkQueue (..),
  OffChainVoteWorkQueue (..),
  SimplifiedOffChainDataType (..),
  SimplifiedOffChainPoolData (..),
  SimplifiedOffChainVoteData (..),
  PraosStandard,
  Retry (..),
) where

import Cardano.Db (
  OffChainPoolData,
  OffChainPoolFetchError,
  OffChainVoteData,
  OffChainVoteFetchError,
  PoolHashId,
  PoolMetaHash,
  PoolMetadataRefId,
  PoolUrl,
  VoteMetaHash,
  VoteUrl,
  VotingAnchorId,
 )
import qualified Cardano.Ledger.Credential as Ledger
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Hashes as Ledger
import Cardano.Ledger.Keys
import Cardano.Prelude hiding (Meta, show)
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

-------------------------------------------------------------------------------------
-- OffChain
-------------------------------------------------------------------------------------
data OffChainPoolResult
  = OffChainPoolResultMetadata !OffChainPoolData
  | OffChainPoolResultError !OffChainPoolFetchError

data OffChainVoteResult
  = OffChainVoteResultMetadata !OffChainVoteData
  | OffChainVoteResultError !OffChainVoteFetchError

data OffChainWorkQueueType
  = OffChainPoolWorkQueueType !OffChainPoolWorkQueue
  | OffChainVoteWorkQueueType !OffChainVoteWorkQueue
  deriving (Show)

data OffChainPoolWorkQueue = OffChainPoolWorkQueue
  { oPoolWqHashId :: !PoolHashId
  , oPoolWqReferenceId :: !PoolMetadataRefId
  , oPoolWqUrl :: !PoolUrl
  , oPoolWqMetaHash :: !PoolMetaHash
  , oPoolWqRetry :: !Retry
  }
  deriving (Show)

data OffChainVoteWorkQueue = OffChainVoteWorkQueue
  { oVoteWqMetaHash :: !VoteMetaHash
  , oVoteWqReferenceId :: !VotingAnchorId
  , oVoteWqRetry :: !Retry
  , oVoteWqUrl :: !VoteUrl
  }
  deriving (Show)

data SimplifiedOffChainDataType
  = SimplifiedOffChainPoolDataType !SimplifiedOffChainPoolData
  | SimplifiedOffChainVoteDataType !SimplifiedOffChainVoteData

data SimplifiedOffChainPoolData = SimplifiedOffChainPoolData
  { spodTickerName :: !Text
  , spodHash :: !ByteString
  , spodBytes :: !ByteString
  , spodJson :: !Text
  , spodContentType :: !(Maybe ByteString)
  }

data SimplifiedOffChainVoteData = SimplifiedOffChainVoteData
  { sovaHash :: !ByteString
  , sovaBytes :: !ByteString
  , sovaJson :: !Text
  , sovaContentType :: !(Maybe ByteString)
  , sovaWarning :: !(Maybe Text)
  }

data Retry = Retry
  { retryFetchTime :: !POSIXTime -- Time last time time
  , retryRetryTime :: !POSIXTime -- Time to retry
  , retryCount :: !Word
  }
  deriving (Eq, Show, Generic)

data OffChainUrlType
  = OffChainPoolUrl !PoolUrl
  | OffChainVoteUrl !VoteUrl
  deriving (Eq, Generic)

data OffChainHashType
  = OffChainPoolHash PoolMetaHash
  | OffChainVoteHash VoteMetaHash
  deriving (Eq, Generic)

instance Show OffChainUrlType where
  show =
    \case
      OffChainPoolUrl url -> show url
      OffChainVoteUrl url -> show url

-------------------------------------------------------------------------------------
-- OffChain Fetch error for the HTTP client fetching the pool offchain metadata.
-------------------------------------------------------------------------------------
data OffChainFetchError
  = OCFErrHashMismatch !OffChainUrlType !Text !Text
  | OCFErrDataTooLong !OffChainUrlType
  | OCFErrUrlParseFail !OffChainUrlType !Text
  | OCFErrJsonDecodeFail !OffChainUrlType !Text
  | OCFErrHttpException !OffChainUrlType !Text
  | OCFErrHttpResponse !OffChainUrlType !Int !Text
  | OCFErrBadContentType !OffChainUrlType !Text
  | OCFErrBadContentTypeHtml !OffChainUrlType !Text
  | OCFErrIOException !Text
  | OCFErrTimeout !OffChainUrlType !Text
  | OCFErrConnectionFailure !OffChainUrlType
  deriving (Eq, Generic)

instance Exception OffChainFetchError

instance Show OffChainFetchError where
  show =
    \case
      OCFErrHashMismatch url xpt act ->
        mconcat
          [ "Hash mismatch when fetching metadata from "
          , show url
          , ". Expected "
          , show xpt
          , " but got "
          , show act
          , "."
          ]
      OCFErrDataTooLong url ->
        mconcat
          [fetchUrlToString url, "Size error, fetching metadata from ", show url, " exceeded 512 bytes."]
      OCFErrUrlParseFail url err ->
        mconcat
          [fetchUrlToString url, "URL parse error for ", show url, " resulted in : ", show err]
      OCFErrJsonDecodeFail url err ->
        mconcat
          [fetchUrlToString url, "JSON decode error from when fetching metadata from ", show url, " resulted in : ", show err]
      OCFErrHttpException url err ->
        mconcat [fetchUrlToString url, "HTTP Exception error for ", show url, " resulted in : ", show err]
      OCFErrHttpResponse url sc msg ->
        mconcat [fetchUrlToString url, "HTTP Response error from ", show url, " resulted in HTTP status code : ", show sc, " ", show msg]
      OCFErrBadContentType url ct ->
        mconcat [fetchUrlToString url, "HTTP Response error from ", show url, ": expected JSON, but got : ", show ct]
      OCFErrBadContentTypeHtml url ct ->
        mconcat [fetchUrlToString url, "HTTP Response error from ", show url, ": expected JSON, but got : ", show ct]
      OCFErrTimeout url ctx ->
        mconcat [fetchUrlToString url, "Timeout error when fetching metadata from ", show url, ": ", show ctx]
      OCFErrConnectionFailure url ->
        mconcat
          [fetchUrlToString url, "Connection failure error when fetching metadata from ", show url, "'."]
      OCFErrIOException err -> "IO Exception: " <> show err

fetchUrlToString :: OffChainUrlType -> String
fetchUrlToString url =
  case url of
    OffChainPoolUrl _ -> "Error Offchain Pool: "
    OffChainVoteUrl _ -> "Error Offchain Voting Anchor: "
