{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Db.Schema.Core.OffChain where

import Contravariant.Extras (contrazip3, contrazip4, contrazip5, contrazip6, contrazip8)
import Data.ByteString.Char8 (ByteString)
import Data.Functor.Contravariant
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Hasql.Encoders as E

import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Schema.Types (utcTimeAsTimestampEncoder)
import Cardano.Db.Statement.Function.Core (bulkEncoder)
import Cardano.Db.Statement.Types (DbInfo (..), Entity (..), Key)

-----------------------------------------------------------------------------------------------------------------------------------
-- OFFCHAIN
-- These tables manage off-chain data, including pool and vote data.
----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: off_chain_pool_data
-- Description:
data OffChainPoolData = OffChainPoolData
  { offChainPoolDataPoolId :: !Id.PoolHashId -- noreference
  , offChainPoolDataTickerName :: !Text
  , offChainPoolDataHash :: !ByteString -- sqltype=hash32type
  , offChainPoolDataJson :: !Text -- sqltype=jsonb
  , offChainPoolDataBytes :: !ByteString -- sqltype=bytea
  , offChainPoolDataPmrId :: !Id.PoolMetadataRefId -- noreference
  }
  deriving (Eq, Show, Generic)

type instance Key OffChainPoolData = Id.OffChainPoolDataId

instance DbInfo OffChainPoolData where
  uniqueFields _ = ["pool_id", "pmr_id"]
  jsonbFields _ = ["json"]

offChainPoolDataEncoder :: E.Params OffChainPoolData
offChainPoolDataEncoder =
  mconcat
    [ offChainPoolDataPoolId >$< Id.idEncoder Id.getPoolHashId
    , offChainPoolDataTickerName >$< E.param (E.nonNullable E.text)
    , offChainPoolDataHash >$< E.param (E.nonNullable E.bytea)
    , offChainPoolDataJson >$< E.param (E.nonNullable E.text)
    , offChainPoolDataBytes >$< E.param (E.nonNullable E.bytea)
    , offChainPoolDataPmrId >$< Id.idEncoder Id.getPoolMetadataRefId
    ]

-- |
-- Table Name: off_chain_pool_fetch_error
-- Description:
-- The pool metadata fetch error. We duplicate the poolId for easy access.
-- TODO(KS): Debatable whether we need to persist this between migrations!
data OffChainPoolFetchError = OffChainPoolFetchError
  { offChainPoolFetchErrorPoolId :: !Id.PoolHashId -- noreference
  , offChainPoolFetchErrorFetchTime :: !UTCTime -- sqltype=timestamp
  , offChainPoolFetchErrorPmrId :: !Id.PoolMetadataRefId -- noreference
  , offChainPoolFetchErrorFetchError :: !Text
  , offChainPoolFetchErrorRetryCount :: !Word -- sqltype=word31type
  }
  deriving (Eq, Show, Generic)

type instance Key OffChainPoolFetchError = Id.OffChainPoolFetchErrorId
instance DbInfo OffChainPoolFetchError where
  uniqueFields _ = ["pool_id", "fetch_time", "retry_count"]

offChainPoolFetchErrorEncoder :: E.Params OffChainPoolFetchError
offChainPoolFetchErrorEncoder =
  mconcat
    [ offChainPoolFetchErrorPoolId >$< Id.idEncoder Id.getPoolHashId
    , offChainPoolFetchErrorFetchTime >$< E.param (E.nonNullable utcTimeAsTimestampEncoder)
    , offChainPoolFetchErrorPmrId >$< Id.idEncoder Id.getPoolMetadataRefId
    , offChainPoolFetchErrorFetchError >$< E.param (E.nonNullable E.text)
    , offChainPoolFetchErrorRetryCount >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-- |
-- Table Name: off_chain_vote_data
-- Description: Stores off-chain voting anchor data with validation status
--
-- The is_valid column indicates the parsing status:
--   • TRUE: Content is valid JSON AND conforms to CIP schema
--           All related tables are populated
--   • FALSE: Content is valid JSON BUT does not conform to CIP schema
--           The json column contains the actual JSON, but related tables are empty
--   • NULL: Content is not valid JSON at all
--           The json column contains an error message, bytes column has raw data
data OffChainVoteData = OffChainVoteData
  { offChainVoteDataVotingAnchorId :: !Id.VotingAnchorId -- noreference
  , offChainVoteDataHash :: !ByteString
  , offChainVoteDataJson :: !Text -- sqltype=jsonb
  , offChainVoteDataBytes :: !ByteString -- sqltype=bytea
  , offChainVoteDataWarning :: !(Maybe Text)
  , offChainVoteDataLanguage :: !Text
  , offChainVoteDataComment :: !(Maybe Text)
  , offChainVoteDataIsValid :: !(Maybe Bool)
  }
  deriving (Eq, Show, Generic)

type instance Key OffChainVoteData = Id.OffChainVoteDataId

instance DbInfo OffChainVoteData where
  uniqueFields _ = ["hash", "voting_anchor_id"]
  jsonbFields _ = ["json"]
  unnestParamTypes _ =
    [ ("voting_anchor_id", "bigint[]")
    , ("hash", "bytea[]")
    , ("json", "text[]")
    , ("bytes", "bytea[]")
    , ("warning", "text[]")
    , ("language", "text[]")
    , ("comment", "text[]")
    , ("is_valid", "boolean[]")
    ]

offChainVoteDataBulkEncoder :: E.Params ([Id.VotingAnchorId], [ByteString], [Text], [ByteString], [Maybe Text], [Text], [Maybe Text], [Maybe Bool])
offChainVoteDataBulkEncoder =
  contrazip8
    (bulkEncoder (Id.idBulkEncoder Id.getVotingAnchorId))
    (bulkEncoder (E.nonNullable E.bytea))
    (bulkEncoder (E.nonNullable E.text))
    (bulkEncoder (E.nonNullable E.bytea))
    (bulkEncoder (E.nullable E.text))
    (bulkEncoder (E.nonNullable E.text))
    (bulkEncoder (E.nullable E.text))
    (bulkEncoder (E.nullable E.bool))

-- |
-- Table Name: off_chain_vote_gov_action_data
-- Description:
data OffChainVoteGovActionData = OffChainVoteGovActionData
  { offChainVoteGovActionDataOffChainVoteDataId :: !Id.OffChainVoteDataId -- noreference
  , offChainVoteGovActionDataTitle :: !Text
  , offChainVoteGovActionDataAbstract :: !Text
  , offChainVoteGovActionDataMotivation :: !Text
  , offChainVoteGovActionDataRationale :: !Text
  }
  deriving (Eq, Show, Generic)

type instance Key OffChainVoteGovActionData = Id.OffChainVoteGovActionDataId

instance DbInfo OffChainVoteGovActionData where
  unnestParamTypes _ =
    [ ("off_chain_vote_data_id", "bigint[]")
    , ("title", "text[]")
    , ("abstract", "text[]")
    , ("motivation", "text[]")
    , ("rationale", "text[]")
    ]

entityOffChainVoteGovActionDataEncoder :: E.Params (Entity OffChainVoteGovActionData)
entityOffChainVoteGovActionDataEncoder =
  mconcat
    [ entityKey >$< Id.idEncoder Id.getOffChainVoteGovActionDataId
    , entityVal >$< offChainVoteGovActionDataEncoder
    ]

offChainVoteGovActionDataEncoder :: E.Params OffChainVoteGovActionData
offChainVoteGovActionDataEncoder =
  mconcat
    [ offChainVoteGovActionDataOffChainVoteDataId >$< Id.idEncoder Id.getOffChainVoteDataId
    , offChainVoteGovActionDataTitle >$< E.param (E.nonNullable E.text)
    , offChainVoteGovActionDataAbstract >$< E.param (E.nonNullable E.text)
    , offChainVoteGovActionDataMotivation >$< E.param (E.nonNullable E.text)
    , offChainVoteGovActionDataRationale >$< E.param (E.nonNullable E.text)
    ]

offChainVoteGovActionDataBulkEncoder :: E.Params ([Id.OffChainVoteDataId], [Text], [Text], [Text], [Text])
offChainVoteGovActionDataBulkEncoder =
  contrazip5
    (bulkEncoder (Id.idBulkEncoder Id.getOffChainVoteDataId))
    (bulkEncoder (E.nonNullable E.text))
    (bulkEncoder (E.nonNullable E.text))
    (bulkEncoder (E.nonNullable E.text))
    (bulkEncoder (E.nonNullable E.text))

-- |
-- Table Name: off_chain_vote_drep_data
-- Description:
data OffChainVoteDrepData = OffChainVoteDrepData
  { offChainVoteDrepDataOffChainVoteDataId :: !Id.OffChainVoteDataId -- noreference
  , offChainVoteDrepDataPaymentAddress :: !(Maybe Text)
  , offChainVoteDrepDataGivenName :: !Text
  , offChainVoteDrepDataObjectives :: !(Maybe Text)
  , offChainVoteDrepDataMotivations :: !(Maybe Text)
  , offChainVoteDrepDataQualifications :: !(Maybe Text)
  , offChainVoteDrepDataImageUrl :: !(Maybe Text)
  , offChainVoteDrepDataImageHash :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)

type instance Key OffChainVoteDrepData = Id.OffChainVoteDrepDataId
instance DbInfo OffChainVoteDrepData where
  unnestParamTypes _ =
    [ ("off_chain_vote_data_id", "bigint[]")
    , ("payment_address", "text[]")
    , ("given_name", "text[]")
    , ("objectives", "text[]")
    , ("motivations", "text[]")
    , ("qualifications", "text[]")
    , ("image_url", "text[]")
    , ("image_hash", "text[]")
    ]

offChainVoteDrepDataBulkEncoder :: E.Params ([Id.OffChainVoteDataId], [Maybe Text], [Text], [Maybe Text], [Maybe Text], [Maybe Text], [Maybe Text], [Maybe Text])
offChainVoteDrepDataBulkEncoder =
  contrazip8
    (bulkEncoder (Id.idBulkEncoder Id.getOffChainVoteDataId))
    (bulkEncoder (E.nullable E.text))
    (bulkEncoder (E.nonNullable E.text))
    (bulkEncoder (E.nullable E.text))
    (bulkEncoder (E.nullable E.text))
    (bulkEncoder (E.nullable E.text))
    (bulkEncoder (E.nullable E.text))
    (bulkEncoder (E.nullable E.text))

-- |
-- Table Name: off_chain_vote_author
-- Description:
data OffChainVoteAuthor = OffChainVoteAuthor
  { offChainVoteAuthorOffChainVoteDataId :: !Id.OffChainVoteDataId -- noreference
  , offChainVoteAuthorName :: !(Maybe Text)
  , offChainVoteAuthorWitnessAlgorithm :: !Text
  , offChainVoteAuthorPublicKey :: !Text
  , offChainVoteAuthorSignature :: !Text
  , offChainVoteAuthorWarning :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)

type instance Key OffChainVoteAuthor = Id.OffChainVoteAuthorId

instance DbInfo OffChainVoteAuthor where
  unnestParamTypes _ =
    [ ("off_chain_vote_data_id", "bigint[]")
    , ("name", "text[]")
    , ("witness_algorithm", "text[]")
    , ("public_key", "text[]")
    , ("signature", "text[]")
    , ("warning", "text[]")
    ]

offChainVoteAuthorBulkEncoder ::
  E.Params ([Id.OffChainVoteDataId], [Maybe Text], [Text], [Text], [Text], [Maybe Text])
offChainVoteAuthorBulkEncoder =
  contrazip6
    (bulkEncoder $ Id.idBulkEncoder Id.getOffChainVoteDataId)
    (bulkEncoder $ E.nullable E.text)
    (bulkEncoder $ E.nonNullable E.text)
    (bulkEncoder $ E.nonNullable E.text)
    (bulkEncoder $ E.nonNullable E.text)
    (bulkEncoder $ E.nullable E.text)

-- |
-- Table Name: off_chain_vote_reference
-- Description:
data OffChainVoteReference = OffChainVoteReference
  { offChainVoteReferenceOffChainVoteDataId :: !Id.OffChainVoteDataId -- noreference
  , offChainVoteReferenceLabel :: !Text
  , offChainVoteReferenceUri :: !Text
  , offChainVoteReferenceHashDigest :: !(Maybe Text)
  , offChainVoteReferenceHashAlgorithm :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)

type instance Key OffChainVoteReference = Id.OffChainVoteReferenceId
instance DbInfo OffChainVoteReference where
  unnestParamTypes _ =
    [ ("off_chain_vote_data_id", "bigint[]")
    , ("label", "text[]")
    , ("uri", "text[]")
    , ("hash_digest", "text[]")
    , ("hash_algorithm", "text[]")
    ]

offChainVoteReferenceBulkEncoder :: E.Params ([Id.OffChainVoteDataId], [Text], [Text], [Maybe Text], [Maybe Text])
offChainVoteReferenceBulkEncoder =
  contrazip5
    (bulkEncoder $ Id.idBulkEncoder Id.getOffChainVoteDataId)
    (bulkEncoder $ E.nonNullable E.text)
    (bulkEncoder $ E.nonNullable E.text)
    (bulkEncoder $ E.nullable E.text)
    (bulkEncoder $ E.nullable E.text)

-- |
-- Table Name: off_chain_vote_external_update
-- Description:
data OffChainVoteExternalUpdate = OffChainVoteExternalUpdate
  { offChainVoteExternalUpdateOffChainVoteDataId :: !Id.OffChainVoteDataId -- noreference
  , offChainVoteExternalUpdateTitle :: !Text
  , offChainVoteExternalUpdateUri :: !Text
  }
  deriving (Eq, Show, Generic)

type instance Key OffChainVoteExternalUpdate = Id.OffChainVoteExternalUpdateId
instance DbInfo OffChainVoteExternalUpdate where
  unnestParamTypes _ =
    [ ("off_chain_vote_data_id", "bigint[]")
    , ("title", "text[]")
    , ("uri", "text[]")
    ]

offChainVoteExternalUpdatesBulkEncoder :: E.Params ([Id.OffChainVoteDataId], [Text], [Text])
offChainVoteExternalUpdatesBulkEncoder =
  contrazip3
    (bulkEncoder $ Id.idBulkEncoder Id.getOffChainVoteDataId)
    (bulkEncoder $ E.nonNullable E.text)
    (bulkEncoder $ E.nonNullable E.text)

-- |
-- Table Name: off_chain_vote_fetch_error
-- Description:
data OffChainVoteFetchError = OffChainVoteFetchError
  { offChainVoteFetchErrorVotingAnchorId :: !Id.VotingAnchorId -- noreference
  , offChainVoteFetchErrorFetchError :: !Text
  , offChainVoteFetchErrorFetchTime :: !UTCTime -- sqltype=timestamp
  , offChainVoteFetchErrorRetryCount :: !Word -- sqltype=word31type
  }
  deriving (Eq, Show, Generic)

type instance Key OffChainVoteFetchError = Id.OffChainVoteFetchErrorId
instance DbInfo OffChainVoteFetchError where
  uniqueFields _ = ["voting_anchor_id", "retry_count"]
  unnestParamTypes _ =
    [ ("voting_anchor_id", "bigint[]")
    , ("fetch_error", "text[]")
    , ("fetch_time", "timestamp[]")
    , ("retry_count", "bigint[]")
    ]

offChainVoteFetchErrorBulkEncoder :: E.Params ([Id.VotingAnchorId], [Text], [UTCTime], [Word])
offChainVoteFetchErrorBulkEncoder =
  contrazip4
    (bulkEncoder (Id.idBulkEncoder Id.getVotingAnchorId))
    (bulkEncoder (E.nonNullable E.text))
    (bulkEncoder (E.nonNullable utcTimeAsTimestampEncoder))
    (bulkEncoder (E.nonNullable (fromIntegral >$< E.int4)))
