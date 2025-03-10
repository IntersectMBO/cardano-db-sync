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

import Contravariant.Extras (contrazip3, contrazip5, contrazip6)
import Data.ByteString.Char8 (ByteString)
import Data.Functor.Contravariant
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Hasql.Decoders as D
import Hasql.Encoders as E

import Cardano.Db.Schema.Ids
import Cardano.Db.Schema.Orphans ()
import Cardano.Db.Statement.Function.Core (manyEncoder)
import Cardano.Db.Statement.Types (DbInfo (..), Entity (..), Key)

-----------------------------------------------------------------------------------------------------------------------------------
-- OFFCHAIN
-- These tables manage off-chain data, including pool and vote data.
----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: off_chain_pool_data
-- Description:
data OffChainPoolData = OffChainPoolData
  { offChainPoolDataPoolId :: !PoolHashId -- noreference
  , offChainPoolDataTickerName :: !Text
  , offChainPoolDataHash :: !ByteString -- sqltype=hash32type
  , offChainPoolDataJson :: !Text -- sqltype=jsonb
  , offChainPoolDataBytes :: !ByteString -- sqltype=bytea
  , offChainPoolDataPmrId :: !PoolMetadataRefId -- noreference
  }
  deriving (Eq, Show, Generic)

type instance Key OffChainPoolData = OffChainPoolDataId
instance DbInfo OffChainPoolData where
  uniqueFields _ = ["pool_id", "prm_id"]

entityNameOffChainPoolDataDecoder :: D.Row (Entity OffChainPoolData)
entityNameOffChainPoolDataDecoder =
  Entity
    <$> idDecoder OffChainPoolDataId
    <*> offChainPoolDataDecoder

offChainPoolDataDecoder :: D.Row OffChainPoolData
offChainPoolDataDecoder =
  OffChainPoolData
    <$> idDecoder PoolHashId -- offChainPoolDataPoolId
    <*> D.column (D.nonNullable D.text) -- offChainPoolDataTickerName
    <*> D.column (D.nonNullable D.bytea) -- offChainPoolDataHash
    <*> D.column (D.nonNullable D.text) -- offChainPoolDataJson
    <*> D.column (D.nonNullable D.bytea) -- offChainPoolDataBytes
    <*> idDecoder PoolMetadataRefId -- offChainPoolDataPmrId

entityNameOffChainPoolDataEncoder :: E.Params (Entity OffChainPoolData)
entityNameOffChainPoolDataEncoder =
  mconcat
    [ entityKey >$< idEncoder getOffChainPoolDataId
    , entityVal >$< offChainPoolDataEncoder
    ]

offChainPoolDataEncoder :: E.Params OffChainPoolData
offChainPoolDataEncoder =
  mconcat
    [ offChainPoolDataPoolId >$< idEncoder getPoolHashId
    , offChainPoolDataTickerName >$< E.param (E.nonNullable E.text)
    , offChainPoolDataHash >$< E.param (E.nonNullable E.bytea)
    , offChainPoolDataJson >$< E.param (E.nonNullable E.text)
    , offChainPoolDataBytes >$< E.param (E.nonNullable E.bytea)
    , offChainPoolDataPmrId >$< idEncoder getPoolMetadataRefId
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: off_chain_pool_fetch_error
-- Description:

-- The pool metadata fetch error. We duplicate the poolId for easy access.
-- TODO(KS): Debatable whether we need to persist this between migrations!
data OffChainPoolFetchError = OffChainPoolFetchError
  { offChainPoolFetchErrorPoolId :: !PoolHashId -- noreference
  , offChainPoolFetchErrorFetchTime :: !UTCTime -- sqltype=timestamp
  , offChainPoolFetchErrorPmrId :: !PoolMetadataRefId -- noreference
  , offChainPoolFetchErrorFetchError :: !Text
  , offChainPoolFetchErrorRetryCount :: !Word -- sqltype=word31type
  }
  deriving (Eq, Show, Generic)

type instance Key OffChainPoolFetchError = OffChainPoolFetchErrorId
instance DbInfo OffChainPoolFetchError where
  uniqueFields _ = ["pool_id", "fetch_time", "retry_count"]

entityNameOffChainPoolFetchErrorDecoder :: D.Row (Entity OffChainPoolFetchError)
entityNameOffChainPoolFetchErrorDecoder =
  Entity
    <$> idDecoder OffChainPoolFetchErrorId
    <*> offChainPoolFetchErrorDecoder

offChainPoolFetchErrorDecoder :: D.Row OffChainPoolFetchError
offChainPoolFetchErrorDecoder =
  OffChainPoolFetchError
    <$> idDecoder PoolHashId -- offChainPoolFetchErrorPoolId
    <*> D.column (D.nonNullable D.timestamptz) -- offChainPoolFetchErrorFetchTime
    <*> idDecoder PoolMetadataRefId -- offChainPoolFetchErrorPmrId
    <*> D.column (D.nonNullable D.text) -- offChainPoolFetchErrorFetchError
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- offChainPoolFetchErrorRetryCount

entityNameOffChainPoolFetchErrorEncoder :: E.Params (Entity OffChainPoolFetchError)
entityNameOffChainPoolFetchErrorEncoder =
  mconcat
    [ entityKey >$< idEncoder getOffChainPoolFetchErrorId
    , entityVal >$< offChainPoolFetchErrorEncoder
    ]

offChainPoolFetchErrorEncoder :: E.Params OffChainPoolFetchError
offChainPoolFetchErrorEncoder =
  mconcat
    [ offChainPoolFetchErrorPoolId >$< idEncoder getPoolHashId
    , offChainPoolFetchErrorFetchTime >$< E.param (E.nonNullable E.timestamptz)
    , offChainPoolFetchErrorPmrId >$< idEncoder getPoolMetadataRefId
    , offChainPoolFetchErrorFetchError >$< E.param (E.nonNullable E.text)
    , offChainPoolFetchErrorRetryCount >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: off_chain_vote_data
-- Description:
data OffChainVoteData = OffChainVoteData
  { offChainVoteDataVotingAnchorId :: !VotingAnchorId -- noreference
  , offChainVoteDataHash :: !ByteString
  , offChainVoteDataLanguage :: !Text
  , offChainVoteDataComment :: !(Maybe Text)
  , offChainVoteDataJson :: !Text -- sqltype=jsonb
  , offChainVoteDataBytes :: !ByteString -- sqltype=bytea
  , offChainVoteDataWarning :: !(Maybe Text)
  , offChainVoteDataIsValid :: !(Maybe Bool)
  }
  deriving (Eq, Show, Generic)

type instance Key OffChainVoteData = OffChainVoteDataId
instance DbInfo OffChainVoteData where
  uniqueFields _ = ["hash", "voting_anchor_id"]

entityNameOffChainVoteDataDecoder :: D.Row (Entity OffChainVoteData)
entityNameOffChainVoteDataDecoder =
  Entity
    <$> idDecoder OffChainVoteDataId
    <*> offChainVoteDataDecoder

offChainVoteDataDecoder :: D.Row OffChainVoteData
offChainVoteDataDecoder =
  OffChainVoteData
    <$> idDecoder VotingAnchorId -- offChainVoteDataVotingAnchorId
    <*> D.column (D.nonNullable D.bytea) -- offChainVoteDataHash
    <*> D.column (D.nonNullable D.text) -- offChainVoteDataLanguage
    <*> D.column (D.nullable D.text) -- offChainVoteDataComment
    <*> D.column (D.nonNullable D.text) -- offChainVoteDataJson
    <*> D.column (D.nonNullable D.bytea) -- offChainVoteDataBytes
    <*> D.column (D.nullable D.text) -- offChainVoteDataWarning
    <*> D.column (D.nullable D.bool) -- offChainVoteDataIsValid

entityNameOffChainVoteDataEncoder :: E.Params (Entity OffChainVoteData)
entityNameOffChainVoteDataEncoder =
  mconcat
    [ entityKey >$< idEncoder getOffChainVoteDataId
    , entityVal >$< offChainVoteDataEncoder
    ]

offChainVoteDataEncoder :: E.Params OffChainVoteData
offChainVoteDataEncoder =
  mconcat
    [ offChainVoteDataVotingAnchorId >$< idEncoder getVotingAnchorId
    , offChainVoteDataHash >$< E.param (E.nonNullable E.bytea)
    , offChainVoteDataLanguage >$< E.param (E.nonNullable E.text)
    , offChainVoteDataComment >$< E.param (E.nullable E.text)
    , offChainVoteDataJson >$< E.param (E.nonNullable E.text)
    , offChainVoteDataBytes >$< E.param (E.nonNullable E.bytea)
    , offChainVoteDataWarning >$< E.param (E.nullable E.text)
    , offChainVoteDataIsValid >$< E.param (E.nullable E.bool)
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: off_chain_vote_gov_action_data
-- Description:
data OffChainVoteGovActionData = OffChainVoteGovActionData
  { offChainVoteGovActionDataOffChainVoteDataId :: !OffChainVoteDataId -- noreference
  , offChainVoteGovActionDataTitle :: !Text
  , offChainVoteGovActionDataAbstract :: !Text
  , offChainVoteGovActionDataMotivation :: !Text
  , offChainVoteGovActionDataRationale :: !Text
  }
  deriving (Eq, Show, Generic)

type instance Key OffChainVoteGovActionData = OffChainVoteGovActionDataId
instance DbInfo OffChainVoteGovActionData

entityNameOffChainVoteGovActionDataDecoder :: D.Row (Entity OffChainVoteGovActionData)
entityNameOffChainVoteGovActionDataDecoder =
  Entity
    <$> idDecoder OffChainVoteGovActionDataId
    <*> offChainVoteGovActionDataDecoder

offChainVoteGovActionDataDecoder :: D.Row OffChainVoteGovActionData
offChainVoteGovActionDataDecoder =
  OffChainVoteGovActionData
    <$> idDecoder OffChainVoteDataId -- offChainVoteGovActionDataOffChainVoteDataId
    <*> D.column (D.nonNullable D.text) -- offChainVoteGovActionDataTitle
    <*> D.column (D.nonNullable D.text) -- offChainVoteGovActionDataAbstract
    <*> D.column (D.nonNullable D.text) -- offChainVoteGovActionDataMotivation
    <*> D.column (D.nonNullable D.text) -- offChainVoteGovActionDataRationale

entityNameOffChainVoteGovActionDataEncoder :: E.Params (Entity OffChainVoteGovActionData)
entityNameOffChainVoteGovActionDataEncoder =
  mconcat
    [ entityKey >$< idEncoder getOffChainVoteGovActionDataId
    , entityVal >$< offChainVoteGovActionDataEncoder
    ]

offChainVoteGovActionDataEncoder :: E.Params OffChainVoteGovActionData
offChainVoteGovActionDataEncoder =
  mconcat
    [ offChainVoteGovActionDataOffChainVoteDataId >$< idEncoder getOffChainVoteDataId
    , offChainVoteGovActionDataTitle >$< E.param (E.nonNullable E.text)
    , offChainVoteGovActionDataAbstract >$< E.param (E.nonNullable E.text)
    , offChainVoteGovActionDataMotivation >$< E.param (E.nonNullable E.text)
    , offChainVoteGovActionDataRationale >$< E.param (E.nonNullable E.text)
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: off_chain_vote_drep_data
-- Description:
data OffChainVoteDrepData = OffChainVoteDrepData
  { offChainVoteDrepDataOffChainVoteDataId :: !OffChainVoteDataId -- noreference
  , offChainVoteDrepDataPaymentAddress :: !(Maybe Text)
  , offChainVoteDrepDataGivenName :: !Text
  , offChainVoteDrepDataObjectives :: !(Maybe Text)
  , offChainVoteDrepDataMotivations :: !(Maybe Text)
  , offChainVoteDrepDataQualifications :: !(Maybe Text)
  , offChainVoteDrepDataImageUrl :: !(Maybe Text)
  , offChainVoteDrepDataImageHash :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)

type instance Key OffChainVoteDrepData = OffChainVoteDrepDataId
instance DbInfo OffChainVoteDrepData

entityNameOffChainVoteDrepDataDecoder :: D.Row (Entity OffChainVoteDrepData)
entityNameOffChainVoteDrepDataDecoder =
  Entity
    <$> idDecoder OffChainVoteDrepDataId
    <*> offChainVoteDrepDataDecoder

offChainVoteDrepDataDecoder :: D.Row OffChainVoteDrepData
offChainVoteDrepDataDecoder =
  OffChainVoteDrepData
    <$> idDecoder OffChainVoteDataId -- offChainVoteDrepDataOffChainVoteDataId
    <*> D.column (D.nullable D.text) -- offChainVoteDrepDataPaymentAddress
    <*> D.column (D.nonNullable D.text) -- offChainVoteDrepDataGivenName
    <*> D.column (D.nullable D.text) -- offChainVoteDrepDataObjectives
    <*> D.column (D.nullable D.text) -- offChainVoteDrepDataMotivations
    <*> D.column (D.nullable D.text) -- offChainVoteDrepDataQualifications
    <*> D.column (D.nullable D.text) -- offChainVoteDrepDataImageUrl
    <*> D.column (D.nullable D.text) -- offChainVoteDrepDataImageHash

entityNameOffChainVoteDrepDataEncoder :: E.Params (Entity OffChainVoteDrepData)
entityNameOffChainVoteDrepDataEncoder =
  mconcat
    [ entityKey >$< idEncoder getOffChainVoteDrepDataId
    , entityVal >$< offChainVoteDrepDataEncoder
    ]

offChainVoteDrepDataEncoder :: E.Params OffChainVoteDrepData
offChainVoteDrepDataEncoder =
  mconcat
    [ offChainVoteDrepDataOffChainVoteDataId >$< idEncoder getOffChainVoteDataId
    , offChainVoteDrepDataPaymentAddress >$< E.param (E.nullable E.text)
    , offChainVoteDrepDataGivenName >$< E.param (E.nonNullable E.text)
    , offChainVoteDrepDataObjectives >$< E.param (E.nullable E.text)
    , offChainVoteDrepDataMotivations >$< E.param (E.nullable E.text)
    , offChainVoteDrepDataQualifications >$< E.param (E.nullable E.text)
    , offChainVoteDrepDataImageUrl >$< E.param (E.nullable E.text)
    , offChainVoteDrepDataImageHash >$< E.param (E.nullable E.text)
    ]

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: off_chain_vote_author
-- Description:
data OffChainVoteAuthor = OffChainVoteAuthor
  { offChainVoteAuthorOffChainVoteDataId :: !OffChainVoteDataId -- noreference
  , offChainVoteAuthorName :: !(Maybe Text)
  , offChainVoteAuthorWitnessAlgorithm :: !Text
  , offChainVoteAuthorPublicKey :: !Text
  , offChainVoteAuthorSignature :: !Text
  , offChainVoteAuthorWarning :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)

type instance Key OffChainVoteAuthor = OffChainVoteAuthorId
instance DbInfo OffChainVoteAuthor

entityNameOffChainVoteAuthorDecoder :: D.Row (Entity OffChainVoteAuthor)
entityNameOffChainVoteAuthorDecoder =
  Entity
    <$> idDecoder OffChainVoteAuthorId
    <*> offChainVoteAuthorDecoder

offChainVoteAuthorDecoder :: D.Row OffChainVoteAuthor
offChainVoteAuthorDecoder =
  OffChainVoteAuthor
    <$> idDecoder OffChainVoteDataId -- offChainVoteAuthorOffChainVoteDataId
    <*> D.column (D.nullable D.text) -- offChainVoteAuthorName
    <*> D.column (D.nonNullable D.text) -- offChainVoteAuthorWitnessAlgorithm
    <*> D.column (D.nonNullable D.text) -- offChainVoteAuthorPublicKey
    <*> D.column (D.nonNullable D.text) -- offChainVoteAuthorSignature
    <*> D.column (D.nullable D.text) -- offChainVoteAuthorWarning

entityNameOffChainVoteAuthorEncoder :: E.Params (Entity OffChainVoteAuthor)
entityNameOffChainVoteAuthorEncoder =
  mconcat
    [ entityKey >$< idEncoder getOffChainVoteAuthorId
    , entityVal >$< offChainVoteAuthorEncoder
    ]

offChainVoteAuthorEncoder :: E.Params OffChainVoteAuthor
offChainVoteAuthorEncoder =
  mconcat
    [ offChainVoteAuthorOffChainVoteDataId >$< idEncoder getOffChainVoteDataId
    , offChainVoteAuthorName >$< E.param (E.nullable E.text)
    , offChainVoteAuthorWitnessAlgorithm >$< E.param (E.nonNullable E.text)
    , offChainVoteAuthorPublicKey >$< E.param (E.nonNullable E.text)
    , offChainVoteAuthorSignature >$< E.param (E.nonNullable E.text)
    , offChainVoteAuthorWarning >$< E.param (E.nullable E.text)
    ]

offChainVoteAuthorBulkEncoder ::
  E.Params ([OffChainVoteDataId], [Maybe Text], [Text], [Text], [Text], [Maybe Text])
offChainVoteAuthorBulkEncoder =
  contrazip6
    (manyEncoder $ idBulkEncoder getOffChainVoteDataId)
    (manyEncoder $ E.nullable E.text)
    (manyEncoder $ E.nonNullable E.text)
    (manyEncoder $ E.nonNullable E.text)
    (manyEncoder $ E.nonNullable E.text)
    (manyEncoder $ E.nullable E.text)

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: off_chain_vote_reference
-- Description:
data OffChainVoteReference = OffChainVoteReference
  { offChainVoteReferenceOffChainVoteDataId :: !OffChainVoteDataId -- noreference
  , offChainVoteReferenceLabel :: !Text
  , offChainVoteReferenceUri :: !Text
  , offChainVoteReferenceHashDigest :: !(Maybe Text)
  , offChainVoteReferenceHashAlgorithm :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)

type instance Key OffChainVoteReference = OffChainVoteReferenceId
instance DbInfo OffChainVoteReference

entityNameOffChainVoteReferenceDecoder :: D.Row (Entity OffChainVoteReference)
entityNameOffChainVoteReferenceDecoder =
  Entity
    <$> idDecoder OffChainVoteReferenceId
    <*> offChainVoteReferenceDecoder

offChainVoteReferenceDecoder :: D.Row OffChainVoteReference
offChainVoteReferenceDecoder =
  OffChainVoteReference
    <$> idDecoder OffChainVoteDataId -- offChainVoteReferenceOffChainVoteDataId
    <*> D.column (D.nonNullable D.text) -- offChainVoteReferenceLabel
    <*> D.column (D.nonNullable D.text) -- offChainVoteReferenceUri
    <*> D.column (D.nullable D.text) -- offChainVoteReferenceHashDigest
    <*> D.column (D.nullable D.text) -- offChainVoteReferenceHashAlgorithm

entityNameOffChainVoteReferenceEncoder :: E.Params (Entity OffChainVoteReference)
entityNameOffChainVoteReferenceEncoder =
  mconcat
    [ entityKey >$< idEncoder getOffChainVoteReferenceId
    , entityVal >$< offChainVoteReferenceEncoder
    ]

offChainVoteReferenceEncoder :: E.Params OffChainVoteReference
offChainVoteReferenceEncoder =
  mconcat
    [ offChainVoteReferenceOffChainVoteDataId >$< idEncoder getOffChainVoteDataId
    , offChainVoteReferenceLabel >$< E.param (E.nonNullable E.text)
    , offChainVoteReferenceUri >$< E.param (E.nonNullable E.text)
    , offChainVoteReferenceHashDigest >$< E.param (E.nullable E.text)
    , offChainVoteReferenceHashAlgorithm >$< E.param (E.nullable E.text)
    ]

offChainVoteReferenceBulkEncoder :: E.Params ([OffChainVoteDataId], [Text], [Text], [Maybe Text], [Maybe Text])
offChainVoteReferenceBulkEncoder =
  contrazip5
    (manyEncoder $ idBulkEncoder getOffChainVoteDataId)
    (manyEncoder $ E.nonNullable E.text)
    (manyEncoder $ E.nonNullable E.text)
    (manyEncoder $ E.nullable E.text)
    (manyEncoder $ E.nullable E.text)

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: off_chain_vote_external_update
-- Description:
data OffChainVoteExternalUpdate = OffChainVoteExternalUpdate
  { offChainVoteExternalUpdateOffChainVoteDataId :: !OffChainVoteDataId -- noreference
  , offChainVoteExternalUpdateTitle :: !Text
  , offChainVoteExternalUpdateUri :: !Text
  }
  deriving (Eq, Show, Generic)

type instance Key OffChainVoteExternalUpdate = OffChainVoteExternalUpdateId
instance DbInfo OffChainVoteExternalUpdate

entityNameOffChainVoteExternalUpdateDecoder :: D.Row (Entity OffChainVoteExternalUpdate)
entityNameOffChainVoteExternalUpdateDecoder =
  Entity
    <$> idDecoder OffChainVoteExternalUpdateId
    <*> offChainVoteExternalUpdateDecoder

offChainVoteExternalUpdateDecoder :: D.Row OffChainVoteExternalUpdate
offChainVoteExternalUpdateDecoder =
  OffChainVoteExternalUpdate
    <$> idDecoder OffChainVoteDataId -- offChainVoteExternalUpdateOffChainVoteDataId
    <*> D.column (D.nonNullable D.text) -- offChainVoteExternalUpdateTitle
    <*> D.column (D.nonNullable D.text) -- offChainVoteExternalUpdateUri

entityNameOffChainVoteExternalUpdateEncoder :: E.Params (Entity OffChainVoteExternalUpdate)
entityNameOffChainVoteExternalUpdateEncoder =
  mconcat
    [ entityKey >$< idEncoder getOffChainVoteExternalUpdateId
    , entityVal >$< offChainVoteExternalUpdateEncoder
    ]

offChainVoteExternalUpdateEncoder :: E.Params OffChainVoteExternalUpdate
offChainVoteExternalUpdateEncoder =
  mconcat
    [ offChainVoteExternalUpdateOffChainVoteDataId >$< idEncoder getOffChainVoteDataId
    , offChainVoteExternalUpdateTitle >$< E.param (E.nonNullable E.text)
    , offChainVoteExternalUpdateUri >$< E.param (E.nonNullable E.text)
    ]

offChainVoteExternalUpdatesEncoder :: E.Params ([OffChainVoteDataId], [Text], [Text])
offChainVoteExternalUpdatesEncoder =
  contrazip3
    (manyEncoder $ idBulkEncoder getOffChainVoteDataId)
    (manyEncoder $ E.nonNullable E.text)
    (manyEncoder $ E.nonNullable E.text)

-----------------------------------------------------------------------------------------------------------------------------------

-- |
-- Table Name: off_chain_vote_fetch_error
-- Description:
data OffChainVoteFetchError = OffChainVoteFetchError
  { offChainVoteFetchErrorVotingAnchorId :: !VotingAnchorId -- noreference
  , offChainVoteFetchErrorFetchError :: !Text
  , offChainVoteFetchErrorFetchTime :: !UTCTime -- sqltype=timestamp
  , offChainVoteFetchErrorRetryCount :: !Word -- sqltype=word31type
  }
  deriving (Eq, Show, Generic)

type instance Key OffChainVoteFetchError = OffChainVoteFetchErrorId
instance DbInfo OffChainVoteFetchError where
  uniqueFields _ = ["voting_anchor_id", "retry_count"]

entityNameOffChainVoteFetchErrorDecoder :: D.Row (Entity OffChainVoteFetchError)
entityNameOffChainVoteFetchErrorDecoder =
  Entity
    <$> idDecoder OffChainVoteFetchErrorId
    <*> offChainVoteFetchErrorDecoder

offChainVoteFetchErrorDecoder :: D.Row OffChainVoteFetchError
offChainVoteFetchErrorDecoder =
  OffChainVoteFetchError
    <$> idDecoder VotingAnchorId -- offChainVoteFetchErrorVotingAnchorId
    <*> D.column (D.nonNullable D.text) -- offChainVoteFetchErrorFetchError
    <*> D.column (D.nonNullable D.timestamptz) -- offChainVoteFetchErrorFetchTime
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- offChainVoteFetchErrorRetryCount

entityNameOffChainVoteFetchErrorEncoder :: E.Params (Entity OffChainVoteFetchError)
entityNameOffChainVoteFetchErrorEncoder =
  mconcat
    [ entityKey >$< idEncoder getOffChainVoteFetchErrorId
    , entityVal >$< offChainVoteFetchErrorEncoder
    ]

offChainVoteFetchErrorEncoder :: E.Params OffChainVoteFetchError
offChainVoteFetchErrorEncoder =
  mconcat
    [ offChainVoteFetchErrorVotingAnchorId >$< idEncoder getVotingAnchorId
    , offChainVoteFetchErrorFetchError >$< E.param (E.nonNullable E.text)
    , offChainVoteFetchErrorFetchTime >$< E.param (E.nonNullable E.timestamptz)
    , offChainVoteFetchErrorRetryCount >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]
