{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Db.Schema.Core.OffChain where

import Cardano.Db.Schema.Orphans ()
import Cardano.Db.Schema.Ids
import Cardano.Db.Types (HasDbInfo (..)
 )
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Functor.Contravariant
import GHC.Generics (Generic)

import Hasql.Decoders as D
import Hasql.Encoders as E
import Contravariant.Extras (contrazip5, contrazip6, contrazip3)
import Cardano.Db.Statement.Helpers (manyEncoder)

-----------------------------------------------------------------------------------------------------------------------------------
-- OFFCHAIN
-- These tables manage off-chain data, including pool and vote data.
----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: off_chain_pool_data
Description:
-}
data OffChainPoolData = OffChainPoolData
  { offChainPoolData_Id :: !OffChainPoolDataId
  , offChainPoolData_PoolId :: !PoolHashId           -- noreference
  , offChainPoolData_TickerName :: !Text
  , offChainPoolData_Hash :: !ByteString             -- sqltype=hash32type
  , offChainPoolData_Json :: !Text                   -- sqltype=jsonb
  , offChainPoolData_Bytes :: !ByteString            -- sqltype=bytea
  , offChainPoolData_PmrId :: !PoolMetadataRefId     -- noreference
  } deriving (Eq, Show, Generic)

instance HasDbInfo OffChainPoolData

offChainPoolDataDecoder :: D.Row OffChainPoolData
offChainPoolDataDecoder =
  OffChainPoolData
    <$> idDecoder OffChainPoolDataId -- offChainPoolData_Id
    <*> idDecoder PoolHashId -- offChainPoolData_PoolId
    <*> D.column (D.nonNullable D.text) -- offChainPoolData_TickerName
    <*> D.column (D.nonNullable D.bytea) -- offChainPoolData_Hash
    <*> D.column (D.nonNullable D.text) -- offChainPoolData_Json
    <*> D.column (D.nonNullable D.bytea) -- offChainPoolData_Bytes
    <*> idDecoder PoolMetadataRefId -- offChainPoolData_PmrId

offChainPoolDataEncoder :: E.Params OffChainPoolData
offChainPoolDataEncoder =
  mconcat
    [ offChainPoolData_Id >$< idEncoder getOffChainPoolDataId
    , offChainPoolData_PoolId >$< idEncoder getPoolHashId
    , offChainPoolData_TickerName >$< E.param (E.nonNullable E.text)
    , offChainPoolData_Hash >$< E.param (E.nonNullable E.bytea)
    , offChainPoolData_Json >$< E.param (E.nonNullable E.text)
    , offChainPoolData_Bytes >$< E.param (E.nonNullable E.bytea)
    , offChainPoolData_PmrId >$< idEncoder getPoolMetadataRefId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: off_chain_pool_fetch_error
Description:
-}
-- The pool metadata fetch error. We duplicate the poolId for easy access.
-- TODO(KS): Debatable whether we need to persist this between migrations!
data OffChainPoolFetchError = OffChainPoolFetchError
  { offChainPoolFetchError_Id :: !OffChainPoolFetchErrorId
  , offChainPoolFetchError_PoolId :: !PoolHashId              -- noreference
  , offChainPoolFetchError_FetchTime :: !UTCTime              -- sqltype=timestamp
  , offChainPoolFetchError_PmrId :: !PoolMetadataRefId        -- noreference
  , offChainPoolFetchError_FetchError :: !Text
  , offChainPoolFetchError_RetryCount :: !Word                -- sqltype=word31type
  } deriving (Eq, Show, Generic)

instance HasDbInfo OffChainPoolFetchError

offChainPoolFetchErrorDecoder :: D.Row OffChainPoolFetchError
offChainPoolFetchErrorDecoder =
  OffChainPoolFetchError
    <$> idDecoder OffChainPoolFetchErrorId -- offChainPoolFetchError_Id
    <*> idDecoder PoolHashId -- offChainPoolFetchError_PoolId
    <*> D.column (D.nonNullable D.timestamptz) -- offChainPoolFetchError_FetchTime
    <*> idDecoder PoolMetadataRefId -- offChainPoolFetchError_PmrId
    <*> D.column (D.nonNullable D.text) -- offChainPoolFetchError_FetchError
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- offChainPoolFetchError_RetryCount

offChainPoolFetchErrorEncoder :: E.Params OffChainPoolFetchError
offChainPoolFetchErrorEncoder =
  mconcat
    [ offChainPoolFetchError_Id >$< idEncoder getOffChainPoolFetchErrorId
    , offChainPoolFetchError_PoolId >$< idEncoder getPoolHashId
    , offChainPoolFetchError_FetchTime >$< E.param (E.nonNullable E.timestamptz)
    , offChainPoolFetchError_PmrId >$< idEncoder getPoolMetadataRefId
    , offChainPoolFetchError_FetchError >$< E.param (E.nonNullable E.text)
    , offChainPoolFetchError_RetryCount >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: off_chain_vote_data
Description:
-}
data OffChainVoteData = OffChainVoteData
  { offChainVoteData_Id :: !OffChainVoteDataId
  , offChainVoteData_VotingAnchorId :: !VotingAnchorId         -- noreference
  , offChainVoteData_Hash :: !ByteString
  , offChainVoteData_Language :: !Text
  , offChainVoteData_Comment :: !(Maybe Text)
  , offChainVoteData_Json :: !Text                             -- sqltype=jsonb
  , offChainVoteData_Bytes :: !ByteString                      -- sqltype=bytea
  , offChainVoteData_Warning :: !(Maybe Text)
  , offChainVoteData_IsValid :: !(Maybe Bool)
  } deriving (Eq, Show, Generic)

instance HasDbInfo OffChainVoteData

offChainVoteDataDecoder :: D.Row OffChainVoteData
offChainVoteDataDecoder =
  OffChainVoteData
    <$> idDecoder OffChainVoteDataId -- offChainVoteData_Id
    <*> idDecoder VotingAnchorId -- offChainVoteData_VotingAnchorId
    <*> D.column (D.nonNullable D.bytea) -- offChainVoteData_Hash
    <*> D.column (D.nonNullable D.text) -- offChainVoteData_Language
    <*> D.column (D.nullable D.text) -- offChainVoteData_Comment
    <*> D.column (D.nonNullable D.text) -- offChainVoteData_Json
    <*> D.column (D.nonNullable D.bytea) -- offChainVoteData_Bytes
    <*> D.column (D.nullable D.text) -- offChainVoteData_Warning
    <*> D.column (D.nullable D.bool) -- offChainVoteData_IsValid

offChainVoteDataEncoder :: E.Params OffChainVoteData
offChainVoteDataEncoder =
  mconcat
    [ offChainVoteData_Id >$< idEncoder getOffChainVoteDataId
    , offChainVoteData_VotingAnchorId >$< idEncoder getVotingAnchorId
    , offChainVoteData_Hash >$< E.param (E.nonNullable E.bytea)
    , offChainVoteData_Language >$< E.param (E.nonNullable E.text)
    , offChainVoteData_Comment >$< E.param (E.nullable E.text)
    , offChainVoteData_Json >$< E.param (E.nonNullable E.text)
    , offChainVoteData_Bytes >$< E.param (E.nonNullable E.bytea)
    , offChainVoteData_Warning >$< E.param (E.nullable E.text)
    , offChainVoteData_IsValid >$< E.param (E.nullable E.bool)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: off_chain_vote_gov_action_data
Description:
-}
data OffChainVoteGovActionData = OffChainVoteGovActionData
  { offChainVoteGovActionData_Id :: !OffChainVoteGovActionDataId
  , offChainVoteGovActionData_OffChainVoteDataId :: !OffChainVoteDataId -- noreference
  , offChainVoteGovActionData_Title :: !Text
  , offChainVoteGovActionData_Abstract :: !Text
  , offChainVoteGovActionData_Motivation :: !Text
  , offChainVoteGovActionData_Rationale :: !Text
  } deriving (Eq, Show, Generic)

instance HasDbInfo OffChainVoteGovActionData

offChainVoteGovActionDataDecoder :: D.Row OffChainVoteGovActionData
offChainVoteGovActionDataDecoder =
  OffChainVoteGovActionData
    <$> idDecoder OffChainVoteGovActionDataId -- offChainVoteGovActionData_Id
    <*> idDecoder OffChainVoteDataId -- offChainVoteGovActionData_OffChainVoteDataId
    <*> D.column (D.nonNullable D.text) -- offChainVoteGovActionData_Title
    <*> D.column (D.nonNullable D.text) -- offChainVoteGovActionData_Abstract
    <*> D.column (D.nonNullable D.text) -- offChainVoteGovActionData_Motivation
    <*> D.column (D.nonNullable D.text) -- offChainVoteGovActionData_Rationale

offChainVoteGovActionDataEncoder :: E.Params OffChainVoteGovActionData
offChainVoteGovActionDataEncoder =
  mconcat
    [ offChainVoteGovActionData_Id >$< idEncoder getOffChainVoteGovActionDataId
    , offChainVoteGovActionData_OffChainVoteDataId >$< idEncoder getOffChainVoteDataId
    , offChainVoteGovActionData_Title >$< E.param (E.nonNullable E.text)
    , offChainVoteGovActionData_Abstract >$< E.param (E.nonNullable E.text)
    , offChainVoteGovActionData_Motivation >$< E.param (E.nonNullable E.text)
    , offChainVoteGovActionData_Rationale >$< E.param (E.nonNullable E.text)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: off_chain_vote_drep_data
Description:
-}
data OffChainVoteDrepData = OffChainVoteDrepData
  { offChainVoteDrepData_Id :: !OffChainVoteDrepDataId
  , offChainVoteDrepData_OffChainVoteDataId :: !OffChainVoteDataId -- noreference
  , offChainVoteDrepData_PaymentAddress :: !(Maybe Text)
  , offChainVoteDrepData_GivenName :: !Text
  , offChainVoteDrepData_Objectives :: !(Maybe Text)
  , offChainVoteDrepData_Motivations :: !(Maybe Text)
  , offChainVoteDrepData_Qualifications :: !(Maybe Text)
  , offChainVoteDrepData_ImageUrl :: !(Maybe Text)
  , offChainVoteDrepData_ImageHash :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

instance HasDbInfo OffChainVoteDrepData

offChainVoteDrepDataDecoder :: D.Row OffChainVoteDrepData
offChainVoteDrepDataDecoder =
  OffChainVoteDrepData
    <$> idDecoder OffChainVoteDrepDataId -- offChainVoteDrepData_Id
    <*> idDecoder OffChainVoteDataId -- offChainVoteDrepData_OffChainVoteDataId
    <*> D.column (D.nullable D.text) -- offChainVoteDrepData_PaymentAddress
    <*> D.column (D.nonNullable D.text) -- offChainVoteDrepData_GivenName
    <*> D.column (D.nullable D.text) -- offChainVoteDrepData_Objectives
    <*> D.column (D.nullable D.text) -- offChainVoteDrepData_Motivations
    <*> D.column (D.nullable D.text) -- offChainVoteDrepData_Qualifications
    <*> D.column (D.nullable D.text) -- offChainVoteDrepData_ImageUrl
    <*> D.column (D.nullable D.text) -- offChainVoteDrepData_ImageHash

offChainVoteDrepDataEncoder :: E.Params OffChainVoteDrepData
offChainVoteDrepDataEncoder =
  mconcat
    [ offChainVoteDrepData_Id >$< idEncoder getOffChainVoteDrepDataId
    , offChainVoteDrepData_OffChainVoteDataId >$< idEncoder getOffChainVoteDataId
    , offChainVoteDrepData_PaymentAddress >$< E.param (E.nullable E.text)
    , offChainVoteDrepData_GivenName >$< E.param (E.nonNullable E.text)
    , offChainVoteDrepData_Objectives >$< E.param (E.nullable E.text)
    , offChainVoteDrepData_Motivations >$< E.param (E.nullable E.text)
    , offChainVoteDrepData_Qualifications >$< E.param (E.nullable E.text)
    , offChainVoteDrepData_ImageUrl >$< E.param (E.nullable E.text)
    , offChainVoteDrepData_ImageHash >$< E.param (E.nullable E.text)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: off_chain_vote_author
Description:
-}
data OffChainVoteAuthor = OffChainVoteAuthor
  { offChainVoteAuthor_Id :: !OffChainVoteAuthorId
  , offChainVoteAuthor_OffChainVoteDataId :: !OffChainVoteDataId -- noreference
  , offChainVoteAuthor_Name :: !(Maybe Text)
  , offChainVoteAuthor_WitnessAlgorithm :: !Text
  , offChainVoteAuthor_PublicKey :: !Text
  , offChainVoteAuthor_Signature :: !Text
  , offChainVoteAuthor_Warning :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

instance HasDbInfo OffChainVoteAuthor

offChainVoteAuthorDecoder :: D.Row OffChainVoteAuthor
offChainVoteAuthorDecoder =
  OffChainVoteAuthor
    <$> idDecoder OffChainVoteAuthorId -- offChainVoteAuthor_Id
    <*> idDecoder OffChainVoteDataId -- offChainVoteAuthor_OffChainVoteDataId
    <*> D.column (D.nullable D.text) -- offChainVoteAuthor_Name
    <*> D.column (D.nonNullable D.text) -- offChainVoteAuthor_WitnessAlgorithm
    <*> D.column (D.nonNullable D.text) -- offChainVoteAuthor_PublicKey
    <*> D.column (D.nonNullable D.text) -- offChainVoteAuthor_Signature
    <*> D.column (D.nullable D.text) -- offChainVoteAuthor_Warning

offChainVoteAuthorEncoder :: E.Params OffChainVoteAuthor
offChainVoteAuthorEncoder =
  mconcat
    [ -- offChainVoteAuthor_Id >$< idEncoder getOffChainVoteAuthorId
      offChainVoteAuthor_OffChainVoteDataId >$< idEncoder getOffChainVoteDataId
    , offChainVoteAuthor_Name >$< E.param (E.nullable E.text)
    , offChainVoteAuthor_WitnessAlgorithm >$< E.param (E.nonNullable E.text)
    , offChainVoteAuthor_PublicKey >$< E.param (E.nonNullable E.text)
    , offChainVoteAuthor_Signature >$< E.param (E.nonNullable E.text)
    , offChainVoteAuthor_Warning >$< E.param (E.nullable E.text)
    ]

offChainVoteAuthorManyEncoder
  :: E.Params ([OffChainVoteDataId], [Maybe Text], [Text], [Text], [Text], [Maybe Text])
offChainVoteAuthorManyEncoder =
  contrazip6
    (manyEncoder $ idEncoderMany getOffChainVoteDataId)
    (manyEncoder $ E.nullable E.text)
    (manyEncoder $ E.nonNullable E.text)
    (manyEncoder $ E.nonNullable E.text)
    (manyEncoder $ E.nonNullable E.text)
    (manyEncoder $ E.nullable E.text)

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: off_chain_vote_reference
Description:
-}
data OffChainVoteReference = OffChainVoteReference
  { offChainVoteReference_Id :: !OffChainVoteReferenceId
  , offChainVoteReference_OffChainVoteDataId :: !OffChainVoteDataId -- noreference
  , offChainVoteReference_Label :: !Text
  , offChainVoteReference_Uri :: !Text
  , offChainVoteReference_HashDigest :: !(Maybe Text)
  , offChainVoteReference_HashAlgorithm :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

instance HasDbInfo OffChainVoteReference

offChainVoteReferenceDecoder :: D.Row OffChainVoteReference
offChainVoteReferenceDecoder =
  OffChainVoteReference
    <$> idDecoder OffChainVoteReferenceId -- offChainVoteReference_Id
    <*> idDecoder OffChainVoteDataId -- offChainVoteReference_OffChainVoteDataId
    <*> D.column (D.nonNullable D.text) -- offChainVoteReference_Label
    <*> D.column (D.nonNullable D.text) -- offChainVoteReference_Uri
    <*> D.column (D.nullable D.text) -- offChainVoteReference_HashDigest
    <*> D.column (D.nullable D.text) -- offChainVoteReference_HashAlgorithm

offChainVoteReferenceEncoder :: E.Params OffChainVoteReference
offChainVoteReferenceEncoder =
  mconcat
    [ -- offChainVoteReference_Id >$< idEncoder getOffChainVoteReferenceId
      offChainVoteReference_OffChainVoteDataId >$< idEncoder getOffChainVoteDataId
    , offChainVoteReference_Label >$< E.param (E.nonNullable E.text)
    , offChainVoteReference_Uri >$< E.param (E.nonNullable E.text)
    , offChainVoteReference_HashDigest >$< E.param (E.nullable E.text)
    , offChainVoteReference_HashAlgorithm >$< E.param (E.nullable E.text)
    ]

offChainVoteReferenceManyEncoder :: E.Params ([OffChainVoteDataId], [Text], [Text], [Maybe Text], [Maybe Text])
offChainVoteReferenceManyEncoder =
  contrazip5
    (manyEncoder $ idEncoderMany getOffChainVoteDataId)
    (manyEncoder $ E.nonNullable E.text)
    (manyEncoder $ E.nonNullable E.text)
    (manyEncoder $ E.nullable E.text)
    (manyEncoder $ E.nullable E.text)

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: off_chain_vote_external_update
Description:
-}
data OffChainVoteExternalUpdate = OffChainVoteExternalUpdate
  { offChainVoteExternalUpdate_Id :: !OffChainVoteExternalUpdateId
  , offChainVoteExternalUpdate_OffChainVoteDataId :: !OffChainVoteDataId -- noreference
  , offChainVoteExternalUpdate_Title :: !Text
  , offChainVoteExternalUpdate_Uri :: !Text
  } deriving (Eq, Show, Generic)

instance HasDbInfo OffChainVoteExternalUpdate

offChainVoteExternalUpdateDecoder :: D.Row OffChainVoteExternalUpdate
offChainVoteExternalUpdateDecoder =
  OffChainVoteExternalUpdate
    <$> idDecoder OffChainVoteExternalUpdateId -- offChainVoteExternalUpdate_Id
    <*> idDecoder OffChainVoteDataId -- offChainVoteExternalUpdate_OffChainVoteDataId
    <*> D.column (D.nonNullable D.text) -- offChainVoteExternalUpdate_Title
    <*> D.column (D.nonNullable D.text) -- offChainVoteExternalUpdate_Uri

offChainVoteExternalUpdateEncoder :: E.Params OffChainVoteExternalUpdate
offChainVoteExternalUpdateEncoder =
  mconcat
    [ offChainVoteExternalUpdate_Id >$< idEncoder getOffChainVoteExternalUpdateId
    , offChainVoteExternalUpdate_OffChainVoteDataId >$< idEncoder getOffChainVoteDataId
    , offChainVoteExternalUpdate_Title >$< E.param (E.nonNullable E.text)
    , offChainVoteExternalUpdate_Uri >$< E.param (E.nonNullable E.text)
    ]

offChainVoteExternalUpdatesEncoder :: E.Params ([OffChainVoteDataId], [Text], [Text])
offChainVoteExternalUpdatesEncoder =
  contrazip3
    (manyEncoder $ idEncoderMany getOffChainVoteDataId)
    (manyEncoder $ E.nonNullable E.text)
    (manyEncoder $ E.nonNullable E.text)

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name: off_chain_vote_fetch_error
Description:
-}
data OffChainVoteFetchError = OffChainVoteFetchError
  { offChainVoteFetchError_Id :: !OffChainVoteFetchErrorId
  , offChainVoteFetchError_VotingAnchorId :: !VotingAnchorId        -- noreference
  , offChainVoteFetchError_FetchError :: !Text
  , offChainVoteFetchError_FetchTime :: !UTCTime                    -- sqltype=timestamp
  , offChainVoteFetchError_RetryCount :: !Word                      -- sqltype=word31type
  } deriving (Eq, Show, Generic)

instance HasDbInfo OffChainVoteFetchError

offChainVoteFetchErrorDecoder :: D.Row OffChainVoteFetchError
offChainVoteFetchErrorDecoder =
  OffChainVoteFetchError
    <$> idDecoder OffChainVoteFetchErrorId -- offChainVoteFetchError_Id
    <*> idDecoder VotingAnchorId -- offChainVoteFetchError_VotingAnchorId
    <*> D.column (D.nonNullable D.text) -- offChainVoteFetchError_FetchError
    <*> D.column (D.nonNullable D.timestamptz) -- offChainVoteFetchError_FetchTime
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- offChainVoteFetchError_RetryCount

offChainVoteFetchErrorEncoder :: E.Params OffChainVoteFetchError
offChainVoteFetchErrorEncoder =
  mconcat
    [ offChainVoteFetchError_Id >$< idEncoder getOffChainVoteFetchErrorId
    , offChainVoteFetchError_VotingAnchorId >$< idEncoder getVotingAnchorId
    , offChainVoteFetchError_FetchError >$< E.param (E.nonNullable E.text)
    , offChainVoteFetchError_FetchTime >$< E.param (E.nonNullable E.timestamptz)
    , offChainVoteFetchError_RetryCount >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]
