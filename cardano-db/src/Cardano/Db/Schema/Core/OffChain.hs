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
import Cardano.Db.Types (
 )
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Functor.Contravariant
import GHC.Generics (Generic)

import Hasql.Decoders as D
import Hasql.Encoders as E

-----------------------------------------------------------------------------------------------------------------------------------
-- OFFCHAIN
-- These tables manage off-chain data, including pool and vote data.
----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name:
Description:
-}
data OffChainPoolData = OffChainPoolData
  { offChainPoolDataId :: !OffChainPoolDataId
  , offChainPoolDataPoolId :: !PoolHashId           -- noreference
  , offChainPoolDataTickerName :: !Text
  , offChainPoolDataHash :: !ByteString             -- sqltype=hash32type
  , offChainPoolDataJson :: !Text                   -- sqltype=jsonb
  , offChainPoolDataBytes :: !ByteString            -- sqltype=bytea
  , offChainPoolDataPmrId :: !PoolMetadataRefId     -- noreference
  } deriving (Eq, Show, Generic)

offChainPoolDataDecoder :: D.Row OffChainPoolData
offChainPoolDataDecoder =
  OffChainPoolData
    <$> idDecoder OffChainPoolDataId -- offChainPoolDataId
    <*> idDecoder PoolHashId -- offChainPoolDataPoolId
    <*> D.column (D.nonNullable D.text) -- offChainPoolDataTickerName
    <*> D.column (D.nonNullable D.bytea) -- offChainPoolDataHash
    <*> D.column (D.nonNullable D.text) -- offChainPoolDataJson
    <*> D.column (D.nonNullable D.bytea) -- offChainPoolDataBytes
    <*> idDecoder PoolMetadataRefId -- offChainPoolDataPmrId

offChainPoolDataEncoder :: E.Params OffChainPoolData
offChainPoolDataEncoder =
  mconcat
    [ offChainPoolDataId >$< idEncoder getOffChainPoolDataId
    , offChainPoolDataPoolId >$< idEncoder getPoolHashId
    , offChainPoolDataTickerName >$< E.param (E.nonNullable E.text)
    , offChainPoolDataHash >$< E.param (E.nonNullable E.bytea)
    , offChainPoolDataJson >$< E.param (E.nonNullable E.text)
    , offChainPoolDataBytes >$< E.param (E.nonNullable E.bytea)
    , offChainPoolDataPmrId >$< idEncoder getPoolMetadataRefId
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name:
Description:
-}
-- The pool metadata fetch error. We duplicate the poolId for easy access.
-- TODO(KS): Debatable whether we need to persist this between migrations!
data OffChainPoolFetchError = OffChainPoolFetchError
  { offChainPoolFetchErrorId :: !OffChainPoolFetchErrorId
  , offChainPoolFetchErrorPoolId :: !PoolHashId              -- noreference
  , offChainPoolFetchErrorFetchTime :: !UTCTime              -- sqltype=timestamp
  , offChainPoolFetchErrorPmrId :: !PoolMetadataRefId        -- noreference
  , offChainPoolFetchErrorFetchError :: !Text
  , offChainPoolFetchErrorRetryCount :: !Word                -- sqltype=word31type
  } deriving (Eq, Show, Generic)

offChainPoolFetchErrorDecoder :: D.Row OffChainPoolFetchError
offChainPoolFetchErrorDecoder =
  OffChainPoolFetchError
    <$> idDecoder OffChainPoolFetchErrorId -- offChainPoolFetchErrorId
    <*> idDecoder PoolHashId -- offChainPoolFetchErrorPoolId
    <*> D.column (D.nonNullable D.timestamptz) -- offChainPoolFetchErrorFetchTime
    <*> idDecoder PoolMetadataRefId -- offChainPoolFetchErrorPmrId
    <*> D.column (D.nonNullable D.text) -- offChainPoolFetchErrorFetchError
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- offChainPoolFetchErrorRetryCount

offChainPoolFetchErrorEncoder :: E.Params OffChainPoolFetchError
offChainPoolFetchErrorEncoder =
  mconcat
    [ offChainPoolFetchErrorId >$< idEncoder getOffChainPoolFetchErrorId
    , offChainPoolFetchErrorPoolId >$< idEncoder getPoolHashId
    , offChainPoolFetchErrorFetchTime >$< E.param (E.nonNullable E.timestamptz)
    , offChainPoolFetchErrorPmrId >$< idEncoder getPoolMetadataRefId
    , offChainPoolFetchErrorFetchError >$< E.param (E.nonNullable E.text)
    , offChainPoolFetchErrorRetryCount >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name:
Description:
-}
data OffChainVoteData = OffChainVoteData
  { offChainVoteDataId :: !OffChainVoteDataId
  , offChainVoteDataVotingAnchorId :: !VotingAnchorId         -- noreference
  , offChainVoteDataHash :: !ByteString
  , offChainVoteDataLanguage :: !Text
  , offChainVoteDataComment :: !(Maybe Text)
  , offChainVoteDataJson :: !Text                             -- sqltype=jsonb
  , offChainVoteDataBytes :: !ByteString                      -- sqltype=bytea
  , offChainVoteDataWarning :: !(Maybe Text)
  , offChainVoteDataIsValid :: !(Maybe Bool)
  } deriving (Eq, Show, Generic)

offChainVoteDataDecoder :: D.Row OffChainVoteData
offChainVoteDataDecoder =
  OffChainVoteData
    <$> idDecoder OffChainVoteDataId -- offChainVoteDataId
    <*> idDecoder VotingAnchorId -- offChainVoteDataVotingAnchorId
    <*> D.column (D.nonNullable D.bytea) -- offChainVoteDataHash
    <*> D.column (D.nonNullable D.text) -- offChainVoteDataLanguage
    <*> D.column (D.nullable D.text) -- offChainVoteDataComment
    <*> D.column (D.nonNullable D.text) -- offChainVoteDataJson
    <*> D.column (D.nonNullable D.bytea) -- offChainVoteDataBytes
    <*> D.column (D.nullable D.text) -- offChainVoteDataWarning
    <*> D.column (D.nullable D.bool) -- offChainVoteDataIsValid

offChainVoteDataEncoder :: E.Params OffChainVoteData
offChainVoteDataEncoder =
  mconcat
    [ offChainVoteDataId >$< idEncoder getOffChainVoteDataId
    , offChainVoteDataVotingAnchorId >$< idEncoder getVotingAnchorId
    , offChainVoteDataHash >$< E.param (E.nonNullable E.bytea)
    , offChainVoteDataLanguage >$< E.param (E.nonNullable E.text)
    , offChainVoteDataComment >$< E.param (E.nullable E.text)
    , offChainVoteDataJson >$< E.param (E.nonNullable E.text)
    , offChainVoteDataBytes >$< E.param (E.nonNullable E.bytea)
    , offChainVoteDataWarning >$< E.param (E.nullable E.text)
    , offChainVoteDataIsValid >$< E.param (E.nullable E.bool)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name:
Description:
-}
data OffChainVoteGovActionData = OffChainVoteGovActionData
  { offChainVoteGovActionDataId :: !OffChainVoteGovActionDataId
  , offChainVoteGovActionDataOffChainVoteDataId :: !OffChainVoteDataId -- noreference
  , offChainVoteGovActionDataTitle :: !Text
  , offChainVoteGovActionDataAbstract :: !Text
  , offChainVoteGovActionDataMotivation :: !Text
  , offChainVoteGovActionDataRationale :: !Text
  } deriving (Eq, Show, Generic)

offChainVoteGovActionDataDecoder :: D.Row OffChainVoteGovActionData
offChainVoteGovActionDataDecoder =
  OffChainVoteGovActionData
    <$> idDecoder OffChainVoteGovActionDataId -- offChainVoteGovActionDataId
    <*> idDecoder OffChainVoteDataId -- offChainVoteGovActionDataOffChainVoteDataId
    <*> D.column (D.nonNullable D.text) -- offChainVoteGovActionDataTitle
    <*> D.column (D.nonNullable D.text) -- offChainVoteGovActionDataAbstract
    <*> D.column (D.nonNullable D.text) -- offChainVoteGovActionDataMotivation
    <*> D.column (D.nonNullable D.text) -- offChainVoteGovActionDataRationale

offChainVoteGovActionDataEncoder :: E.Params OffChainVoteGovActionData
offChainVoteGovActionDataEncoder =
  mconcat
    [ offChainVoteGovActionDataId >$< idEncoder getOffChainVoteGovActionDataId
    , offChainVoteGovActionDataOffChainVoteDataId >$< idEncoder getOffChainVoteDataId
    , offChainVoteGovActionDataTitle >$< E.param (E.nonNullable E.text)
    , offChainVoteGovActionDataAbstract >$< E.param (E.nonNullable E.text)
    , offChainVoteGovActionDataMotivation >$< E.param (E.nonNullable E.text)
    , offChainVoteGovActionDataRationale >$< E.param (E.nonNullable E.text)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name:
Description:
-}
data OffChainVoteDrepData = OffChainVoteDrepData
  { offChainVoteDrepDataId :: !OffChainVoteDrepDataId
  , offChainVoteDrepDataOffChainVoteDataId :: !OffChainVoteDataId -- noreference
  , offChainVoteDrepDataPaymentAddress :: !(Maybe Text)
  , offChainVoteDrepDataGivenName :: !Text
  , offChainVoteDrepDataObjectives :: !(Maybe Text)
  , offChainVoteDrepDataMotivations :: !(Maybe Text)
  , offChainVoteDrepDataQualifications :: !(Maybe Text)
  , offChainVoteDrepDataImageUrl :: !(Maybe Text)
  , offChainVoteDrepDataImageHash :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

offChainVoteDrepDataDecoder :: D.Row OffChainVoteDrepData
offChainVoteDrepDataDecoder =
  OffChainVoteDrepData
    <$> idDecoder OffChainVoteDrepDataId -- offChainVoteDrepDataId
    <*> idDecoder OffChainVoteDataId -- offChainVoteDrepDataOffChainVoteDataId
    <*> D.column (D.nullable D.text) -- offChainVoteDrepDataPaymentAddress
    <*> D.column (D.nonNullable D.text) -- offChainVoteDrepDataGivenName
    <*> D.column (D.nullable D.text) -- offChainVoteDrepDataObjectives
    <*> D.column (D.nullable D.text) -- offChainVoteDrepDataMotivations
    <*> D.column (D.nullable D.text) -- offChainVoteDrepDataQualifications
    <*> D.column (D.nullable D.text) -- offChainVoteDrepDataImageUrl
    <*> D.column (D.nullable D.text) -- offChainVoteDrepDataImageHash

offChainVoteDrepDataEncoder :: E.Params OffChainVoteDrepData
offChainVoteDrepDataEncoder =
  mconcat
    [ offChainVoteDrepDataId >$< idEncoder getOffChainVoteDrepDataId
    , offChainVoteDrepDataOffChainVoteDataId >$< idEncoder getOffChainVoteDataId
    , offChainVoteDrepDataPaymentAddress >$< E.param (E.nullable E.text)
    , offChainVoteDrepDataGivenName >$< E.param (E.nonNullable E.text)
    , offChainVoteDrepDataObjectives >$< E.param (E.nullable E.text)
    , offChainVoteDrepDataMotivations >$< E.param (E.nullable E.text)
    , offChainVoteDrepDataQualifications >$< E.param (E.nullable E.text)
    , offChainVoteDrepDataImageUrl >$< E.param (E.nullable E.text)
    , offChainVoteDrepDataImageHash >$< E.param (E.nullable E.text)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name:
Description:
-}
data OffChainVoteAuthor = OffChainVoteAuthor
  { offChainVoteAuthorId :: !OffChainVoteAuthorId
  , offChainVoteAuthorOffChainVoteDataId :: !OffChainVoteDataId -- noreference
  , offChainVoteAuthorName :: !(Maybe Text)
  , offChainVoteAuthorWitnessAlgorithm :: !Text
  , offChainVoteAuthorPublicKey :: !Text
  , offChainVoteAuthorSignature :: !Text
  , offChainVoteAuthorWarning :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

offChainVoteAuthorDecoder :: D.Row OffChainVoteAuthor
offChainVoteAuthorDecoder =
  OffChainVoteAuthor
    <$> idDecoder OffChainVoteAuthorId -- offChainVoteAuthorId
    <*> idDecoder OffChainVoteDataId -- offChainVoteAuthorOffChainVoteDataId
    <*> D.column (D.nullable D.text) -- offChainVoteAuthorName
    <*> D.column (D.nonNullable D.text) -- offChainVoteAuthorWitnessAlgorithm
    <*> D.column (D.nonNullable D.text) -- offChainVoteAuthorPublicKey
    <*> D.column (D.nonNullable D.text) -- offChainVoteAuthorSignature
    <*> D.column (D.nullable D.text) -- offChainVoteAuthorWarning

offChainVoteAuthorEncoder :: E.Params OffChainVoteAuthor
offChainVoteAuthorEncoder =
  mconcat
    [ offChainVoteAuthorId >$< idEncoder getOffChainVoteAuthorId
    , offChainVoteAuthorOffChainVoteDataId >$< idEncoder getOffChainVoteDataId
    , offChainVoteAuthorName >$< E.param (E.nullable E.text)
    , offChainVoteAuthorWitnessAlgorithm >$< E.param (E.nonNullable E.text)
    , offChainVoteAuthorPublicKey >$< E.param (E.nonNullable E.text)
    , offChainVoteAuthorSignature >$< E.param (E.nonNullable E.text)
    , offChainVoteAuthorWarning >$< E.param (E.nullable E.text)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name:
Description:
-}
data OffChainVoteReference = OffChainVoteReference
  { offChainVoteReferenceId :: !OffChainVoteReferenceId
  , offChainVoteReferenceOffChainVoteDataId :: !OffChainVoteDataId -- noreference
  , offChainVoteReferenceLabel :: !Text
  , offChainVoteReferenceUri :: !Text
  , offChainVoteReferenceHashDigest :: !(Maybe Text)
  , offChainVoteReferenceHashAlgorithm :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

offChainVoteReferenceDecoder :: D.Row OffChainVoteReference
offChainVoteReferenceDecoder =
  OffChainVoteReference
    <$> idDecoder OffChainVoteReferenceId -- offChainVoteReferenceId
    <*> idDecoder OffChainVoteDataId -- offChainVoteReferenceOffChainVoteDataId
    <*> D.column (D.nonNullable D.text) -- offChainVoteReferenceLabel
    <*> D.column (D.nonNullable D.text) -- offChainVoteReferenceUri
    <*> D.column (D.nullable D.text) -- offChainVoteReferenceHashDigest
    <*> D.column (D.nullable D.text) -- offChainVoteReferenceHashAlgorithm

offChainVoteReferenceEncoder :: E.Params OffChainVoteReference
offChainVoteReferenceEncoder =
  mconcat
    [ offChainVoteReferenceId >$< idEncoder getOffChainVoteReferenceId
    , offChainVoteReferenceOffChainVoteDataId >$< idEncoder getOffChainVoteDataId
    , offChainVoteReferenceLabel >$< E.param (E.nonNullable E.text)
    , offChainVoteReferenceUri >$< E.param (E.nonNullable E.text)
    , offChainVoteReferenceHashDigest >$< E.param (E.nullable E.text)
    , offChainVoteReferenceHashAlgorithm >$< E.param (E.nullable E.text)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name:
Description:
-}
data OffChainVoteExternalUpdate = OffChainVoteExternalUpdate
  { offChainVoteExternalUpdateId :: !OffChainVoteExternalUpdateId
  , offChainVoteExternalUpdateOffChainVoteDataId :: !OffChainVoteDataId -- noreference
  , offChainVoteExternalUpdateTitle :: !Text
  , offChainVoteExternalUpdateUri :: !Text
  } deriving (Eq, Show, Generic)

offChainVoteExternalUpdateDecoder :: D.Row OffChainVoteExternalUpdate
offChainVoteExternalUpdateDecoder =
  OffChainVoteExternalUpdate
    <$> idDecoder OffChainVoteExternalUpdateId -- offChainVoteExternalUpdateId
    <*> idDecoder OffChainVoteDataId -- offChainVoteExternalUpdateOffChainVoteDataId
    <*> D.column (D.nonNullable D.text) -- offChainVoteExternalUpdateTitle
    <*> D.column (D.nonNullable D.text) -- offChainVoteExternalUpdateUri

offChainVoteExternalUpdateEncoder :: E.Params OffChainVoteExternalUpdate
offChainVoteExternalUpdateEncoder =
  mconcat
    [ offChainVoteExternalUpdateId >$< idEncoder getOffChainVoteExternalUpdateId
    , offChainVoteExternalUpdateOffChainVoteDataId >$< idEncoder getOffChainVoteDataId
    , offChainVoteExternalUpdateTitle >$< E.param (E.nonNullable E.text)
    , offChainVoteExternalUpdateUri >$< E.param (E.nonNullable E.text)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
{-|
Table Name:
Description:
-}
data OffChainVoteFetchError = OffChainVoteFetchError
  { offChainVoteFetchErrorId :: !OffChainVoteFetchErrorId
  , offChainVoteFetchErrorVotingAnchorId :: !VotingAnchorId        -- noreference
  , offChainVoteFetchErrorFetchError :: !Text
  , offChainVoteFetchErrorFetchTime :: !UTCTime                    -- sqltype=timestamp
  , offChainVoteFetchErrorRetryCount :: !Word                      -- sqltype=word31type
  } deriving (Eq, Show, Generic)

offChainVoteFetchErrorDecoder :: D.Row OffChainVoteFetchError
offChainVoteFetchErrorDecoder =
  OffChainVoteFetchError
    <$> idDecoder OffChainVoteFetchErrorId -- offChainVoteFetchErrorId
    <*> idDecoder VotingAnchorId -- offChainVoteFetchErrorVotingAnchorId
    <*> D.column (D.nonNullable D.text) -- offChainVoteFetchErrorFetchError
    <*> D.column (D.nonNullable D.timestamptz) -- offChainVoteFetchErrorFetchTime
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- offChainVoteFetchErrorRetryCount

offChainVoteFetchErrorEncoder :: E.Params OffChainVoteFetchError
offChainVoteFetchErrorEncoder =
  mconcat
    [ offChainVoteFetchErrorId >$< idEncoder getOffChainVoteFetchErrorId
    , offChainVoteFetchErrorVotingAnchorId >$< idEncoder getVotingAnchorId
    , offChainVoteFetchErrorFetchError >$< E.param (E.nonNullable E.text)
    , offChainVoteFetchErrorFetchTime >$< E.param (E.nonNullable E.timestamptz)
    , offChainVoteFetchErrorRetryCount >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]
