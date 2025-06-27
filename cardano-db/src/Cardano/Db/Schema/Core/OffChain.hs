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
import Hasql.Decoders as D
import Hasql.Encoders as E

import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Schema.Orphans ()
import Cardano.Db.Schema.Types (utcTimeAsTimestampDecoder, utcTimeAsTimestampEncoder)
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

entityOffChainPoolDataDecoder :: D.Row (Entity OffChainPoolData)
entityOffChainPoolDataDecoder =
  Entity
    <$> Id.idDecoder Id.OffChainPoolDataId
    <*> offChainPoolDataDecoder

offChainPoolDataDecoder :: D.Row OffChainPoolData
offChainPoolDataDecoder =
  OffChainPoolData
    <$> Id.idDecoder Id.PoolHashId -- offChainPoolDataPoolId
    <*> D.column (D.nonNullable D.text) -- offChainPoolDataTickerName
    <*> D.column (D.nonNullable D.bytea) -- offChainPoolDataHash
    <*> D.column (D.nonNullable D.text) -- offChainPoolDataJson
    <*> D.column (D.nonNullable D.bytea) -- offChainPoolDataBytes
    <*> Id.idDecoder Id.PoolMetadataRefId -- offChainPoolDataPmrId

entityOffChainPoolDataEncoder :: E.Params (Entity OffChainPoolData)
entityOffChainPoolDataEncoder =
  mconcat
    [ entityKey >$< Id.idEncoder Id.getOffChainPoolDataId
    , entityVal >$< offChainPoolDataEncoder
    ]

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

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: off_chain_pool_fetch_error
-- Description:
-----------------------------------------------------------------------------------------------------------------------------------

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

entityOffChainPoolFetchErrorDecoder :: D.Row (Entity OffChainPoolFetchError)
entityOffChainPoolFetchErrorDecoder =
  Entity
    <$> Id.idDecoder Id.OffChainPoolFetchErrorId
    <*> offChainPoolFetchErrorDecoder

offChainPoolFetchErrorDecoder :: D.Row OffChainPoolFetchError
offChainPoolFetchErrorDecoder =
  OffChainPoolFetchError
    <$> Id.idDecoder Id.PoolHashId -- offChainPoolFetchErrorPoolId
    <*> D.column (D.nonNullable utcTimeAsTimestampDecoder) -- offChainPoolFetchErrorFetchTime
    <*> Id.idDecoder Id.PoolMetadataRefId -- offChainPoolFetchErrorPmrId
    <*> D.column (D.nonNullable D.text) -- offChainPoolFetchErrorFetchError
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- offChainPoolFetchErrorRetryCount

entityOffChainPoolFetchErrorEncoder :: E.Params (Entity OffChainPoolFetchError)
entityOffChainPoolFetchErrorEncoder =
  mconcat
    [ entityKey >$< Id.idEncoder Id.getOffChainPoolFetchErrorId
    , entityVal >$< offChainPoolFetchErrorEncoder
    ]

offChainPoolFetchErrorEncoder :: E.Params OffChainPoolFetchError
offChainPoolFetchErrorEncoder =
  mconcat
    [ offChainPoolFetchErrorPoolId >$< Id.idEncoder Id.getPoolHashId
    , offChainPoolFetchErrorFetchTime >$< E.param (E.nonNullable utcTimeAsTimestampEncoder)
    , offChainPoolFetchErrorPmrId >$< Id.idEncoder Id.getPoolMetadataRefId
    , offChainPoolFetchErrorFetchError >$< E.param (E.nonNullable E.text)
    , offChainPoolFetchErrorRetryCount >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: off_chain_vote_data
-- Description:
-----------------------------------------------------------------------------------------------------------------------------------
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

-- ["voting_anchor_id","hash","json","bytes","warning","language","comment","is_valid"]

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

entityOffChainVoteDataDecoder :: D.Row (Entity OffChainVoteData)
entityOffChainVoteDataDecoder =
  Entity
    <$> Id.idDecoder Id.OffChainVoteDataId
    <*> offChainVoteDataDecoder

offChainVoteDataDecoder :: D.Row OffChainVoteData
offChainVoteDataDecoder =
  OffChainVoteData
    <$> Id.idDecoder Id.VotingAnchorId -- offChainVoteDataVotingAnchorId
    <*> D.column (D.nonNullable D.bytea) -- offChainVoteDataHash
    <*> D.column (D.nonNullable D.text) -- offChainVoteDataJson
    <*> D.column (D.nonNullable D.bytea) -- offChainVoteDataBytes
    <*> D.column (D.nullable D.text) -- offChainVoteDataWarning
    <*> D.column (D.nonNullable D.text) -- offChainVoteDataLanguage
    <*> D.column (D.nullable D.text) -- offChainVoteDataComment
    <*> D.column (D.nullable D.bool) -- offChainVoteDataIsValid

entityOffChainVoteDataEncoder :: E.Params (Entity OffChainVoteData)
entityOffChainVoteDataEncoder =
  mconcat
    [ entityKey >$< Id.idEncoder Id.getOffChainVoteDataId
    , entityVal >$< offChainVoteDataEncoder
    ]

offChainVoteDataEncoder :: E.Params OffChainVoteData
offChainVoteDataEncoder =
  mconcat
    [ offChainVoteDataVotingAnchorId >$< Id.idEncoder Id.getVotingAnchorId
    , offChainVoteDataHash >$< E.param (E.nonNullable E.bytea)
    , offChainVoteDataJson >$< E.param (E.nonNullable E.text)
    , offChainVoteDataBytes >$< E.param (E.nonNullable E.bytea)
    , offChainVoteDataWarning >$< E.param (E.nullable E.text)
    , offChainVoteDataLanguage >$< E.param (E.nonNullable E.text)
    , offChainVoteDataComment >$< E.param (E.nullable E.text)
    , offChainVoteDataIsValid >$< E.param (E.nullable E.bool)
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

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: off_chain_vote_gov_action_data
-- Description:
-----------------------------------------------------------------------------------------------------------------------------------
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

entityOffChainVoteGovActionDataDecoder :: D.Row (Entity OffChainVoteGovActionData)
entityOffChainVoteGovActionDataDecoder =
  Entity
    <$> Id.idDecoder Id.OffChainVoteGovActionDataId
    <*> offChainVoteGovActionDataDecoder

offChainVoteGovActionDataDecoder :: D.Row OffChainVoteGovActionData
offChainVoteGovActionDataDecoder =
  OffChainVoteGovActionData
    <$> Id.idDecoder Id.OffChainVoteDataId -- offChainVoteGovActionDataOffChainVoteDataId
    <*> D.column (D.nonNullable D.text) -- offChainVoteGovActionDataTitle
    <*> D.column (D.nonNullable D.text) -- offChainVoteGovActionDataAbstract
    <*> D.column (D.nonNullable D.text) -- offChainVoteGovActionDataMotivation
    <*> D.column (D.nonNullable D.text) -- offChainVoteGovActionDataRationale

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

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: off_chain_vote_drep_data
-- Description:
-----------------------------------------------------------------------------------------------------------------------------------
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

entityOffChainVoteDrepDataDecoder :: D.Row (Entity OffChainVoteDrepData)
entityOffChainVoteDrepDataDecoder =
  Entity
    <$> Id.idDecoder Id.OffChainVoteDrepDataId
    <*> offChainVoteDrepDataDecoder

offChainVoteDrepDataDecoder :: D.Row OffChainVoteDrepData
offChainVoteDrepDataDecoder =
  OffChainVoteDrepData
    <$> Id.idDecoder Id.OffChainVoteDataId -- offChainVoteDrepDataOffChainVoteDataId
    <*> D.column (D.nullable D.text) -- offChainVoteDrepDataPaymentAddress
    <*> D.column (D.nonNullable D.text) -- offChainVoteDrepDataGivenName
    <*> D.column (D.nullable D.text) -- offChainVoteDrepDataObjectives
    <*> D.column (D.nullable D.text) -- offChainVoteDrepDataMotivations
    <*> D.column (D.nullable D.text) -- offChainVoteDrepDataQualifications
    <*> D.column (D.nullable D.text) -- offChainVoteDrepDataImageUrl
    <*> D.column (D.nullable D.text) -- offChainVoteDrepDataImageHash

entityOffChainVoteDrepDataEncoder :: E.Params (Entity OffChainVoteDrepData)
entityOffChainVoteDrepDataEncoder =
  mconcat
    [ entityKey >$< Id.idEncoder Id.getOffChainVoteDrepDataId
    , entityVal >$< offChainVoteDrepDataEncoder
    ]

offChainVoteDrepDataEncoder :: E.Params OffChainVoteDrepData
offChainVoteDrepDataEncoder =
  mconcat
    [ offChainVoteDrepDataOffChainVoteDataId >$< Id.idEncoder Id.getOffChainVoteDataId
    , offChainVoteDrepDataPaymentAddress >$< E.param (E.nullable E.text)
    , offChainVoteDrepDataGivenName >$< E.param (E.nonNullable E.text)
    , offChainVoteDrepDataObjectives >$< E.param (E.nullable E.text)
    , offChainVoteDrepDataMotivations >$< E.param (E.nullable E.text)
    , offChainVoteDrepDataQualifications >$< E.param (E.nullable E.text)
    , offChainVoteDrepDataImageUrl >$< E.param (E.nullable E.text)
    , offChainVoteDrepDataImageHash >$< E.param (E.nullable E.text)
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

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: off_chain_vote_author
-- Description:
-----------------------------------------------------------------------------------------------------------------------------------
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

entityOffChainVoteAuthorDecoder :: D.Row (Entity OffChainVoteAuthor)
entityOffChainVoteAuthorDecoder =
  Entity
    <$> Id.idDecoder Id.OffChainVoteAuthorId
    <*> offChainVoteAuthorDecoder

offChainVoteAuthorDecoder :: D.Row OffChainVoteAuthor
offChainVoteAuthorDecoder =
  OffChainVoteAuthor
    <$> Id.idDecoder Id.OffChainVoteDataId -- offChainVoteAuthorOffChainVoteDataId
    <*> D.column (D.nullable D.text) -- offChainVoteAuthorName
    <*> D.column (D.nonNullable D.text) -- offChainVoteAuthorWitnessAlgorithm
    <*> D.column (D.nonNullable D.text) -- offChainVoteAuthorPublicKey
    <*> D.column (D.nonNullable D.text) -- offChainVoteAuthorSignature
    <*> D.column (D.nullable D.text) -- offChainVoteAuthorWarning

entityOffChainVoteAuthorEncoder :: E.Params (Entity OffChainVoteAuthor)
entityOffChainVoteAuthorEncoder =
  mconcat
    [ entityKey >$< Id.idEncoder Id.getOffChainVoteAuthorId
    , entityVal >$< offChainVoteAuthorEncoder
    ]

offChainVoteAuthorEncoder :: E.Params OffChainVoteAuthor
offChainVoteAuthorEncoder =
  mconcat
    [ offChainVoteAuthorOffChainVoteDataId >$< Id.idEncoder Id.getOffChainVoteDataId
    , offChainVoteAuthorName >$< E.param (E.nullable E.text)
    , offChainVoteAuthorWitnessAlgorithm >$< E.param (E.nonNullable E.text)
    , offChainVoteAuthorPublicKey >$< E.param (E.nonNullable E.text)
    , offChainVoteAuthorSignature >$< E.param (E.nonNullable E.text)
    , offChainVoteAuthorWarning >$< E.param (E.nullable E.text)
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

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: off_chain_vote_reference
-- Description:
-----------------------------------------------------------------------------------------------------------------------------------
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

entityOffChainVoteReferenceDecoder :: D.Row (Entity OffChainVoteReference)
entityOffChainVoteReferenceDecoder =
  Entity
    <$> Id.idDecoder Id.OffChainVoteReferenceId
    <*> offChainVoteReferenceDecoder

offChainVoteReferenceDecoder :: D.Row OffChainVoteReference
offChainVoteReferenceDecoder =
  OffChainVoteReference
    <$> Id.idDecoder Id.OffChainVoteDataId -- offChainVoteReferenceOffChainVoteDataId
    <*> D.column (D.nonNullable D.text) -- offChainVoteReferenceLabel
    <*> D.column (D.nonNullable D.text) -- offChainVoteReferenceUri
    <*> D.column (D.nullable D.text) -- offChainVoteReferenceHashDigest
    <*> D.column (D.nullable D.text) -- offChainVoteReferenceHashAlgorithm

entityOffChainVoteReferenceEncoder :: E.Params (Entity OffChainVoteReference)
entityOffChainVoteReferenceEncoder =
  mconcat
    [ entityKey >$< Id.idEncoder Id.getOffChainVoteReferenceId
    , entityVal >$< offChainVoteReferenceEncoder
    ]

offChainVoteReferenceEncoder :: E.Params OffChainVoteReference
offChainVoteReferenceEncoder =
  mconcat
    [ offChainVoteReferenceOffChainVoteDataId >$< Id.idEncoder Id.getOffChainVoteDataId
    , offChainVoteReferenceLabel >$< E.param (E.nonNullable E.text)
    , offChainVoteReferenceUri >$< E.param (E.nonNullable E.text)
    , offChainVoteReferenceHashDigest >$< E.param (E.nullable E.text)
    , offChainVoteReferenceHashAlgorithm >$< E.param (E.nullable E.text)
    ]

offChainVoteReferenceBulkEncoder :: E.Params ([Id.OffChainVoteDataId], [Text], [Text], [Maybe Text], [Maybe Text])
offChainVoteReferenceBulkEncoder =
  contrazip5
    (bulkEncoder $ Id.idBulkEncoder Id.getOffChainVoteDataId)
    (bulkEncoder $ E.nonNullable E.text)
    (bulkEncoder $ E.nonNullable E.text)
    (bulkEncoder $ E.nullable E.text)
    (bulkEncoder $ E.nullable E.text)

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: off_chain_vote_external_update
-- Description:
-----------------------------------------------------------------------------------------------------------------------------------
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

entityOffChainVoteExternalUpdateDecoder :: D.Row (Entity OffChainVoteExternalUpdate)
entityOffChainVoteExternalUpdateDecoder =
  Entity
    <$> Id.idDecoder Id.OffChainVoteExternalUpdateId
    <*> offChainVoteExternalUpdateDecoder

offChainVoteExternalUpdateDecoder :: D.Row OffChainVoteExternalUpdate
offChainVoteExternalUpdateDecoder =
  OffChainVoteExternalUpdate
    <$> Id.idDecoder Id.OffChainVoteDataId -- offChainVoteExternalUpdateOffChainVoteDataId
    <*> D.column (D.nonNullable D.text) -- offChainVoteExternalUpdateTitle
    <*> D.column (D.nonNullable D.text) -- offChainVoteExternalUpdateUri

entityOffChainVoteExternalUpdateEncoder :: E.Params (Entity OffChainVoteExternalUpdate)
entityOffChainVoteExternalUpdateEncoder =
  mconcat
    [ entityKey >$< Id.idEncoder Id.getOffChainVoteExternalUpdateId
    , entityVal >$< offChainVoteExternalUpdateEncoder
    ]

offChainVoteExternalUpdateEncoder :: E.Params OffChainVoteExternalUpdate
offChainVoteExternalUpdateEncoder =
  mconcat
    [ offChainVoteExternalUpdateOffChainVoteDataId >$< Id.idEncoder Id.getOffChainVoteDataId
    , offChainVoteExternalUpdateTitle >$< E.param (E.nonNullable E.text)
    , offChainVoteExternalUpdateUri >$< E.param (E.nonNullable E.text)
    ]

offChainVoteExternalUpdatesBulkEncoder :: E.Params ([Id.OffChainVoteDataId], [Text], [Text])
offChainVoteExternalUpdatesBulkEncoder =
  contrazip3
    (bulkEncoder $ Id.idBulkEncoder Id.getOffChainVoteDataId)
    (bulkEncoder $ E.nonNullable E.text)
    (bulkEncoder $ E.nonNullable E.text)

-----------------------------------------------------------------------------------------------------------------------------------
-- Table Name: off_chain_vote_fetch_error
-- Description:
-----------------------------------------------------------------------------------------------------------------------------------
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

entityOffChainVoteFetchErrorDecoder :: D.Row (Entity OffChainVoteFetchError)
entityOffChainVoteFetchErrorDecoder =
  Entity
    <$> Id.idDecoder Id.OffChainVoteFetchErrorId
    <*> offChainVoteFetchErrorDecoder

offChainVoteFetchErrorDecoder :: D.Row OffChainVoteFetchError
offChainVoteFetchErrorDecoder =
  OffChainVoteFetchError
    <$> Id.idDecoder Id.VotingAnchorId -- offChainVoteFetchErrorVotingAnchorId
    <*> D.column (D.nonNullable D.text) -- offChainVoteFetchErrorFetchError
    <*> D.column (D.nonNullable utcTimeAsTimestampDecoder) -- offChainVoteFetchErrorFetchTime
    <*> D.column (D.nonNullable $ fromIntegral <$> D.int8) -- offChainVoteFetchErrorRetryCount

entityOffChainVoteFetchErrorEncoder :: E.Params (Entity OffChainVoteFetchError)
entityOffChainVoteFetchErrorEncoder =
  mconcat
    [ entityKey >$< Id.idEncoder Id.getOffChainVoteFetchErrorId
    , entityVal >$< offChainVoteFetchErrorEncoder
    ]

offChainVoteFetchErrorEncoder :: E.Params OffChainVoteFetchError
offChainVoteFetchErrorEncoder =
  mconcat
    [ offChainVoteFetchErrorVotingAnchorId >$< Id.idEncoder Id.getVotingAnchorId
    , offChainVoteFetchErrorFetchError >$< E.param (E.nonNullable E.text)
    , offChainVoteFetchErrorFetchTime >$< E.param (E.nonNullable utcTimeAsTimestampEncoder)
    , offChainVoteFetchErrorRetryCount >$< E.param (E.nonNullable $ fromIntegral >$< E.int8)
    ]

offChainVoteFetchErrorBulkEncoder :: E.Params ([Id.VotingAnchorId], [Text], [UTCTime], [Word])
offChainVoteFetchErrorBulkEncoder =
  contrazip4
    (bulkEncoder (Id.idBulkEncoder Id.getVotingAnchorId))
    (bulkEncoder (E.nonNullable E.text))
    (bulkEncoder (E.nonNullable utcTimeAsTimestampEncoder))
    (bulkEncoder (E.nonNullable (fromIntegral >$< E.int4)))
