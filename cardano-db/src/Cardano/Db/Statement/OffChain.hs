{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.OffChain where

import Cardano.Prelude (ByteString, MonadIO (..), Proxy (..), Text, when)
import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import Data.Time (UTCTime)
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Pipeline as HsqlP
import qualified Hasql.Session as HsqlS
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

import qualified Cardano.Db.Schema.Core.OffChain as SO
import qualified Cardano.Db.Schema.Core.Pool as SP
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Statement.Function.Core (ResultType (..), ResultTypeBulk (..), mkCallInfo, runDbSession)
import Cardano.Db.Statement.Function.Insert (insert, insertBulk, insertCheckUnique)
import Cardano.Db.Statement.GovernanceAndVoting (queryVotingAnchorIdExists)
import Cardano.Db.Statement.Pool (queryPoolHashIdExistsStmt, queryPoolMetadataRefIdExistsStmt)
import Cardano.Db.Statement.Types (DbInfo (..), Entity (..))
import Cardano.Db.Types (DbAction)

--------------------------------------------------------------------------------
-- OffChainPoolData
--------------------------------------------------------------------------------
insertOffChainPoolDataStmt :: HsqlStmt.Statement SO.OffChainPoolData ()
insertOffChainPoolDataStmt =
  insertCheckUnique
    SO.offChainPoolDataEncoder
    NoResult

insertCheckOffChainPoolData :: MonadIO m => SO.OffChainPoolData -> DbAction m ()
insertCheckOffChainPoolData offChainPoolData = do
  let poolHashId = SO.offChainPoolDataPoolId offChainPoolData
  let metadataRefId = SO.offChainPoolDataPmrId offChainPoolData

  -- Run checks in pipeline
  (poolExists, metadataExists) <- runDbSession (mkCallInfo "checkPoolAndMetadata") $
    HsqlS.pipeline $ do
      poolResult <- HsqlP.statement poolHashId queryPoolHashIdExistsStmt
      metadataResult <- HsqlP.statement metadataRefId queryPoolMetadataRefIdExistsStmt
      pure (poolResult, metadataResult)

  -- Only insert if both exist
  when (poolExists && metadataExists) $
    runDbSession (mkCallInfo "insertOffChainPoolData") $
      HsqlS.statement offChainPoolData insertOffChainPoolDataStmt

--------------------------------------------------------------------------------
queryOffChainPoolDataStmt :: HsqlStmt.Statement (ByteString, ByteString) (Maybe (Text, ByteString))
queryOffChainPoolDataStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    offChainPoolDataTable = tableName (Proxy @SO.OffChainPoolData)
    poolHashTable = tableName (Proxy @SP.PoolHash)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT pod.ticker_name, pod.bytes FROM "
          , offChainPoolDataTable
          , " pod"
          , " INNER JOIN "
          , poolHashTable
          , " ph ON pod.pool_id = ph.id"
          , " WHERE ph.hash_raw = $1"
          , " AND pod.hash = $2"
          , " LIMIT 1"
          ]

    encoder =
      mconcat
        [ fst >$< HsqlE.param (HsqlE.nonNullable HsqlE.bytea)
        , snd >$< HsqlE.param (HsqlE.nonNullable HsqlE.bytea)
        ]

    decoder =
      HsqlD.rowMaybe $
        (,)
          <$> HsqlD.column (HsqlD.nonNullable HsqlD.text)
          <*> HsqlD.column (HsqlD.nonNullable HsqlD.bytea)

queryOffChainPoolData :: MonadIO m => ByteString -> ByteString -> DbAction m (Maybe (Text, ByteString))
queryOffChainPoolData poolHash poolMetadataHash =
  runDbSession (mkCallInfo "queryOffChainPoolData") $
    HsqlSes.statement (poolHash, poolMetadataHash) queryOffChainPoolDataStmt

--------------------------------------------------------------------------------
queryUsedTickerStmt :: HsqlStmt.Statement (ByteString, ByteString) (Maybe Text)
queryUsedTickerStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    offChainPoolDataTable = tableName (Proxy @SO.OffChainPoolData)
    poolHashTable = tableName (Proxy @SP.PoolHash)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT pod.ticker_name FROM "
          , offChainPoolDataTable
          , " pod"
          , " INNER JOIN "
          , poolHashTable
          , " ph ON ph.id = pod.pool_id"
          , " WHERE ph.hash_raw = $1"
          , " AND pod.hash = $2"
          , " LIMIT 1"
          ]

    encoder =
      mconcat
        [ fst >$< HsqlE.param (HsqlE.nonNullable HsqlE.bytea)
        , snd >$< HsqlE.param (HsqlE.nonNullable HsqlE.bytea)
        ]

    decoder = HsqlD.rowMaybe (HsqlD.column $ HsqlD.nonNullable HsqlD.text)

queryUsedTicker :: MonadIO m => ByteString -> ByteString -> DbAction m (Maybe Text)
queryUsedTicker poolHash metaHash =
  runDbSession (mkCallInfo "queryUsedTicker") $
    HsqlSes.statement (poolHash, metaHash) queryUsedTickerStmt

--------------------------------------------------------------------------------
queryOffChainPoolFetchErrorStmt :: HsqlStmt.Statement (ByteString, Maybe UTCTime) [(SO.OffChainPoolFetchError, ByteString)]
queryOffChainPoolFetchErrorStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    offChainPoolFetchErrorTable = tableName (Proxy @SO.OffChainPoolFetchError)
    poolHashTable = tableName (Proxy @SP.PoolHash)
    poolMetadataRefTable = tableName (Proxy @SP.PoolMetadataRef)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT ocpfe.pool_id, ocpfe.fetch_time, ocpfe.pmr_id, "
          , "       ocpfe.fetch_error, ocpfe.retry_count, pmr.hash "
          , "FROM "
          , offChainPoolFetchErrorTable
          , " ocpfe "
          , "INNER JOIN "
          , poolHashTable
          , " ph ON ocpfe.pool_id = ph.id "
          , "INNER JOIN "
          , poolMetadataRefTable
          , " pmr ON ocpfe.pmr_id = pmr.id "
          , "WHERE ph.hash_raw = $1 "
          , "AND ($2 IS NULL OR ocpfe.fetch_time >= $2) "
          , "ORDER BY ocpfe.fetch_time DESC "
          , "LIMIT 10"
          ]

    encoder =
      mconcat
        [ fst >$< HsqlE.param (HsqlE.nonNullable HsqlE.bytea)
        , snd >$< HsqlE.param (HsqlE.nullable HsqlE.timestamptz)
        ]

    decoder = HsqlD.rowList $ do
      poolId <- Id.idDecoder Id.PoolHashId
      fetchTime <- HsqlD.column (HsqlD.nonNullable HsqlD.timestamptz)
      pmrId <- Id.idDecoder Id.PoolMetadataRefId
      fetchError <- HsqlD.column (HsqlD.nonNullable HsqlD.text)
      retryCount <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
      metadataHash <- HsqlD.column (HsqlD.nonNullable HsqlD.bytea)

      let fetchErr =
            SO.OffChainPoolFetchError
              { SO.offChainPoolFetchErrorPoolId = poolId
              , SO.offChainPoolFetchErrorFetchTime = fetchTime
              , SO.offChainPoolFetchErrorPmrId = pmrId
              , SO.offChainPoolFetchErrorFetchError = fetchError
              , SO.offChainPoolFetchErrorRetryCount = retryCount
              }

      pure (fetchErr, metadataHash)

queryOffChainPoolFetchError :: MonadIO m => ByteString -> Maybe UTCTime -> DbAction m [(SO.OffChainPoolFetchError, ByteString)]
queryOffChainPoolFetchError hash mFromTime =
  runDbSession (mkCallInfo "queryOffChainPoolFetchError") $
    HsqlSes.statement (hash, mFromTime) queryOffChainPoolFetchErrorStmt

--------------------------------------------------------------------------------
-- OffChainVoteAuthor
--------------------------------------------------------------------------------
insertBulkOffChainVoteAuthorsStmt :: HsqlStmt.Statement [SO.OffChainVoteAuthor] ()
insertBulkOffChainVoteAuthorsStmt =
  insertBulk
    extractOffChainVoteAuthor
    SO.offChainVoteAuthorBulkEncoder
    NoResultBulk
  where
    extractOffChainVoteAuthor :: [SO.OffChainVoteAuthor] -> ([Id.OffChainVoteDataId], [Maybe Text], [Text], [Text], [Text], [Maybe Text])
    extractOffChainVoteAuthor xs =
      ( map SO.offChainVoteAuthorOffChainVoteDataId xs
      , map SO.offChainVoteAuthorName xs
      , map SO.offChainVoteAuthorWitnessAlgorithm xs
      , map SO.offChainVoteAuthorPublicKey xs
      , map SO.offChainVoteAuthorSignature xs
      , map SO.offChainVoteAuthorWarning xs
      )

insertBulkOffChainVoteAuthors :: MonadIO m => [SO.OffChainVoteAuthor] -> DbAction m ()
insertBulkOffChainVoteAuthors offChainVoteAuthors =
  runDbSession (mkCallInfo "insertBulkOffChainVoteAuthors") $
    HsqlS.statement offChainVoteAuthors insertBulkOffChainVoteAuthorsStmt

--------------------------------------------------------------------------------
insertOffChainVoteDataStmt :: HsqlStmt.Statement SO.OffChainVoteData (Entity SO.OffChainVoteData)
insertOffChainVoteDataStmt =
  insertCheckUnique
    SO.offChainVoteDataEncoder
    (WithResult $ HsqlD.singleRow SO.entityOffChainVoteDataDecoder)

insertOffChainVoteData :: MonadIO m => SO.OffChainVoteData -> DbAction m (Maybe Id.OffChainVoteDataId)
insertOffChainVoteData offChainVoteData = do
  foundVotingAnchorId <- queryVotingAnchorIdExists (SO.offChainVoteDataVotingAnchorId offChainVoteData)
  if foundVotingAnchorId
    then do
      entity <-
        runDbSession (mkCallInfo "insertOffChainVoteData") $
          HsqlS.statement offChainVoteData insertOffChainVoteDataStmt
      pure $ Just (entityKey entity)
    else pure Nothing

--------------------------------------------------------------------------------
insertOffChainVoteDrepDataStmt :: HsqlStmt.Statement SO.OffChainVoteDrepData (Entity SO.OffChainVoteDrepData)
insertOffChainVoteDrepDataStmt =
  insert
    SO.offChainVoteDrepDataEncoder
    (WithResult $ HsqlD.singleRow SO.entityOffChainVoteDrepDataDecoder)

insertOffChainVoteDrepData :: MonadIO m => SO.OffChainVoteDrepData -> DbAction m Id.OffChainVoteDrepDataId
insertOffChainVoteDrepData drepData = do
  entity <-
    runDbSession (mkCallInfo "insertOffChainVoteDrepData") $
      HsqlS.statement drepData insertOffChainVoteDrepDataStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- OffChainVoteExternalUpdate
--------------------------------------------------------------------------------
insertBulkOffChainVoteExternalUpdatesStmt :: HsqlStmt.Statement [SO.OffChainVoteExternalUpdate] ()
insertBulkOffChainVoteExternalUpdatesStmt =
  insertBulk
    extractOffChainVoteExternalUpdate
    SO.offChainVoteExternalUpdatesBulkEncoder
    NoResultBulk
  where
    extractOffChainVoteExternalUpdate :: [SO.OffChainVoteExternalUpdate] -> ([Id.OffChainVoteDataId], [Text], [Text])
    extractOffChainVoteExternalUpdate xs =
      ( map SO.offChainVoteExternalUpdateOffChainVoteDataId xs
      , map SO.offChainVoteExternalUpdateTitle xs
      , map SO.offChainVoteExternalUpdateUri xs
      )

insertBulkOffChainVoteExternalUpdate :: MonadIO m => [SO.OffChainVoteExternalUpdate] -> DbAction m ()
insertBulkOffChainVoteExternalUpdate offChainVoteExternalUpdates =
  runDbSession (mkCallInfo "insertBulkOffChainVoteExternalUpdate") $
    HsqlS.statement offChainVoteExternalUpdates insertBulkOffChainVoteExternalUpdatesStmt

--------------------------------------------------------------------------------
insertOffChainVoteFetchErrorStmt :: HsqlStmt.Statement SO.OffChainVoteFetchError ()
insertOffChainVoteFetchErrorStmt =
  insert
    SO.offChainVoteFetchErrorEncoder
    NoResult

insertOffChainVoteFetchError :: MonadIO m => SO.OffChainVoteFetchError -> DbAction m ()
insertOffChainVoteFetchError offChainVoteFetchError = do
  foundVotingAnchor <-
    queryVotingAnchorIdExists (SO.offChainVoteFetchErrorVotingAnchorId offChainVoteFetchError)
  when foundVotingAnchor $ do
    runDbSession (mkCallInfo "insertOffChainVoteFetchError") $
      HsqlS.statement offChainVoteFetchError insertOffChainVoteFetchErrorStmt

--------------------------------------------------------------------------------
-- OffChainVoteGovActionData
--------------------------------------------------------------------------------
insertOffChainVoteGovActionDataStmt :: HsqlStmt.Statement SO.OffChainVoteGovActionData (Entity SO.OffChainVoteGovActionData)
insertOffChainVoteGovActionDataStmt =
  insert
    SO.offChainVoteGovActionDataEncoder
    (WithResult $ HsqlD.singleRow SO.entityOffChainVoteGovActionDataDecoder)

insertOffChainVoteGovActionData :: MonadIO m => SO.OffChainVoteGovActionData -> DbAction m Id.OffChainVoteGovActionDataId
insertOffChainVoteGovActionData offChainVoteGovActionData = do
  entity <-
    runDbSession (mkCallInfo "insertOffChainVoteGovActionData") $
      HsqlS.statement offChainVoteGovActionData insertOffChainVoteGovActionDataStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- OffChainVoteReference
--------------------------------------------------------------------------------
insertBulkOffChainVoteReferencesStmt :: HsqlStmt.Statement [SO.OffChainVoteReference] ()
insertBulkOffChainVoteReferencesStmt =
  insertBulk
    extractOffChainVoteReference
    SO.offChainVoteReferenceBulkEncoder
    NoResultBulk
  where
    extractOffChainVoteReference :: [SO.OffChainVoteReference] -> ([Id.OffChainVoteDataId], [Text], [Text], [Maybe Text], [Maybe Text])
    extractOffChainVoteReference xs =
      ( map SO.offChainVoteReferenceOffChainVoteDataId xs
      , map SO.offChainVoteReferenceLabel xs
      , map SO.offChainVoteReferenceUri xs
      , map SO.offChainVoteReferenceHashDigest xs
      , map SO.offChainVoteReferenceHashAlgorithm xs
      )

insertBulkOffChainVoteReferences :: MonadIO m => [SO.OffChainVoteReference] -> DbAction m ()
insertBulkOffChainVoteReferences offChainVoteReferences =
  runDbSession (mkCallInfo "insertBulkOffChainVoteReferences") $
    HsqlS.statement offChainVoteReferences insertBulkOffChainVoteReferencesStmt

-- off_chain_pool_data
-- off_chain_pool_fetch_error
-- off_chain_vote_author
-- off_chain_vote_data
-- off_chain_vote_drep_data
-- off_chain_vote_external_update
-- off_chain_vote_fetch_error
-- off_chain_vote_gov_action_data
-- off_chain_vote_reference
