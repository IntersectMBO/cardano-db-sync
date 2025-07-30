{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.OffChain where

import Cardano.Prelude (ByteString, MonadIO (..), Proxy (..), Text, Word64, when)
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

import qualified Cardano.Db.Schema.Core.GovernanceAndVoting as SV
import qualified Cardano.Db.Schema.Core.OffChain as SO
import qualified Cardano.Db.Schema.Core.Pool as SP
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Schema.Types (PoolUrl, poolUrlDecoder, utcTimeAsTimestampDecoder, utcTimeAsTimestampEncoder)
import Cardano.Db.Statement.Function.Core (ResultType (..), ResultTypeBulk (..), mkDbCallStack, runDbSessionMain)
import Cardano.Db.Statement.Function.Delete (parameterisedDeleteWhere)
import Cardano.Db.Statement.Function.Insert (insertCheckUnique)
import Cardano.Db.Statement.Function.InsertBulk (ConflictStrategy (..), insertBulk, insertBulkWith)
import Cardano.Db.Statement.Function.Query (countAll)
import Cardano.Db.Statement.Pool (queryPoolHashIdExistsStmt, queryPoolMetadataRefIdExistsStmt)
import Cardano.Db.Statement.Types (DbInfo (..))
import Cardano.Db.Types (AnchorType, DbAction, VoteUrl, anchorTypeDecoder, voteUrlDecoder)

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
  (poolExists, metadataExists) <- runDbSessionMain (mkDbCallStack "checkPoolAndMetadata") $
    HsqlS.pipeline $ do
      poolResult <- HsqlP.statement poolHashId queryPoolHashIdExistsStmt
      metadataResult <- HsqlP.statement metadataRefId queryPoolMetadataRefIdExistsStmt
      pure (poolResult, metadataResult)

  -- Only insert if both exist
  when (poolExists && metadataExists) $
    runDbSessionMain (mkDbCallStack "insertOffChainPoolData") $
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
  runDbSessionMain (mkDbCallStack "queryOffChainPoolData") $
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
  runDbSessionMain (mkDbCallStack "queryUsedTicker") $
    HsqlSes.statement (poolHash, metaHash) queryUsedTickerStmt

--------------------------------------------------------------------------------
queryTestOffChainDataStmt :: HsqlStmt.Statement () [(Text, PoolUrl, ByteString, Id.PoolHashId)]
queryTestOffChainDataStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    offChainPoolDataTable = tableName (Proxy @SO.OffChainPoolData)
    poolMetadataRefTable = tableName (Proxy @SP.PoolMetadataRef)
    poolRetireTable = tableName (Proxy @SP.PoolRetire)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT pod.ticker_name, pmr.url, pmr.hash, pod.pool_id"
          , " FROM " <> offChainPoolDataTable <> " pod"
          , " INNER JOIN " <> poolMetadataRefTable <> " pmr"
          , " ON pod.pmr_id = pmr.id"
          , " WHERE NOT EXISTS ("
          , "   SELECT 1 FROM " <> poolRetireTable <> " pr"
          , "   WHERE pod.pool_id = pr.hash_id"
          , " )"
          ]

    decoder = HsqlD.rowList $ do
      tickerName <- HsqlD.column (HsqlD.nonNullable HsqlD.text)
      url <- HsqlD.column (HsqlD.nonNullable poolUrlDecoder)
      hash <- HsqlD.column (HsqlD.nonNullable HsqlD.bytea)
      poolId <- Id.idDecoder Id.PoolHashId
      pure (tickerName, url, hash, poolId)

queryTestOffChainData :: MonadIO m => DbAction m [(Text, PoolUrl, ByteString, Id.PoolHashId)]
queryTestOffChainData =
  runDbSessionMain (mkDbCallStack "queryTestOffChainData") $
    HsqlSes.statement () queryTestOffChainDataStmt

--------------------------------------------------------------------------------

-- | Query pool ticker name for pool
queryPoolTickerStmt :: HsqlStmt.Statement Id.PoolHashId (Maybe Text)
queryPoolTickerStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder = Id.idEncoder Id.getPoolHashId
    decoder = HsqlD.rowMaybe (HsqlD.column $ HsqlD.nonNullable HsqlD.text)
    offChainPoolDataTable = tableName (Proxy @SO.OffChainPoolData)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT " <> offChainPoolDataTable <> ".ticker_name"
          , " FROM " <> offChainPoolDataTable
          , " WHERE " <> offChainPoolDataTable <> ".pool_id = $1"
          , " ORDER BY " <> offChainPoolDataTable <> ".id DESC"
          , " LIMIT 1"
          ]

queryPoolTicker :: MonadIO m => Id.PoolHashId -> DbAction m (Maybe Text)
queryPoolTicker poolId =
  runDbSessionMain (mkDbCallStack "queryPoolTicker") $
    HsqlSes.statement poolId queryPoolTickerStmt

--------------------------------------------------------------------------------
-- OffChainPoolFetchError
--------------------------------------------------------------------------------
insertOffChainPoolFetchErrorStmt :: HsqlStmt.Statement SO.OffChainPoolFetchError ()
insertOffChainPoolFetchErrorStmt =
  insertCheckUnique
    SO.offChainPoolFetchErrorEncoder
    NoResult

insertCheckOffChainPoolFetchError :: MonadIO m => SO.OffChainPoolFetchError -> DbAction m ()
insertCheckOffChainPoolFetchError offChainPoolFetchError = do
  let poolHashId = SO.offChainPoolFetchErrorPoolId offChainPoolFetchError
  let metadataRefId = SO.offChainPoolFetchErrorPmrId offChainPoolFetchError

  -- Run checks in pipeline
  (poolExists, metadataExists) <- runDbSessionMain (mkDbCallStack "checkPoolAndMetadata") $
    HsqlS.pipeline $ do
      poolResult <- HsqlP.statement poolHashId queryPoolHashIdExistsStmt
      metadataResult <- HsqlP.statement metadataRefId queryPoolMetadataRefIdExistsStmt
      pure (poolResult, metadataResult)

  -- Only insert if both exist
  when (poolExists && metadataExists) $
    runDbSessionMain (mkDbCallStack "insertOffChainPoolFetchError") $
      HsqlS.statement offChainPoolFetchError insertOffChainPoolFetchErrorStmt

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
        , snd >$< HsqlE.param (HsqlE.nullable utcTimeAsTimestampEncoder)
        ]

    decoder = HsqlD.rowList $ do
      poolId <- Id.idDecoder Id.PoolHashId
      fetchTime <- HsqlD.column (HsqlD.nonNullable utcTimeAsTimestampDecoder)
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
  runDbSessionMain (mkDbCallStack "queryOffChainPoolFetchError") $
    HsqlSes.statement (hash, mFromTime) queryOffChainPoolFetchErrorStmt

--------------------------------------------------------------------------------

-- Count OffChainPoolFetchError records
countOffChainPoolFetchError :: MonadIO m => DbAction m Word64
countOffChainPoolFetchError =
  runDbSessionMain (mkDbCallStack "countOffChainPoolFetchError") $
    HsqlSes.statement () (countAll @SO.OffChainPoolFetchError)

--------------------------------------------------------------------------------
deleteOffChainPoolFetchErrorByPmrId :: MonadIO m => Id.PoolMetadataRefId -> DbAction m ()
deleteOffChainPoolFetchErrorByPmrId pmrId =
  runDbSessionMain (mkDbCallStack "deleteOffChainPoolFetchErrorByPmrId") $
    HsqlSes.statement pmrId (parameterisedDeleteWhere @SO.OffChainPoolFetchError "pmr_id" ">=" (Id.idEncoder Id.getPoolMetadataRefId))

--------------------------------------------------------------------------------
queryOffChainVoteWorkQueueDataStmt :: HsqlStmt.Statement Int [(UTCTime, Id.VotingAnchorId, ByteString, VoteUrl, AnchorType, Word)]
queryOffChainVoteWorkQueueDataStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    votingAnchorTableN = tableName (Proxy @SV.VotingAnchor)
    offChainVoteFetchErrorTableN = tableName (Proxy @SO.OffChainVoteFetchError)
    offChainVoteDataTableN = tableName (Proxy @SO.OffChainVoteData)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "WITH latest_errors AS ("
          , "  SELECT MAX(id) as max_id"
          , "  FROM " <> offChainVoteFetchErrorTableN
          , "  WHERE NOT EXISTS ("
          , "    SELECT 1 FROM " <> offChainVoteDataTableN <> " ocvd"
          , "    WHERE ocvd.voting_anchor_id = " <> offChainVoteFetchErrorTableN <> ".voting_anchor_id"
          , "  )"
          , "  GROUP BY voting_anchor_id"
          , ")"
          , "SELECT ocpfe.fetch_time, va.id, va.data_hash, va.url, va.type, ocpfe.retry_count"
          , " FROM " <> votingAnchorTableN <> " va"
          , " INNER JOIN " <> offChainVoteFetchErrorTableN <> " ocpfe ON ocpfe.voting_anchor_id = va.id"
          , " WHERE ocpfe.id IN (SELECT max_id FROM latest_errors)"
          , " AND va.type != 'constitution'"
          , " ORDER BY ocpfe.id ASC"
          , " LIMIT $1"
          ]

    encoder = HsqlE.param (HsqlE.nonNullable (fromIntegral >$< HsqlE.int4))

    decoder = HsqlD.rowList $ do
      fetchTime <- HsqlD.column (HsqlD.nonNullable utcTimeAsTimestampDecoder)
      vaId <- HsqlD.column (HsqlD.nonNullable (Id.VotingAnchorId <$> HsqlD.int8))
      vaHash <- HsqlD.column (HsqlD.nonNullable HsqlD.bytea)
      url <- HsqlD.column (HsqlD.nonNullable voteUrlDecoder)
      anchorType <- HsqlD.column (HsqlD.nonNullable anchorTypeDecoder)
      retryCount <- HsqlD.column (HsqlD.nonNullable (fromIntegral <$> HsqlD.int4))
      pure (fetchTime, vaId, vaHash, url, anchorType, retryCount)

queryOffChainVoteWorkQueueData :: MonadIO m => Int -> DbAction m [(UTCTime, Id.VotingAnchorId, ByteString, VoteUrl, AnchorType, Word)]
queryOffChainVoteWorkQueueData maxCount =
  runDbSessionMain (mkDbCallStack "queryOffChainVoteWorkQueueData") $
    HsqlSes.statement maxCount queryOffChainVoteWorkQueueDataStmt

--------------------------------------------------------------------------------
queryNewPoolWorkQueueDataStmt :: HsqlStmt.Statement Int [(Id.PoolHashId, Id.PoolMetadataRefId, PoolUrl, ByteString)]
queryNewPoolWorkQueueDataStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    poolHashTableN = tableName (Proxy @SP.PoolHash)
    poolMetadataRefTableN = tableName (Proxy @SP.PoolMetadataRef)
    offChainPoolDataTableN = tableName (Proxy @SO.OffChainPoolData)
    offChainPoolFetchErrorTableN = tableName (Proxy @SO.OffChainPoolFetchError)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "WITH latest_refs AS ("
          , "  SELECT MAX(id) as max_id"
          , "  FROM " <> poolMetadataRefTableN
          , "  GROUP BY pool_id"
          , ")"
          , "SELECT ph.id, pmr.id, pmr.url, pmr.hash"
          , " FROM " <> poolHashTableN <> " ph"
          , " INNER JOIN " <> poolMetadataRefTableN <> " pmr ON ph.id = pmr.pool_id"
          , " WHERE pmr.id IN (SELECT max_id FROM latest_refs)"
          , " AND NOT EXISTS ("
          , "   SELECT 1 FROM " <> offChainPoolDataTableN <> " pod"
          , "   WHERE pod.pmr_id = pmr.id"
          , " )"
          , " AND NOT EXISTS ("
          , "   SELECT 1 FROM " <> offChainPoolFetchErrorTableN <> " pofe"
          , "   WHERE pofe.pmr_id = pmr.id"
          , " )"
          , " LIMIT $1"
          ]

    encoder = HsqlE.param (HsqlE.nonNullable (fromIntegral >$< HsqlE.int4))

    decoder = HsqlD.rowList $ do
      phId <- HsqlD.column (HsqlD.nonNullable (Id.PoolHashId <$> HsqlD.int8))
      pmrId <- HsqlD.column (HsqlD.nonNullable (Id.PoolMetadataRefId <$> HsqlD.int8))
      url <- HsqlD.column (HsqlD.nonNullable poolUrlDecoder)
      hash <- HsqlD.column (HsqlD.nonNullable HsqlD.bytea)
      pure (phId, pmrId, url, hash)

queryNewPoolWorkQueueData :: MonadIO m => Int -> DbAction m [(Id.PoolHashId, Id.PoolMetadataRefId, PoolUrl, ByteString)]
queryNewPoolWorkQueueData maxCount =
  runDbSessionMain (mkDbCallStack "queryNewPoolWorkQueueData") $
    HsqlSes.statement maxCount queryNewPoolWorkQueueDataStmt

--------------------------------------------------------------------------------
queryOffChainPoolWorkQueueDataStmt :: HsqlStmt.Statement Int [(UTCTime, Id.PoolMetadataRefId, PoolUrl, ByteString, Id.PoolHashId, Word)]
queryOffChainPoolWorkQueueDataStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    poolHashTableN = tableName (Proxy @SP.PoolHash)
    poolMetadataRefTableN = tableName (Proxy @SP.PoolMetadataRef)
    offChainPoolFetchErrorTableN = tableName (Proxy @SO.OffChainPoolFetchError)
    offChainPoolDataTableN = tableName (Proxy @SO.OffChainPoolData)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "WITH latest_errors AS ("
          , "  SELECT MAX(id) as max_id"
          , "  FROM " <> offChainPoolFetchErrorTableN <> " pofe"
          , "  WHERE NOT EXISTS ("
          , "    SELECT 1 FROM " <> offChainPoolDataTableN <> " pod"
          , "    WHERE pod.pmr_id = pofe.pmr_id"
          , "  )"
          , "  GROUP BY pool_id"
          , ")"
          , "SELECT pofe.fetch_time, pofe.pmr_id, pmr.url, pmr.hash, ph.id, pofe.retry_count"
          , " FROM " <> poolHashTableN <> " ph"
          , " INNER JOIN " <> poolMetadataRefTableN <> " pmr ON ph.id = pmr.pool_id"
          , " INNER JOIN " <> offChainPoolFetchErrorTableN <> " pofe ON pofe.pmr_id = pmr.id"
          , " WHERE pofe.id IN (SELECT max_id FROM latest_errors)"
          , " ORDER BY pofe.id ASC"
          , " LIMIT $1"
          ]

    encoder = HsqlE.param (HsqlE.nonNullable (fromIntegral >$< HsqlE.int4))

    decoder = HsqlD.rowList $ do
      fetchTime <- HsqlD.column (HsqlD.nonNullable utcTimeAsTimestampDecoder)
      pmrId <- HsqlD.column (HsqlD.nonNullable (Id.PoolMetadataRefId <$> HsqlD.int8))
      url <- HsqlD.column (HsqlD.nonNullable poolUrlDecoder)
      hash <- HsqlD.column (HsqlD.nonNullable HsqlD.bytea)
      phId <- HsqlD.column (HsqlD.nonNullable (Id.PoolHashId <$> HsqlD.int8))
      retryCount <- HsqlD.column (HsqlD.nonNullable (fromIntegral <$> HsqlD.int4))
      pure (fetchTime, pmrId, url, hash, phId, retryCount)

queryOffChainPoolWorkQueueData :: MonadIO m => Int -> DbAction m [(UTCTime, Id.PoolMetadataRefId, PoolUrl, ByteString, Id.PoolHashId, Word)]
queryOffChainPoolWorkQueueData maxCount =
  runDbSessionMain (mkDbCallStack "queryOffChainPoolWorkQueueData") $
    HsqlSes.statement maxCount queryOffChainPoolWorkQueueDataStmt

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

--------------------------------------------------------------------------------

insertBulkOffChainVoteDataStmt :: HsqlStmt.Statement [SO.OffChainVoteData] [Id.OffChainVoteDataId]
insertBulkOffChainVoteDataStmt =
  insertBulk
    extractOffChainVoteData
    SO.offChainVoteDataBulkEncoder
    (WithResultBulk $ Id.idBulkDecoder Id.OffChainVoteDataId)
  where
    extractOffChainVoteData :: [SO.OffChainVoteData] -> ([Id.VotingAnchorId], [ByteString], [Text], [ByteString], [Maybe Text], [Text], [Maybe Text], [Maybe Bool])
    extractOffChainVoteData xs =
      ( map SO.offChainVoteDataVotingAnchorId xs
      , map SO.offChainVoteDataHash xs
      , map SO.offChainVoteDataJson xs
      , map SO.offChainVoteDataBytes xs
      , map SO.offChainVoteDataWarning xs
      , map SO.offChainVoteDataLanguage xs
      , map SO.offChainVoteDataComment xs
      , map SO.offChainVoteDataIsValid xs
      )

--------------------------------------------------------------------------------

insertBulkOffChainVoteDrepDataStmt :: HsqlStmt.Statement [SO.OffChainVoteDrepData] ()
insertBulkOffChainVoteDrepDataStmt =
  insertBulk
    extractOffChainVoteDrepData
    SO.offChainVoteDrepDataBulkEncoder
    NoResultBulk
  where
    extractOffChainVoteDrepData :: [SO.OffChainVoteDrepData] -> ([Id.OffChainVoteDataId], [Maybe Text], [Text], [Maybe Text], [Maybe Text], [Maybe Text], [Maybe Text], [Maybe Text])
    extractOffChainVoteDrepData xs =
      ( map SO.offChainVoteDrepDataOffChainVoteDataId xs
      , map SO.offChainVoteDrepDataPaymentAddress xs
      , map SO.offChainVoteDrepDataGivenName xs
      , map SO.offChainVoteDrepDataObjectives xs
      , map SO.offChainVoteDrepDataMotivations xs
      , map SO.offChainVoteDrepDataQualifications xs
      , map SO.offChainVoteDrepDataImageUrl xs
      , map SO.offChainVoteDrepDataImageHash xs
      )

--------------------------------------------------------------------------------
queryNewVoteWorkQueueDataStmt :: HsqlStmt.Statement Int [(Id.VotingAnchorId, ByteString, VoteUrl, AnchorType)]
queryNewVoteWorkQueueDataStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    votingAnchorTableN = tableName (Proxy @SV.VotingAnchor)
    offChainVoteDataTableN = tableName (Proxy @SO.OffChainVoteData)
    offChainVoteFetchErrorTableN = tableName (Proxy @SO.OffChainVoteFetchError)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id, data_hash, url, type"
          , " FROM " <> votingAnchorTableN <> " va"
          , " WHERE NOT EXISTS ("
          , "   SELECT 1 FROM " <> offChainVoteDataTableN <> " ocvd"
          , "   WHERE ocvd.voting_anchor_id = va.id"
          , " )"
          , " AND va.type != 'constitution'"
          , " AND NOT EXISTS ("
          , "   SELECT 1 FROM " <> offChainVoteFetchErrorTableN <> " ocvfe"
          , "   WHERE ocvfe.voting_anchor_id = va.id"
          , " )"
          , " LIMIT $1"
          ]

    encoder = HsqlE.param (HsqlE.nonNullable (fromIntegral >$< HsqlE.int4))

    decoder = HsqlD.rowList $ do
      vaId <- HsqlD.column (HsqlD.nonNullable (Id.VotingAnchorId <$> HsqlD.int8))
      vaHash <- HsqlD.column (HsqlD.nonNullable HsqlD.bytea)
      url <- HsqlD.column (HsqlD.nonNullable voteUrlDecoder)
      anchorType <- HsqlD.column (HsqlD.nonNullable anchorTypeDecoder)
      pure (vaId, vaHash, url, anchorType)

queryNewVoteWorkQueueData :: MonadIO m => Int -> DbAction m [(Id.VotingAnchorId, ByteString, VoteUrl, AnchorType)]
queryNewVoteWorkQueueData maxCount =
  runDbSessionMain (mkDbCallStack "queryNewVoteWorkQueueData") $
    HsqlSes.statement maxCount queryNewVoteWorkQueueDataStmt

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

--------------------------------------------------------------------------------

insertBulkOffChainVoteFetchErrorStmt :: HsqlStmt.Statement [SO.OffChainVoteFetchError] ()
insertBulkOffChainVoteFetchErrorStmt =
  insertBulkWith
    (IgnoreWithColumns ["voting_anchor_id", "retry_count"]) -- ON CONFLICT DO NOTHING
    False
    extractOffChainVoteFetchError
    SO.offChainVoteFetchErrorBulkEncoder
    NoResultBulk
  where
    extractOffChainVoteFetchError :: [SO.OffChainVoteFetchError] -> ([Id.VotingAnchorId], [Text], [UTCTime], [Word])
    extractOffChainVoteFetchError xs =
      ( map SO.offChainVoteFetchErrorVotingAnchorId xs
      , map SO.offChainVoteFetchErrorFetchError xs
      , map SO.offChainVoteFetchErrorFetchTime xs
      , map SO.offChainVoteFetchErrorRetryCount xs
      )

--------------------------------------------------------------------------------
insertBulkOffChainVoteGovActionDataStmt :: HsqlStmt.Statement [SO.OffChainVoteGovActionData] ()
insertBulkOffChainVoteGovActionDataStmt =
  insertBulk
    extractOffChainVoteGovActionData
    SO.offChainVoteGovActionDataBulkEncoder
    NoResultBulk
  where
    extractOffChainVoteGovActionData :: [SO.OffChainVoteGovActionData] -> ([Id.OffChainVoteDataId], [Text], [Text], [Text], [Text])
    extractOffChainVoteGovActionData xs =
      ( map SO.offChainVoteGovActionDataOffChainVoteDataId xs
      , map SO.offChainVoteGovActionDataTitle xs
      , map SO.offChainVoteGovActionDataAbstract xs
      , map SO.offChainVoteGovActionDataMotivation xs
      , map SO.offChainVoteGovActionDataRationale xs
      )

insertBulkOffChainVoteGovActionData :: MonadIO m => [SO.OffChainVoteGovActionData] -> DbAction m ()
insertBulkOffChainVoteGovActionData offChainVoteGovActionData =
  runDbSessionMain (mkDbCallStack "insertBulkOffChainVoteGovActionData") $
    HsqlS.statement offChainVoteGovActionData insertBulkOffChainVoteGovActionDataStmt

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
