{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Redundant pure" #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Cardano.DbSync.OffChain (
  insertOffChainPoolResults,
  insertOffChainVoteResults,
  loadOffChainPoolWorkQueue,
  loadOffChainVoteWorkQueue,
  runFetchOffChainPoolThread,
  runFetchOffChainVoteThread,
  fetchOffChainPoolData,
  fetchOffChainVoteData,
) where

import Cardano.BM.Trace (Trace, logInfo)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncEnv (..))
import Cardano.DbSync.Config.Types
import qualified Cardano.DbSync.Default as Default
import Cardano.DbSync.OffChain.Http
import Cardano.DbSync.OffChain.Query
import qualified Cardano.DbSync.OffChain.Vote.Types as Vote
import Cardano.DbSync.Types
import Cardano.Prelude
import Control.Concurrent.Class.MonadSTM.Strict (
  StrictTBQueue (..),
  flushTBQueue,
  isEmptyTBQueue,
  writeTBQueue,
 )
import Data.List (nubBy)
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Time.Clock.POSIX as Time
import GHC.IO.Exception (userError)
import qualified Hasql.Connection as HsqlC
import qualified Hasql.Pipeline as HsqlP
import qualified Hasql.Session as HsqlSes
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (tlsManagerSettings)

---------------------------------------------------------------------------------------------------------------------------------
-- Load OffChain Work Queue
---------------------------------------------------------------------------------------------------------------------------------
data LoadOffChainWorkQueue a m = LoadOffChainWorkQueue
  { lQueue :: StrictTBQueue IO a
  , lRetryTime :: a -> Retry
  , lGetData :: MonadIO m => POSIXTime -> Int -> DB.DbM [a]
  }

loadOffChainPoolWorkQueue ::
  Trace IO Text ->
  StrictTBQueue IO OffChainPoolWorkQueue ->
  DB.DbM ()
loadOffChainPoolWorkQueue trce workQueue =
  loadOffChainWorkQueue @DB.DbM
    trce
    LoadOffChainWorkQueue
      { lQueue = workQueue
      , lRetryTime = oPoolWqRetry
      , lGetData = getOffChainPoolData
      }

loadOffChainVoteWorkQueue ::
  Trace IO Text ->
  StrictTBQueue IO OffChainVoteWorkQueue ->
  DB.DbM ()
loadOffChainVoteWorkQueue trce workQueue =
  loadOffChainWorkQueue @DB.DbM
    trce
    LoadOffChainWorkQueue
      { lQueue = workQueue
      , lRetryTime = oVoteWqRetry
      , lGetData = getOffChainVoteData
      }

loadOffChainWorkQueue ::
  MonadIO m =>
  Trace IO Text ->
  LoadOffChainWorkQueue a m ->
  DB.DbM ()
loadOffChainWorkQueue _trce offChainWorkQueue = do
  whenM (liftIO $ atomically (isEmptyTBQueue (lQueue offChainWorkQueue))) $ do
    now <- liftIO Time.getPOSIXTime
    runnableOffChainData <- filter (isRunnable now) <$> lGetData offChainWorkQueue now 100
    liftIO $ mapM_ queueInsert runnableOffChainData
  where
    isRunnable now locWq = retryRetryTime (lRetryTime offChainWorkQueue locWq) <= now
    queueInsert locWq = atomically $ writeTBQueue (lQueue offChainWorkQueue) locWq

---------------------------------------------------------------------------------------------------------------------------------
-- Insert OffChain
---------------------------------------------------------------------------------------------------------------------------------
insertOffChainPoolResults ::
  Trace IO Text ->
  StrictTBQueue IO OffChainPoolResult ->
  DB.DbM ()
insertOffChainPoolResults trce resultQueue = do
  res <- liftIO . atomically $ flushTBQueue resultQueue
  unless (null res) $ do
    let resLength = length res
        resErrorsLength = length $ filter isFetchError res
    liftIO
      . logInfo trce
      $ logInsertOffChainResults "Pool" resLength resErrorsLength
  mapM_ insert res
  where
    insert :: OffChainPoolResult -> DB.DbM ()
    insert = \case
      OffChainPoolResultMetadata md -> void $ DB.insertCheckOffChainPoolData md
      OffChainPoolResultError fe -> void $ DB.insertCheckOffChainPoolFetchError fe

    isFetchError :: OffChainPoolResult -> Bool
    isFetchError = \case
      OffChainPoolResultMetadata {} -> False
      OffChainPoolResultError {} -> True

insertOffChainVoteResults ::
  Trace IO Text ->
  StrictTBQueue IO OffChainVoteResult ->
  DB.DbM ()
insertOffChainVoteResults trce resultQueue = do
  results <- liftIO . atomically $ flushTBQueue resultQueue
  unless (null results) $ do
    let resLength = length results
        resErrorsLength = length $ filter isFetchError results
    liftIO
      . logInfo trce
      $ logInsertOffChainResults "Voting Anchor" resLength resErrorsLength
  -- Process using a pipeline approach
  processResultsBatched results
  where
    isFetchError :: OffChainVoteResult -> Bool
    isFetchError = \case
      OffChainVoteResultMetadata {} -> False
      OffChainVoteResultError {} -> True

    processResultsBatched :: [OffChainVoteResult] -> DB.DbM ()
    processResultsBatched results = do
      -- Split by type
      let errors = [e | OffChainVoteResultError e <- results]
          metadataWithAccessors = [(md, acc) | OffChainVoteResultMetadata md acc <- results]
      -- Process errors in bulk if any
      unless (null errors) $
        insertBulkOffChainVoteFetchErrors errors
      -- Process metadata in a pipeline if any
      unless (null metadataWithAccessors) $ do
        -- First insert all metadata and collect the IDs
        metadataIds <- insertMetadataWithIds metadataWithAccessors
        -- Now prepare all the related data for bulk inserts
        let allGovActions = catMaybes [offChainVoteGovAction acc id | (_, acc, id) <- metadataIds]
            allDrepData = catMaybes [offChainVoteDrep acc id | (_, acc, id) <- metadataIds]
            allAuthors = concatMap (\(_, acc, id) -> offChainVoteAuthors acc id) metadataIds
            allReferences = concatMap (\(_, acc, id) -> offChainVoteReferences acc id) metadataIds
            allExternalUpdates = concatMap (\(_, acc, id) -> offChainVoteExternalUpdates acc id) metadataIds
        -- Execute all bulk inserts in a pipeline
        DB.runSession DB.mkDbCallStack $
          HsqlSes.pipeline $
            do
              -- Insert all related data in one pipeline
              unless (null allGovActions) $
                void $
                  HsqlP.statement allGovActions DB.insertBulkOffChainVoteGovActionDataStmt
              unless (null allDrepData) $
                void $
                  HsqlP.statement allDrepData DB.insertBulkOffChainVoteDrepDataStmt
              unless (null allAuthors) $
                void $
                  HsqlP.statement allAuthors DB.insertBulkOffChainVoteAuthorsStmt
              unless (null allReferences) $
                void $
                  HsqlP.statement allReferences DB.insertBulkOffChainVoteReferencesStmt
              unless (null allExternalUpdates) $
                void $
                  HsqlP.statement allExternalUpdates DB.insertBulkOffChainVoteExternalUpdatesStmt
              pure ()

    -- Helper function to insert metadata and get back IDs
    insertMetadataWithIds ::
      [(DB.OffChainVoteData, OffChainVoteAccessors)] ->
      DB.DbM [(DB.OffChainVoteData, OffChainVoteAccessors, DB.OffChainVoteDataId)]
    insertMetadataWithIds metadataWithAccessors = do
      -- Extract just the metadata for insert and deduplicate by unique key
      let metadata = map fst metadataWithAccessors
          deduplicatedMetadata =
            nubBy
              ( \a b ->
                  DB.offChainVoteDataVotingAnchorId a
                    == DB.offChainVoteDataVotingAnchorId b
                    && DB.offChainVoteDataHash a
                      == DB.offChainVoteDataHash b
              )
              metadata
      -- Insert and get IDs
      ids <- DB.insertBulkOffChainVoteData deduplicatedMetadata

      -- Return original data with IDs (note: length mismatch possible if duplicates were removed)
      pure $ zipWith (\(md, acc) id -> (md, acc, id)) metadataWithAccessors ids

    -- Bulk insert for errors (you'll need to create this statement)
    insertBulkOffChainVoteFetchErrors :: [DB.OffChainVoteFetchError] -> DB.DbM ()
    insertBulkOffChainVoteFetchErrors errors =
      DB.runSession DB.mkDbCallStack $ HsqlSes.statement errors DB.insertBulkOffChainVoteFetchErrorStmt

logInsertOffChainResults ::
  Text -> -- Pool of Vote
  Int -> -- length of tbQueue
  Int -> -- length of errors in tbQueue
  Text
logInsertOffChainResults offChainType resLength resErrorsLength =
  mconcat
    [ offChainType
    , " Offchain "
    , "metadata fetch: "
    , textShow (resLength - resErrorsLength)
    , " results, "
    , textShow resErrorsLength
    , " fetch errors"
    ]

---------------------------------------------------------------------------------------------------------------------------------
-- Run OffChain threads
---------------------------------------------------------------------------------------------------------------------------------
runFetchOffChainPoolThread :: SyncEnv -> IO ()
runFetchOffChainPoolThread syncEnv = do
  -- if disable gov is active then don't run voting anchor thread
  when (ioOffChainPoolData iopts) $ do
    logInfo trce "Running Offchain Pool fetch thread"
    pgconfig <- DB.runOrThrowIO (DB.readPGPass DB.PGPassDefaultEnv)
    connSetting <- case DB.toConnectionSetting pgconfig of
      Left err -> throwIO $ userError err
      Right setting -> pure setting

    bracket
      (DB.acquireConnection [connSetting])
      HsqlC.release
      ( \dbConn -> do
          let dbEnv = DB.createDbEnv dbConn Nothing (Just trce)
              -- Create a new SyncEnv with the new DbEnv but preserving all other fields
              threadSyncEnv = syncEnv {envDbEnv = dbEnv}
          forever $ do
            tDelay
            -- Determine isolation level based on sync state (dynamic optimization)
            mIsolationLevel <- Default.determineIsolationLevel syncEnv
            -- Use transaction runner with dynamic isolation level to eliminate redundant BEGIN/COMMIT
            _ <-
              DB.runDbTransLogged trce dbEnv mIsolationLevel $
                loadOffChainPoolWorkQueue trce (envOffChainPoolWorkQueue threadSyncEnv)
            poolq <- atomically $ flushTBQueue (envOffChainPoolWorkQueue threadSyncEnv)
            manager <- Http.newManager tlsManagerSettings
            now <- liftIO Time.getPOSIXTime
            mapM_ (queuePoolInsert <=< fetchOffChainPoolData trce manager now) poolq
      )
  where
    trce = getTrace syncEnv
    iopts = getInsertOptions syncEnv

    queuePoolInsert :: OffChainPoolResult -> IO ()
    queuePoolInsert = atomically . writeTBQueue (envOffChainPoolResultQueue syncEnv)

runFetchOffChainVoteThread :: SyncEnv -> IO ()
runFetchOffChainVoteThread syncEnv = do
  -- if disable gov is active then don't run voting anchor thread
  when (ioGov iopts) $ do
    logInfo trce "Running Offchain Vote Anchor fetch thread"
    pgconfig <- DB.runOrThrowIO (DB.readPGPass DB.PGPassDefaultEnv)
    connSetting <- case DB.toConnectionSetting pgconfig of
      Left err -> throwIO $ userError err
      Right setting -> pure setting

    bracket
      (DB.acquireConnection [connSetting])
      HsqlC.release
      ( \dbConn -> do
          let dbEnv = DB.createDbEnv dbConn Nothing (Just trce)
              -- Create a new SyncEnv with the new DbEnv but preserving all other fields
              threadSyncEnv = syncEnv {envDbEnv = dbEnv}
          -- Use the thread-specific SyncEnv for all operations
          forever $ do
            tDelay
            -- Determine isolation level based on sync state (dynamic optimization)
            mIsolationLevel <- Default.determineIsolationLevel syncEnv
            -- Use transaction runner with dynamic isolation level to eliminate redundant BEGIN/COMMIT
            _ <-
              DB.runDbTransLogged trce dbEnv mIsolationLevel $
                loadOffChainVoteWorkQueue trce (envOffChainVoteWorkQueue threadSyncEnv)
            voteq <- atomically $ flushTBQueue (envOffChainVoteWorkQueue threadSyncEnv)
            now <- liftIO Time.getPOSIXTime
            mapM_ (queueVoteInsert <=< fetchOffChainVoteData gateways now) voteq
      )
  where
    trce = getTrace syncEnv
    iopts = getInsertOptions syncEnv
    gateways = dncIpfsGateway $ envSyncNodeConfig syncEnv

    queueVoteInsert :: OffChainVoteResult -> IO ()
    queueVoteInsert = atomically . writeTBQueue (envOffChainVoteResultQueue syncEnv)

-- 5 minute sleep in milliseconds
tDelay :: IO ()
tDelay = threadDelay 300_000_000

---------------------------------------------------------------------------------------------------------------------------------
-- Fetch OffChain data
---------------------------------------------------------------------------------------------------------------------------------
fetchOffChainPoolData :: Trace IO Text -> Http.Manager -> Time.POSIXTime -> OffChainPoolWorkQueue -> IO OffChainPoolResult
fetchOffChainPoolData _tracer manager time oPoolWorkQ =
  convert <<$>> runExceptT $ do
    let url = oPoolWqUrl oPoolWorkQ
        metaHash = oPoolWqMetaHash oPoolWorkQ
    request <- parseOffChainUrl $ OffChainPoolUrl url
    httpGetOffChainPoolData manager request url (Just metaHash)
  where
    convert :: Either OffChainFetchError SimplifiedOffChainPoolData -> OffChainPoolResult
    convert eres =
      case eres of
        Right sPoolData ->
          OffChainPoolResultMetadata $
            DB.OffChainPoolData
              { DB.offChainPoolDataPoolId = oPoolWqHashId oPoolWorkQ
              , DB.offChainPoolDataTickerName = spodTickerName sPoolData
              , DB.offChainPoolDataHash = spodHash sPoolData
              , DB.offChainPoolDataBytes = spodBytes sPoolData
              , DB.offChainPoolDataJson = spodJson sPoolData
              , DB.offChainPoolDataPmrId = oPoolWqReferenceId oPoolWorkQ
              }
        Left err ->
          OffChainPoolResultError $
            DB.OffChainPoolFetchError
              { DB.offChainPoolFetchErrorPoolId = oPoolWqHashId oPoolWorkQ
              , DB.offChainPoolFetchErrorFetchTime = Time.posixSecondsToUTCTime time
              , DB.offChainPoolFetchErrorPmrId = oPoolWqReferenceId oPoolWorkQ
              , DB.offChainPoolFetchErrorFetchError = show err
              , DB.offChainPoolFetchErrorRetryCount = retryCount (oPoolWqRetry oPoolWorkQ)
              }

fetchOffChainVoteData :: [Text] -> Time.POSIXTime -> OffChainVoteWorkQueue -> IO OffChainVoteResult
fetchOffChainVoteData gateways time oVoteWorkQ =
  convert <<$>> runExceptT $ do
    let url = oVoteWqUrl oVoteWorkQ
        metaHash = oVoteWqMetaHash oVoteWorkQ
    httpGetOffChainVoteData gateways url (Just metaHash) (oVoteWqType oVoteWorkQ)
  where
    convert :: Either OffChainFetchError SimplifiedOffChainVoteData -> OffChainVoteResult
    convert eres =
      case eres of
        Right sVoteData ->
          case (sovaIsValidJson sVoteData, sovaOffChainVoteData sVoteData) of
            -- Scenario 1: Valid JSON + Valid CIP schema
            (True, Just offChainData) ->
              let
                minimalBody = Vote.getMinimalBody offChainData
                vdt =
                  DB.OffChainVoteData
                    { DB.offChainVoteDataLanguage = Vote.getLanguage offChainData
                    , DB.offChainVoteDataComment = Vote.textValue <$> Vote.comment minimalBody
                    , DB.offChainVoteDataBytes = sovaBytes sVoteData
                    , DB.offChainVoteDataHash = sovaHash sVoteData
                    , DB.offChainVoteDataJson = sovaJson sVoteData
                    , DB.offChainVoteDataVotingAnchorId = oVoteWqReferenceId oVoteWorkQ
                    , DB.offChainVoteDataWarning = sovaWarning sVoteData
                    , DB.offChainVoteDataIsValid = Just True
                    }
                gaF ocvdId = mkGovAction ocvdId offChainData
                drepF ocvdId = mkDrep ocvdId offChainData
                authorsF ocvdId = map (mkAuthor ocvdId) $ Vote.getAuthors offChainData
                referencesF ocvdId = map (mkReference ocvdId) $ mListToList $ Vote.references minimalBody
                externalUpdatesF ocvdId = map (mkexternalUpdates ocvdId) $ mListToList $ Vote.externalUpdates minimalBody
               in
                OffChainVoteResultMetadata vdt (OffChainVoteAccessors gaF drepF authorsF referencesF externalUpdatesF)
            -- Scenario 2 & 3: Valid JSON but invalid CIP schema OR Invalid JSON
            (_, _) ->
              let
                vdt =
                  DB.OffChainVoteData
                    { DB.offChainVoteDataLanguage = ""
                    , DB.offChainVoteDataComment = Nothing
                    , DB.offChainVoteDataBytes = sovaBytes sVoteData
                    , DB.offChainVoteDataHash = sovaHash sVoteData
                    , DB.offChainVoteDataJson = sovaJson sVoteData
                    , DB.offChainVoteDataVotingAnchorId = oVoteWqReferenceId oVoteWorkQ
                    , DB.offChainVoteDataWarning = sovaWarning sVoteData
                    , -- Just False if valid JSON but invalid schema, NULL if unparseable JSON
                      DB.offChainVoteDataIsValid =
                        if sovaIsValidJson sVoteData
                          then Just False
                          else Nothing
                    }
                gaF _ = Nothing
                drepF _ = Nothing
                authorsF _ = []
                referencesF _ = []
                externalUpdatesF _ = []
               in
                OffChainVoteResultMetadata vdt (OffChainVoteAccessors gaF drepF authorsF referencesF externalUpdatesF)
        Left err ->
          OffChainVoteResultError $
            DB.OffChainVoteFetchError
              { DB.offChainVoteFetchErrorVotingAnchorId = oVoteWqReferenceId oVoteWorkQ
              , DB.offChainVoteFetchErrorFetchError = show err
              , DB.offChainVoteFetchErrorFetchTime = Time.posixSecondsToUTCTime time
              , DB.offChainVoteFetchErrorRetryCount = retryCount (oVoteWqRetry oVoteWorkQ)
              }
    mkGovAction ocvdId = \case
      Vote.OffChainVoteDataGa dt ->
        Just $
          DB.OffChainVoteGovActionData
            { DB.offChainVoteGovActionDataOffChainVoteDataId = ocvdId
            , DB.offChainVoteGovActionDataTitle = Vote.textValue $ Vote.title $ Vote.body dt
            , DB.offChainVoteGovActionDataAbstract = Vote.textValue $ Vote.abstract $ Vote.body dt
            , DB.offChainVoteGovActionDataMotivation = Vote.textValue $ Vote.motivation $ Vote.body dt
            , DB.offChainVoteGovActionDataRationale = Vote.textValue $ Vote.rationale $ Vote.body dt
            }
      _ -> Nothing

    mkDrep ocvdId = \case
      Vote.OffChainVoteDataDr dt ->
        Just $
          DB.OffChainVoteDrepData
            { DB.offChainVoteDrepDataOffChainVoteDataId = ocvdId
            , DB.offChainVoteDrepDataPaymentAddress = Vote.textValue <$> Vote.paymentAddress (Vote.body dt)
            , DB.offChainVoteDrepDataGivenName = Vote.textValue $ Vote.givenName $ Vote.body dt
            , DB.offChainVoteDrepDataObjectives = Vote.textValue <$> Vote.objectives (Vote.body dt)
            , DB.offChainVoteDrepDataMotivations = Vote.textValue <$> Vote.motivations (Vote.body dt)
            , DB.offChainVoteDrepDataQualifications = Vote.textValue <$> Vote.qualifications (Vote.body dt)
            , DB.offChainVoteDrepDataImageUrl = Vote.textValue . Vote.content <$> Vote.image (Vote.body dt)
            , DB.offChainVoteDrepDataImageHash = Vote.textValue <$> (Vote.msha256 =<< Vote.image (Vote.body dt))
            }
      _ -> Nothing

    mkAuthor ocvdId au =
      DB.OffChainVoteAuthor
        { DB.offChainVoteAuthorOffChainVoteDataId = ocvdId
        , DB.offChainVoteAuthorName = Vote.textValue <$> Vote.name au
        , DB.offChainVoteAuthorWitnessAlgorithm = Vote.textValue $ Vote.witnessAlgorithm $ Vote.witness au
        , DB.offChainVoteAuthorPublicKey = Vote.textValue $ Vote.publicKey $ Vote.witness au
        , DB.offChainVoteAuthorSignature = Vote.textValue $ Vote.signature $ Vote.witness au
        , DB.offChainVoteAuthorWarning = Just "Failed to validate this signature" -- TODO: Conway
        }

    mkReference ocvdId ref =
      DB.OffChainVoteReference
        { DB.offChainVoteReferenceOffChainVoteDataId = ocvdId
        , DB.offChainVoteReferenceLabel = Vote.textValue $ Vote.label ref
        , DB.offChainVoteReferenceUri = Vote.textValue $ Vote.uri ref
        , DB.offChainVoteReferenceHashDigest = Vote.textValue . Vote.hashDigest <$> Vote.referenceHash ref
        , DB.offChainVoteReferenceHashAlgorithm = Vote.textValue . Vote.rhHashAlgorithm <$> Vote.referenceHash ref
        }

    mkexternalUpdates ocvdId eupd =
      DB.OffChainVoteExternalUpdate
        { DB.offChainVoteExternalUpdateOffChainVoteDataId = ocvdId
        , DB.offChainVoteExternalUpdateTitle = Vote.textValue $ Vote.euTitle eupd
        , DB.offChainVoteExternalUpdateUri = Vote.textValue $ Vote.euUri eupd
        }

    mListToList :: Maybe [a] -> [a]
    mListToList = \case
      Nothing -> []
      Just ls -> ls
