{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
import Cardano.Db (runIohkLogging)
import qualified Cardano.Db as DB
import Cardano.DbSync.AppT (App, AppT (..), InsertOptions (..), MonadAppDB (..), SyncEnv (..), askInsertOptions, askTrace, runAppInIO)
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
import Control.Monad.Extra (whenJust)
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Time.Clock.POSIX as Time
import Database.Persist.Postgresql (withPostgresqlConn)
import Database.Persist.Sql (SqlBackend)
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (tlsManagerSettings)

---------------------------------------------------------------------------------------------------------------------------------
-- Load OffChain Work Queue
---------------------------------------------------------------------------------------------------------------------------------
data LoadOffChainWorkQueue a m = LoadOffChainWorkQueue
  { lQueue :: StrictTBQueue IO a
  , lRetryTime :: a -> Retry
  , lGetData :: MonadIO m => POSIXTime -> Int -> ReaderT SqlBackend m [a]
  }

loadOffChainPoolWorkQueue :: StrictTBQueue IO OffChainPoolWorkQueue -> ReaderT SqlBackend (AppT IO) ()
loadOffChainPoolWorkQueue workQueue = do
  trce <- lift askTrace
  loadOffChainWorkQueue
    trce
    LoadOffChainWorkQueue
      { lQueue = workQueue
      , lRetryTime = oPoolWqRetry
      , lGetData = getOffChainPoolData
      }

loadOffChainVoteWorkQueue :: StrictTBQueue IO OffChainVoteWorkQueue -> ReaderT SqlBackend (AppT IO) ()
loadOffChainVoteWorkQueue workQueue = do
  trce <- lift askTrace
  loadOffChainWorkQueue
    trce
    LoadOffChainWorkQueue
      { lQueue = workQueue
      , lRetryTime = oVoteWqRetry
      , lGetData = getOffChainVoteData
      }

loadOffChainWorkQueue ::
  forall a.
  Trace IO Text ->
  LoadOffChainWorkQueue a (AppT IO) ->
  ReaderT SqlBackend (AppT IO) ()
loadOffChainWorkQueue _trce offChainWorkQueue = do
  whenM (liftIO $ atomically (isEmptyTBQueue (lQueue offChainWorkQueue))) $ do
    now <- liftIO Time.getPOSIXTime
    runnableOffChainData <- filter (isRunnable now) <$> lGetData offChainWorkQueue now 100
    liftIO $ mapM_ queueInsert runnableOffChainData
  where
    isRunnable :: POSIXTime -> a -> Bool
    isRunnable now locWq = retryRetryTime (lRetryTime offChainWorkQueue locWq) <= now

    queueInsert :: a -> IO ()
    queueInsert locWq = atomically $ writeTBQueue (lQueue offChainWorkQueue) locWq

---------------------------------------------------------------------------------------------------------------------------------
-- Insert OffChain
---------------------------------------------------------------------------------------------------------------------------------
insertOffChainPoolResults ::
  StrictTBQueue IO OffChainPoolResult ->
  App ()
insertOffChainPoolResults resultQueue = do
  trce <- askTrace
  res <- liftIO . atomically $ flushTBQueue resultQueue
  unless (null res) $ do
    let resLength = length res
        resErrorsLength = length $ filter isFetchError res
    liftIO . logInfo trce $
      logInsertOffChainResults "Pool" resLength resErrorsLength
  mapM_ insert res
  where
    insert :: OffChainPoolResult -> App ()
    insert = \case
      OffChainPoolResultMetadata md -> dbQueryToApp $ DB.insertCheckOffChainPoolData md
      OffChainPoolResultError fe -> dbQueryToApp $ DB.insertCheckOffChainPoolFetchError fe

    isFetchError :: OffChainPoolResult -> Bool
    isFetchError = \case
      OffChainPoolResultMetadata {} -> False
      OffChainPoolResultError {} -> True

insertOffChainVoteResults ::
  StrictTBQueue IO OffChainVoteResult ->
  App ()
insertOffChainVoteResults resultQueue = do
  res <- liftIO . atomically $ flushTBQueue resultQueue
  unless (null res) $ do
    trce <- askTrace
    let resLength = length res
        resErrorsLength = length $ filter isFetchError res
    liftIO . logInfo trce $
      logInsertOffChainResults "Voting Anchor" resLength resErrorsLength
  mapM_ insert res
  where
    insert :: OffChainVoteResult -> App ()
    insert = \case
      OffChainVoteResultMetadata md accessors -> do
        mocvdId <- dbQueryToApp $ DB.insertOffChainVoteData md
        whenJust mocvdId $ \ocvdId -> do
          whenJust (offChainVoteGovAction accessors ocvdId) $ \ocvga ->
            void $ dbQueryToApp $ DB.insertOffChainVoteGovActionData ocvga
          whenJust (offChainVoteDrep accessors ocvdId) $ \ocvdr ->
            void $ dbQueryToApp $ DB.insertOffChainVoteDrepData ocvdr
          dbQueryToApp $ DB.insertOffChainVoteAuthors $ offChainVoteAuthors accessors ocvdId
          dbQueryToApp $ DB.insertOffChainVoteReference $ offChainVoteReferences accessors ocvdId
          dbQueryToApp $ DB.insertOffChainVoteExternalUpdate $ offChainVoteExternalUpdates accessors ocvdId
      OffChainVoteResultError fe -> dbQueryToApp $ DB.insertOffChainVoteFetchError fe

    isFetchError :: OffChainVoteResult -> Bool
    isFetchError = \case
      OffChainVoteResultMetadata {} -> False
      OffChainVoteResultError {} -> True

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
    , DB.textShow (resLength - resErrorsLength)
    , " results, "
    , DB.textShow resErrorsLength
    , " fetch errors"
    ]

---------------------------------------------------------------------------------------------------------------------------------
-- Run OffChain threads
---------------------------------------------------------------------------------------------------------------------------------

runFetchOffChainPoolThread :: App ()
runFetchOffChainPoolThread = do
  syncEnv@SyncEnv {..} <- ask
  trce <- askTrace
  iopts <- askInsertOptions
  when (ioOffChainPoolData iopts) $ do
    liftIO $ logInfo trce "Running Offchain Pool fetch thread"
    liftIO $
      runIohkLogging trce $
        withPostgresqlConn envConnectionString $
          \backendPool -> liftIO $
            forever $ do
              tDelay
              -- load the offChain vote work queue using the db
              runAppInIO syncEnv $ runReaderT (loadOffChainPoolWorkQueue envOffChainPoolWorkQueue) backendPool
              -- _ <- runReaderT (loadOffChainPoolWorkQueue envOffChainPoolWorkQueue) backendPool
              poolq <- atomically $ flushTBQueue envOffChainPoolWorkQueue
              manager <- Http.newManager tlsManagerSettings
              now <- liftIO Time.getPOSIXTime
              mapM_ (processPoolQueue syncEnv manager now) poolq
  where
    processPoolQueue :: SyncEnv -> Http.Manager -> Time.POSIXTime -> OffChainPoolWorkQueue -> IO ()
    processPoolQueue syncEnv manager now workQueue = do
      result <- runAppInIO syncEnv (fetchOffChainPoolData manager now workQueue)
      atomically $ queuePoolInsert syncEnv result

    queuePoolInsert :: SyncEnv -> OffChainPoolResult -> STM ()
    queuePoolInsert SyncEnv {..} = writeTBQueue envOffChainPoolResultQueue

runFetchOffChainVoteThread :: App ()
runFetchOffChainVoteThread = do
  syncEnv@SyncEnv {..} <- ask
  trce <- askTrace
  iopts <- askInsertOptions
  -- if disable gov is active then don't run voting anchor thread
  when (ioGov iopts) $ do
    liftIO $ logInfo trce "Running Offchain Vote fetch thread"
    liftIO $
      runIohkLogging trce $
        withPostgresqlConn envConnectionString $ \backendVote ->
          liftIO $ forever $ do
            tDelay
            -- Load the offChain vote work queue using the db
            liftIO $ runAppInIO syncEnv $ runReaderT (loadOffChainVoteWorkQueue envOffChainVoteWorkQueue) backendVote
            voteq <- atomically $ flushTBQueue envOffChainVoteWorkQueue
            manager <- Http.newManager tlsManagerSettings
            now <- Time.getPOSIXTime
            mapM_ (processVoteQueue syncEnv manager now) voteq -- Ensure we process each queue item properly
  where
    processVoteQueue :: SyncEnv -> Http.Manager -> Time.POSIXTime -> OffChainVoteWorkQueue -> IO ()
    processVoteQueue syncEnv manager now workQueue = do
      result <- runAppInIO syncEnv (fetchOffChainVoteData manager now workQueue)
      atomically $ queueVoteInsert syncEnv result

    queueVoteInsert :: SyncEnv -> OffChainVoteResult -> STM ()
    queueVoteInsert SyncEnv {..} = writeTBQueue envOffChainVoteResultQueue

-- 5 minute sleep in milliseconds
tDelay :: IO ()
tDelay = threadDelay 300_000_000

---------------------------------------------------------------------------------------------------------------------------------
-- Fetch OffChain data
---------------------------------------------------------------------------------------------------------------------------------
fetchOffChainPoolData :: Http.Manager -> Time.POSIXTime -> OffChainPoolWorkQueue -> App OffChainPoolResult
fetchOffChainPoolData manager time oPoolWorkQ = do
  eres <- liftIO $ runExceptT runFetch
  convert eres
  where
    runFetch :: ExceptT OffChainFetchError IO SimplifiedOffChainPoolData
    runFetch = do
      let url = oPoolWqUrl oPoolWorkQ
          metaHash = oPoolWqMetaHash oPoolWorkQ
      request <- parseOffChainUrl $ OffChainPoolUrl url
      httpGetOffChainPoolData manager request url (Just metaHash)

    -- convert <<$>> runExceptT $ do
    --   let url = oPoolWqUrl oPoolWorkQ
    --       metaHash = oPoolWqMetaHash oPoolWorkQ
    --   request <- parseOffChainUrl $ OffChainPoolUrl url
    --   httpGetOffChainPoolData manager request url (Just metaHash)
    convert :: Either OffChainFetchError SimplifiedOffChainPoolData -> App OffChainPoolResult
    convert eres =
      case eres of
        Right sPoolData ->
          pure $
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
          pure $
            OffChainPoolResultError $
              DB.OffChainPoolFetchError
                { DB.offChainPoolFetchErrorPoolId = oPoolWqHashId oPoolWorkQ
                , DB.offChainPoolFetchErrorFetchTime = Time.posixSecondsToUTCTime time
                , DB.offChainPoolFetchErrorPmrId = oPoolWqReferenceId oPoolWorkQ
                , DB.offChainPoolFetchErrorFetchError = show err
                , DB.offChainPoolFetchErrorRetryCount = retryCount (oPoolWqRetry oPoolWorkQ)
                }

fetchOffChainVoteData :: Http.Manager -> Time.POSIXTime -> OffChainVoteWorkQueue -> App OffChainVoteResult
fetchOffChainVoteData manager time oVoteWorkQ = do
  eres <- liftIO $ runExceptT runFetch
  convert eres
  where
    runFetch :: ExceptT OffChainFetchError IO SimplifiedOffChainVoteData
    runFetch = do
      let url = oVoteWqUrl oVoteWorkQ
          metaHash = oVoteWqMetaHash oVoteWorkQ
      request <- parseOffChainUrl $ OffChainVoteUrl url
      httpGetOffChainVoteData manager request url (Just metaHash) (oVoteWqType oVoteWorkQ)

    convert :: Either OffChainFetchError SimplifiedOffChainVoteData -> App OffChainVoteResult
    convert eres =
      case eres of
        Right sVoteData ->
          let
            offChainData = sovaOffChainVoteData sVoteData
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
                , DB.offChainVoteDataIsValid = Nothing
                }
            gaF ocvdId = mkGovAction ocvdId offChainData
            drepF ocvdId = mkDrep ocvdId offChainData
            authorsF ocvdId = map (mkAuthor ocvdId) $ Vote.getAuthors offChainData
            referencesF ocvdId = map (mkReference ocvdId) $ mListToList $ Vote.references minimalBody
            externalUpdatesF ocvdId = map (mkexternalUpdates ocvdId) $ mListToList $ Vote.externalUpdates minimalBody
           in
            pure $ OffChainVoteResultMetadata vdt (OffChainVoteAccessors gaF drepF authorsF referencesF externalUpdatesF)
        Left err ->
          pure $
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
      _other -> Nothing

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
            , DB.offChainVoteDrepDataImageUrl = Vote.textValue . Vote.contentUrl <$> Vote.image (Vote.body dt)
            , DB.offChainVoteDrepDataImageHash = Vote.textValue . Vote.sha256 <$> Vote.image (Vote.body dt)
            }
      _other -> Nothing

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
