{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
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
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncEnv (..))
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
import Control.Monad.Trans.Control (MonadBaseControl)
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

loadOffChainPoolWorkQueue ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  StrictTBQueue IO OffChainPoolWorkQueue ->
  ReaderT SqlBackend m ()
loadOffChainPoolWorkQueue trce workQueue =
  loadOffChainWorkQueue
    trce
    LoadOffChainWorkQueue
      { lQueue = workQueue
      , lRetryTime = oPoolWqRetry
      , lGetData = getOffChainPoolData
      }

loadOffChainVoteWorkQueue ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  StrictTBQueue IO OffChainVoteWorkQueue ->
  ReaderT SqlBackend m ()
loadOffChainVoteWorkQueue trce workQueue =
  loadOffChainWorkQueue
    trce
    LoadOffChainWorkQueue
      { lQueue = workQueue
      , lRetryTime = oVoteWqRetry
      , lGetData = getOffChainVoteData
      }

loadOffChainWorkQueue ::
  forall a m.
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  LoadOffChainWorkQueue a m ->
  ReaderT SqlBackend m ()
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
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  StrictTBQueue IO OffChainPoolResult ->
  ReaderT SqlBackend m ()
insertOffChainPoolResults trce resultQueue = do
  res <- liftIO . atomically $ flushTBQueue resultQueue
  unless (null res) $ do
    let resLength = length res
        resErrorsLength = length $ filter isFetchError res
    liftIO . logInfo trce $
      logInsertOffChainResults "Pool" resLength resErrorsLength
  mapM_ insert res
  where
    insert :: (MonadBaseControl IO m, MonadIO m) => OffChainPoolResult -> ReaderT SqlBackend m ()
    insert = \case
      OffChainPoolResultMetadata md -> void $ DB.insertCheckOffChainPoolData md
      OffChainPoolResultError fe -> void $ DB.insertCheckOffChainPoolFetchError fe

    isFetchError :: OffChainPoolResult -> Bool
    isFetchError = \case
      OffChainPoolResultMetadata {} -> False
      OffChainPoolResultError {} -> True

insertOffChainVoteResults ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  StrictTBQueue IO OffChainVoteResult ->
  ReaderT SqlBackend m ()
insertOffChainVoteResults trce resultQueue = do
  res <- liftIO . atomically $ flushTBQueue resultQueue
  unless (null res) $ do
    let resLength = length res
        resErrorsLength = length $ filter isFetchError res
    liftIO . logInfo trce $
      logInsertOffChainResults "Voting Anchor" resLength resErrorsLength
  mapM_ insert res
  where
    insert :: (MonadBaseControl IO m, MonadIO m) => OffChainVoteResult -> ReaderT SqlBackend m ()
    insert = \case
      OffChainVoteResultMetadata md accessors -> do
        mocvdId <- DB.insertOffChainVoteData md
        whenJust mocvdId $ \ocvdId -> do
          DB.insertOffChainVoteAuthors $ offChainVoteAuthors accessors ocvdId
          DB.insertOffChainVoteReference $ offChainVoteReferences accessors ocvdId
          DB.insertOffChainVoteExternalUpdate $ offChainVoteExternalUpdates accessors ocvdId
      OffChainVoteResultError fe -> void $ DB.insertOffChainVoteFetchError fe

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
runFetchOffChainPoolThread :: SyncEnv -> IO ()
runFetchOffChainPoolThread syncEnv = do
  -- if dissable gov is active then don't run voting anchor thread
  when (ioOffChainPoolData iopts) $ do
    logInfo trce "Running Offchain Pool fetch thread"
    runIohkLogging trce $
      withPostgresqlConn (envConnectionString syncEnv) $
        \backendPool -> liftIO $
          forever $ do
            tDelay
            -- load the offChain vote work queue using the db
            _ <- runReaderT (loadOffChainPoolWorkQueue trce (envOffChainPoolWorkQueue syncEnv)) backendPool
            poolq <- atomically $ flushTBQueue (envOffChainPoolWorkQueue syncEnv)
            manager <- Http.newManager tlsManagerSettings
            now <- liftIO Time.getPOSIXTime
            mapM_ (queuePoolInsert <=< fetchOffChainPoolData trce manager now) poolq
  where
    trce = getTrace syncEnv
    iopts = getInsertOptions syncEnv

    queuePoolInsert :: OffChainPoolResult -> IO ()
    queuePoolInsert = atomically . writeTBQueue (envOffChainPoolResultQueue syncEnv)

runFetchOffChainVoteThread :: SyncEnv -> IO ()
runFetchOffChainVoteThread syncEnv = do
  -- if dissable gov is active then don't run voting anchor thread
  when (ioGov iopts) $ do
    logInfo trce "Running Offchain Vote Anchor fetch thread"
    runIohkLogging trce $
      withPostgresqlConn (envConnectionString syncEnv) $
        \backendVote -> liftIO $
          forever $ do
            tDelay
            -- load the offChain vote work queue using the db
            _ <- runReaderT (loadOffChainVoteWorkQueue trce (envOffChainVoteWorkQueue syncEnv)) backendVote
            voteq <- atomically $ flushTBQueue (envOffChainVoteWorkQueue syncEnv)
            manager <- Http.newManager tlsManagerSettings
            now <- liftIO Time.getPOSIXTime
            mapM_ (queueVoteInsert <=< fetchOffChainVoteData trce manager now) voteq
  where
    trce = getTrace syncEnv
    iopts = getInsertOptions syncEnv

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

fetchOffChainVoteData :: Trace IO Text -> Http.Manager -> Time.POSIXTime -> OffChainVoteWorkQueue -> IO OffChainVoteResult
fetchOffChainVoteData _tracer manager time oVoteWorkQ =
  convert <<$>> runExceptT $ do
    let url = oVoteWqUrl oVoteWorkQ
        metaHash = oVoteWqMetaHash oVoteWorkQ
    request <- parseOffChainUrl $ OffChainVoteUrl url
    httpGetOffChainVoteData manager request url (Just metaHash) (oVoteWqType oVoteWorkQ == DB.GovActionAnchor)
  where
    convert :: Either OffChainFetchError SimplifiedOffChainVoteData -> OffChainVoteResult
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
                , DB.offChainVoteDataTitle = Vote.getTitle offChainData
                , DB.offChainVoteDataAbstract = Vote.getAbstract offChainData
                , DB.offChainVoteDataMotivation = Vote.getMotivation offChainData
                , DB.offChainVoteDataRationale = Vote.getRationale offChainData
                , DB.offChainVoteDataBytes = sovaBytes sVoteData
                , DB.offChainVoteDataHash = sovaHash sVoteData
                , DB.offChainVoteDataJson = sovaJson sVoteData
                , DB.offChainVoteDataVotingAnchorId = oVoteWqReferenceId oVoteWorkQ
                , DB.offChainVoteDataWarning = sovaWarning sVoteData
                , DB.offChainVoteDataIsValid = Nothing
                }
            authorsF ocvdId = map (mkAuthor ocvdId) $ Vote.getAuthors offChainData
            referencesF ocvdId = map (mkReference ocvdId) $ mListToList $ Vote.references minimalBody
            externalUpdatesF ocvdId = map (mkexternalUpdates ocvdId) $ mListToList $ Vote.externalUpdates minimalBody
           in
            OffChainVoteResultMetadata vdt (OffChainVoteAccessors authorsF referencesF externalUpdatesF)
        Left err ->
          OffChainVoteResultError $
            DB.OffChainVoteFetchError
              { DB.offChainVoteFetchErrorVotingAnchorId = oVoteWqReferenceId oVoteWorkQ
              , DB.offChainVoteFetchErrorFetchError = show err
              , DB.offChainVoteFetchErrorFetchTime = Time.posixSecondsToUTCTime time
              , DB.offChainVoteFetchErrorRetryCount = retryCount (oVoteWqRetry oVoteWorkQ)
              }
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
