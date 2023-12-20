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
import Cardano.DbSync.Types
import Cardano.Prelude
import Control.Concurrent.Class.MonadSTM.Strict (
  StrictTBQueue (..),
  flushTBQueue,
  isEmptyTBQueue,
  writeTBQueue,
 )
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
      OffChainVoteResultMetadata md -> void $ DB.insertOffChainVoteData md
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
    request <- parseOffChainPoolUrl $ oPoolWqUrl oPoolWorkQ
    httpGetOffChainPoolData manager request (OffChainPoolUrl url) (Just $ OffChainPoolHash metaHash)
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
    request <- parseOffChainVoteUrl $ oVoteWqUrl oVoteWorkQ
    httpGetOffChainVoteData manager request (OffChainVoteUrl url) (Just $ OffChainVoteHash metaHash)
  where
    convert :: Either OffChainFetchError SimplifiedOffChainVoteData -> OffChainVoteResult
    convert eres =
      case eres of
        Right sVoteData ->
          OffChainVoteResultMetadata $
            DB.OffChainVoteData
              { DB.offChainVoteDataBytes = sovaBytes sVoteData
              , DB.offChainVoteDataHash = sovaHash sVoteData
              , DB.offChainVoteDataJson = sovaJson sVoteData
              , DB.offChainVoteDataVotingAnchorId = oVoteWqReferenceId oVoteWorkQ
              , DB.offChainVoteDataWarning = sovaWarning sVoteData
              }
        Left err ->
          OffChainVoteResultError $
            DB.OffChainVoteFetchError
              { DB.offChainVoteFetchErrorVotingAnchorId = oVoteWqReferenceId oVoteWorkQ
              , DB.offChainVoteFetchErrorFetchError = show err
              , DB.offChainVoteFetchErrorFetchTime = Time.posixSecondsToUTCTime time
              , DB.offChainVoteFetchErrorRetryCount = retryCount (oVoteWqRetry oVoteWorkQ)
              }
