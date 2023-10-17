{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Offline (
  insertOfflineResults,
  loadOfflineWorkQueue,
  runOfflineFetchThread,
) where

import Cardano.BM.Trace (Trace, logInfo)
import Cardano.Db
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncEnv, envOfflineResultQueue, envOfflineWorkQueue)
import Cardano.DbSync.Era.Shelley.Offline.Http
import Cardano.DbSync.Era.Shelley.Offline.Query
import Cardano.DbSync.Types
import Cardano.Prelude
import Control.Concurrent.Class.MonadSTM.Strict (
  StrictTBQueue,
  flushTBQueue,
  isEmptyTBQueue,
  readTBQueue,
  writeTBQueue,
 )
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Time.Clock.POSIX as Time
import Database.Persist.Sql (SqlBackend)
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (tlsManagerSettings)

loadOfflineWorkQueue ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  StrictTBQueue IO PoolFetchRetry ->
  ReaderT SqlBackend m ()
loadOfflineWorkQueue _trce workQueue =
  -- If we try to write the to queue when it is full it will block. Therefore only add more to
  -- the queue if it is empty.
  whenM (liftIO $ atomically (isEmptyTBQueue workQueue)) $ do
    now <- liftIO Time.getPOSIXTime
    runnablePools <- filter (isRunnable now) <$> queryOfflinePoolData now 100
    liftIO $ mapM_ queueInsert runnablePools
  where
    isRunnable :: POSIXTime -> PoolFetchRetry -> Bool
    isRunnable now pfr = retryRetryTime (pfrRetry pfr) <= now

    queueInsert :: PoolFetchRetry -> IO ()
    queueInsert = atomically . writeTBQueue workQueue

insertOfflineResults ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  StrictTBQueue IO FetchResult ->
  ReaderT SqlBackend m ()
insertOfflineResults trce resultQueue = do
  res <- liftIO . atomically $ flushTBQueue resultQueue
  let fetchErrors = length $ filter isFetchError res
  unless (null res) $
    liftIO . logInfo trce $
      mconcat
        [ "Offline pool metadata fetch: "
        , textShow (length res - fetchErrors)
        , " results, "
        , textShow fetchErrors
        , " fetch errors"
        ]
  mapM_ insert res
  where
    insert :: (MonadBaseControl IO m, MonadIO m) => FetchResult -> ReaderT SqlBackend m ()
    insert fr =
      case fr of
        ResultMetadata md -> void $ DB.insertCheckPoolOfflineData md
        ResultError fe -> void $ DB.insertCheckPoolOfflineFetchError fe

    isFetchError :: FetchResult -> Bool
    isFetchError fe =
      case fe of
        ResultMetadata {} -> False
        ResultError {} -> True

runOfflineFetchThread :: SyncEnv -> IO ()
runOfflineFetchThread syncEnv = do
  when (ioOfflineData iopts) $ do
    logInfo trce "Running Offline fetch thread"
    forever $ do
      threadDelay 60_000_000 -- 60 second sleep
      xs <- blockingFlushTBQueue (envOfflineWorkQueue syncEnv)
      manager <- Http.newManager tlsManagerSettings
      now <- liftIO Time.getPOSIXTime
      mapM_ (queueInsert <=< fetchOfflineData trce manager now) xs
  where
    trce = getTrace syncEnv
    iopts = getInsertOptions syncEnv

    queueInsert :: FetchResult -> IO ()
    queueInsert = atomically . writeTBQueue (envOfflineResultQueue syncEnv)

-- -------------------------------------------------------------------------------------------------

-- Blocks on an empty queue, but gets all elements in the queue if there is more than one.
blockingFlushTBQueue :: StrictTBQueue IO a -> IO [a]
blockingFlushTBQueue queue = do
  atomically $ do
    x <- readTBQueue queue
    xs <- flushTBQueue queue
    pure $ x : xs

fetchOfflineData :: Trace IO Text -> Http.Manager -> Time.POSIXTime -> PoolFetchRetry -> IO FetchResult
fetchOfflineData _tracer manager time pfr =
  convert <<$>> runExceptT $ do
    request <- parsePoolUrl $ pfrPoolUrl pfr
    httpGetPoolOfflineData manager request (pfrPoolUrl pfr) (pfrPoolMDHash pfr)
  where
    convert :: Either FetchError SimplifiedPoolOfflineData -> FetchResult
    convert eres =
      case eres of
        Right smd ->
          ResultMetadata $
            DB.PoolOfflineData
              { DB.poolOfflineDataPoolId = pfrPoolHashId pfr
              , DB.poolOfflineDataTickerName = spodTickerName smd
              , DB.poolOfflineDataHash = spodHash smd
              , DB.poolOfflineDataBytes = spodBytes smd
              , DB.poolOfflineDataJson = spodJson smd
              , DB.poolOfflineDataPmrId = pfrReferenceId pfr
              }
        Left err ->
          ResultError $
            DB.PoolOfflineFetchError
              { DB.poolOfflineFetchErrorPoolId = pfrPoolHashId pfr
              , DB.poolOfflineFetchErrorFetchTime = Time.posixSecondsToUTCTime time
              , DB.poolOfflineFetchErrorPmrId = pfrReferenceId pfr
              , DB.poolOfflineFetchErrorFetchError = show err
              , DB.poolOfflineFetchErrorRetryCount = retryCount (pfrRetry pfr)
              }
