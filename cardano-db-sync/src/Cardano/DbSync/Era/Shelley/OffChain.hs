{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.OffChain (
  insertOffChainResults,
  loadOffChainWorkQueue,
  runOffChainFetchThread,
) where

import Cardano.BM.Trace (Trace, logInfo)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncEnv, envOffChainPoolResultQueue, envOffChainPoolWorkQueue)
import Cardano.DbSync.Era.Shelley.OffChain.Http
import Cardano.DbSync.Era.Shelley.OffChain.Query
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

loadOffChainWorkQueue ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  StrictTBQueue IO OffChainPoolFetchRetry ->
  ReaderT SqlBackend m ()
loadOffChainWorkQueue _trce workQueue =
  -- If we try to write the to queue when it is full it will block. Therefore only add more to
  -- the queue if it is empty.
  whenM (liftIO $ atomically (isEmptyTBQueue workQueue)) $ do
    now <- liftIO Time.getPOSIXTime
    runnablePools <- filter (isRunnable now) <$> aquireOffChainPoolData now 100
    liftIO $ mapM_ queueInsert runnablePools
  where
    isRunnable :: POSIXTime -> OffChainPoolFetchRetry -> Bool
    isRunnable now pfr = retryRetryTime (opfrRetry pfr) <= now

    queueInsert :: OffChainPoolFetchRetry -> IO ()
    queueInsert = atomically . writeTBQueue workQueue

insertOffChainResults ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  StrictTBQueue IO OffChainPoolResult ->
  ReaderT SqlBackend m ()
insertOffChainResults trce resultQueue = do
  res <- liftIO . atomically $ flushTBQueue resultQueue
  let fetchErrorsLength = length $ filter isFetchError res
  unless (null res) $
    liftIO . logInfo trce $
      mconcat
        [ "Offchain pool metadata fetch: "
        , DB.textShow (length res - fetchErrorsLength)
        , " results, "
        , DB.textShow fetchErrorsLength
        , " fetch errors"
        ]
  mapM_ insert res
  where
    insert :: (MonadBaseControl IO m, MonadIO m) => OffChainPoolResult -> ReaderT SqlBackend m ()
    insert ofr =
      case ofr of
        OffChainPoolResultMetadata md -> void $ DB.insertCheckOffChainPoolData md
        OffChainPoolResultError fe -> void $ DB.insertCheckOffChainPoolFetchError fe

    isFetchError :: OffChainPoolResult -> Bool
    isFetchError fe =
      case fe of
        OffChainPoolResultMetadata {} -> False
        OffChainPoolResultError {} -> True

runOffChainFetchThread :: SyncEnv -> IO ()
runOffChainFetchThread syncEnv = do
  when (ioOffChainPoolData iopts) $ do
    logInfo trce "Running Offchain fetch thread"
    forever $ do
      threadDelay 60_000_000 -- 60 second sleep
      xs <- blockingFlushTBQueue (envOffChainPoolWorkQueue syncEnv)
      manager <- Http.newManager tlsManagerSettings
      now <- liftIO Time.getPOSIXTime
      mapM_ (queueInsert <=< fetchOffChainData trce manager now) xs
  where
    trce = getTrace syncEnv
    iopts = getInsertOptions syncEnv

    queueInsert :: OffChainPoolResult -> IO ()
    queueInsert = atomically . writeTBQueue (envOffChainPoolResultQueue syncEnv)

-- -------------------------------------------------------------------------------------------------

-- Blocks on an empty queue, but gets all elements in the queue if there is more than one.
blockingFlushTBQueue :: StrictTBQueue IO a -> IO [a]
blockingFlushTBQueue queue = do
  atomically $ do
    x <- readTBQueue queue
    xs <- flushTBQueue queue
    pure $ x : xs

fetchOffChainData :: Trace IO Text -> Http.Manager -> Time.POSIXTime -> OffChainPoolFetchRetry -> IO OffChainPoolResult
fetchOffChainData _tracer manager time pfr =
  convert <<$>> runExceptT $ do
    request <- parsePoolUrl $ opfrPoolUrl pfr
    httpGetOffChainPoolData manager request (opfrPoolUrl pfr) (opfrPoolMDHash pfr)
  where
    convert :: Either FetchError SimplifiedOffChainPoolData -> OffChainPoolResult
    convert eres =
      case eres of
        Right smd ->
          OffChainPoolResultMetadata $
            DB.OffChainPoolData
              { DB.offChainPoolDataPoolId = opfrPoolHashId pfr
              , DB.offChainPoolDataTickerName = spodTickerName smd
              , DB.offChainPoolDataHash = spodHash smd
              , DB.offChainPoolDataBytes = spodBytes smd
              , DB.offChainPoolDataJson = spodJson smd
              , DB.offChainPoolDataPmrId = opfrReferenceId pfr
              }
        Left err ->
          OffChainPoolResultError $
            DB.OffChainPoolFetchError
              { DB.offChainPoolFetchErrorPoolId = opfrPoolHashId pfr
              , DB.offChainPoolFetchErrorFetchTime = Time.posixSecondsToUTCTime time
              , DB.offChainPoolFetchErrorPmrId = opfrReferenceId pfr
              , DB.offChainPoolFetchErrorFetchError = show err
              , DB.offChainPoolFetchErrorRetryCount = retryCount (opfrRetry pfr)
              }
