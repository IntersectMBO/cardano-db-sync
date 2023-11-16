{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.OffChain (
  insertOffChainResults,
  loadOffChainWorkQueue,
  runFetchOffChainThread,
) where

import Cardano.BM.Trace (Trace, logInfo)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncEnv (..))
import Cardano.DbSync.OffChain.Http
import Cardano.DbSync.OffChain.Query
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
  StrictTBQueue IO OffChainWorkQueueType ->
  ReaderT SqlBackend m ()
loadOffChainWorkQueue _trce workQueue =
  -- If we try to write the to queue when it is full it will block. Therefore only add more to
  -- the queue if it is empty.
  whenM (liftIO $ atomically (isEmptyTBQueue workQueue)) $ do
    now <- liftIO Time.getPOSIXTime
    runnablePools <- filter (isRunnable now) <$> aquireOffChainPoolData now 100
    liftIO $ mapM_ queueInsert runnablePools
  where
    isRunnable :: POSIXTime -> OffChainWorkQueueType -> Bool
    isRunnable now pfr = retryRetryTime (opfrRetry pfr) <= now

    queueInsert :: OffChainWorkQueueType -> IO ()
    queueInsert = atomically . writeTBQueue workQueue

insertOffChainResults ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  StrictTBQueue IO OffChainResultType ->
  ReaderT SqlBackend m ()
insertOffChainResults trce resultQueue = do
  res <- liftIO . atomically $ flushTBQueue resultQueue
  let fetchErrorsLength = length $ filter isFetchError res
  unless (null res)
    $ liftIO
      . logInfo trce
    $ mconcat
      [ "Offchain pool metadata fetch: "
      , DB.textShow (length res - fetchErrorsLength)
      , " results, "
      , DB.textShow fetchErrorsLength
      , " fetch errors"
      ]
  mapM_ insert res
  where
    insert :: (MonadBaseControl IO m, MonadIO m) => OffChainResultType -> ReaderT SqlBackend m ()
    insert ocrt =
      case ocrt of
        OffChainPoolResultType a ->
          case a of
            OffChainPoolResultMetadata md -> void $ DB.insertCheckOffChainPoolData md
            OffChainPoolResultError fe -> void $ DB.insertCheckOffChainPoolFetchError fe
        OffChainVoteResultType a ->
          case a of
            OffChainVoteResultMetadata md -> void $ DB.insertOffChainVoteData md
            OffChainVoteResultError fe -> void $ DB.insertOffChainVoteFetchError fe

    isFetchError :: OffChainResultType -> Bool
    isFetchError ocrt =
      case ocrt of
        OffChainPoolResultType a ->
          case a of
            OffChainPoolResultMetadata {} -> False
            OffChainPoolResultError {} -> True
        OffChainVoteResultType a ->
          case a of
            OffChainVoteResultMetadata {} -> False
            OffChainVoteResultError {} -> True

runFetchOffChainThread :: SyncEnv -> IO ()
runFetchOffChainThread syncEnv = do
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

    queueInsert :: OffChainResultType -> IO ()
    queueInsert ocResultType =
      case ocResultType of
        OffChainPoolResultType res -> atomically $ writeTBQueue (envOffChainPoolResultQueue syncEnv) $ OffChainPoolResultType res
        OffChainVoteResultType res -> atomically $ writeTBQueue (envOffChainVoteResultQueue syncEnv) $ OffChainVoteResultType res

-- -------------------------------------------------------------------------------------------------

-- Blocks on an empty queue, but gets all elements in the queue if there is more than one.
blockingFlushTBQueue :: StrictTBQueue IO a -> IO [a]
blockingFlushTBQueue queue = do
  atomically $ do
    x <- readTBQueue queue
    xs <- flushTBQueue queue
    pure $ x : xs

-- fetch the offchain data
fetchOffChainData :: Trace IO Text -> Http.Manager -> Time.POSIXTime -> OffChainWorkQueueType -> IO OffChainResultType
fetchOffChainData _tracer manager time offChainWorkQueue =
  convert <<$>> runExceptT $ do
    request <- parseOffChainUrl offChainWorkQueue
    httpGetOffChainData manager request offChainWorkQueue
  where
    convert :: Either OffChainFetchError SimplifiedOffChainDataType -> OffChainResultType
    convert eres =
      case eres of
        -- the response was good so we handle each offchain type
        Right sOffChainData ->
          case sOffChainData of
            SimplifiedOffChainPoolDataType oPoolWorkQ sPoolData ->
              OffChainPoolResultType $
                OffChainPoolResultMetadata $
                  DB.OffChainPoolData
                    { DB.offChainPoolDataPoolId = oPoolWqHashId oPoolWorkQ
                    , DB.offChainPoolDataTickerName = spodTickerName sPoolData
                    , DB.offChainPoolDataHash = spodHash sPoolData
                    , DB.offChainPoolDataBytes = spodBytes sPoolData
                    , DB.offChainPoolDataJson = spodJson sPoolData
                    , DB.offChainPoolDataPmrId = oPoolWqReferenceId oPoolWorkQ
                    }
            SimplifiedOffChainVoteDataType oVoteWorkQ sVoteData ->
              OffChainVoteResultType $
                OffChainVoteResultMetadata $
                  DB.OffChainVoteData
                    { DB.offChainVoteDataBytes = sovaBytes sVoteData
                    , DB.offChainVoteDataHash = sovaHash sVoteData
                    , DB.offChainVoteDataJson = sovaJson sVoteData
                    , DB.offChainVoteDataVotingAnchorId = oVoteWqReferenceId oVoteWorkQ
                    }
        -- there was an error when gettign offchain data
        Left offChainFetchErr ->
          case offChainFetchErr of
            OffChainPoolFetchError err ocPoolWQ ->
              OffChainPoolResultType $
                OffChainPoolResultError $
                  DB.OffChainPoolFetchError
                    { DB.offChainPoolFetchErrorPoolId = oPoolWqHashId ocPoolWQ
                    , DB.offChainPoolFetchErrorFetchTime = Time.posixSecondsToUTCTime time
                    , DB.offChainPoolFetchErrorPmrId = oPoolWqReferenceId ocPoolWQ
                    , DB.offChainPoolFetchErrorFetchError = show err
                    , DB.offChainPoolFetchErrorRetryCount = retryCount (oPoolWqRetry ocPoolWQ)
                    }
            OffChainVoteFetchError err ocVoteWQ ->
              OffChainVoteResultType $
                OffChainVoteResultError $
                  DB.OffChainVoteFetchError
                    { DB.offChainVoteFetchErrorVotingAnchorId = oVoteWqReferenceId ocVoteWQ
                    , DB.offChainVoteFetchErrorFetchError = show err
                    , DB.offChainVoteFetchErrorRetryCount = retryCount (oVoteWqRetry ocVoteWQ)
                    }
