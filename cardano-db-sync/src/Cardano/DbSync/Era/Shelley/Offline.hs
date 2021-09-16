{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.DbSync.Era.Shelley.Offline
  ( insertOfflineResults
  , loadOfflineWorkQueue
  , runOfflineFetchThread
  ) where

import           Cardano.Prelude hiding (handle)

import           Cardano.BM.Trace (Trace, logInfo, logWarning)

import           Cardano.DbSync.Era.Shelley.Offline.Http
import           Cardano.DbSync.Era.Shelley.Offline.Query
import           Cardano.DbSync.Era.Shelley.Offline.Types

import           Cardano.Sync.Types

import           Control.Monad.Class.MonadSTM.Strict (TBQueue, flushTBQueue, isEmptyTBQueue,
                   readTBQueue, writeTBQueue)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Except.Extra (handleExceptT, left)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Time.Clock.POSIX as Time

import qualified Cardano.Crypto.Hash.Blake2b as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto

import           Cardano.Db
import qualified Cardano.Db as DB

import           Cardano.Sync.LedgerState
import           Cardano.Sync.Util

import           Database.Persist.Sql (SqlBackend)

import           Network.HTTP.Client (HttpException (..))
import qualified Network.HTTP.Client as Http
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Types.Status as Http


loadOfflineWorkQueue
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> TBQueue IO PoolFetchRetry -> ReaderT SqlBackend m ()
loadOfflineWorkQueue _trce workQueue =
    -- If we try to writ the to queue when it is full it will block. Therefore only add more to
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

insertOfflineResults
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> TBQueue IO FetchResult -> ReaderT SqlBackend m ()
insertOfflineResults trce resultQueue = do
    res <- liftIO . atomically $ flushTBQueue resultQueue
    let fetchErrors = length $ filter isFetchError res
    unless (null res) $
      liftIO . logInfo trce $ mconcat
        [ "Offline pool metadata fetch: ", textShow (length res - fetchErrors), " results, "
        , textShow fetchErrors, " fetch errors"
        ]
    mapM_ insert res
  where
    insert :: (MonadBaseControl IO m, MonadIO m) => FetchResult -> ReaderT SqlBackend m ()
    insert fr =
      -- If a rollback occurs after the PoolMetadataRef is fetched, the insertion of the
      -- result can fail. In this insert fail case, we simply log and drop the exception.
      case fr of
          ResultMetadata md -> DB.insertAbortForeignKey (logWarning trce) $ void $ DB.insertPoolOfflineData md
          ResultError fe -> DB.insertAbortForeignKey (logWarning trce) $ void $ DB.insertPoolOfflineFetchError fe

    isFetchError :: FetchResult -> Bool
    isFetchError fe =
       case fe of
        ResultMetadata {} -> False
        ResultError {} -> True

runOfflineFetchThread :: Trace IO Text -> LedgerEnv -> IO ()
runOfflineFetchThread trce lenv = do
    logInfo trce "Running Offline fetch thread"
    forever $ do
      threadDelay 60_000_000 -- 60 second sleep
      xs <- blockingFlushTBQueue (leOfflineWorkQueue lenv)
      manager <- Http.newManager tlsManagerSettings
      mapM_ (queueInsert <=< fetchOfflineData trce manager) xs
  where
    queueInsert :: FetchResult -> IO ()
    queueInsert = atomically . writeTBQueue (leOfflineResultQueue lenv)

-- -------------------------------------------------------------------------------------------------

-- Blocks on an empty queue, but gets all elements in the queue if there is more than one.
blockingFlushTBQueue :: TBQueue IO a -> IO [a]
blockingFlushTBQueue queue = do
  atomically $ do
    x <- readTBQueue queue
    xs <- flushTBQueue queue
    pure $ x : xs


fetchOfflineData :: Trace IO Text -> Http.Manager -> PoolFetchRetry -> IO FetchResult
fetchOfflineData _tracer manager pfr =
    convert <<$>> runExceptT $ do
      request <- handleExceptT wrapHttpException
                    $ Http.parseRequest (Text.unpack $ unPoolUrl poolMetadataUrl)

      (respBS, status) <- httpGet512BytesMax poolMetadataUrl request manager

      when (Http.statusCode status /= 200) .
        left $ FEHttpResponse poolMetadataUrl (Http.statusCode status)

      decodedMetadata <-
            case Aeson.eitherDecode' (LBS.fromStrict respBS) of
              Left err -> left $ FEJsonDecodeFail poolMetadataUrl (Text.pack err)
              Right res -> pure res

      let metadataHash = Crypto.digest (Proxy :: Proxy Crypto.Blake2b_256) respBS
          expectedHash = unPoolMetaHash (pfrPoolMDHash pfr)

      when (PoolMetaHash metadataHash /= pfrPoolMDHash pfr) .
        left $ FEHashMismatch poolMetadataUrl (renderByteArray expectedHash) (renderByteArray metadataHash)

      pure $ DB.PoolOfflineData
                { DB.poolOfflineDataPoolId = pfrPoolHashId pfr
                , DB.poolOfflineDataTickerName = unPoolTicker $ pomTicker decodedMetadata
                , DB.poolOfflineDataHash = metadataHash
                , DB.poolOfflineDataBytes = respBS
                  -- Instead of inserting the `respBS` here, we encode the JSON and then store that.
                  -- This is necessary because the PostgreSQL JSON parser can reject some ByteStrings
                  -- that the Aeson parser accepts.
                , DB.poolOfflineDataJson = Text.decodeUtf8 $ LBS.toStrict (Aeson.encode decodedMetadata)
                , DB.poolOfflineDataPmrId = pfrReferenceId pfr
                }
  where
    poolMetadataUrl :: PoolUrl
    poolMetadataUrl = pfrPoolUrl pfr

    convert :: Either FetchError DB.PoolOfflineData -> FetchResult
    convert eres =
      case eres of
        Right md -> ResultMetadata md
        Left err ->
            ResultError $
              DB.PoolOfflineFetchError
                { DB.poolOfflineFetchErrorPoolId = pfrPoolHashId pfr
                , DB.poolOfflineFetchErrorFetchTime = Time.posixSecondsToUTCTime (retryFetchTime $ pfrRetry pfr)
                , DB.poolOfflineFetchErrorPmrId = pfrReferenceId pfr
                , DB.poolOfflineFetchErrorFetchError = renderFetchError err
                , DB.poolOfflineFetchErrorRetryCount = retryCount (pfrRetry pfr)
                }

    wrapHttpException :: HttpException -> FetchError
    wrapHttpException err = FEUrlParseFail poolMetadataUrl (textShow err)
