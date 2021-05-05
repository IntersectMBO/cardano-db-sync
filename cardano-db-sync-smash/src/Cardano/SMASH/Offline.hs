{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.SMASH.Offline
  ( fetchInsertNewPoolMetadata
  , fetchInsertNewPoolMetadataOld
  , runOfflineFetchThread
  ) where

import           Cardano.Prelude hiding (from, groupBy, on, retry)

import           Cardano.BM.Trace (Trace, logInfo, logWarning)

import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Trans.Except.Extra (handleExceptT, hoistEither, left)

import           Cardano.SMASH.DB (DataLayer (..), postgresqlDataLayer)
import           Cardano.SMASH.FetchQueue
import           Cardano.SMASH.Types (FetchError (..), PoolFetchError (..),
                   bytestringToPoolMetaHash, pomTicker)

import           Data.Aeson (eitherDecode')
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Data.Time as Time
import           Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import qualified Data.Time.Clock.POSIX as Time

import qualified Cardano.Crypto.Hash.Blake2b as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto

import           Cardano.Db
import qualified Cardano.Db as DB

import qualified Data.ByteString.Base16 as B16

import           Database.Esqueleto (Entity (..), InnerJoin (..), SqlExpr, Value, ValueList, desc,
                   from, groupBy, in_, just, max_, notExists, on, orderBy, select, subList_select,
                   unValue, where_, (==.), (^.))
import           Database.Persist.Sql (SqlBackend)

import           Network.HTTP.Client (HttpException (..))
import qualified Network.HTTP.Client as Http
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Types.Status as Http

import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import qualified Shelley.Spec.Ledger.TxBody as Shelley

-- This is what we call from the actual block-syncing code.
fetchInsertNewPoolMetadata
    :: DataLayer
    -> Trace IO Text
    -> DB.PoolMetadataRefId
    -> PoolIdentifier
    -> Shelley.PoolMetadata
    -> IO ()
fetchInsertNewPoolMetadata dataLayer tracer refId poolId md  = do
    now <- Time.getPOSIXTime
    void . fetchInsertNewPoolMetadataOld dataLayer tracer fetchInsertDefault $
      PoolFetchRetry
        { pfrReferenceId = refId
        , pfrPoolIdWtf = poolId
        , pfrPoolUrl = PoolUrl . Shelley.urlToText . Shelley._poolMDUrl $ md
        , pfrPoolMDHash = bytestringToPoolMetaHash . Shelley._poolMDHash $ md
        , pfrRetry = newRetry now
        }

-- Please note that it is possible that a user submits the same pool hash
-- several times which will result in the fetch being done for each of the entry.
-- We do this in order to make sure that the metadata wasn't fixed in the meantime.
fetchInsertNewPoolMetadataOld
    :: DataLayer
    -> Trace IO Text
    -> (DataLayer -> PoolIdentifier -> Trace IO Text -> PoolFetchRetry -> ExceptT FetchError IO ())
    -> PoolFetchRetry
    -> IO PoolFetchRetry
fetchInsertNewPoolMetadataOld dataLayer tracer fetchInsert pfr = do

    logInfo tracer . showRetryTimes $ pfrRetry pfr

    -- We extract the @PoolId@ before so we can map the error to that @PoolId@.
    let poolId = pfrPoolIdWtf pfr
    let poolHash = pfrPoolMDHash pfr

    -- POSIX fetch time for the (initial) fetch.
    let fetchTimePOSIX :: POSIXTime
        fetchTimePOSIX = fetchTime $ pfrRetry pfr

    -- The current retry counter.
    let currRetryCount :: Word
        currRetryCount = retryCount $ pfrRetry pfr

    res <- runExceptT (fetchInsert dataLayer poolId tracer pfr)

    -- In the case all went well, we do nothing, but if something went wrong
    -- we log that and add the error to the database.
    case res of
        Right () -> do
            logInfo tracer "Pool metadata was fetched with success!"
            pure pfr

        Left err -> do
            logInfo tracer "Pool metadata was NOT fetched with success!"

            -- Increase the retry count.
            let newRetryCount = currRetryCount + 1

            -- Re-calculate the time for the next retry.
            let retry = retryAgain fetchTimePOSIX newRetryCount

            let poolMetadataReferenceId = pfrReferenceId pfr
            let fetchError = renderFetchError err

            -- The generated fetch error
            let _poolFetchError = PoolFetchError fetchTimePOSIX poolId poolHash fetchError

            let addFetchError = dlAddFetchError dataLayer

            -- Here we add the fetch error. The fetch time is always constant.
            _pmfeIdE <- addFetchError $ PoolMetadataFetchError
                (posixSecondsToUTCTime fetchTimePOSIX)
                poolId
                poolHash
                poolMetadataReferenceId
                fetchError
                newRetryCount

            logWarning tracer fetchError

            -- Here we update the the counter and retry time to update.
            pure $ pfr { pfrRetry = retry }

-- |We pass in the @PoolId@ so we can know from which pool the error occured.
fetchInsertDefault
    :: DataLayer
    -> PoolIdentifier
    -> Trace IO Text
    -> PoolFetchRetry
    -> ExceptT FetchError IO ()
fetchInsertDefault dataLayer poolId tracer pfr = do
    -- This is a bit bad to do each time, but good enough for now.
    manager <- liftIO $ Http.newManager tlsManagerSettings

    let poolMetadataURL = getPoolUrl $ pfrPoolUrl pfr

    liftIO . logInfo tracer $ "Request URL: " <> poolMetadataURL

    -- This is a weird Error.
    request <- handleExceptT (\(_ :: HttpException) -> FEUrlParseFail poolId poolMetadataURL poolMetadataURL)
                $ Http.parseRequest (toS poolMetadataURL)

    (respBS, status) <- httpGetMax512Bytes poolId poolMetadataURL request manager

    when (Http.statusCode status /= 200) .
      left $ FEHttpResponse poolId poolMetadataURL (Http.statusCode status)

    liftIO . logInfo tracer $ "Response: " <> show (Http.statusCode status)

    decodedMetadata <- case eitherDecode' (LBS.fromStrict respBS) of
                        Left err     -> left $ FEJsonDecodeFail poolId poolMetadataURL (toS err)
                        Right result -> pure result

    -- Let's check the hash
    let hashFromMetadata = Crypto.digest (Proxy :: Proxy Crypto.Blake2b_256) respBS
        expectedHash = getPoolMetaHash (pfrPoolMDHash pfr)

    if bytestringToPoolMetaHash hashFromMetadata /= pfrPoolMDHash pfr
      then left $ FEHashMismatch poolId expectedHash (renderByteStringHex hashFromMetadata) poolMetadataURL
      else liftIO . logInfo tracer $ "Inserting pool data with hash: " <> expectedHash

    let addPoolMetadata = dlAddPoolMetadata dataLayer

    _ <- liftIO $
            addPoolMetadata
                (Just $ pfrReferenceId pfr)
                (pfrPoolIdWtf pfr)
                (pfrPoolMDHash pfr)
                (PoolMetadataRaw $ decodeUtf8 respBS)
                (pomTicker decodedMetadata)

    liftIO $ logInfo tracer (decodeUtf8 respBS)

-- This is run as a new thread with the fetchLoop looping forever.
runOfflineFetchThread :: SqlBackend -> Trace IO Text -> IO ()
runOfflineFetchThread backend trce = do
    liftIO $ logInfo trce "Runing Offline fetch thread"
    let dataLayer = postgresqlDataLayer backend trce
    fetchLoop backend dataLayer FetchLoopForever trce queryPoolFetchRetryDefault

---------------------------------------------------------------------------------------------------

data FetchLoopType
    = FetchLoopForever
    | FetchLoopOnce
    deriving (Eq, Show)

isFetchLoopForever :: FetchLoopType -> Bool
isFetchLoopForever FetchLoopForever = True
isFetchLoopForever FetchLoopOnce    = False

fetchLoop
    :: forall m. MonadIO m
    => SqlBackend
    -> DataLayer
    -> FetchLoopType
    -> Trace IO Text
    -> ReaderT SqlBackend (LoggingT IO) [PoolFetchRetry] -- This should be in the @DataLayer@
    -> m ()
fetchLoop sqlBackend dataLayer fetchLoopType trce queryPoolFetchRetry =
    loop
  where
    loop :: m ()
    loop = do

      -- A interval pause so we don't do too much work on this thread.
      liftIO $ threadDelay 60_000_000 -- 60 seconds

      now <- liftIO Time.getPOSIXTime

      -- Fetch all pools that have not been run with success.
      -- This has to be stateful in order to count.
      pools <- liftIO $ runDbIohkLogging sqlBackend trce queryPoolFetchRetry

      -- Filter for figuring out if it's time for a retry.
      let isRunnable :: PoolFetchRetry -> Bool
          isRunnable pfr = retryTime (pfrRetry pfr) < now

      let runnablePools = filter isRunnable pools

      liftIO $ logInfo trce $
        mconcat
          [ " ***************************** "
          , "Pools with errors: total "
          , show (length pools)
          , ", runnable "
          , show (length runnablePools)
          , "."
          ]

      -- We actually run the fetch again.
      _ <- liftIO $ forM runnablePools $ \pool ->
          fetchInsertNewPoolMetadataOld dataLayer trce fetchInsertDefault pool

      -- If it loops forever then loop, else finish. For testing.
      if isFetchLoopForever fetchLoopType
        then loop
        else pure ()

httpGetMax512Bytes
    :: PoolIdentifier
    -> Text
    -> Http.Request
    -> Http.Manager
    -> ExceptT FetchError IO (ByteString, Http.Status)
httpGetMax512Bytes poolId poolMetadataURL request manager = do
    res <- handleExceptT (convertHttpException poolId poolMetadataURL) $
            Http.withResponse request manager $ \responseBR -> do
              -- We read the first chunk that should contain all the bytes from the reponse.
              responseBSFirstChunk <- Http.brReadSome (Http.responseBody responseBR) 512
              -- If there are more bytes in the second chunk, we don't go any further since that
              -- violates the size constraint.
              responseBSSecondChunk <- Http.brReadSome (Http.responseBody responseBR) 1
              if LBS.null responseBSSecondChunk
                then pure $ Right (LBS.toStrict responseBSFirstChunk, Http.responseStatus responseBR)
                else pure $ Left $ FEDataTooLong poolId poolMetadataURL

    hoistEither res

convertHttpException :: PoolIdentifier -> Text -> HttpException -> FetchError
convertHttpException poolId poolMetadataURL he =
  case he of
    HttpExceptionRequest _req hec ->
      case hec of
        Http.ResponseTimeout      -> FETimeout poolId poolMetadataURL "Response"
        Http.ConnectionTimeout    -> FETimeout poolId poolMetadataURL "Connection"
        Http.ConnectionFailure {} -> FEConnectionFailure poolId poolMetadataURL
        other                     -> FEHttpException poolId poolMetadataURL (show other)
    InvalidUrlException url _ -> FEUrlParseFail poolId poolMetadataURL (Text.pack url)

-- select * from pool_metadata_fetch_error pmfr
--   where pmfr.id in (select max(id) from pool_metadata_fetch_error group by pool_id, pool_hash)
--   and not exists (select * from pool_metadata where pmr_id = pmfr.pmr_id);

-- Get a list of the pools for which there is a PoolMetadataReference entry but there is
-- no PoolMetadata entry.
-- This is a bit questionable because it assumes that the autogenerated 'id' primary key
-- is a reliable proxy for time, ie higher 'id' was added later in time.
queryPoolFetchRetryDefault :: MonadIO m => ReaderT SqlBackend m [PoolFetchRetry]
queryPoolFetchRetryDefault = do

    pmfr <- select . from $ \((pmfr :: SqlExpr (Entity PoolMetadataFetchError)) `InnerJoin` (pmr :: SqlExpr (Entity PoolMetadataRef))) -> do
                on (pmfr ^. DB.PoolMetadataFetchErrorPmrId ==. pmr ^. DB.PoolMetadataRefId)
                where_ (just (pmfr ^. DB.PoolMetadataFetchErrorId) `in_` latestReferences)
                where_ (notExists . from $ \pod -> where_ (pod ^. DB.PoolMetadataPmrId ==. just (pmfr ^. DB.PoolMetadataFetchErrorPmrId)))
                orderBy [desc (pmfr ^. DB.PoolMetadataFetchErrorFetchTime)]
                pure
                    ( pmfr ^. DB.PoolMetadataFetchErrorFetchTime
                    , pmfr ^. DB.PoolMetadataFetchErrorPmrId
                    , pmfr ^. DB.PoolMetadataFetchErrorPoolId
                    , pmr ^. DB.PoolMetadataRefUrl
                    , pmfr ^. DB.PoolMetadataFetchErrorPoolHash
                    , pmfr ^. DB.PoolMetadataFetchErrorRetryCount
                    )

    pure $ map (convert . unValue6) pmfr
  where
    latestReferences :: SqlExpr (ValueList (Maybe DB.PoolMetadataFetchErrorId))
    latestReferences =
      subList_select . from $ \(pmfr :: SqlExpr (Entity PoolMetadataFetchError)) -> do
        groupBy (pmfr ^. DB.PoolMetadataFetchErrorPoolId, pmfr ^. DB.PoolMetadataFetchErrorPoolHash)
        pure $ max_ (pmfr ^. DB.PoolMetadataFetchErrorId)

    convert :: (Time.UTCTime, PoolMetadataRefId, PoolIdentifier, PoolUrl, PoolMetaHash, Word) -> PoolFetchRetry
    convert (fetchTime', poolMetadataReferenceId, poolId, poolUrl, poolMetadataHash', existingRetryCount) =
        let fetchTimePOSIX = Time.utcTimeToPOSIXSeconds fetchTime'
            retry = retryAgain fetchTimePOSIX existingRetryCount
        in
            PoolFetchRetry
              { pfrReferenceId = poolMetadataReferenceId
              , pfrPoolIdWtf = poolId
              , pfrPoolUrl = poolUrl
              , pfrPoolMDHash = poolMetadataHash'
              , pfrRetry = retry
              }

    -- Util
    unValue6 :: (Value a, Value b, Value c, Value d, Value e, Value f) -> (a, b, c, d, e, f)
    unValue6 (a, b, c, d, e, f) = (unValue a, unValue b, unValue c, unValue d, unValue e, unValue f)


renderByteStringHex :: ByteString -> Text
renderByteStringHex = Text.decodeUtf8 . B16.encode

renderFetchError :: FetchError -> Text
renderFetchError fe =
  case fe of
    FEHashMismatch poolId xpt act poolMetaUrl ->
        mconcat
            [ "Hash mismatch from poolId '"
            , getPoolIdentifier poolId
            , "' when fetching metadata from '"
            , poolMetaUrl
            , "'. Expected "
            , xpt
            , " but got "
            , act
            , "."
            ]
    FEDataTooLong poolId poolMetaUrl ->
        mconcat
            [ "Offline pool data from poolId '"
            , getPoolIdentifier poolId
            , "' when fetching metadata from '"
            , poolMetaUrl
            , "' exceeded 512 bytes."
            ]
    FEUrlParseFail poolId poolMetaUrl err ->
        mconcat
            [ "URL parse error from poolId '"
            , getPoolIdentifier poolId
            , "' when fetching metadata from '"
            , poolMetaUrl
            , "' resulted in : "
            , err
            ]
    FEJsonDecodeFail poolId poolMetaUrl err ->
        mconcat
            [ "JSON decode error from poolId '"
            , getPoolIdentifier poolId
            , "' when fetching metadata from '"
            , poolMetaUrl
            , "' resulted in : "
            , err
            ]
    FEHttpException poolId poolMetaUrl err ->
        mconcat
            [ "HTTP Exception from poolId '"
            , getPoolIdentifier poolId
            , "' when fetching metadata from '"
            , poolMetaUrl
            , "' resulted in : "
            , err
            ]
    FEHttpResponse poolId poolMetaUrl sc ->
        mconcat
            [ "HTTP Response from poolId '"
            , getPoolIdentifier poolId
            , "' when fetching metadata from '"
            , poolMetaUrl
            , "' resulted in : "
            , show sc
            ]
    FETimeout poolId poolMetaUrl ctx ->
        mconcat
            [ ctx
            , " timeout from poolId '"
            , getPoolIdentifier poolId
            , "' when fetching metadata from '"
            , poolMetaUrl
            , "'."
            ]
    FEConnectionFailure poolId poolMetaUrl ->
        mconcat
            [ "Connection failure from poolId '"
            , getPoolIdentifier poolId
            , "' when fetching metadata from '"
            , poolMetaUrl
            , "'."
            ]
    FEIOException err -> "IO Exception: " <> err

