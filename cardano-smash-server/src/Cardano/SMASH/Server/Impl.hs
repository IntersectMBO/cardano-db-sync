{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module           Cardano.SMASH.Server.Impl where

import           Cardano.Prelude hiding (Handler)

import           Data.Aeson (encode)

import           Data.Swagger (Contact (..), Info (..), License (..), Swagger (..), URL (..))
import           Data.Version (showVersion)

import           Servant (Handler (..), Header, Headers, Server, err400, err403, err404, errBody,
                   (:<|>) (..))
import           Servant.API.ResponseHeaders (addHeader)
import           Servant.Swagger (toSwagger)

import           Cardano.SMASH.Server.Api
import           Cardano.SMASH.Server.FetchPolicies
import           Cardano.SMASH.Server.PoolDataLayer
import           Cardano.SMASH.Server.Types

import           Paths_cardano_smash_server (version)

-- | Combined server of a Smash service with Swagger documentation.
server :: PoolDataLayer -> Server API
server poolApi
    =       pure todoSwagger
    :<|>    getPoolOfflineMetadata poolApi
    :<|>    getHealthStatus
    :<|>    getReservedTickers poolApi
    :<|>    getDelistedPools poolApi
    :<|>    delistPool poolApi
    :<|>    enlistPool poolApi
    :<|>    getPoolErrorAPI poolApi
    :<|>    getRetiredPools poolApi
    :<|>    checkPool poolApi
    :<|>    addTicker poolApi
    :<|>    fetchPolicies poolApi
#ifdef TESTING_MODE
    :<|>    retirePool poolApi
    :<|>    addPool poolApi
#endif

-- | Swagger spec for Todo API.
todoSwagger :: Swagger
todoSwagger =
    let swaggerDefinition = toSwagger smashApi

    in swaggerDefinition {_swaggerInfo = swaggerInfo}
  where
    smashVersion :: Text
    smashVersion = toS $ showVersion version

    swaggerInfo :: Info
    swaggerInfo = Info
        { _infoTitle = "Smash"
        , _infoDescription = Just "Stakepool Metadata Aggregation Server"
        , _infoTermsOfService = Nothing
        , _infoContact = Just $ Contact
            { _contactName = Just "IOHK"
            , _contactUrl = Just $ URL "https://iohk.io/"
            , _contactEmail = Just "operations@iohk.io"
            }

        , _infoLicense = Just $ License
            { _licenseName = "APACHE2"
            , _licenseUrl = Just $ URL "https://github.com/input-output-hk/cardano-db-sync/blob/master/LICENSE"
            }
        , _infoVersion = smashVersion
        }

-- 403 if it is delisted
-- 404 if it is not available (e.g. it could not be downloaded, or was invalid)
-- 200 with the JSON content. Note that this must be the original content with the expected hash, not a re-rendering of the original.
getPoolOfflineMetadata
    :: PoolDataLayer
    -> PoolId
    -> PoolMetadataHash
    -> Handler (Headers '[Header "Cache-Control" Text] (ApiResult DBFail PoolMetadataRaw))
getPoolOfflineMetadata dataLayer poolId poolHash = fmap (addHeader $ cacheControlHeader NoCache) . convertIOToHandler $ do

    isDelisted <- dlCheckDelistedPool dataLayer poolId

    -- When it is delisted, return 403. We don't need any more info.
    when isDelisted $
        throwIO err403

    isRetired <- dlCheckRetiredPool dataLayer poolId
    when isRetired $
        throwIO err404

    mmetadata <- dlGetPoolMetadata dataLayer poolId poolHash
    case mmetadata of
        Left _err -> throwIO err404
        Right meta -> pure $ ApiResult $ Right meta

-- |Simple health status, there are ideas for improvement.
getHealthStatus :: Handler (ApiResult DBFail HealthStatus)
getHealthStatus = pure . ApiResult . Right $
    HealthStatus
        { hsStatus = "OK"
        , hsVersion = toS $ showVersion version
        }

-- |Get all reserved tickers.
getReservedTickers :: PoolDataLayer -> Handler (ApiResult DBFail [UniqueTicker])
getReservedTickers dataLayer = convertIOToHandler $ do

    reservedTickers <- dlGetReservedTickers dataLayer

    pure . ApiResult . Right . map UniqueTicker $ reservedTickers

-- |Get all delisted pools
getDelistedPools :: PoolDataLayer -> Handler (ApiResult DBFail [PoolId])
getDelistedPools dataLayer = convertIOToHandler $ do

    allDelistedPools <- dlGetDelistedPools dataLayer

    pure . ApiResult . Right $ allDelistedPools

#ifdef DISABLE_BASIC_AUTH
delistPool :: PoolDataLayer -> PoolId -> Handler (ApiResult DBFail PoolId)
delistPool dataLayer = delistPool' dataLayer
#else
delistPool :: PoolDataLayer -> User -> PoolId -> Handler (ApiResult DBFail PoolId)
delistPool dataLayer _user = delistPool' dataLayer
#endif

-- |General delist pool.
delistPool' :: PoolDataLayer -> PoolId -> Handler (ApiResult DBFail PoolId)
delistPool' dataLayer poolId = convertIOToHandler $ do

    delistedPoolE <- dlAddDelistedPool dataLayer poolId

    case delistedPoolE of
        Left dbFail   -> throwDBFailException dbFail
        Right poolId' -> pure . ApiResult . Right $ poolId'

#ifdef DISABLE_BASIC_AUTH
enlistPool :: PoolDataLayer -> PoolId -> Handler (ApiResult DBFail PoolId)
enlistPool dataLayer poolId = enlistPool' dataLayer poolId
#else
enlistPool :: PoolDataLayer -> User -> PoolId -> Handler (ApiResult DBFail PoolId)
enlistPool dataLayer _user = enlistPool' dataLayer
#endif

-- |General enlist pool function.
enlistPool' :: PoolDataLayer -> PoolId -> Handler (ApiResult DBFail PoolId)
enlistPool' dataLayer poolId = convertIOToHandler $ do

    delistedPool' <- dlRemoveDelistedPool dataLayer poolId

    case delistedPool' of
        Left _err     -> throwIO err404
        Right poolId' -> pure . ApiResult . Right $ poolId'

getPoolErrorAPI :: PoolDataLayer -> PoolId -> Maybe TimeStringFormat -> Handler (ApiResult DBFail [PoolFetchError])
getPoolErrorAPI dataLayer poolId mTimeInt = convertIOToHandler $ do

    let getFetchErrors = dlGetFetchErrors dataLayer

    -- Unless the user defines the date from which he wants to display the errors,
    -- we show the latest 10 errors that occured. Those 10 errors can be from a single
    -- pool, or they can be from different pools, we order them chronologically.
    fetchErrors <- case mTimeInt of
        Nothing -> getFetchErrors poolId Nothing
        Just (TimeStringFormat time) -> getFetchErrors poolId (Just time)

    pure . ApiResult $ fetchErrors

getRetiredPools :: PoolDataLayer -> Handler (ApiResult DBFail [PoolId])
getRetiredPools dataLayer = convertIOToHandler $ do

    retiredPools <- dlGetRetiredPools dataLayer

    pure . ApiResult $ retiredPools

checkPool :: PoolDataLayer -> PoolId -> Handler (ApiResult DBFail PoolId)
checkPool dataLayer poolId = convertIOToHandler $ do

    existingPoolId <- dlGetPool dataLayer poolId

    pure . ApiResult $ existingPoolId

addTicker :: PoolDataLayer -> TickerName -> PoolMetadataHash -> Handler (ApiResult DBFail TickerName)
addTicker dataLayer tickerName poolMetadataHash = convertIOToHandler $ do

    reservedTickerE <- dlAddReservedTicker dataLayer tickerName poolMetadataHash

    case reservedTickerE of
        Left dbFail           -> throwDBFailException dbFail
        Right _reservedTicker -> pure . ApiResult . Right $ tickerName

#ifdef DISABLE_BASIC_AUTH
fetchPolicies :: PoolDataLayer -> SmashURL -> Handler (ApiResult DBFail PolicyResult)
fetchPolicies = fetchPolicies'
#else
fetchPolicies :: PoolDataLayer -> User -> SmashURL -> Handler (ApiResult DBFail PolicyResult)
fetchPolicies dataLayer _user = fetchPolicies' dataLayer
#endif

-- |General fetch policies function.
fetchPolicies' :: PoolDataLayer -> SmashURL -> Handler (ApiResult DBFail PolicyResult)
fetchPolicies' dataLayer smashURL = convertIOToHandler $ do

    -- Fetch from the remote SMASH server.
    policyResult <- httpClientFetchPolicies smashURL

    let delistedPools =
            case policyResult of
                Left httpClientErr -> panic $ renderHttpClientError httpClientErr
                Right policyResult' -> prDelistedPools policyResult'

    -- Clear the database
    let getDelistedPools' = dlGetDelistedPools dataLayer
    existingDelistedPools <- getDelistedPools'

    let removeDelistedPool = dlRemoveDelistedPool dataLayer
    mapM_ removeDelistedPool existingDelistedPools

    let addDelistedPool = dlAddDelistedPool dataLayer
    _newDelistedPools <- mapM addDelistedPool delistedPools

    -- Horrible.
    case policyResult of
        Left httpClientErr -> pure . ApiResult . Left . UnknownError $ renderHttpClientError httpClientErr
        Right policyResult' -> pure . ApiResult . Right $ policyResult'

#ifdef TESTING_MODE
retirePool :: PoolDataLayer -> PoolIdBlockNumber -> Handler (ApiResult DBFail PoolId)
retirePool dataLayer (PoolIdBlockNumber poolId blockNo) = convertIOToHandler $ do

    let addRetiredPool = dlAddRetiredPool dataLayer
    retiredPoolId <- addRetiredPool poolId blockNo

    pure . ApiResult $ retiredPoolId

addPool :: PoolDataLayer -> PoolId -> PoolMetadataHash -> PoolMetadataRaw -> Handler (ApiResult DBFail PoolId)
addPool dataLayer poolId poolHash poolMetadataRaw = convertIOToHandler $ do

    poolMetadataE <- runPoolInsertion dataLayer poolMetadataRaw poolId poolHash

    case poolMetadataE of
        Left dbFail         -> throwDBFailException dbFail
        Right _poolMetadata -> pure . ApiResult . Right $ poolId

runPoolInsertion :: PoolDataLayer -> PoolMetadataRaw -> PoolId -> PoolMetadataHash -> IO (Either DBFail PoolMetadataRaw)
runPoolInsertion dataLayer poolMetadataRaw poolId poolHash = do

    decodedMetadata <-  case (eitherDecode' . BL.fromStrict . encodeUtf8 . getPoolMetadata $ poolMetadataRaw) of
                            Left err     -> panic $ toS err
                            Right result -> pure result

    dlAddPoolMetadata dataLayer Nothing poolId poolHash poolMetadataRaw (pomTicker decodedMetadata)
#endif


-- Generic throwing of exception when something goes bad.
throwDBFailException :: DBFail -> IO (ApiResult DBFail a)
throwDBFailException dbFail = throwIO $ err400 { errBody = encode dbFail }

-- | Natural transformation from @IO@ to @Handler@.
convertIOToHandler :: IO a -> Handler a
convertIOToHandler = Handler . ExceptT . try


-- | Cache control header.
data CacheControl
    = NoCache
    | CacheSeconds Int
    | CacheOneHour
    | CacheOneDay

-- | Render the cache control header.
cacheControlHeader :: CacheControl -> Text
cacheControlHeader NoCache = "no-store"
cacheControlHeader (CacheSeconds sec) = "max-age=" <> show sec
cacheControlHeader CacheOneHour = cacheControlHeader $ CacheSeconds (60 * 60)
cacheControlHeader CacheOneDay = cacheControlHeader $ CacheSeconds (24 * 60 * 60)

