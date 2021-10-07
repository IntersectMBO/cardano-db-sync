{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module           Cardano.SMASH.Server.Impl where

import           Cardano.Prelude hiding (Handler)

import           Data.Aeson                  (encode)

import           Data.Swagger                (Contact (..), Info (..),
                                              License (..), Swagger (..),
                                              URL (..))
import           Data.Version                (showVersion)

import           Servant                     (Handler (..),
                                              Header, Headers, Server, err400,
                                              err403, err404, errBody,
                                              (:<|>) (..))
import           Servant.API.ResponseHeaders (addHeader)
import           Servant.Swagger             (toSwagger)

import           Cardano.SMASH.Server.Api
import           Cardano.SMASH.Server.FetchPolicies
import           Cardano.SMASH.Server.Orphans
import           Cardano.SMASH.Server.PoolApi
import           Cardano.SMASH.Server.Types

import           Paths_cardano_smash_server                 (version)

-- | Combined server of a Smash service with Swagger documentation.
server :: PoolApi -> Server API
server poolApi
    =       return todoSwagger
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
            , _licenseUrl = Just $ URL "https://github.com/input-output-hk/smash/blob/master/LICENSE"
            }
        , _infoVersion = smashVersion
        }

-- 403 if it is delisted
-- 404 if it is not available (e.g. it could not be downloaded, or was invalid)
-- 200 with the JSON content. Note that this must be the original content with the expected hash, not a re-rendering of the original.
getPoolOfflineMetadata
    :: PoolApi
    -> PoolId
    -> PoolMetadataHash
    -> Handler ((Headers '[Header "Cache-Control" Text] (ApiResult DBFail PoolMetadataRaw)))
getPoolOfflineMetadata dataLayer poolId poolHash = fmap (addHeader $ cacheControlHeader NoCache) . convertIOToHandler $ do

    let checkDelistedPool = dlCheckDelistedPool dataLayer
    isDelisted <- checkDelistedPool poolId

    -- When it is delisted, return 403. We don't need any more info.
    when (isDelisted) $
        throwIO err403

    let checkRetiredPool = dlCheckRetiredPool dataLayer
    retiredPoolId <- checkRetiredPool poolId

    -- When that pool id is retired, return 404.
    when (isRight retiredPoolId) $
        throwIO err404

    let dbGetPoolMetadata = dlGetPoolMetadata dataLayer
    poolRecord <- dbGetPoolMetadata poolId poolHash

    case poolRecord of
        -- We return 404 when the hash is not found.
        Left _err -> throwIO err404
        Right (tickerName, poolMetadata) -> do
            let checkReservedTicker = dlCheckReservedTicker dataLayer

            -- We now check whether the reserved ticker name has been reserved for the specific
            -- pool hash.
            reservedTicker <- checkReservedTicker tickerName poolHash
            case reservedTicker of
                Nothing -> return . ApiResult . Right $ poolMetadata
                Just _foundReservedTicker -> throwIO err404

-- |Simple health status, there are ideas for improvement.
getHealthStatus :: Handler (ApiResult DBFail HealthStatus)
getHealthStatus = return . ApiResult . Right $
    HealthStatus
        { hsStatus = "OK"
        , hsVersion = toS $ showVersion version
        }

-- |Get all reserved tickers.
getReservedTickers :: PoolApi -> Handler (ApiResult DBFail [UniqueTicker])
getReservedTickers dataLayer = convertIOToHandler $ do

    let getReservedTickers' = dlGetReservedTickers dataLayer
    reservedTickers <- getReservedTickers'

    let uniqueTickers = map UniqueTicker reservedTickers

    return . ApiResult . Right $ uniqueTickers

-- |Get all delisted pools
getDelistedPools :: PoolApi -> Handler (ApiResult DBFail [PoolId])
getDelistedPools dataLayer = convertIOToHandler $ do

    let getAllDelisted = dlGetDelistedPools dataLayer
    allDelistedPools <- getAllDelisted

    return . ApiResult . Right $ allDelistedPools

#ifdef DISABLE_BASIC_AUTH
delistPool :: PoolApi -> PoolId -> Handler (ApiResult DBFail PoolId)
delistPool dataLayer poolId = delistPool' dataLayer poolId
#else
delistPool :: PoolApi -> User -> PoolId -> Handler (ApiResult DBFail PoolId)
delistPool dataLayer _user poolId = delistPool' dataLayer poolId
#endif

-- |General delist pool.
delistPool' :: PoolApi -> PoolId -> Handler (ApiResult DBFail PoolId)
delistPool' dataLayer poolId = convertIOToHandler $ do

    let addDelistedPool = dlAddDelistedPool dataLayer
    delistedPoolE <- addDelistedPool poolId

    case delistedPoolE of
        Left dbFail   -> throwDBFailException dbFail
        Right poolId' -> return . ApiResult . Right $ poolId'

#ifdef DISABLE_BASIC_AUTH
enlistPool :: PoolApi -> PoolId -> Handler (ApiResult DBFail PoolId)
enlistPool dataLayer poolId = enlistPool' dataLayer poolId
#else
enlistPool :: PoolApi -> User -> PoolId -> Handler (ApiResult DBFail PoolId)
enlistPool dataLayer _user poolId = enlistPool' dataLayer poolId
#endif

-- |General enlist pool function.
enlistPool' :: PoolApi -> PoolId -> Handler (ApiResult DBFail PoolId)
enlistPool' dataLayer poolId = convertIOToHandler $ do

    let removeDelistedPool = dlRemoveDelistedPool dataLayer
    delistedPool' <- removeDelistedPool poolId

    case delistedPool' of
        Left _err     -> throwIO err404
        Right poolId' -> return . ApiResult . Right $ poolId'

getPoolErrorAPI :: PoolApi -> PoolId -> Maybe TimeStringFormat -> Handler (ApiResult DBFail [PoolFetchError])
getPoolErrorAPI dataLayer poolId mTimeInt = convertIOToHandler $ do

    let getFetchErrors = dlGetFetchErrors dataLayer

    -- Unless the user defines the date from which he wants to display the errors,
    -- we show the latest 10 errors that occured. Those 10 errors can be from a single
    -- pool, or they can be from different pools, we order them chronologically.
    fetchErrors <- case mTimeInt of
        Nothing -> getFetchErrors poolId Nothing
        Just (TimeStringFormat time) -> getFetchErrors poolId (Just time)

    return . ApiResult $ fetchErrors

getRetiredPools :: PoolApi -> Handler (ApiResult DBFail [PoolId])
getRetiredPools dataLayer = convertIOToHandler $ do

    let getRetiredPools' = dlGetRetiredPools dataLayer
    retiredPools <- getRetiredPools'

    return . ApiResult $ retiredPools

checkPool :: PoolApi -> PoolId -> Handler (ApiResult DBFail PoolId)
checkPool dataLayer poolId = convertIOToHandler $ do

    let getPool = dlGetPool dataLayer
    existingPoolId <- getPool poolId

    return . ApiResult $ existingPoolId

addTicker :: PoolApi -> TickerName -> PoolMetadataHash -> Handler (ApiResult DBFail TickerName)
addTicker dataLayer tickerName poolMetadataHash = convertIOToHandler $ do

    let addReservedTicker = dlAddReservedTicker dataLayer
    reservedTickerE <- addReservedTicker tickerName poolMetadataHash

    case reservedTickerE of
        Left dbFail           -> throwDBFailException dbFail
        Right _reservedTicker -> return . ApiResult . Right $ tickerName

#ifdef DISABLE_BASIC_AUTH
fetchPolicies :: PoolApi -> SmashURL -> Handler (ApiResult DBFail PolicyResult)
fetchPolicies dataLayer smashURL = fetchPolicies' dataLayer smashURL
#else
fetchPolicies :: PoolApi -> User -> SmashURL -> Handler (ApiResult DBFail PolicyResult)
fetchPolicies dataLayer _user smashURL = fetchPolicies' dataLayer smashURL
#endif

-- |General fetch policies function.
fetchPolicies' :: PoolApi -> SmashURL -> Handler (ApiResult DBFail PolicyResult)
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
    _ <- mapM removeDelistedPool existingDelistedPools

    let addDelistedPool = dlAddDelistedPool dataLayer
    _newDelistedPools <- mapM addDelistedPool delistedPools

    -- Horrible.
    case policyResult of
        Left httpClientErr -> return . ApiResult . Left . UnknownError $ renderHttpClientError httpClientErr
        Right policyResult' -> return . ApiResult . Right $ policyResult'

#ifdef TESTING_MODE
retirePool :: PoolApi -> PoolIdBlockNumber -> Handler (ApiResult DBFail PoolId)
retirePool dataLayer (PoolIdBlockNumber poolId blockNo) = convertIOToHandler $ do

    let addRetiredPool = dlAddRetiredPool dataLayer
    retiredPoolId <- addRetiredPool poolId blockNo

    return . ApiResult $ retiredPoolId

addPool :: PoolApi -> PoolId -> PoolMetadataHash -> PoolMetadataRaw -> Handler (ApiResult DBFail PoolId)
addPool dataLayer poolId poolHash poolMetadataRaw = convertIOToHandler $ do

    poolMetadataE <- runPoolInsertion dataLayer poolMetadataRaw poolId poolHash

    case poolMetadataE of
        Left dbFail         -> throwDBFailException dbFail
        Right _poolMetadata -> return . ApiResult . Right $ poolId

runPoolInsertion :: PoolApi -> PoolMetadataRaw -> PoolId -> PoolMetadataHash -> IO (Either DBFail PoolMetadataRaw)
runPoolInsertion dataLayer poolMetadataRaw poolId poolHash = do

    decodedMetadata <-  case (eitherDecode' . BL.fromStrict . encodeUtf8 . getPoolMetadata $ poolMetadataRaw) of
                            Left err     -> panic $ toS err
                            Right result -> return result

    let addPoolMetadata = dlAddPoolMetadata dataLayer

    addPoolMetadata Nothing poolId poolHash poolMetadataRaw (pomTicker decodedMetadata)
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

