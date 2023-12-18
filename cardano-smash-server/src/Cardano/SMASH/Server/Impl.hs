{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}

module Cardano.SMASH.Server.Impl (
  ServerEnv (..),
  server,
) where

import Cardano.BM.Trace
import Cardano.Db (textShow)
import Cardano.Prelude hiding (Handler)
import Cardano.SMASH.Server.Api
import Cardano.SMASH.Server.FetchPolicies
import Cardano.SMASH.Server.PoolDataLayer
import Cardano.SMASH.Server.Types
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Swagger (Contact (..), Info (..), License (..), Swagger (..), URL (..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Version (showVersion)
import Paths_cardano_smash_server (version)
import Servant (Handler (..), Server, err400, err403, err404, errBody, (:<|>) (..))
import Servant.Swagger (toSwagger)

data ServerEnv = ServerEnv
  { seTrace :: Trace IO Text
  , seDataLayer :: PoolDataLayer
  }

-- | Combined server of a Smash service with Swagger documentation.
server :: ServerEnv -> Server API
server serverEnv =
  pure todoSwagger
    :<|> getOffChainPoolMetadata serverEnv
    :<|> getHealthStatus
    :<|> getReservedTickers serverEnv
    :<|> getDelistedPools serverEnv
    :<|> delistPool serverEnv
    :<|> enlistPool serverEnv
    :<|> getPoolErrorAPI serverEnv
    :<|> getRetiredPools serverEnv
    :<|> checkPool serverEnv
    :<|> addTicker serverEnv
    :<|> fetchPolicies serverEnv

-- | Swagger spec for Todo API.
todoSwagger :: Swagger
todoSwagger =
  let swaggerDefinition = toSwagger smashApi
   in swaggerDefinition {_swaggerInfo = swaggerInfo}
  where
    smashVersion :: Text
    smashVersion = toS $ showVersion version

    swaggerInfo :: Info
    swaggerInfo =
      Info
        { _infoTitle = "Smash"
        , _infoDescription = Just "Stakepool Metadata Aggregation Server"
        , _infoTermsOfService = Nothing
        , _infoContact =
            Just $
              Contact
                { _contactName = Just "IOHK"
                , _contactUrl = Just $ URL "https://iohk.io/"
                , _contactEmail = Just "operations@iohk.io"
                }
        , _infoLicense =
            Just $
              License
                { _licenseName = "APACHE2"
                , _licenseUrl = Just $ URL "https://github.com/IntersectMBO/cardano-db-sync/blob/master/LICENSE"
                }
        , _infoVersion = smashVersion
        }

-- 403 if it is delisted
-- 404 if it is not available (e.g. it could not be downloaded, or was invalid)
-- 200 with the JSON content. Note that this must be the original content with the expected hash, not a re-rendering of the original.
getOffChainPoolMetadata ::
  ServerEnv ->
  PoolId ->
  PoolMetadataHash ->
  Handler (ApiResult DBFail PoolMetadataRaw)
getOffChainPoolMetadata (ServerEnv trce dataLayer) poolId poolMetaHash =
  convertIOToHandler $ do
    isDelisted <- dlCheckDelistedPool dataLayer poolId

    -- When it is delisted, return 403. We don't need any more info.
    when isDelisted $ do
      let msg = Text.unwords ["Pool", getPoolId poolId, "is delisted"]
      logWarning trce msg
      throwIO $ err403 {errBody = LBS.fromStrict $ Text.encodeUtf8 msg}

    isRetired <- dlCheckRetiredPool dataLayer poolId
    case isRetired of
      Right True -> do
        let msg = Text.unwords ["Pool", getPoolId poolId, "is retired"]
        logWarning trce msg
        throwIO err404 {errBody = LBS.fromStrict $ Text.encodeUtf8 msg}
      Left err -> throwIO $ err404 {errBody = encode err}
      Right False -> pure ()

    mmetadata <- dlGetPoolMetadata dataLayer poolId poolMetaHash
    case mmetadata of
      Left err -> do
        logWarning trce $ textShow err
        throwIO err404 {errBody = encode err}
      Right (tickerName, meta) -> do
        mPoolHash <- dlCheckReservedTicker dataLayer tickerName
        case mPoolHash of
          Nothing -> pure $ ApiResult (Right meta)
          Just tickerPoolHash | tickerPoolHash == poolId -> pure $ ApiResult (Right meta)
          Just _poolHash -> do
            let msg = Text.unwords ["Ticker name", getTickerName tickerName, "is reserved by pool", getPoolId poolId]
            logWarning trce msg
            throwIO err404 -- ticker is reserved by another pool.

-- | Simple health status, there are ideas for improvement.
getHealthStatus :: Handler (ApiResult DBFail HealthStatus)
getHealthStatus =
  pure . ApiResult . Right $
    HealthStatus
      { hsStatus = "OK"
      , hsVersion = toS $ showVersion version
      }

-- | Get all reserved tickers.
getReservedTickers :: ServerEnv -> Handler (ApiResult DBFail [UniqueTicker])
getReservedTickers (ServerEnv _trce dataLayer) =
  convertIOToHandler $ do
    reservedTickers <- dlGetReservedTickers dataLayer
    pure . ApiResult . Right . map UniqueTicker $ reservedTickers

-- | Get all delisted pools
getDelistedPools :: ServerEnv -> Handler (ApiResult DBFail [PoolId])
getDelistedPools (ServerEnv _trce dataLayer) =
  convertIOToHandler $ do
    allDelistedPools <- dlGetDelistedPools dataLayer
    pure . ApiResult . Right $ allDelistedPools

#ifdef DISABLE_BASIC_AUTH
delistPool :: ServerEnv -> PoolId -> Handler (ApiResult DBFail PoolId)
delistPool env = delistPool' env
#else
delistPool :: ServerEnv -> User -> PoolId -> Handler (ApiResult DBFail PoolId)
delistPool env _user = delistPool' env
#endif

-- | General delist pool.
delistPool' :: ServerEnv -> PoolId -> Handler (ApiResult DBFail PoolId)
delistPool' (ServerEnv trce dataLayer) poolId =
  convertIOToHandler $ do
    delistedPoolE <- dlAddDelistedPool dataLayer poolId
    case delistedPoolE of
      Left dbFail -> throwDBFailException trce dbFail
      Right poolId' -> pure . ApiResult . Right $ poolId'

#ifdef DISABLE_BASIC_AUTH
enlistPool :: ServerEnv -> PoolId -> Handler (ApiResult DBFail PoolId)
enlistPool env poolId = enlistPool' env poolId
#else
enlistPool :: ServerEnv -> User -> PoolId -> Handler (ApiResult DBFail PoolId)
enlistPool env _user = enlistPool' env
#endif

-- | General enlist pool function.
enlistPool' :: ServerEnv -> PoolId -> Handler (ApiResult DBFail PoolId)
enlistPool' (ServerEnv trce dataLayer) poolId =
  convertIOToHandler $ do
    delistedPool' <- dlRemoveDelistedPool dataLayer poolId

    case delistedPool' of
      Left err -> do
        logWarning trce $ textShow err
        throwIO err404 {errBody = encode err}
      Right poolId' -> pure . ApiResult . Right $ poolId'

getPoolErrorAPI :: ServerEnv -> PoolId -> Maybe TimeStringFormat -> Handler (ApiResult DBFail [PoolFetchError])
getPoolErrorAPI (ServerEnv _trce dataLayer) poolId mTimeInt =
  convertIOToHandler $ do
    let getFetchErrors = dlGetFetchErrors dataLayer

    -- Unless the user defines the date from which he wants to display the errors,
    -- we show the latest 10 errors that occured. Those 10 errors can be from a single
    -- pool, or they can be from different pools, we order them chronologically.
    fetchErrors <- case mTimeInt of
      Nothing -> getFetchErrors poolId Nothing
      Just (TimeStringFormat time) -> getFetchErrors poolId (Just time)

    pure . ApiResult $ fetchErrors

getRetiredPools :: ServerEnv -> Handler (ApiResult DBFail [PoolId])
getRetiredPools (ServerEnv _trce dataLayer) =
  convertIOToHandler $ do
    retiredPools <- dlGetRetiredPools dataLayer

    pure . ApiResult $ retiredPools

checkPool :: ServerEnv -> PoolId -> Handler (ApiResult DBFail PoolId)
checkPool (ServerEnv _trce dataLayer) poolId =
  convertIOToHandler $ do
    existingPoolId <- dlGetPool dataLayer poolId
    pure $ ApiResult existingPoolId

#ifdef DISABLE_BASIC_AUTH
addTicker :: ServerEnv -> TickerName -> PoolId -> Handler (ApiResult DBFail TickerName)
addTicker = addTicker'
#else
addTicker :: ServerEnv -> User -> TickerName -> PoolId -> Handler (ApiResult DBFail TickerName)
addTicker dataLayer _user = addTicker' dataLayer
#endif

addTicker' :: ServerEnv -> TickerName -> PoolId -> Handler (ApiResult DBFail TickerName)
addTicker' (ServerEnv trce dataLayer) tickerName poolId =
  convertIOToHandler $ do
    reservedTickerE <- dlAddReservedTicker dataLayer tickerName poolId
    case reservedTickerE of
      Left dbFail -> throwDBFailException trce dbFail
      Right _reservedTicker -> pure . ApiResult . Right $ tickerName

#ifdef DISABLE_BASIC_AUTH
fetchPolicies :: ServerEnv -> SmashURL -> Handler (ApiResult DBFail PolicyResult)
fetchPolicies = fetchPolicies'
#else
fetchPolicies :: ServerEnv -> User -> SmashURL -> Handler (ApiResult DBFail PolicyResult)
fetchPolicies dataLayer _user = fetchPolicies' dataLayer
#endif

-- | General fetch policies function.
fetchPolicies' :: ServerEnv -> SmashURL -> Handler (ApiResult DBFail PolicyResult)
fetchPolicies' (ServerEnv trce dataLayer) smashURL =
  convertIOToHandler $ do
    logInfo trce $ "Fetch policies from " <> textShow smashURL
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

-- | Generic throwing of exception when something goes bad.
throwDBFailException :: Trace IO Text -> DBFail -> IO (ApiResult DBFail a)
throwDBFailException trce dbFail = do
  logWarning trce $ textShow dbFail
  throwIO $ err400 {errBody = encode dbFail}

-- | Natural transformation from @IO@ to @Handler@.
convertIOToHandler :: IO a -> Handler a
convertIOToHandler = Handler . ExceptT . try
