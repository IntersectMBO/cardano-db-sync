{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.SMASH.Server.FetchPolicies
  ( httpClientFetchPolicies
  , httpApiCall
  , renderHttpClientError
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra

import           Cardano.SMASH.Server.Types       (HealthStatus,
                                                   PolicyResult (..),
                                                   PoolId,
                                                   SmashURL (..))
import           Data.Aeson                       (FromJSON, parseJSON)
import           Data.Aeson.Types                 (parseEither)

import           Network.HTTP.Simple              (Request, getResponseBody,
                                                   getResponseStatusCode,
                                                   httpJSONEither,
                                                   parseRequestThrow)

-- |The possible errors for the http client.
data HttpClientError
    = HttpClientCannotParseEndpoint !Text
    | HttpClientInvalidClientBody
    | HttpClientCannotParseJSON !Text
    | HttpClientStatusNotOk

-- |Render the http client error.
renderHttpClientError :: HttpClientError -> Text
renderHttpClientError = \case
    HttpClientCannotParseEndpoint endpoint ->
        mconcat
            [ "Http client cannot parse the '"
            , endpoint
            , "' endpoint"
            ]
    HttpClientInvalidClientBody ->
        "Http client invalid response body."
    HttpClientCannotParseJSON reason ->
        mconcat
            [ "Http client cannot parse the response JSON - '"
            , reason
            , "'."
            ]
    HttpClientStatusNotOk ->
        "Http client returned status not ok. Status should be 200."

-- |Fetch the remote SMASH server policies.
httpClientFetchPolicies :: SmashURL -> IO (Either HttpClientError PolicyResult)
httpClientFetchPolicies smashURL = runExceptT $ do

    -- https://smash.cardano-mainnet.iohk.io
    let baseSmashURL = show $ getSmashURL smashURL

    -- TODO(KS): This would be nice.
    --let delistedEndpoint = symbolVal (Proxy :: Proxy DelistedPoolsAPI)
    --let smashDelistedEndpoint = baseSmashURL <> delistedEndpoint

    let statusEndpoint = baseSmashURL <> "/api/v1/status"
    let delistedEndpoint = baseSmashURL <> "/api/v1/delisted"
    let reservedTickersEndpoint = baseSmashURL <> "/api/v1/tickers"

    statusRequest <- parseRequestEither statusEndpoint
    delistedRequest <- parseRequestEither delistedEndpoint
    _reservedTickersRequest <- parseRequestEither reservedTickersEndpoint

    healthStatus :: HealthStatus <- httpApiCall statusRequest
    delistedPools :: [PoolId] <- httpApiCall delistedRequest

    -- TODO(KS): Current version doesn't have exposed the tickers endpoint and would fail!
    -- uniqueTickers :: [UniqueTicker] <- httpApiCall reservedTickersRequest
    uniqueTickers <- pure []

    let policyResult =
            PolicyResult
                { prSmashURL = smashURL
                , prHealthStatus = healthStatus
                , prDelistedPools = delistedPools
                , prUniqueTickers = uniqueTickers
                }

    return policyResult

-- |A simple HTTP call for remote server.
httpApiCall :: forall a. (FromJSON a) => Request -> ExceptT HttpClientError IO a
httpApiCall request = do

    httpResult <- httpJSONEither request
    let httpResponseBody = getResponseBody httpResult

    httpResponse <- firstExceptT (\_ -> HttpClientInvalidClientBody) $ hoistEither httpResponseBody

    let httpStatusCode  = getResponseStatusCode httpResult

    when (httpStatusCode /= 200) $
        left HttpClientStatusNotOk

    case parseEither parseJSON httpResponse of
        Left reason -> left $ HttpClientCannotParseJSON (toS reason)
        Right value -> right value

-- |When any exception occurs, we simply map it to an http client error.
parseRequestEither :: Text -> ExceptT HttpClientError IO Request
parseRequestEither requestEndpoint = do
    let parsedRequest = newExceptT (try $ parseRequestThrow (toS requestEndpoint) :: IO (Either SomeException Request))
    firstExceptT (\_ -> HttpClientCannotParseEndpoint requestEndpoint) parsedRequest

