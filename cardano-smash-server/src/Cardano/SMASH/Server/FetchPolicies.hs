{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.SMASH.Server.FetchPolicies (
  httpClientFetchPolicies,
  httpApiCall,
  renderHttpClientError,
) where

import Cardano.Prelude
import Cardano.SMASH.Server.Types (HealthStatus, PolicyResult (..), PoolId, SmashURL (..), isLocalhostHost)
import Control.Monad.Trans.Except.Extra
import Data.Aeson (FromJSON, parseJSON)
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as Text
import qualified Net.IPv4 as IPv4
import qualified Net.IPv6 as IPv6
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Simple as HttpSimple
import qualified Network.Socket as Socket

-- | The possible errors for the http client.
data HttpClientError
  = HttpClientCannotParseEndpoint !Text
  | HttpClientInvalidClientBody
  | HttpClientCannotParseJSON !Text
  | HttpClientStatusNotOk
  | HttpClientBlockedHost !Text

-- | Render the http client error.
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
  HttpClientBlockedHost host ->
    mconcat
      [ "Access to host '"
      , host
      , "' is not allowed. Only public IP addresses are permitted."
      ]

isRestrictedIPv6 :: IPv6.IPv6 -> Bool
isRestrictedIPv6 ipv6 =
  let (h1, _, _, _, _, _, _, _) = IPv6.toWord16s ipv6
   in (ipv6 == IPv6.loopback)
        || ((h1 .&. 0xfe00) == 0xfc00)
        || (h1 == 0xfe80)

validateResolvedHost :: Http.Request -> ExceptT HttpClientError IO ()
validateResolvedHost request = do
  let hostBS = Http.host request
  eAddrInfos <- liftIO $ try $ Socket.getAddrInfo Nothing (Just $ BS.unpack hostBS) Nothing
  addrInfos <- case eAddrInfos of
    Left (_ :: SomeException) -> pure []
    Right addrs -> pure addrs
  when (null addrInfos) $
    left $
      HttpClientBlockedHost (Text.decodeUtf8 hostBS)
  forM_ addrInfos $ \addrInfo -> do
    let sockAddr = Socket.addrAddress addrInfo
    case sockAddr of
      Socket.SockAddrInet _ hostAddr -> do
        let ipv4 = IPv4.fromTupleOctets (Socket.hostAddressToTuple hostAddr)
        when (IPv4.reserved ipv4) $
          left $
            HttpClientBlockedHost (Text.decodeUtf8 hostBS)
      Socket.SockAddrInet6 _ _ hostAddr6 _ -> do
        let ipv6 = IPv6.fromTupleWord32s hostAddr6
        when (isRestrictedIPv6 ipv6) $
          left $
            HttpClientBlockedHost (Text.decodeUtf8 hostBS)
      _ -> pure ()

-- | Fetch the remote SMASH server policies.
httpClientFetchPolicies :: SmashURL -> IO (Either HttpClientError PolicyResult)
httpClientFetchPolicies smashURL = runExceptT $ do
  -- https://smash.cardano-mainnet.iohk.io
  let baseSmashURL = show $ getSmashURL smashURL

  -- TODO(KS): This would be nice.
  -- let delistedEndpoint = symbolVal (Proxy :: Proxy DelistedPoolsAPI)
  -- let smashDelistedEndpoint = baseSmashURL <> delistedEndpoint

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
  let uniqueTickers = []

  let policyResult =
        PolicyResult
          { prSmashURL = smashURL
          , prHealthStatus = healthStatus
          , prDelistedPools = delistedPools
          , prUniqueTickers = uniqueTickers
          }

  pure policyResult

-- | A simple HTTP call for remote server.
httpApiCall :: forall a. FromJSON a => Http.Request -> ExceptT HttpClientError IO a
httpApiCall request = do
  httpResult <- HttpSimple.httpJSONEither request
  let httpResponseBody = HttpSimple.getResponseBody httpResult

  httpResponse <- firstExceptT (const HttpClientInvalidClientBody) $ hoistEither httpResponseBody

  let httpStatusCode = HttpSimple.getResponseStatusCode httpResult

  when (httpStatusCode /= 200) $
    left HttpClientStatusNotOk

  case parseEither parseJSON httpResponse of
    Left reason -> left $ HttpClientCannotParseJSON (toS reason)
    Right value -> right value

-- | When any exception occurs, we simply map it to an http client error.
parseRequestEither :: Text -> ExceptT HttpClientError IO Http.Request
parseRequestEither requestEndpoint = do
  let parsedRequest = newExceptT (try $ HttpSimple.parseRequestThrow (toS requestEndpoint) :: IO (Either SomeException Http.Request))
  request <- firstExceptT (\_ -> HttpClientCannotParseEndpoint requestEndpoint) parsedRequest
  unless (isLocalhostHost $ Text.decodeUtf8 $ Http.host request) $
    validateResolvedHost request
  pure request
