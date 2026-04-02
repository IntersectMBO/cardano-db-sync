{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.OffChain.Http (
  httpGetOffChainPoolData,
  httpGetOffChainVoteData,
  parseAndValidateVoteData,
  parseOffChainUrl,
) where

import qualified Cardano.Crypto.Hash.Blake2b as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto
import Cardano.Db (PoolMetaHash (..), PoolUrl (..), VoteMetaHash (..), VoteUrl (..))
import qualified Cardano.Db as DB
import Cardano.DbSync.OffChain.Types (
  PoolOffChainMetadata (..),
  PoolTicker (..),
 )
import qualified Cardano.DbSync.OffChain.Vote.Types as Vote
import Cardano.DbSync.Types (
  OffChainFetchError (..),
  OffChainUrlType (..),
  SimplifiedOffChainPoolData (..),
  SimplifiedOffChainVoteData (..),
  showUrl,
 )
import Cardano.DbSync.Util (renderByteArray)
import Cardano.Prelude hiding (show)
import Control.Monad.Trans.Except.Extra (handleExceptT, hoistEither, left)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Show (show)
import qualified Net.IPv4 as IPv4
import qualified Net.IPv6 as IPv6
import Network.HTTP.Client (HttpException (..))
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Types as Http
import qualified Network.Socket as Socket

-------------------------------------------------------------------------------------
-- Get OffChain data
-------------------------------------------------------------------------------------
httpGetOffChainPoolData ::
  Http.Manager ->
  Http.Request ->
  PoolUrl ->
  Maybe PoolMetaHash ->
  ExceptT OffChainFetchError IO SimplifiedOffChainPoolData
httpGetOffChainPoolData manager request purl expectedMetaHash = do
  httpRes <- handleExceptT (convertHttpException url) req
  (respBS, respLBS, mContentType) <- hoistEither httpRes
  let metadataHash = Crypto.digest (Proxy :: Proxy Crypto.Blake2b_256) respBS
  case unPoolMetaHash <$> expectedMetaHash of
    Just expectedMetaHashBs
      | metadataHash /= expectedMetaHashBs ->
          left $ OCFErrHashMismatch (Just url) (renderByteArray expectedMetaHashBs) (renderByteArray metadataHash)
    _ -> pure ()
  decodedMetadata <-
    case Aeson.eitherDecode' respLBS of
      Left err -> left $ OCFErrJsonDecodeFail (Just url) (Text.pack err)
      Right res -> pure res
  pure $
    SimplifiedOffChainPoolData
      { spodTickerName = unPoolTicker $ pomTicker decodedMetadata
      , spodHash = metadataHash
      , spodBytes = respBS
      , -- Instead of inserting the `respBS` here, we encode the JSON and then store that.
        -- This is necessary because the PostgreSQL JSON parser can reject some ByteStrings
        -- that the Aeson parser accepts.
        spodJson = Text.decodeUtf8 $ LBS.toStrict (Aeson.encode decodedMetadata)
      , spodContentType = mContentType
      }
  where
    req = httpGetBytes manager request 600 512 url
    url = OffChainPoolUrl purl

httpGetOffChainVoteData ::
  [Text] ->
  VoteUrl ->
  Maybe VoteMetaHash ->
  DB.AnchorType ->
  ExceptT OffChainFetchError IO SimplifiedOffChainVoteData
httpGetOffChainVoteData gateways vurl metaHash anchorType = do
  case useIpfsGatewayMaybe vurl gateways of
    Nothing -> httpGetOffChainVoteDataSingle vurl metaHash anchorType
    Just [] -> left $ OCFErrNoIpfsGateway (OffChainVoteUrl vurl)
    Just urls -> tryAllGatewaysRec urls []
  where
    tryAllGatewaysRec [] acc = left $ OCFErrIpfsGatewayFailures (OffChainVoteUrl vurl) (reverse acc)
    tryAllGatewaysRec (url : rest) acc = do
      msocd <- liftIO $ runExceptT $ httpGetOffChainVoteDataSingle url metaHash anchorType
      case msocd of
        Right socd -> pure socd
        Left err -> tryAllGatewaysRec rest (err : acc)

httpGetOffChainVoteDataSingle ::
  VoteUrl ->
  Maybe VoteMetaHash ->
  DB.AnchorType ->
  ExceptT OffChainFetchError IO SimplifiedOffChainVoteData
httpGetOffChainVoteDataSingle vurl metaHash anchorType = do
  manager <- liftIO $ Http.newManager tlsManagerSettings
  request <- parseOffChainUrl url
  let req = httpGetBytes manager request 3000000 3000000 url
  httpRes <- handleExceptT (convertHttpException url) req
  (respBS, respLBS, mContentType) <- hoistEither httpRes
  (mocvd, decodedValue, metadataHash, mWarning, isValidJson) <- parseAndValidateVoteData respBS respLBS metaHash anchorType (Just $ OffChainVoteUrl vurl)
  pure $
    SimplifiedOffChainVoteData
      { sovaHash = metadataHash
      , sovaBytes = respBS
      , sovaJson = Text.decodeUtf8 $ LBS.toStrict (Aeson.encode decodedValue)
      , sovaContentType = mContentType
      , sovaOffChainVoteData = mocvd
      , sovaWarning = mWarning
      , sovaIsValidJson = isValidJson
      }
  where
    url = OffChainVoteUrl vurl

parseAndValidateVoteData :: ByteString -> LBS.ByteString -> Maybe VoteMetaHash -> DB.AnchorType -> Maybe OffChainUrlType -> ExceptT OffChainFetchError IO (Maybe Vote.OffChainVoteData, Aeson.Value, ByteString, Maybe Text, Bool)
parseAndValidateVoteData bs lbs metaHash anchorType murl = do
  let metadataHash = Crypto.digest (Proxy :: Proxy Crypto.Blake2b_256) bs
  -- First check if hash matches - this is critical and must fail if mismatch
  case unVoteMetaHash <$> metaHash of
    Just expectedMetaHashBs
      | metadataHash /= expectedMetaHashBs ->
          left $ OCFErrHashMismatch murl (renderByteArray expectedMetaHashBs) (renderByteArray metadataHash)
    _ -> pure ()
  -- Hash matches, now try to decode as generic JSON
  -- If this fails, we still want to store the data with is_valid = NULL and an error message
  (decodedValue, isValidJson) <-
    case Aeson.eitherDecode' @Aeson.Value lbs of
      Left err ->
        -- Not valid JSON - create an error message object
        pure (Aeson.object [("error", Aeson.String "Content is not valid JSON. See bytes column for raw data."), ("parse_error", Aeson.String $ Text.pack err)], False)
      Right res -> pure (res, True)
  -- Try to decode into strongly-typed vote data structure (only if JSON was valid)
  -- If this fails (e.g., doNotList is string instead of bool), we still store with is_valid = false
  let (ocvd, mWarning) =
        if isValidJson
          then case Vote.eitherDecodeOffChainVoteData lbs anchorType of
            Left err -> (Nothing, Just $ Text.pack err)
            Right res -> (Just res, Nothing)
          else (Nothing, Nothing) -- Not valid JSON, so can't parse as CIP
  pure (ocvd, decodedValue, metadataHash, mWarning, isValidJson)

httpGetBytes ::
  Http.Manager ->
  Http.Request ->
  Int ->
  Int ->
  OffChainUrlType ->
  IO (Either OffChainFetchError (ByteString, LBS.ByteString, Maybe ByteString))
httpGetBytes manager request bytesToRead maxBytes url =
  Http.withResponse request manager $ \responseBR -> do
    runExceptT $ do
      let status = Http.responseStatus responseBR
      unless (Http.statusCode status == 200)
        . left
        $ OCFErrHttpResponse url (Http.statusCode status) (Text.decodeLatin1 $ Http.statusMessage status)

      respLBS <- liftIO $ Http.brReadSome (Http.responseBody responseBR) bytesToRead
      let respBS = LBS.toStrict respLBS

      let mContentType = List.lookup Http.hContentType (Http.responseHeaders responseBR)
      case mContentType of
        Nothing -> pure () -- If there is no "content-type" header, assume its JSON.
        Just ct -> do
          -- There are existing pool metadata URLs in the database that specify a content type of
          -- "text/html" but provide pure valid JSON.
          -- Eventually this hack should be removed.
          if "text/html" `BS.isInfixOf` ct && isPossiblyJsonObject respBS
            then pure ()
            else do
              when ("text/html" `BS.isInfixOf` ct) $
                left $
                  OCFErrBadContentTypeHtml url (Text.decodeLatin1 ct)
              unless
                ( "application/json"
                    `BS.isInfixOf` ct
                    || "application/ld+json"
                      `BS.isInfixOf` ct
                    || "text/plain"
                      `BS.isInfixOf` ct
                    || "binary/octet-stream"
                      `BS.isInfixOf` ct
                    || "application/octet-stream"
                      `BS.isInfixOf` ct
                    || "application/binary"
                      `BS.isInfixOf` ct
                )
                . left
                $ OCFErrBadContentType url (Text.decodeLatin1 ct)

      unless (BS.length respBS <= maxBytes)
        . left
        $ OCFErrDataTooLong url
      pure (respBS, respLBS, mContentType)

-- | Is the provided ByteSring possibly JSON object?
-- Ignoring any leading whitespace, if the ByteString starts with a '{` character it might possibly
-- be a JSON object. We are are really only interested in differentiating between JSON and HTML
-- (where the first non-whitespace character will be '<'.
isPossiblyJsonObject :: ByteString -> Bool
isPossiblyJsonObject bs =
  case BS.uncons (BS.strip bs) of
    Just ('{', _) -> True
    _otherwise -> False

-------------------------------------------------------------------------------------
-- Url Validation
-------------------------------------------------------------------------------------
isLocalhostHost :: ByteString -> Bool
isLocalhostHost host =
  host == "localhost" || host == "127.0.0.1" || host == "::1"

validateUrlScheme :: OffChainUrlType -> ExceptT OffChainFetchError IO ()
validateUrlScheme url = do
  let urlText = Text.pack $ showUrl url
  unless (Text.isPrefixOf "https://" urlText || (Text.isPrefixOf "http://" urlText && isLocalhostUrl urlText)) $
    left $
      OCFErrUrlParseFail url "Only HTTPS URLs are allowed"
  where
    isLocalhostUrl :: Text -> Bool
    isLocalhostUrl u =
      "http://localhost" `Text.isPrefixOf` u
        || "http://127.0.0.1" `Text.isPrefixOf` u
        || "http://[::1]" `Text.isPrefixOf` u

isRestrictedIPv6 :: IPv6.IPv6 -> Bool
isRestrictedIPv6 ipv6 =
  let (h1, _, _, _, _, _, _, _) = IPv6.toWord16s ipv6
   in (ipv6 == IPv6.loopback)
        || ((h1 .&. 0xfe00) == 0xfc00)
        || (h1 == 0xfe80)

validateResolvedHost :: Http.Request -> ExceptT OffChainFetchError IO ()
validateResolvedHost request = do
  let hostBS = Http.host request
  eAddrInfos <- liftIO $ try $ Socket.getAddrInfo Nothing (Just $ BS.unpack hostBS) Nothing
  addrInfos <- case eAddrInfos of
    Left (_ :: SomeException) -> pure []
    Right addrs -> pure addrs
  when (null addrInfos) $
    left $
      OCFErrConnectionFailure (OffChainPoolUrl $ PoolUrl $ Text.decodeUtf8 hostBS)
  forM_ addrInfos $ \addrInfo -> do
    let sockAddr = Socket.addrAddress addrInfo
    case sockAddr of
      Socket.SockAddrInet _ hostAddr -> do
        let ipv4 = IPv4.fromTupleOctets (Socket.hostAddressToTuple hostAddr)
        when (IPv4.reserved ipv4) $
          left $
            OCFErrUrlParseFail (OffChainPoolUrl $ PoolUrl $ Text.decodeUtf8 hostBS) "Access to private, loopback, or link-local IP addresses is not allowed"
      Socket.SockAddrInet6 _ _ hostAddr6 _ -> do
        let ipv6 = IPv6.fromTupleWord32s hostAddr6
        when (isRestrictedIPv6 ipv6) $
          left $
            OCFErrUrlParseFail (OffChainPoolUrl $ PoolUrl $ Text.decodeUtf8 hostBS) "Access to private, loopback, or link-local IP addresses is not allowed"
      _ -> pure ()

-------------------------------------------------------------------------------------
-- Url
-------------------------------------------------------------------------------------
parseOffChainUrl :: OffChainUrlType -> ExceptT OffChainFetchError IO Http.Request
parseOffChainUrl url = do
  validateUrlScheme url
  request <- handleExceptT wrapHttpException $ Http.parseRequest (showUrl url)
  unless (isLocalhostHost $ Http.host request) $
    validateResolvedHost request
  pure $ applyContentType request
  where
    wrapHttpException :: HttpException -> OffChainFetchError
    wrapHttpException err = OCFErrHttpException url (textShow err)

applyContentType :: Http.Request -> Http.Request
applyContentType req =
  req
    { Http.requestHeaders =
        Http.requestHeaders req ++ [(CI.mk "content-type", "application/json")]
    }

-------------------------------------------------------------------------------------
-- Exceptions to Error
-------------------------------------------------------------------------------------
convertHttpException :: OffChainUrlType -> HttpException -> OffChainFetchError
convertHttpException url he =
  case he of
    HttpExceptionRequest _req hec ->
      case hec of
        Http.ResponseTimeout -> OCFErrTimeout url "Response"
        Http.ConnectionTimeout -> OCFErrTimeout url "Connection"
        Http.ConnectionFailure {} -> OCFErrConnectionFailure url
        Http.TooManyRedirects {} -> OCFErrHttpException url "Too many redirects"
        Http.OverlongHeaders -> OCFErrHttpException url "Overlong headers"
        Http.StatusCodeException resp _ -> OCFErrHttpException url ("Status code exception " <> Text.pack (show $ Http.responseStatus resp))
        Http.InvalidStatusLine {} -> OCFErrHttpException url "Invalid status line"
        other -> OCFErrHttpException url (Text.take 100 $ Text.pack $ show other)
    InvalidUrlException urlx err ->
      case url of
        OffChainPoolUrl _ -> OCFErrUrlParseFail (OffChainPoolUrl $ PoolUrl $ Text.pack urlx) (Text.pack err)
        OffChainVoteUrl _ -> OCFErrUrlParseFail (OffChainVoteUrl $ VoteUrl $ Text.pack urlx) (Text.pack err)

useIpfsGatewayMaybe :: VoteUrl -> [Text] -> Maybe [VoteUrl]
useIpfsGatewayMaybe vu gateways =
  case Text.stripPrefix "ipfs://" (unVoteUrl vu) of
    Just sf -> Just $ VoteUrl . (<> sf) <$> gateways
    Nothing -> Nothing
