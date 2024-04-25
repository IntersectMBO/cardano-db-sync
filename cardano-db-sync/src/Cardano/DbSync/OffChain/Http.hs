{-# LANGUAGE OverloadedStrings #-}
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
import Cardano.Db (PoolMetaHash (..), PoolUrl (..), VoteMetaHash (..), VoteUrl (..), textShow)
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
import Network.HTTP.Client (HttpException (..))
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types as Http

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
          left $ OCFErrHashMismatch url (renderByteArray expectedMetaHashBs) (renderByteArray metadataHash)
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
  Http.Manager ->
  Http.Request ->
  VoteUrl ->
  Maybe VoteMetaHash ->
  Bool ->
  ExceptT OffChainFetchError IO SimplifiedOffChainVoteData
httpGetOffChainVoteData manager request vurl metaHash isGovAction = do
  httpRes <- handleExceptT (convertHttpException url) req
  (respBS, respLBS, mContentType) <- hoistEither httpRes
  (ocvd, decodedValue, metadataHash, mWarning) <- parseAndValidateVoteData respBS respLBS metaHash isGovAction (Just $ OffChainVoteUrl vurl)
  pure $
    SimplifiedOffChainVoteData
      { sovaHash = metadataHash
      , sovaBytes = respBS
      , sovaJson = Text.decodeUtf8 $ LBS.toStrict (Aeson.encode decodedValue)
      , sovaContentType = mContentType
      , sovaOffChainVoteData = ocvd
      , sovaWarning = mWarning
      }
  where
    req = httpGetBytes manager request 10000 30000 url
    url = OffChainVoteUrl vurl

parseAndValidateVoteData :: ByteString -> LBS.ByteString -> Maybe VoteMetaHash -> Bool -> Maybe OffChainUrlType -> ExceptT OffChainFetchError IO (Vote.OffChainVoteData, Aeson.Value, ByteString, Maybe Text)
parseAndValidateVoteData bs lbs metaHash isGa murl = do
  let metadataHash = Crypto.digest (Proxy :: Proxy Crypto.Blake2b_256) bs
  (hsh, mWarning) <- case unVoteMetaHash <$> metaHash of
    Nothing -> pure (metadataHash, Nothing)
    Just expectedMetaHash -> pure (expectedMetaHash, Just "Failed to validate metadata hash") -- TODO: Conway
  decodedValue <-
    case Aeson.eitherDecode' @Aeson.Value lbs of
      Left err -> left $ OCFErrJsonDecodeFail murl (Text.pack err)
      Right res -> pure res
  ocvd <-
    case Vote.eitherDecodeOffChainVoteData lbs isGa of
      Left err -> left $ OCFErrJsonDecodeFail murl (Text.pack err)
      Right res -> pure res
  pure (ocvd, decodedValue, hsh, mWarning)

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
-- Url
-------------------------------------------------------------------------------------
parseOffChainUrl :: OffChainUrlType -> ExceptT OffChainFetchError IO Http.Request
parseOffChainUrl url =
  handleExceptT wrapHttpException $ applyContentType <$> Http.parseRequest (showUrl url)
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
