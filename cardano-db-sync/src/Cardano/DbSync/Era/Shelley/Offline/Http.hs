{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Offline.Http (
  FetchError (..),
  SimplifiedPoolOfflineData (..),
  httpGetPoolOfflineData,
  parsePoolUrl,
) where

import qualified Cardano.Crypto.Hash.Blake2b as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto
import Cardano.Db (PoolMetaHash (..), PoolUrl (..), textShow)
import Cardano.DbSync.Era.Shelley.Offline.Types (
  PoolOfflineMetadata (..),
  PoolTicker (..),
 )
import Cardano.DbSync.Util (renderByteArray)
import Cardano.Prelude hiding (show)
import Control.Monad.Extra (whenJust)
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

-- | Fetch error for the HTTP client fetching the pool offline metadata.
data FetchError
  = FEHashMismatch !PoolUrl !Text !Text
  | FEDataTooLong !PoolUrl
  | FEUrlParseFail !PoolUrl !Text
  | FEJsonDecodeFail !PoolUrl !Text
  | FEHttpException !PoolUrl !Text
  | FEHttpResponse !PoolUrl !Int !Text
  | FEBadContentType !PoolUrl !Text
  | FEBadContentTypeHtml !PoolUrl !Text
  | FEIOException !Text
  | FETimeout !PoolUrl !Text
  | FEConnectionFailure !PoolUrl
  deriving (Eq, Generic)

instance Exception FetchError

instance Show FetchError where
  show =
    \case
      FEHashMismatch (PoolUrl url) xpt act ->
        mconcat
          [ "Hash mismatch from when fetching metadata from "
          , show url
          , ". Expected "
          , show xpt
          , " but got "
          , show act
          , "."
          ]
      FEDataTooLong (PoolUrl url) ->
        mconcat
          ["Offline pool data when fetching metadata from ", show url, " exceeded 512 bytes."]
      FEUrlParseFail (PoolUrl url) err ->
        mconcat
          ["URL parse error for ", show url, " resulted in : ", show err]
      FEJsonDecodeFail (PoolUrl url) err ->
        mconcat
          ["JSON decode error from when fetching metadata from ", show url, " resulted in : ", show err]
      FEHttpException (PoolUrl url) err ->
        mconcat ["HTTP Exception for ", show url, " resulted in : ", show err]
      FEHttpResponse (PoolUrl url) sc msg ->
        mconcat ["HTTP Response from ", show url, " resulted in HTTP status code : ", show sc, " ", show msg]
      FEBadContentType (PoolUrl url) ct ->
        mconcat ["HTTP Response from ", show url, ": expected JSON, but got : ", show ct]
      FEBadContentTypeHtml (PoolUrl url) ct ->
        mconcat ["HTTP Response from ", show url, ": expected JSON, but got : ", show ct]
      FETimeout (PoolUrl url) ctx ->
        mconcat ["Timeout when fetching metadata from ", show url, ": ", show ctx]
      FEConnectionFailure (PoolUrl url) ->
        mconcat
          ["Connection failure when fetching metadata from ", show url, "'."]
      FEIOException err -> "IO Exception: " <> show err

data SimplifiedPoolOfflineData = SimplifiedPoolOfflineData
  { spodTickerName :: !Text
  , spodHash :: !ByteString
  , spodBytes :: !ByteString
  , spodJson :: !Text
  , spodContentType :: !(Maybe ByteString)
  }

httpGetPoolOfflineData ::
  Http.Manager ->
  Http.Request ->
  PoolUrl ->
  Maybe PoolMetaHash ->
  ExceptT FetchError IO SimplifiedPoolOfflineData
httpGetPoolOfflineData manager request poolUrl mHash = do
  res <- handleExceptT (convertHttpException poolUrl) httpGet
  hoistEither res
  where
    httpGet :: IO (Either FetchError SimplifiedPoolOfflineData)
    httpGet =
      Http.withResponse request manager $ \responseBR -> do
        runExceptT $ do
          let status = Http.responseStatus responseBR
          unless (Http.statusCode status == 200)
            . left
            $ FEHttpResponse poolUrl (Http.statusCode status) (Text.decodeLatin1 $ Http.statusMessage status)

          -- Read a maxiumm of 600 bytes and then later check if the length exceeds 512 bytes.
          respLBS <- liftIO $ Http.brReadSome (Http.responseBody responseBR) 600
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
                      FEBadContentTypeHtml poolUrl (Text.decodeLatin1 ct)
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
                    $ FEBadContentType poolUrl (Text.decodeLatin1 ct)

          unless (BS.length respBS <= 512)
            . left
            $ FEDataTooLong poolUrl

          let metadataHash = Crypto.digest (Proxy :: Proxy Crypto.Blake2b_256) respBS

          whenJust mHash $ \(PoolMetaHash expectedHash) ->
            when (metadataHash /= expectedHash)
              . left
              $ FEHashMismatch poolUrl (renderByteArray expectedHash) (renderByteArray metadataHash)

          decodedMetadata <-
            case Aeson.eitherDecode' respLBS of
              Left err -> left $ FEJsonDecodeFail poolUrl (Text.pack err)
              Right res -> pure res

          pure $
            SimplifiedPoolOfflineData
              { spodTickerName = unPoolTicker $ pomTicker decodedMetadata
              , spodHash = metadataHash
              , spodBytes = respBS
              , -- Instead of inserting the `respBS` here, we encode the JSON and then store that.
                -- This is necessary because the PostgreSQL JSON parser can reject some ByteStrings
                -- that the Aeson parser accepts.
                spodJson = Text.decodeUtf8 $ LBS.toStrict (Aeson.encode decodedMetadata)
              , spodContentType = mContentType
              }

-- | Is the provided ByteSring possibly JSON object?
-- Ignoring any leading whitespace, if the ByteString starts with a '{` character it might possibly
-- be a JSON object. We are are really only interested in differentiating between JSON and HTML
-- (where the first non-whitespace character will be '<'.
isPossiblyJsonObject :: ByteString -> Bool
isPossiblyJsonObject bs =
  case BS.uncons (BS.strip bs) of
    Just ('{', _) -> True
    _otherwise -> False

parsePoolUrl :: PoolUrl -> ExceptT FetchError IO Http.Request
parsePoolUrl poolUrl =
  handleExceptT wrapHttpException $ applyContentType <$> Http.parseRequest (Text.unpack $ unPoolUrl poolUrl)
  where
    applyContentType :: Http.Request -> Http.Request
    applyContentType req =
      req
        { Http.requestHeaders =
            Http.requestHeaders req ++ [(CI.mk "content-type", "application/json")]
        }

    wrapHttpException :: HttpException -> FetchError
    wrapHttpException err = FEHttpException poolUrl (textShow err)

-- -------------------------------------------------------------------------------------------------

convertHttpException :: PoolUrl -> HttpException -> FetchError
convertHttpException url he =
  case he of
    HttpExceptionRequest _req hec ->
      case hec of
        Http.ResponseTimeout -> FETimeout url "Response"
        Http.ConnectionTimeout -> FETimeout url "Connection"
        Http.ConnectionFailure {} -> FEConnectionFailure url
        Http.TooManyRedirects {} -> FEHttpException url "Too many redirects"
        Http.OverlongHeaders -> FEHttpException url "Overlong headers"
        Http.StatusCodeException resp _ -> FEHttpException url ("Status code exception " <> Text.pack (show $ Http.responseStatus resp))
        Http.InvalidStatusLine {} -> FEHttpException url "Invalid status line"
        other -> FEHttpException url (Text.take 100 $ Text.pack $ show other)
    InvalidUrlException urlx err -> FEUrlParseFail (PoolUrl $ Text.pack urlx) (Text.pack err)
