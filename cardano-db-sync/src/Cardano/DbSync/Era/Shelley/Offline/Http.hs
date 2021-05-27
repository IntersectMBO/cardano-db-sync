{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Era.Shelley.Offline.Http
  ( FetchError (..)
  , httpGet512BytesMax
  , renderFetchError
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (handleExceptT, hoistEither)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text

import           Cardano.Db

import           Cardano.Sync.Util

import           Network.HTTP.Client (HttpException (..))
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Status as Http


-- |Fetch error for the HTTP client fetching the pool.
data FetchError
  = FEHashMismatch !PoolUrl !Text !Text
  | FEDataTooLong !PoolUrl
  | FEUrlParseFail !PoolUrl !Text
  | FEJsonDecodeFail !PoolUrl !Text
  | FEHttpException !PoolUrl !Text
  | FEHttpResponse !PoolUrl !Int
  | FEIOException !Text
  | FETimeout !PoolUrl !Text
  | FEConnectionFailure !PoolUrl
  deriving (Eq, Generic)

renderFetchError :: FetchError -> Text
renderFetchError fe =
  case fe of
    FEHashMismatch (PoolUrl url) xpt act ->
      mconcat
        [ "Hash mismatch from when fetching metadata from ", url, ". Expected ", xpt
        , " but got ", act, "."
        ]
    FEDataTooLong (PoolUrl url) ->
      mconcat
        [ "Offline pool data when fetching metadata from ", url, " exceeded 512 bytes." ]
    FEUrlParseFail (PoolUrl url) err ->
      mconcat
        [ "URL parse error from for ", url, " resulted in : ", err ]
    FEJsonDecodeFail (PoolUrl url) err ->
      mconcat
        [ "JSON decode error from when fetching metadata from ", url, " resulted in : ", err ]
    FEHttpException (PoolUrl url) err ->
      mconcat [ "HTTP Exception for ", url, " resulted in : ", err ]
    FEHttpResponse (PoolUrl url) sc ->
      mconcat [ "HTTP Response from for ", url, " resulted in : ", textShow sc ]
    FETimeout (PoolUrl url) ctx ->
      mconcat [ "Timeout when fetching metadata from ", url, ": ", ctx ]
    FEConnectionFailure (PoolUrl url) ->
      mconcat
        [ "Connection failure when fetching metadata from ", url, "'." ]
    FEIOException err -> "IO Exception: " <> err

httpGet512BytesMax
    :: PoolUrl -> Http.Request -> Http.Manager
    -> ExceptT FetchError IO (ByteString, Http.Status)
httpGet512BytesMax url request manager = do
    res <- handleExceptT (convertHttpException url) httpGet
    hoistEither res
  where
    httpGet :: IO (Either FetchError (ByteString, Http.Status))
    httpGet =
      Http.withResponse request manager $ \responseBR -> do
        -- We read the first chunk that should contain all the bytes from the reponse.
        responseBSFirstChunk <- Http.brReadSome (Http.responseBody responseBR) 512
        -- If there are more bytes in the second chunk, we don't go any further since that
        -- violates the size constraint.
        responseBSSecondChunk <- Http.brReadSome (Http.responseBody responseBR) 1
        if LBS.null responseBSSecondChunk
          then pure $ Right (LBS.toStrict responseBSFirstChunk, Http.responseStatus responseBR)
          else pure $ Left (FEDataTooLong url)

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
        Http.StatusCodeException resp _ -> FEHttpException url ("Status code exception " <> show (Http.responseStatus resp))
        Http.InvalidStatusLine {} -> FEHttpException url "Invalid status line"
        other -> FEHttpException url (Text.take 100 $ show other)
    InvalidUrlException urlx err -> FEUrlParseFail (PoolUrl $ Text.pack urlx) (Text.pack err)
