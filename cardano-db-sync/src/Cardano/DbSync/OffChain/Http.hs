{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.OffChain.Http (
  httpGetOffChainPoolData,
  httpGetOffChainVoteData,
  parseOffChainPoolUrl,
  parseOffChainVoteUrl,
) where

import qualified Cardano.Crypto.Hash.Blake2b as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto
import Cardano.Db (PoolMetaHash (..), PoolUrl (..), VoteMetaHash (..), VoteUrl (..), textShow)
import Cardano.DbSync.OffChain.Types (
  PoolOffChainMetadata (..),
  PoolTicker (..),
 )
import Cardano.DbSync.Types (
  OffChainFetchError (..),
  OffChainHashType (..),
  OffChainUrlType (..),
  SimplifiedOffChainDataType (..),
  SimplifiedOffChainPoolData (..),
  SimplifiedOffChainVoteData (..),
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
  OffChainUrlType ->
  Maybe OffChainHashType ->
  ExceptT OffChainFetchError IO SimplifiedOffChainPoolData
httpGetOffChainPoolData = do
  httpGetOffChainData
    ( \case
        SimplifiedOffChainPoolDataType r -> Right r
        _ -> Left $ OCFErrIOException "Incorrect SimplifiedOffChainDataType when calling httpGetOffChainPoolData"
    )

httpGetOffChainVoteData ::
  Http.Manager ->
  Http.Request ->
  OffChainUrlType ->
  Maybe OffChainHashType ->
  ExceptT OffChainFetchError IO SimplifiedOffChainVoteData
httpGetOffChainVoteData = do
  httpGetOffChainData
    ( \case
        SimplifiedOffChainVoteDataType r -> Right r
        _ -> Left $ OCFErrIOException "Incorrect SimplifiedOffChainDataType when calling httpGetOffChainVoteData"
    )

httpGetOffChainData ::
  (SimplifiedOffChainDataType -> Either OffChainFetchError a) -> -- conversion function
  Http.Manager ->
  Http.Request ->
  OffChainUrlType ->
  Maybe OffChainHashType -> -- metahash
  ExceptT OffChainFetchError IO a
httpGetOffChainData convert manager request urlType metaHash = do
  let httpGet = httpGetOffChain manager request metaHash urlType
  httpRes <- handleExceptT (convertHttpException urlType) httpGet
  case httpRes of
    Left err -> hoistEither $ Left err
    Right res ->
      case convert res of
        Left err -> left err
        Right r -> hoistEither $ Right r

httpGetOffChain ::
  Http.Manager ->
  Http.Request ->
  Maybe OffChainHashType ->
  OffChainUrlType ->
  IO (Either OffChainFetchError SimplifiedOffChainDataType)
httpGetOffChain manager request mHash url =
  Http.withResponse request manager $ \responseBR -> do
    runExceptT $ do
      let status = Http.responseStatus responseBR
      unless (Http.statusCode status == 200)
        . left
        $ OCFErrHttpResponse url (Http.statusCode status) (Text.decodeLatin1 $ Http.statusMessage status)

      let (bytesToRead, maxBytes) =
            case url of
              OffChainPoolUrl _ -> (600, 512)
              OffChainVoteUrl _ -> (3000, 3000)
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

      let metadataHash = Crypto.digest (Proxy :: Proxy Crypto.Blake2b_256) respBS

      mWarning <- case mHash of
        Nothing -> pure Nothing
        Just expectedMetaHash -> do
          case expectedMetaHash of
            OffChainPoolHash (PoolMetaHash m) ->
              if metadataHash /= m
                then left $ OCFErrHashMismatch url (renderByteArray m) (renderByteArray metadataHash)
                else pure Nothing
            OffChainVoteHash (VoteMetaHash m) ->
              if metadataHash /= m
                then pure $ Just $ textShow $ OCFErrHashMismatch url (renderByteArray m) (renderByteArray metadataHash)
                else pure Nothing

      case url of
        OffChainPoolUrl _ -> do
          decodedMetadata <-
            case Aeson.eitherDecode' respLBS of
              Left err -> left $ OCFErrJsonDecodeFail url (Text.pack err)
              Right res -> pure res
          pure $
            SimplifiedOffChainPoolDataType
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
        OffChainVoteUrl _ -> do
          decodedMetadata <-
            case Aeson.eitherDecode' @Aeson.Value respLBS of
              Left err -> left $ OCFErrJsonDecodeFail url (Text.pack err)
              Right res -> pure res
          pure $
            SimplifiedOffChainVoteDataType
              SimplifiedOffChainVoteData
                { sovaHash = metadataHash
                , sovaBytes = respBS
                , sovaJson = Text.decodeUtf8 $ LBS.toStrict (Aeson.encode decodedMetadata)
                , sovaContentType = mContentType
                , sovaWarning = mWarning
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

-------------------------------------------------------------------------------------
-- Url
-------------------------------------------------------------------------------------
parseOffChainPoolUrl :: PoolUrl -> ExceptT OffChainFetchError IO Http.Request
parseOffChainPoolUrl poolUrl =
  handleExceptT wrapHttpException $ applyContentType <$> Http.parseRequest (Text.unpack $ unPoolUrl poolUrl)
  where
    wrapHttpException :: HttpException -> OffChainFetchError
    wrapHttpException err = OCFErrHttpException (OffChainPoolUrl poolUrl) (textShow err)

parseOffChainVoteUrl :: VoteUrl -> ExceptT OffChainFetchError IO Http.Request
parseOffChainVoteUrl voteUrl =
  handleExceptT wrapHttpException $ applyContentType <$> Http.parseRequest (Text.unpack $ unVoteUrl voteUrl)
  where
    wrapHttpException :: HttpException -> OffChainFetchError
    wrapHttpException err = OCFErrHttpException (OffChainVoteUrl voteUrl) (textShow err)

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
