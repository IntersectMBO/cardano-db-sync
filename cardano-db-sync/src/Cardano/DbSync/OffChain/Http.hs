{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.OffChain.Http (
  httpGetOffChainData,
  parseOffChainUrl,
) where

import qualified Cardano.Crypto.Hash.Blake2b as Crypto
import qualified Cardano.Crypto.Hash.Class as Crypto
import Cardano.Db (PoolMetaHash (..), PoolUrl (..), VoteMetaHash (..), VoteUrl (..), textShow)
import Cardano.DbSync.OffChain.Types (
  PoolOffChainMetadata (..),
  PoolTicker (..),
 )
import Cardano.DbSync.Types (
  FetchError (..),
  FetchUrlType (..),
  OffChainFetchError(..),
  OffChainPoolWorkQueue (..),
  OffChainVoteWorkQueue (..),
  OffChainWorkQueueType (..),
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
import GHC.Base (String)
import GHC.Show (show)
import Network.HTTP.Client (HttpException (..))
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types as Http

httpGetOffChainData ::
  Http.Manager ->
  Http.Request ->
  OffChainWorkQueueType ->
  ExceptT OffChainFetchError IO SimplifiedOffChainDataType
httpGetOffChainData manager request oWorkQueueType = do
  res <- handleExceptT (convertHttpException oWorkQueueType url) httpGet
  hoistEither res
  where
    -- get
    _vals :: (FetchUrlType, ByteString)
    _vals@(url, expectedHash) =
      case oWorkQueueType of
        OffChainPoolWorkQueueType OffChainPoolWorkQueue {oPoolWqMetaHash = PoolMetaHash poolMetaHash, oPoolWqUrl} -> (FetchPoolUrl oPoolWqUrl, poolMetaHash)
        OffChainVoteWorkQueueType OffChainVoteWorkQueue {oVoteWqMetaHash = VoteMetaHash voteMetaHash, oVoteWqUrl} -> (FetchVoteUrl oVoteWqUrl, voteMetaHash)

    httpGet :: IO (Either OffChainFetchError SimplifiedOffChainDataType)
    httpGet =
      Http.withResponse request manager $ \responseBR -> do
        runExceptT $ do
          let status = Http.responseStatus responseBR
          unless (Http.statusCode status == 200)
            . left
            $ makeOffChainFetchError oWorkQueueType (FEHttpResponse url (Http.statusCode status) (Text.decodeLatin1 $ Http.statusMessage status))

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
                  when ("text/html" `BS.isInfixOf` ct)
                    $ left
                    $ makeOffChainFetchError oWorkQueueType
                    $ FEBadContentTypeHtml url (Text.decodeLatin1 ct)
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
                    $ makeOffChainFetchError oWorkQueueType
                    $ FEBadContentType url (Text.decodeLatin1 ct)

          unless (BS.length respBS <= 512)
            . left
            $ makeOffChainFetchError oWorkQueueType
            $ FEDataTooLong url

          let metadataHash = Crypto.digest (Proxy :: Proxy Crypto.Blake2b_256) respBS

          when (metadataHash /= expectedHash)
            . left
            $ makeOffChainFetchError oWorkQueueType
            $ FEHashMismatch url (renderByteArray expectedHash) (renderByteArray metadataHash)

          decodedMetadata <-
            case Aeson.eitherDecode' respLBS of
              Left err -> left $ makeOffChainFetchError oWorkQueueType $ FEJsonDecodeFail url (Text.pack err)
              Right res -> pure res

          case oWorkQueueType of
            OffChainPoolWorkQueueType ocp ->
              pure
                $ SimplifiedOffChainPoolDataType
                  ocp
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
            OffChainVoteWorkQueueType ocv ->
              pure
                $ SimplifiedOffChainVoteDataType
                  ocv
                  SimplifiedOffChainVoteData
                    { sovaHash = metadataHash
                    , sovaBytes = respBS
                    , -- Instead of inserting the `respBS` here, we encode the JSON and then store that.
                      -- This is necessary because the PostgreSQL JSON parser can reject some ByteStrings
                      -- that the Aeson parser accepts.
                      sovaJson = Text.decodeUtf8 $ LBS.toStrict (Aeson.encode decodedMetadata)
                    , sovaContentType = mContentType
                    }

makeOffChainFetchError :: OffChainWorkQueueType -> FetchError -> OffChainFetchError
makeOffChainFetchError ocwq fetchErr = do
  case ocwq of
    OffChainPoolWorkQueueType poolWq -> OffChainPoolFetchError fetchErr poolWq
    OffChainVoteWorkQueueType voteWq -> OffChainVoteFetchError fetchErr voteWq

-- | Is the provided ByteSring possibly JSON object?
-- Ignoring any leading whitespace, if the ByteString starts with a '{` character it might possibly
-- be a JSON object. We are are really only interested in differentiating between JSON and HTML
-- (where the first non-whitespace character will be '<'.
isPossiblyJsonObject :: ByteString -> Bool
isPossiblyJsonObject bs =
  case BS.uncons (BS.strip bs) of
    Just ('{', _) -> True
    _otherwise -> False

parseOffChainUrl :: OffChainWorkQueueType -> ExceptT OffChainFetchError IO Http.Request
parseOffChainUrl offChainWQT = do
  handleExceptT wrapHttpException $ applyContentType <$> Http.parseRequest offChainUrl
  where
    _allUrls :: (FetchUrlType, String)
    _allUrls@(fetchUrlType, offChainUrl) =
      case offChainWQT of
        OffChainPoolWorkQueueType ocpwk -> (FetchPoolUrl $ oPoolWqUrl ocpwk, Text.unpack . unPoolUrl $ oPoolWqUrl ocpwk)
        OffChainVoteWorkQueueType ocvawk -> (FetchVoteUrl $ oVoteWqUrl ocvawk, Text.unpack . unVoteUrl $ oVoteWqUrl ocvawk)

    applyContentType :: Http.Request -> Http.Request
    applyContentType req =
      req
        { Http.requestHeaders =
            Http.requestHeaders req ++ [(CI.mk "content-type", "application/json")]
        }

    wrapHttpException :: HttpException -> OffChainFetchError
    wrapHttpException err = makeOffChainFetchError offChainWQT $ FEHttpException fetchUrlType (textShow err)

-- -------------------------------------------------------------------------------------------------

convertHttpException :: OffChainWorkQueueType -> FetchUrlType -> HttpException -> OffChainFetchError
convertHttpException offChainWQT url he =
  case he of
    HttpExceptionRequest _req hec ->
      case hec of
        Http.ResponseTimeout -> makeOffChainFetchError offChainWQT $ FETimeout url "Response"
        Http.ConnectionTimeout -> makeOffChainFetchError offChainWQT $ FETimeout url "Connection"
        Http.ConnectionFailure {} -> makeOffChainFetchError offChainWQT $ FEConnectionFailure url
        Http.TooManyRedirects {} -> makeOffChainFetchError offChainWQT $ FEHttpException url "Too many redirects"
        Http.OverlongHeaders -> makeOffChainFetchError offChainWQT $ FEHttpException url "Overlong headers"
        Http.StatusCodeException resp _ -> makeOffChainFetchError offChainWQT $ FEHttpException url ("Status code exception " <> Text.pack (show $ Http.responseStatus resp))
        Http.InvalidStatusLine {} -> makeOffChainFetchError offChainWQT $ FEHttpException url "Invalid status line"
        other -> makeOffChainFetchError offChainWQT $ FEHttpException url (Text.take 100 $ Text.pack $ show other)
    InvalidUrlException urlx err ->
      case url of
        FetchPoolUrl _ -> makeOffChainFetchError offChainWQT $ FEUrlParseFail (FetchPoolUrl $ PoolUrl $ Text.pack urlx) (Text.pack err)
        FetchVoteUrl _ -> makeOffChainFetchError offChainWQT $ FEUrlParseFail (FetchVoteUrl $ VoteUrl $ Text.pack urlx) (Text.pack err)
