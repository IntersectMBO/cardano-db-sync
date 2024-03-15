{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Cardano.Db (
  EntityField (..),
  OffChainPoolData,
  PoolHashId,
  PoolMetaHash (..),
  PoolMetadataRef,
  PoolRetire,
  PoolUrl (..),
  runDbNoLoggingEnv,
  unValue4,
 )
import Cardano.DbSync.OffChain.Http (
  httpGetOffChainPoolData,
  parseOffChainUrl,
 )
import Cardano.DbSync.Types (
  OffChainFetchError (..),
  OffChainUrlType (..),
 )
import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Except.Extra (runExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString.Char8 (ByteString)
import qualified Data.List as List
import qualified Data.List.Extra as List
import Data.Ord (Down (..))
import Data.Text (Text)
import Database.Esqueleto.Experimental (
  SqlBackend,
  from,
  innerJoin,
  notExists,
  on,
  select,
  table,
  where_,
  (:&) ((:&)),
  (==.),
  (^.),
 )
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- A test/debug program that pulls PoolMetadataRef data from the database and then tests all
-- the URLs found.

main :: IO ()
main = do
  manager <- Http.newManager tlsManagerSettings
  xs <- runDbNoLoggingEnv queryTestOffChainData
  putStrLn $ "testOffChainPoolDataFetch: " ++ show (length xs) ++ " tests to run."
  tfs <- foldM (testOne manager) emptyTestFailure xs
  reportTestFailures tfs
  where
    testOne :: Http.Manager -> TestFailure -> TestOffChain -> IO TestFailure
    testOne manager !accum testPoolOffChain = do
      let poolUrl = toUrl testPoolOffChain
          mHash = Just $ toHash testPoolOffChain
      eres <- runExceptT $ do
        request <- parseOffChainUrl (OffChainPoolUrl poolUrl)
        httpGetOffChainPoolData manager request poolUrl mHash
      case eres of
        Left err -> do
          print err
          pure $ classifyFetchError accum err
        Right _ ->
          pure accum

-- -------------------------------------------------------------------------------------------------

data TestOffChain = TestOffChain
  { toTicker :: !Text
  , toUrl :: !PoolUrl
  , toHash :: !PoolMetaHash
  }

data TestFailure = TestFailure
  { tfHashMismatch :: !Word
  , tfDataTooLong :: !Word
  , tfUrlParseFail :: !Word
  , tfJsonDecodeFail :: !Word
  , tfHttpException :: !Word
  , tfHttpResponse :: !Word
  , tfBadContentType :: !Word
  , tfBadContentTypeHtml :: !Word
  , tfIOException :: !Word
  , tfTimeout :: !Word
  , tfConnectionFailure :: !Word
  }

classifyFetchError :: TestFailure -> OffChainFetchError -> TestFailure
classifyFetchError tf fe =
  case fe of
    OCFErrHashMismatch {} -> tf {tfHashMismatch = tfHashMismatch tf + 1}
    OCFErrDataTooLong {} -> tf {tfDataTooLong = tfDataTooLong tf + 1}
    OCFErrUrlParseFail {} -> tf {tfUrlParseFail = tfUrlParseFail tf + 1}
    OCFErrJsonDecodeFail {} -> tf {tfJsonDecodeFail = tfJsonDecodeFail tf + 1}
    OCFErrHttpException {} -> tf {tfHttpException = tfHttpException tf + 1}
    OCFErrHttpResponse {} -> tf {tfHttpResponse = tfHttpResponse tf + 1}
    OCFErrBadContentType {} -> tf {tfBadContentType = tfBadContentType tf + 1}
    OCFErrBadContentTypeHtml {} -> tf {tfBadContentTypeHtml = tfBadContentTypeHtml tf + 1}
    OCFErrIOException {} -> tf {tfIOException = tfIOException tf + 1}
    OCFErrTimeout {} -> tf {tfTimeout = tfTimeout tf + 1}
    OCFErrConnectionFailure {} -> tf {tfConnectionFailure = tfConnectionFailure tf + 1}

emptyTestFailure :: TestFailure
emptyTestFailure = TestFailure 0 0 0 0 0 0 0 0 0 0 0

reportTestFailures :: TestFailure -> IO ()
reportTestFailures tf = do
  putStrLn "\nTest results:"
  mapM_
    putStrLn
    [ "  HashMismatch : " ++ show (tfHashMismatch tf)
    , "  DataTooLong : " ++ show (tfDataTooLong tf)
    , "  UrlParseFail : " ++ show (tfUrlParseFail tf)
    , "  JsonDecodeFail : " ++ show (tfJsonDecodeFail tf)
    , "  HttpException : " ++ show (tfHttpException tf)
    , "  HttpResponse : " ++ show (tfHttpResponse tf)
    , "  BadContentType : " ++ show (tfBadContentType tf)
    , "  IOException : " ++ show (tfIOException tf)
    , "  Timeout : " ++ show (tfTimeout tf)
    , "  ConnectionFailure : " ++ show (tfConnectionFailure tf)
    ]

-- reportTestOffChain :: TestOffChain -> IO ()
-- reportTestOffChain tof = Text.putStrLn $ mconcat [ toTicker tof, " ", unPoolUrl (toUrl tof) ]

queryTestOffChainData :: MonadIO m => ReaderT SqlBackend m [TestOffChain]
queryTestOffChainData = do
  res <- select $ do
    (pod :& pmr) <-
      from
        $ table @OffChainPoolData
          `innerJoin` table @PoolMetadataRef
        `on` (\(pod :& pmr) -> pod ^. OffChainPoolDataPmrId ==. pmr ^. PoolMetadataRefId)
    where_ $ notExists (from (table @PoolRetire) >>= \pr -> where_ (pod ^. OffChainPoolDataPoolId ==. pr ^. PoolRetireHashId))
    pure
      ( pod ^. OffChainPoolDataTickerName
      , pmr ^. PoolMetadataRefUrl
      , pmr ^. PoolMetadataRefHash
      , pod ^. OffChainPoolDataPoolId
      )
  pure . organise $ map (convert . unValue4) res
  where
    convert :: (Text, PoolUrl, ByteString, PoolHashId) -> (PoolHashId, TestOffChain)
    convert (tname, url, hash, poolId) =
      ( poolId
      , TestOffChain
          { toTicker = tname
          , toUrl = url
          , toHash = PoolMetaHash hash
          }
      )

    organise :: [(PoolHashId, TestOffChain)] -> [TestOffChain]
    organise = map (List.head . map snd . List.sortOn (Down . fst)) . List.groupOn fst
