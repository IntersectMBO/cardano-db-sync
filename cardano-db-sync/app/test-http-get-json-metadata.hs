{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Cardano.Db (
  EntityField (..),
  PoolHashId,
  PoolMetaHash (..),
  PoolMetadataRef,
  PoolOfflineData,
  PoolRetire,
  PoolUrl (..),
  runDbNoLoggingEnv,
  unValue4,
 )
import Cardano.DbSync.Era.Shelley.Offline.Http (
  FetchError (..),
  httpGetPoolOfflineData,
  parsePoolUrl,
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
  xs <- runDbNoLoggingEnv queryTestOfflineData
  putStrLn $ "testOfflineDataFetch: " ++ show (length xs) ++ " tests to run."
  tfs <- foldM (testOne manager) emptyTestFailure xs
  reportTestFailures tfs
  where
    testOne :: Http.Manager -> TestFailure -> TestOffline -> IO TestFailure
    testOne manager !accum testOffline = do
      let poolUrl = toUrl testOffline
          mHash = Just $ toHash testOffline
      eres <- runExceptT $ do
        request <- parsePoolUrl poolUrl
        httpGetPoolOfflineData manager request poolUrl mHash
      case eres of
        Left err -> do
          print err
          pure $ classifyFetchError accum err
        Right _ ->
          pure accum

-- -------------------------------------------------------------------------------------------------

data TestOffline = TestOffline
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

classifyFetchError :: TestFailure -> FetchError -> TestFailure
classifyFetchError tf fe =
  case fe of
    FEHashMismatch {} -> tf {tfHashMismatch = tfHashMismatch tf + 1}
    FEDataTooLong {} -> tf {tfDataTooLong = tfDataTooLong tf + 1}
    FEUrlParseFail {} -> tf {tfUrlParseFail = tfUrlParseFail tf + 1}
    FEJsonDecodeFail {} -> tf {tfJsonDecodeFail = tfJsonDecodeFail tf + 1}
    FEHttpException {} -> tf {tfHttpException = tfHttpException tf + 1}
    FEHttpResponse {} -> tf {tfHttpResponse = tfHttpResponse tf + 1}
    FEBadContentType {} -> tf {tfBadContentType = tfBadContentType tf + 1}
    FEBadContentTypeHtml {} -> tf {tfBadContentTypeHtml = tfBadContentTypeHtml tf + 1}
    FEIOException {} -> tf {tfIOException = tfIOException tf + 1}
    FETimeout {} -> tf {tfTimeout = tfTimeout tf + 1}
    FEConnectionFailure {} -> tf {tfConnectionFailure = tfConnectionFailure tf + 1}

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

-- reportTestOffline :: TestOffline -> IO ()
-- reportTestOffline tof = Text.putStrLn $ mconcat [ toTicker tof, " ", unPoolUrl (toUrl tof) ]

queryTestOfflineData :: MonadIO m => ReaderT SqlBackend m [TestOffline]
queryTestOfflineData = do
  res <- select $ do
    (pod :& pmr) <-
      from
        $ table @PoolOfflineData
          `innerJoin` table @PoolMetadataRef
        `on` (\(pod :& pmr) -> pod ^. PoolOfflineDataPmrId ==. pmr ^. PoolMetadataRefId)
    where_ $ notExists (from (table @PoolRetire) >>= \pr -> where_ (pod ^. PoolOfflineDataPoolId ==. pr ^. PoolRetireHashId))
    pure
      ( pod ^. PoolOfflineDataTickerName
      , pmr ^. PoolMetadataRefUrl
      , pmr ^. PoolMetadataRefHash
      , pod ^. PoolOfflineDataPoolId
      )
  pure . organise $ map (convert . unValue4) res
  where
    convert :: (Text, PoolUrl, ByteString, PoolHashId) -> (PoolHashId, TestOffline)
    convert (tname, url, hash, poolId) =
      ( poolId
      , TestOffline
          { toTicker = tname
          , toUrl = url
          , toHash = PoolMetaHash hash
          }
      )

    organise :: [(PoolHashId, TestOffline)] -> [TestOffline]
    organise = map (List.head . map snd . List.sortOn (Down . fst)) . List.groupOn fst
