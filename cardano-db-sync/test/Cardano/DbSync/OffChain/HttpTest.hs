{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.OffChain.HttpTest (tests) where

import qualified Cardano.Db as DB
import Cardano.DbSync.OffChain.Http (parseOffChainUrl)
import Cardano.DbSync.Types (OffChainFetchError (..), OffChainUrlType (..))
import Cardano.Prelude
import qualified Data.Text as Text
import Hedgehog

tests :: IO Bool
tests =
  checkParallel $
    Group
      "Cardano.DbSync.OffChain.Http"
      [
        ( "parseOffChainUrl rejects localhost-family hosts by default"
        , prop_rejectsLocalhostByDefault
        )
      ,
        ( "parseOffChainUrl accepts localhost-family hosts when allowPrivate=True"
        , prop_acceptsLocalhostWhenAllowed
        )
      ,
        ( "parseOffChainUrl accepts public hosts in either mode"
        , prop_acceptsPublicHosts
        )
      ,
        ( "parseOffChainUrl rejects non-HTTP schemes in either mode"
        , prop_rejectsNonHttpSchemes
        )
      ]

-- | URLs whose host string matches @isLocalhostHost@ in
-- 'Cardano.DbSync.OffChain.Http'.
localhostUrls :: [Text]
localhostUrls =
  [ "http://localhost/metadata.json"
  , "http://localhost:23299/pool2.json"
  , "http://127.0.0.1/metadata.json"
  , "http://[::1]/metadata.json"
  , "http://10.5.5.5/metadata.json"
  , "http://192.168.1.1/metadata.json"
  ]

-- | URLs that should always parse successfully.
publicUrls :: [Text]
publicUrls =
  [ "https://example.com/metadata.json"
  , "http://example.com/metadata.json"
  , "https://tinyurl.com/yvkfs7pr"
  ]

asPoolUrl :: Text -> OffChainUrlType
asPoolUrl = OffChainPoolUrl . DB.PoolUrl

prop_rejectsLocalhostByDefault :: Property
prop_rejectsLocalhostByDefault = withTests 1 $ property $ do
  for_ localhostUrls $ \url -> do
    annotateShow url
    result <- liftIO $ runExceptT $ parseOffChainUrl False (asPoolUrl url)
    case result of
      Left (OCFErrUrlParseFail _ msg) -> do
        annotate (Text.unpack msg)
        msg === "Access to localhost is not allowed"
      Left other -> do
        annotateShow other
        failure
      Right _ -> do
        annotate "Expected rejection but parse succeeded"
        failure

prop_acceptsLocalhostWhenAllowed :: Property
prop_acceptsLocalhostWhenAllowed = withTests 1 $ property $ do
  for_ localhostUrls $ \url -> do
    annotateShow url
    result <- liftIO $ runExceptT $ parseOffChainUrl True (asPoolUrl url)
    case result of
      Right _ -> success
      Left err -> do
        annotateShow err
        failure

prop_acceptsPublicHosts :: Property
prop_acceptsPublicHosts = withTests 1 $ property $ do
  for_ publicUrls $ \url -> do
    for_ [False, True] $ \allowPrivate -> do
      annotateShow (allowPrivate, url)
      result <- liftIO $ runExceptT $ parseOffChainUrl allowPrivate (asPoolUrl url)
      case result of
        Right _ -> success
        Left err -> do
          annotateShow err
          failure

prop_rejectsNonHttpSchemes :: Property
prop_rejectsNonHttpSchemes = withTests 1 $ property $ do
  let nonHttp =
        [ "ftp://example.com/x.json"
        , "file:///etc/passwd"
        , "javascript:alert(1)"
        ]
  for_ nonHttp $ \url -> do
    for_ [False, True] $ \allowPrivate -> do
      annotateShow (allowPrivate, url)
      result <- liftIO $ runExceptT $ parseOffChainUrl allowPrivate (asPoolUrl url)
      case result of
        Left (OCFErrUrlParseFail _ msg) -> do
          annotate (Text.unpack msg)
          msg === "Only HTTP/HTTPS URLs are allowed"
        Left other -> do
          annotateShow other
          failure
        Right _ -> do
          annotate "Expected rejection but parse succeeded"
          failure
