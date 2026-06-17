{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.OffChain.HttpTest (tests) where

import qualified Cardano.Db as DB
import Cardano.DbSync.OffChain.Http (isPrivateAddr, parseOffChainUrl)
import Cardano.DbSync.Types (OffChainFetchError (..), OffChainUrlType (..))
import Cardano.Prelude
import qualified Data.Text as Text
import Hedgehog
import qualified Network.Socket as Socket

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
      ,
        ( "isPrivateAddr classifies IPv4, IPv6, and IPv4-mapped IPv6 addresses"
        , prop_isPrivateAddrClassification
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

-- | Table-driven coverage of 'isPrivateAddr'. The IPv4-mapped IPv6 rows are
-- the regression set for the SSRF gap fixed in PR #2132 (e.g. @::ffff:127.0.0.1@
-- previously slipped past the IPv6 branch because the mapped IPv4 was not unwrapped).
prop_isPrivateAddrClassification :: Property
prop_isPrivateAddrClassification = withTests 1 $ property $ do
  for_ classificationCases $ \(name, addr, expected) -> do
    annotate name
    isPrivateAddr addr === expected

classificationCases :: [([Char], Socket.SockAddr, Bool)]
classificationCases =
  -- IPv4-mapped IPv6 — the cases PR #2132 fixed.
  [ ("::ffff:127.0.0.1 (loopback)", mkV6 (0, 0, 0, 0, 0, 0xFFFF, 0x7F00, 0x0001), True)
  , ("::ffff:10.0.0.1 (RFC1918)", mkV6 (0, 0, 0, 0, 0, 0xFFFF, 0x0A00, 0x0001), True)
  , ("::ffff:192.168.1.1 (RFC1918)", mkV6 (0, 0, 0, 0, 0, 0xFFFF, 0xC0A8, 0x0101), True)
  , ("::ffff:172.16.0.1 (RFC1918)", mkV6 (0, 0, 0, 0, 0, 0xFFFF, 0xAC10, 0x0001), True)
  , ("::ffff:169.254.1.1 (link-local)", mkV6 (0, 0, 0, 0, 0, 0xFFFF, 0xA9FE, 0x0101), True)
  , ("::ffff:0.0.0.0 (this-network)", mkV6 (0, 0, 0, 0, 0, 0xFFFF, 0x0000, 0x0000), True)
  , ("::ffff:224.0.0.1 (multicast)", mkV6 (0, 0, 0, 0, 0, 0xFFFF, 0xE000, 0x0001), True)
  , ("::ffff:8.8.8.8 (public)", mkV6 (0, 0, 0, 0, 0, 0xFFFF, 0x0808, 0x0808), False)
  , ("::ffff:1.1.1.1 (public)", mkV6 (0, 0, 0, 0, 0, 0xFFFF, 0x0101, 0x0101), False)
  , -- Native IPv6 — regression guard for the pre-existing IPv6 rules.
    (":: (unspecified)", mkV6 (0, 0, 0, 0, 0, 0, 0, 0), True)
  , ("::1 (loopback)", mkV6 (0, 0, 0, 0, 0, 0, 0, 1), True)
  , ("fc00::1 (ULA)", mkV6 (0xFC00, 0, 0, 0, 0, 0, 0, 1), True)
  , ("fd00::1 (ULA)", mkV6 (0xFD00, 0, 0, 0, 0, 0, 0, 1), True)
  , ("fe80::1 (link-local)", mkV6 (0xFE80, 0, 0, 0, 0, 0, 0, 1), True)
  , ("2001:db8::1 (documentation)", mkV6 (0x2001, 0x0DB8, 0, 0, 0, 0, 0, 1), False)
  , -- IPv4 — regression guard, including range edges.
    ("127.0.0.1 (loopback)", mkV4 (127, 0, 0, 1), True)
  , ("10.0.0.1 (RFC1918)", mkV4 (10, 0, 0, 1), True)
  , ("192.168.1.1 (RFC1918)", mkV4 (192, 168, 1, 1), True)
  , ("172.16.0.1 (RFC1918)", mkV4 (172, 16, 0, 1), True)
  , ("172.32.0.1 (above RFC1918)", mkV4 (172, 32, 0, 1), False)
  , ("169.254.1.1 (link-local)", mkV4 (169, 254, 1, 1), True)
  , ("100.64.0.1 (CGNAT)", mkV4 (100, 64, 0, 1), True)
  , ("100.128.0.1 (above CGNAT)", mkV4 (100, 128, 0, 1), False)
  , ("198.18.0.1 (benchmarking)", mkV4 (198, 18, 0, 1), True)
  , ("224.0.0.1 (multicast)", mkV4 (224, 0, 0, 1), True)
  , ("8.8.8.8 (public)", mkV4 (8, 8, 8, 8), False)
  ]
  where
    mkV4 t = Socket.SockAddrInet 0 (Socket.tupleToHostAddress t)
    mkV6 t = Socket.SockAddrInet6 0 0 (Socket.tupleToHostAddress6 t) 0
