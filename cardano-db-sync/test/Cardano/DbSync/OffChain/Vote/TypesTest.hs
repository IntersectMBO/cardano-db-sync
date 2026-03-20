{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.DbSync.OffChain.Vote.TypesTest (tests) where

import Cardano.DbSync.OffChain.Vote.Types
import Cardano.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Hedgehog
import qualified Hedgehog as H
import Prelude ()

tests :: IO Bool
tests =
  checkParallel $
    Group
      "Cardano.DbSync.OffChain.Vote.Types"
      [ ("Image preserves data URI prefix", prop_image_preserves_data_uri_prefix)
      , ("Image handles URL with hash", prop_image_handles_url_with_hash)
      , ("Image preserves JPEG data URI", prop_image_preserves_jpeg_data_uri)
      ]

prop_image_preserves_data_uri_prefix :: Property
prop_image_preserves_data_uri_prefix = property $ do
  let jsonWithDataUri =
        LBS.fromStrict $
          encodeUtf8
            "{ \"contentUrl\": \"data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAUA\" }"

  case Aeson.eitherDecode jsonWithDataUri of
    Left err -> do
      H.footnote $ "Parse failed: " <> err
      H.failure
    Right (img :: Image) -> do
      let imgContent = textValue $ content img
      H.assert $ "data:" `Text.isPrefixOf` imgContent
      H.assert $ "image/png" `Text.isInfixOf` imgContent
      H.assert $ "base64" `Text.isInfixOf` imgContent
      imgContent === "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAUA"

prop_image_handles_url_with_hash :: Property
prop_image_handles_url_with_hash = property $ do
  let jsonWithUrl =
        LBS.fromStrict $
          encodeUtf8
            "{ \"contentUrl\": \"https://example.com/image.png\", \"sha256\": \"abc123\" }"

  case Aeson.eitherDecode jsonWithUrl of
    Left err -> do
      H.footnote $ "Parse failed: " <> err
      H.failure
    Right (img :: Image) -> do
      textValue (content img) === "https://example.com/image.png"
      (textValue <$> msha256 img) === Just "abc123"

prop_image_preserves_jpeg_data_uri :: Property
prop_image_preserves_jpeg_data_uri = property $ do
  let jsonWithJpeg =
        LBS.fromStrict $
          encodeUtf8
            "{ \"contentUrl\": \"data:image/jpeg;base64,/9j/4AAQSkZJRg\" }"

  case Aeson.eitherDecode jsonWithJpeg of
    Left err -> do
      H.footnote $ "Parse failed: " <> err
      H.failure
    Right (img :: Image) -> do
      let imgContent = textValue $ content img
      imgContent === "data:image/jpeg;base64,/9j/4AAQSkZJRg"
