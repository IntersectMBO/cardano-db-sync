{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbSync.Era.Shelley.Generic.ScriptDataTest (tests) where

import Cardano.DbSync.Era.Shelley.Generic.ScriptData
import Cardano.Ledger.Alonzo.Scripts.Data (Data (..))
import qualified Cardano.Ledger.Alonzo.Scripts.Data as Ledger
import Cardano.Ledger.Api (Shelley ())
import Cardano.Ledger.Binary.Decoding
import Cardano.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LByteString
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Gen.QuickCheck as Gen
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Prelude (String ())

tests :: IO Bool
tests =
  checkParallel $
    Group
      "Cardano.DbSync.Era.Shelley.Generic.ScriptData"
      [ ("scriptDataToJSON simple", prop_scriptDataToJSON)
      , ("scriptDataToJSON negative", prop_scriptDataToJSON_bad)
      , ("scriptDataToJSON roundtrip", prop_scriptDataToJSON_roundtrip)
      ]

prop_scriptDataToJSON :: Property
prop_scriptDataToJSON = property $ do
  (cborHex, jsonText) <- forAll $ Gen.element knownScriptData
  data' <- evalEither $ ScriptData <$> decodeCbor cborHex
  json <- evalEither $ Aeson.eitherDecodeStrict @Aeson.Value $ encodeUtf8 jsonText

  Aeson.toJSON data' === json

prop_scriptDataToJSON_bad :: Property
prop_scriptDataToJSON_bad = property $ do
  jsonText <- forAll $ Gen.element knownBadScriptData
  assert $ isLeft (decodeJson jsonText)
  where
    decodeJson :: Text -> Either String (ScriptData Shelley)
    decodeJson = Aeson.eitherDecodeStrict . encodeUtf8

prop_scriptDataToJSON_roundtrip :: Property
prop_scriptDataToJSON_roundtrip = property $ do
  scriptData <- forAll genScriptData
  tripping scriptData Aeson.toJSON Aeson.fromJSON

knownScriptData :: [(Text, Text)]
knownScriptData =
  [
    ( "581c0aa09220c41d6fad50e703b432527e59c713c9d488264095f9395c01"
    , "{\"bytes\": \"0aa09220c41d6fad50e703b432527e59c713c9d488264095f9395c01\"}"
    )
  ,
    ( "a117581c0aa09220c41d6fad50e703b432527e59c713c9d488264095f9395c01"
    , "{\"map\": [{\"k\": {\"int\": 23}, \"v\": {\"bytes\": \"0aa09220c41d6fad50e703b432527e59c713c9d488264095f9395c01\"}}]}"
    )
  ,
    ( "1a4affce67"
    , "{\"int\": 1258278503}"
    )
  ,
    ( "9f0cff"
    , "{\"list\": [{ \"int\": 12}] }"
    )
  ,
    ( "d87d9f1818ff"
    , "{\"fields\": [{\"int\": 24}], \"constructor\": 4}"
    )
  ]

knownBadScriptData :: [Text]
knownBadScriptData =
  [ "\"some garbage\""
  , "5"
  , "true"
  , "[]"
  , "{\"some garbage\": 10}"
  , "{\"int\": 1.5}"
  , "{\"fields\": [{\"int\": 24}], \"constructor\": 4, \"extra\": 5}"
  ]

genScriptData :: Gen (ScriptData Shelley)
genScriptData = ScriptData . Data <$> Gen.arbitrary

decodeCbor :: Text -> Either DecoderError (Ledger.Data Shelley)
decodeCbor =
  decode'
    . LByteString.fromStrict
    . Base16.decodeLenient
    . encodeUtf8
  where
    decode' :: LByteString.ByteString -> Either DecoderError (Ledger.Data Shelley)
    decode' bytes = do
      Annotator ann <- decodeFull shelleyProtVer bytes
      pure $ ann (Full bytes)
