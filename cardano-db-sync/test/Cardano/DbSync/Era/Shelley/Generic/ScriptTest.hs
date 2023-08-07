{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Era.Shelley.Generic.ScriptTest (tests) where

import qualified Cardano.Api.Shelley as Shelley
import Cardano.Binary (serialize')
import Cardano.DbSync.Era.Shelley.Generic.Script
import Cardano.Ledger.Api (Shelley ())
import Cardano.Ledger.Binary.Decoding
import Cardano.Ledger.Shelley.Scripts (MultiSig (..))
import Cardano.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LByteString
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Prelude (String ())

tests :: IO Bool
tests =
  checkParallel $
    Group
      "Cardano.DbSync.Era.Shelley.Generic.Script"
      [ ("multisigToJSON simple", prop_multisigToJSON)
      , ("multisigToJSON negative", prop_multisigToJSON_bad)
      , ("multisigToJSON roundtrip", prop_multisigToJSON_roundtrip)
      , ("multisigToJSON A/B", prop_multisigToJSON_api)
      ]

prop_multisigToJSON :: Property
prop_multisigToJSON = property $ do
  (cborHex, jsonText) <- forAll $ Gen.element knownMultiSigs
  multiSig <- evalEither $ decodeCbor cborHex
  json <- evalEither $ Aeson.eitherDecodeStrict @Aeson.Value $ encodeUtf8 jsonText

  Aeson.toJSON (fromMultiSig multiSig) === json
  where
    decodeCbor :: Text -> Either DecoderError (MultiSig Shelley)
    decodeCbor cbor = do
      let bytes = LByteString.fromStrict $ deserialiseCborFromBase16 cbor
      Annotator ann <- decodeFull shelleyProtVer bytes
      pure $ ann (Full bytes)

prop_multisigToJSON_bad :: Property
prop_multisigToJSON_bad = property $ do
  jsonText <- forAll $ Gen.element knownBadMultiSigs
  assert $ isLeft (decodeJson jsonText)
  where
    decodeJson :: Text -> Either String MultiSigScript
    decodeJson jsonText = Aeson.eitherDecodeStrict $ encodeUtf8 jsonText

prop_multisigToJSON_roundtrip :: Property
prop_multisigToJSON_roundtrip = property $ do
  (cborHex, _) <- forAll $ Gen.element knownMultiSigs
  multiSig <- evalEither $ decodeCbor cborHex

  tripping (fromMultiSig multiSig) Aeson.toJSON Aeson.fromJSON
  where
    decodeCbor :: Text -> Either DecoderError (MultiSig Shelley)
    decodeCbor cbor = do
      let bytes = LByteString.fromStrict $ deserialiseCborFromBase16 cbor
      Annotator ann <- decodeFull shelleyProtVer bytes
      pure $ ann (Full bytes)

prop_multisigToJSON_api :: Property
prop_multisigToJSON_api = property $ do
  (cborHex, jsonText) <- forAll $ Gen.element knownMultiSigs
  multiSig <- decodeMultiSig $ encodeUtf8 jsonText

  cborHex === serialiseCborToBase16 (serialize' multiSig)
  where
    toMultiSig :: MonadTest m => Shelley.SimpleScript -> m (MultiSig Shelley)
    toMultiSig = evalEither . Shelley.toShelleyMultiSig

    decodeMultiSig :: MonadTest m => ByteString -> m (MultiSig Shelley)
    decodeMultiSig =
      (evalEither . Aeson.eitherDecodeStrict @Shelley.SimpleScript)
        >=> toMultiSig

knownMultiSigs :: [(Text, Text)]
knownMultiSigs =
  [
    ( "8201818200581c2f3f904868add46586c39811cb4c217db565903a13294804cb5fd036"
    , "{\"type\": \"all\", \"scripts\": [{\"type\": \"sig\", \"keyHash\": \"2f3f904868add46586c39811cb4c217db565903a13294804cb5fd036\"}]}"
    )
  ,
    ( "8200581ccda9e29dde371bd2c4f58edb036477ec1cde527dd890f429199f938e"
    , "{\"type\": \"sig\", \"keyHash\": \"cda9e29dde371bd2c4f58edb036477ec1cde527dd890f429199f938e\"}"
    )
  ,
    ( "8202818200581cc4df92cc8704145d9d4c39f548f9e5a9500288d86c2eeb0acef3fe18"
    , "{\"type\": \"any\", \"scripts\": [{\"type\": \"sig\", \"keyHash\": \"c4df92cc8704145d9d4c39f548f9e5a9500288d86c2eeb0acef3fe18\"}]}"
    )
  ,
    ( "830301818200581c58d2196589e3d007deb6871ac3e6b6f15ccf360467ce05c93067c219"
    , "{\"type\": \"atLeast\", \"required\": 1, \"scripts\": [{\"type\": \"sig\", \"keyHash\": \"58d2196589e3d007deb6871ac3e6b6f15ccf360467ce05c93067c219\"}]}"
    )
  ,
    ( "8201828200581c2f3f904868add46586c39811cb4c217db565903a13294804cb5fd0368200581ccda9e29dde371bd2c4f58edb036477ec1cde527dd890f429199f938e"
    , "{\"type\": \"all\", \"scripts\": [{\"type\": \"sig\", \"keyHash\": \"2f3f904868add46586c39811cb4c217db565903a13294804cb5fd036\"}, {\"type\": \"sig\", \"keyHash\": \"cda9e29dde371bd2c4f58edb036477ec1cde527dd890f429199f938e\"}]}"
    )
  ]

knownBadMultiSigs :: [Text]
knownBadMultiSigs =
  [ "\"some garbage\""
  , "{\"type\": \"atLeast\", \"required\": 2, \"scripts\": [{\"type\": \"sig\", \"keyHash\": \"58d2196589e3d007deb6871ac3e6b6f15ccf360467ce05c93067c219\"}]}"
  ]

serialiseCborToBase16 :: ByteString -> Text
serialiseCborToBase16 = decodeUtf8 . Base16.encode

deserialiseCborFromBase16 :: Text -> ByteString
deserialiseCborFromBase16 = Base16.decodeLenient . encodeUtf8
