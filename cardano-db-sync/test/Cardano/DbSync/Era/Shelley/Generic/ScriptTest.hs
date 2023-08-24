{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Era.Shelley.Generic.ScriptTest (tests) where

import Cardano.DbSync.Era.Shelley.Generic.Script
import qualified Cardano.Ledger.Allegra.Scripts as Allegra
import Cardano.Ledger.Api (Era (), Shelley ())
import Cardano.Ledger.Binary.Decoding
import qualified Cardano.Ledger.Shelley.Scripts as Ledger
import Cardano.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LByteString
import Data.Sequence.Strict (fromList)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Hedgehog.Gen.QuickCheck (arbitrary)
import qualified Hedgehog.Range as Range
import Test.Cardano.Ledger.Allegra.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Arbitrary ()
import Prelude (String ())

tests :: IO Bool
tests =
  checkParallel $
    Group
      "Cardano.DbSync.Era.Shelley.Generic.Script"
      [ ("multisigToJSON simple", prop_multisigToJSON)
      , ("multisigToJSON negative", prop_multisigToJSON_bad)
      , ("multisigToJSON roundtrip", prop_multisigToJSON_roundtrip)
      , ("timelockToJSON simple", prop_timelockToJSON)
      , ("timelockToJSON negative", prop_timelockToJSON_bad)
      , ("timelockToJSON roundtrip", prop_timelockToJSON_roundtrip)
      ]

prop_multisigToJSON :: Property
prop_multisigToJSON = property $ do
  (cborHex, jsonText) <- forAll $ Gen.element knownMultiSigs
  multiSig <- evalEither $ decodeCbor cborHex
  json <- evalEither $ Aeson.eitherDecodeStrict @Aeson.Value $ encodeUtf8 jsonText

  Aeson.toJSON (fromMultiSig multiSig) === json
  where
    decodeCbor :: Text -> Either DecoderError (Ledger.MultiSig Shelley)
    decodeCbor cbor = do
      let bytes = LByteString.fromStrict $ deserialiseCborFromBase16 cbor
      Annotator ann <- decodeFull shelleyProtVer bytes
      pure $ ann (Full bytes)

prop_multisigToJSON_bad :: Property
prop_multisigToJSON_bad = property $ do
  jsonText <- forAll $ Gen.element knownBadMultiSigs
  assert $ isLeft (decodeJson jsonText)
  where
    decodeJson :: Text -> Either String (MultiSigScript Shelley)
    decodeJson = Aeson.eitherDecodeStrict . encodeUtf8

prop_multisigToJSON_roundtrip :: Property
prop_multisigToJSON_roundtrip = property $ do
  multiSig <- forAll genValidMultiSig
  tripping multiSig Aeson.toJSON Aeson.fromJSON

prop_timelockToJSON :: Property
prop_timelockToJSON = property $ do
  (cborHex, jsonText) <- forAll $ Gen.element (knownMultiSigs <> knownTimelocks)
  timelock <- evalEither $ decodeCbor cborHex
  json <- evalEither $ Aeson.eitherDecodeStrict @Aeson.Value $ encodeUtf8 jsonText

  Aeson.toJSON (fromTimelock timelock) === json
  where
    decodeCbor :: Text -> Either DecoderError (Allegra.Timelock Shelley)
    decodeCbor cbor = do
      let bytes = LByteString.fromStrict $ deserialiseCborFromBase16 cbor
      Annotator ann <- decodeFull shelleyProtVer bytes
      pure $ ann (Full bytes)

prop_timelockToJSON_bad :: Property
prop_timelockToJSON_bad = property $ do
  jsonText <- forAll $ Gen.element knownBadMultiSigs
  assert $ isLeft (decodeJson jsonText)
  where
    decodeJson :: Text -> Either String (TimelockScript Shelley)
    decodeJson = Aeson.eitherDecodeStrict . encodeUtf8

prop_timelockToJSON_roundtrip :: Property
prop_timelockToJSON_roundtrip = property $ do
  timelock <- forAll (genValidTimelock @Shelley)
  tripping timelock Aeson.toJSON Aeson.fromJSON

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

knownTimelocks :: [(Text, Text)]
knownTimelocks =
  [
    ( "82051a0446e5b4"
    , "{\"slot\": 71755188, \"type\": \"before\"}"
    )
  ,
    ( "8204190230"
    , "{\"slot\": 560, \"type\": \"after\"}"
    )
  ,
    ( "82018282051a0446e5b48200581ccda9e29dde371bd2c4f58edb036477ec1cde527dd890f429199f938e"
    , "{\"type\": \"all\", \"scripts\": [{ \"slot\": 71755188, \"type\": \"before\" }, {\"type\": \"sig\", \"keyHash\": \"cda9e29dde371bd2c4f58edb036477ec1cde527dd890f429199f938e\"}]}"
    )
  ,
    ( "8202818204190230"
    , "{\"type\": \"any\", \"scripts\": [{ \"slot\": 560, \"type\": \"after\" }]}"
    )
  ,
    ( "830301818204190230"
    , "{\"type\": \"atLeast\", \"required\": 1, \"scripts\": [{ \"slot\": 560, \"type\": \"after\" }]}"
    )
  ]

genValidMultiSig :: Gen (MultiSigScript Shelley)
genValidMultiSig = fromMultiSig <$> genValidLedgerMultiSigSized 5 10

genValidLedgerMultiSigSized :: Int -> Size -> Gen (Ledger.MultiSig Shelley)
genValidLedgerMultiSigSized _ 0 = Ledger.RequireSignature <$> arbitrary
genValidLedgerMultiSigSized maxListLen maxDepth =
  Gen.choice
    [ Ledger.RequireSignature <$> arbitrary
    , Ledger.RequireAllOf <$> genList 0 maxListLen
    , Ledger.RequireAnyOf <$> genList 0 maxListLen
    , genRequireMOf
    ]
  where
    genList minLen maxLen =
      genListSized
        (Range.linear minLen maxLen)
        maxDepth
        (genValidLedgerMultiSigSized maxListLen)

    genRequireMOf = do
      req <- Gen.int (Range.linear 0 maxListLen)
      Ledger.RequireMOf req <$> genList req maxListLen

genValidTimelock :: Era era => Gen (TimelockScript era)
genValidTimelock = fromTimelock <$> genValidLedgerTimelockSized 5 10

genValidLedgerTimelockSized :: Era era => Int -> Size -> Gen (Allegra.Timelock era)
genValidLedgerTimelockSized _ 0 = Allegra.RequireSignature <$> arbitrary
genValidLedgerTimelockSized maxListLen maxDepth =
  Gen.choice
    [ Allegra.RequireSignature <$> arbitrary
    , Allegra.RequireTimeExpire <$> arbitrary
    , Allegra.RequireTimeStart <$> arbitrary
    , Allegra.RequireAllOf <$> genList 0 maxListLen
    , Allegra.RequireAnyOf <$> genList 0 maxListLen
    , genRequireMOf
    ]
  where
    genList minLen maxLen =
      fromList
        <$> genListSized
          (Range.linear minLen maxLen)
          maxDepth
          (genValidLedgerTimelockSized maxListLen)

    genRequireMOf = do
      req <- Gen.int (Range.linear 0 maxListLen)
      Allegra.RequireMOf req <$> genList req maxListLen

genListSized :: Range Int -> Size -> (Size -> Gen a) -> Gen [a]
genListSized listLen (Size size) f = Gen.list listLen (f size')
  where
    size' = Size (size - 1)

deserialiseCborFromBase16 :: Text -> ByteString
deserialiseCborFromBase16 = Base16.decodeLenient . encodeUtf8
