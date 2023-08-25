{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbSync.Util.Bech32Test (tests) where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), serialize', unsafeDeserialize')
import Cardano.Crypto.Hash.Blake2b (Blake2b_224 ())
import Cardano.Crypto.Hash.Class (HashAlgorithm (..), hashFromBytes)
import Cardano.Crypto.VRF.Praos (genSeed, keypairFromSeed)
import Cardano.DbSync.Util.Bech32
import Cardano.Ledger.Crypto (StandardCrypto ())
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Prelude
import Data.ByteString.Base16 (decodeLenient, encode)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Prelude ()

tests :: IO Bool
tests =
  checkParallel $
    Group
      "Cardano.DbSync.Util.Bech32"
      [ ("serialiseVerKeyVrfToBech32 simple", prop_serialiseToBech32)
      , ("serialiseVerKeyVrfToBech32 roundtrip", prop_serialiseVerKeyVrfToBech32_roundtrip)
      , ("serialiseStakePoolHashKeyToBech32 simple", prop_serialiseStakePoolHashKeyToBech32)
      , ("serialiseStakePoolKeyHashToBech32 roundtrip", prop_serialiseStakePoolKeyHashToBech32_roundtrip)
      ]

prop_serialiseToBech32 :: Property
prop_serialiseToBech32 = property $ do
  (cborHex, expected) <- forAll $ Gen.element knownVrfKeys
  bech32 cborHex === expected
  where
    bech32 = serialiseVerKeyVrfToBech32 . serialiseBase16

prop_serialiseVerKeyVrfToBech32_roundtrip :: Property
prop_serialiseVerKeyVrfToBech32_roundtrip = property $ do
  cborHex <- genKey
  tripping cborHex toBech32 fromBech32
  where
    toBech32 = serialiseVerKeyVrfToBech32 . serialiseBase16

    fromBech32 :: Text -> Identity Text
    fromBech32 = pure . deserialiseBase16 . deserialiseVerKeyVrfFromBech32

prop_serialiseStakePoolHashKeyToBech32 :: Property
prop_serialiseStakePoolHashKeyToBech32 = property $ do
  (cborHex, expected) <- forAll $ Gen.element knownHashKeys
  bech32 cborHex === expected
  where
    bech32 = serialiseStakePoolKeyHashToBech32 . serialiseBase16

prop_serialiseStakePoolKeyHashToBech32_roundtrip :: Property
prop_serialiseStakePoolKeyHashToBech32_roundtrip = property $ do
  (Just hash') <- forAll genBlake224
  let cborHex = decodeUtf8 $ encode $ serialize' hash'

  tripping cborHex toBech32 fromBech32
  where
    toBech32 = serialiseStakePoolKeyHashToBech32 . serialiseBase16

    fromBech32 :: Text -> Identity Text
    fromBech32 = pure . deserialiseBase16 . deserialiseStakePoolKeyHashFromBech32

knownVrfKeys :: [(Text, Text)]
knownVrfKeys =
  [
    ( "582021be3858cb928b93f40cd1df8604d589e2a8a85af185b6d8e54b90cf236741f5"
    , "vrf_vk1yxlrskxtj29e8aqv680cvpx4383232z67xzmdk89fwgv7gm8g86s9r64v5"
    )
  ,
    ( "5820533977dee9ab19398341214239bacdcab0c8bc40cd921faea8176e92b4dc5594"
    , "vrf_vk12vuh0hhf4vvnnq6py9prnwkde2cv30zqekfplt4gzahf9dxu2k2qu5fsh5"
    )
  ,
    ( "5820c98f95c78f499d4a043d7a42631207ee0a13ee6c84ab49f2f362c9cf6f2e982a"
    , "vrf_vk1ex8et3u0fxw55ppa0fpxxys8ac9p8mnvsj45nuhnvtyu7mewnq4qtekycx"
    )
  ,
    ( "5820465aa7e0c8492c53ea734e3d0dcf28629ee105254d09b9df543feb2b0d19b2a7"
    , "vrf_vk1ged20cxgfyk986nnfc7smnegv20wzpf9f5ymnh658l4jkrgek2nsskfvw9"
    )
  ,
    ( "5820c7d4e5367feb3217d6cafc4e444832313daacfab1db9a557f877f1d924cf2d64"
    , "vrf_vk1cl2w2dnlavep04k2l38ygjpjxy764natrku624lcwlcajfx094jqhwejq3"
    )
  ,
    ( "582059729629c8c687505fbbb857e6ab0d13c3af8cc47aaed14acecb9839941fd9aa"
    , "vrf_vk1t9efv2wgc6r4qhamhpt7d2cdz0p6lrxy02hdzjkwewvrn9qlmx4qrnapu3"
    )
  ]

knownHashKeys :: [(Text, Text)]
knownHashKeys =
  [
    ( "581c84869421e27c3914727433612a599f8d774cfe224eb76ea53cb20bcc"
    , "pool1sjrfgg0z0su3gun5xdsj5kvl34m5el3zf6mkaffukg9ucc0q4zc"
    )
  ,
    ( "581c407fc80d26c3cfec84b51f57d8edea00480eebba455d9119354b9d3c"
    , "pool1gplusrfxc087ep94rata3m02qpyqa6a6g4wezxf4fwwnczzyg3l"
    )
  ,
    ( "581caae152692161a788e55d038c0bd0d0b4882f8456d0f2641171812ea6"
    , "pool14ts4y6fpvxnc3e2aqwxqh5xskjyzlpzk6rexgyt3syh2vdgrv7a"
    )
  ,
    ( "581cc58cdddf6b551416bc8250cae1e9e47aeaecaa47f8aa09b29268a904"
    , "pool1ckxdmhmt252pd0yz2r9wr60y0t4we2j8lz4qnv5jdz5sgrxc3vn"
    )
  ,
    ( "581c4d42609b19cbf77735569b8167039d946999fe0b8431ac0e1b5a96c6"
    , "pool1f4pxpxcee0mhwd2knwqkwquaj35enlstssc6crsmt2tvv0pwymt"
    )
  ]

-- * Generators
genKey :: MonadIO io => PropertyT io Text
genKey = do
  seed <- liftIO genSeed

  let vkey = fst $ keypairFromSeed seed

  pure $ decodeUtf8 $ encode $ serialize' vkey

genBlake224 :: MonadGen m => m (Maybe (KeyHash 'StakePool StandardCrypto))
genBlake224 = do
  serialiseHash <$> Gen.bytes (Range.linear 0 100)
  where
    serialiseHash :: ByteString -> Maybe (KeyHash 'StakePool StandardCrypto)
    serialiseHash = (KeyHash <$>) . hashFromBytes . digest (Proxy @Blake2b_224)

-- * Utilities
serialiseBase16 :: FromCBOR a => Text -> a
serialiseBase16 = unsafeDeserialize' . decodeLenient . encodeUtf8

deserialiseBase16 :: (Show a, ToCBOR b) => Either a b -> Text
deserialiseBase16 = decodeUtf8 . encode . serialize' . fromEither
  where
    fromEither :: Show a => Either a b -> b
    fromEither = either (panic . show) identity
