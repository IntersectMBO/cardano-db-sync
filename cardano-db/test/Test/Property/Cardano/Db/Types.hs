{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Test.Property.Cardano.Db.Types
  ( genAda
  , tests
  ) where

import           Cardano.Chain.Common (maxLovelaceVal)

import qualified Cardano.Crypto.Hash as Crypto

import           Cardano.Db

import qualified Cardano.Ledger.Hashes as Ledger
import           Cardano.Ledger.Mary.Value (AssetName (..), PolicyID (..))

import qualified Data.Aeson as Aeson
import           Data.Bifunctor (first)
import qualified Data.ByteString.Base16 as Base16
import           Data.ByteString.Char8 (ByteString)
import           Data.Either (fromRight)
import           Data.Int (Int64)
import           Data.Maybe (fromMaybe)
import           Data.Ratio ((%))
import qualified Data.Text as Text
import           Data.WideWord.Word128 (Word128 (..))
import           Data.Word (Word64)

import           Database.Persist.Class (PersistField (..))
import           Database.Persist.Types (PersistValue (..))

import           Hedgehog (Gen, Property, discover, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Numeric.Natural (Natural)

import           Ouroboros.Consensus.Shelley.Protocol (StandardCrypto)


prop_roundtrip_Ada_via_JSON :: Property
prop_roundtrip_Ada_via_JSON =
  H.withTests 5000 . H.property $ do
    mv <- H.forAll genAda
    H.tripping mv Aeson.encode Aeson.eitherDecode

prop_AssetFingerprint :: Property
prop_AssetFingerprint =
    H.withTests 1 . H.property $
      mapM_ (\(p, a, f) -> mkAssetFingerprint p a === f) testVectors
  where
    testVectors :: [(PolicyID StandardCrypto, AssetName, AssetFingerprint)]
    testVectors =
      [ ( mkPolicyId "7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373"
        , AssetName ""
        , AssetFingerprint "asset1rjklcrnsdzqp65wjgrg55sy9723kw09mlgvlc3"
        )
      , ( mkPolicyId "7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc37e"
        , AssetName ""
        , AssetFingerprint "asset1nl0puwxmhas8fawxp8nx4e2q3wekg969n2auw3"
        )
      , ( mkPolicyId "1e349c9bdea19fd6c147626a5260bc44b71635f398b67c59881df209"
        , AssetName ""
        , AssetFingerprint "asset1uyuxku60yqe57nusqzjx38aan3f2wq6s93f6ea"
        )
      , ( mkPolicyId "7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373"
        , hexAssetName "504154415445"
        , AssetFingerprint "asset13n25uv0yaf5kus35fm2k86cqy60z58d9xmde92"
        )
      , ( mkPolicyId "1e349c9bdea19fd6c147626a5260bc44b71635f398b67c59881df209"
        , hexAssetName "504154415445"
        , AssetFingerprint "asset1hv4p5tv2a837mzqrst04d0dcptdjmluqvdx9k3"
        )
      , ( mkPolicyId "1e349c9bdea19fd6c147626a5260bc44b71635f398b67c59881df209"
        , hexAssetName "7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373"
        , AssetFingerprint "asset1aqrdypg669jgazruv5ah07nuyqe0wxjhe2el6f"
        )
      , ( mkPolicyId "7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373"
        , hexAssetName "1e349c9bdea19fd6c147626a5260bc44b71635f398b67c59881df209"
        , AssetFingerprint "asset17jd78wukhtrnmjh3fngzasxm8rck0l2r4hhyyt"
        )
      , ( mkPolicyId "7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373"
        , hexAssetName "0000000000000000000000000000000000000000000000000000000000000000"
        , AssetFingerprint "asset1pkpwyknlvul7az0xx8czhl60pyel45rpje4z8w"
        )
      ]

    mkPolicyId :: ByteString -> PolicyID StandardCrypto
    mkPolicyId =
      PolicyID . Ledger.ScriptHash . fromMaybe (error "mkPolicyId:Hash") . Crypto.hashFromBytes
        . fromRight (error "mkPolicyId:Base16") . Base16.decode

    hexAssetName :: ByteString -> AssetName
    hexAssetName = AssetName . fromRight (error "hexAssetName") . Base16.decode


prop_roundtrip_DbInt65_PersistField :: Property
prop_roundtrip_DbInt65_PersistField =
    H.withTests 5000 . H.property $ do
      (i65, pv) <- H.forAll genDbInt65PresistValue
      fromPersistValue pv === Right i65

prop_roundtrip_DbLovelace_PersistField :: Property
prop_roundtrip_DbLovelace_PersistField =
    H.withTests 5000 . H.property $ do
      (w64, pv) <- H.forAll genDbLovelacePresistValue
      fromPersistValue pv === Right w64

prop_roundtrip_DbWord64_PersistField :: Property
prop_roundtrip_DbWord64_PersistField =
    H.withTests 5000 . H.property $ do
      (w64, pv) <- H.forAll genDbWord64PresistValue
      fromPersistValue pv === Right w64

prop_roundtrip_Word128_PersistField :: Property
prop_roundtrip_Word128_PersistField =
  H.withTests 5000 . H.property $ do
    w128 <- H.forAll genWord128
    H.tripping w128 toPersistValue fromPersistValue

-- -----------------------------------------------------------------------------

genAda :: Gen Ada
genAda =
    word64ToAda <$> genWord64Ada
  where
    genWord64Ada :: Gen Word64
    genWord64Ada =
      Gen.choice
        [ Gen.word64 (Range.linear 0 maxLovelaceVal) -- Full range
        , Gen.word64 (Range.linear 0 5000)           -- Small values
        , Gen.word64 (Range.linear (maxLovelaceVal - 5000) maxLovelaceVal) -- Near max.
        ]

genDbWord64 :: Gen DbWord64
genDbWord64 = DbWord64 <$> genWord64

genDbInt65PresistValue :: Gen (DbInt65, PersistValue)
genDbInt65PresistValue = do
    (w64, pv) <- genWord64PresistValue
    Gen.element
      [ (PosInt65 w64, pv)
      , if w64 == 0
          then (PosInt65 0, pv)
          else (NegInt65 w64, negatePresistValue pv)
      ]
  where
    negatePresistValue :: PersistValue -> PersistValue
    negatePresistValue pv =
      case pv of
        PersistText txt -> PersistText ("-" <> txt)
        PersistInt64 i64 -> PersistInt64 (negate i64)
        PersistRational r -> PersistRational (negate r)
        _other -> pv


genDbLovelacePresistValue :: Gen (DbLovelace, PersistValue)
genDbLovelacePresistValue = first DbLovelace <$> genWord64PresistValue

genDbWord64PresistValue :: Gen (DbWord64, PersistValue)
genDbWord64PresistValue = first DbWord64 <$> genWord64PresistValue

genNatural :: Gen Natural
genNatural = fromIntegral <$> Gen.word (Range.linear 0 5000)

genWord64PresistValue :: Gen (Word64, PersistValue)
genWord64PresistValue =
  Gen.choice
    [ (\w64 -> (w64, PersistText (Text.pack $ show w64))) <$> genWord64
    , (\i64 -> (fromIntegral i64, PersistInt64 i64)) . fromIntegral <$> Gen.int64 (Range.linear 0 (maxBound :: Int64))
    , (\w64 -> (w64, PersistRational (fromIntegral w64 % 1))) <$> genWord64
    ]

genWord128 :: Gen Word128
genWord128 = Word128 <$> genWord64 <*> genWord64

genWord64 :: Gen Word64
genWord64 =
  Gen.choice
    [ Gen.word64 Range.constantBounded
    , Gen.word64 (Range.linear 0 5000)           -- Small values
    , Gen.word64 (Range.linear (maxBound - 5000) maxBound) -- Near max.
    ]

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests = H.checkParallel $$discover
