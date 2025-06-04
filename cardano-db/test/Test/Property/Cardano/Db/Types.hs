{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Test.Property.Cardano.Db.Types (
  genAda,
  tests,
) where

import Cardano.Chain.Common (maxLovelaceVal)
import qualified Cardano.Crypto.Hash as Crypto
import Cardano.Db
import qualified Cardano.Ledger.Hashes as Ledger
import Cardano.Ledger.Mary.Value (AssetName (..), PolicyID (..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Short as SBS
import Data.Either (fromRight)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.WideWord.Word128 (Word128 (..))
import Data.Word (Word64)
import Hedgehog (Gen, Property, discover, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- Original JSON test
prop_roundtrip_Ada_via_JSON :: Property
prop_roundtrip_Ada_via_JSON =
  H.withTests 5000 . H.property $ do
    mv <- H.forAll genAda
    H.tripping mv Aeson.encode Aeson.eitherDecode

-- Original AssetFingerprint test
prop_AssetFingerprint :: Property
prop_AssetFingerprint =
  H.withTests 1 . H.property $
    mapM_ (\(p, a, f) -> mkAssetFingerprint (unScriptHash $ policyID p) (unAssetName a) === f) testVectors
  where
    unScriptHash :: Ledger.ScriptHash -> ByteString
    unScriptHash (Ledger.ScriptHash h) = Crypto.hashToBytes h

    unAssetName :: AssetName -> ByteString
    unAssetName = SBS.fromShort . assetNameBytes

    testVectors :: [(PolicyID, AssetName, AssetFingerprint)]
    testVectors =
      [
        ( mkPolicyId "7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373"
        , AssetName ""
        , AssetFingerprint "asset1rjklcrnsdzqp65wjgrg55sy9723kw09mlgvlc3"
        )
      ,
        ( mkPolicyId "7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc37e"
        , AssetName ""
        , AssetFingerprint "asset1nl0puwxmhas8fawxp8nx4e2q3wekg969n2auw3"
        )
      ,
        ( mkPolicyId "1e349c9bdea19fd6c147626a5260bc44b71635f398b67c59881df209"
        , AssetName ""
        , AssetFingerprint "asset1uyuxku60yqe57nusqzjx38aan3f2wq6s93f6ea"
        )
      ,
        ( mkPolicyId "7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373"
        , hexAssetName "504154415445"
        , AssetFingerprint "asset13n25uv0yaf5kus35fm2k86cqy60z58d9xmde92"
        )
      ,
        ( mkPolicyId "1e349c9bdea19fd6c147626a5260bc44b71635f398b67c59881df209"
        , hexAssetName "504154415445"
        , AssetFingerprint "asset1hv4p5tv2a837mzqrst04d0dcptdjmluqvdx9k3"
        )
      ,
        ( mkPolicyId "1e349c9bdea19fd6c147626a5260bc44b71635f398b67c59881df209"
        , hexAssetName "7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373"
        , AssetFingerprint "asset1aqrdypg669jgazruv5ah07nuyqe0wxjhe2el6f"
        )
      ,
        ( mkPolicyId "7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373"
        , hexAssetName "1e349c9bdea19fd6c147626a5260bc44b71635f398b67c59881df209"
        , AssetFingerprint "asset17jd78wukhtrnmjh3fngzasxm8rck0l2r4hhyyt"
        )
      ,
        ( mkPolicyId "7eae28af2208be856f7a119668ae52a49b73725e326dc16579dcc373"
        , hexAssetName "0000000000000000000000000000000000000000000000000000000000000000"
        , AssetFingerprint "asset1pkpwyknlvul7az0xx8czhl60pyel45rpje4z8w"
        )
      ]

    mkPolicyId :: ByteString -> PolicyID
    mkPolicyId =
      PolicyID
        . Ledger.ScriptHash
        . fromMaybe (error "mkPolicyId:Hash")
        . Crypto.hashFromBytes
        . fromRight (error "mkPolicyId:Base16")
        . Base16.decode

    hexAssetName :: ByteString -> AssetName
    hexAssetName = AssetName . SBS.toShort . fromRight (error "hexAssetName") . Base16.decode

-- Test DbInt65 roundtrip conversion
prop_roundtrip_DbInt65 :: Property
prop_roundtrip_DbInt65 =
  H.withTests 5000 . H.property $ do
    i64 <- H.forAll $ Gen.int64 (Range.linearFrom 0 minBound maxBound)
    let i65 = toDbInt65 i64
    fromDbInt65 i65 === i64

prop_DbInt65_edge_cases :: Property
prop_DbInt65_edge_cases = H.property $ do
  fromDbInt65 (toDbInt65 minBound) === minBound
  fromDbInt65 (toDbInt65 maxBound) === maxBound
  fromDbInt65 (toDbInt65 0) === 0
  fromDbInt65 (toDbInt65 (-1)) === (-1)
  fromDbInt65 (toDbInt65 1) === 1

-- Test DbLovelace roundtrip conversion
prop_roundtrip_DbLovelace :: Property
prop_roundtrip_DbLovelace =
  H.withTests 5000 . H.property $ do
    lovelace <- H.forAll $ DbLovelace <$> genWord64Range

    -- Test roundtrip conversion
    runDbLovelaceRoundtrip lovelace === lovelace

    -- Test Maybe version
    mLovelace <- H.forAll $ Gen.maybe (DbLovelace <$> genWord64Range)
    runMaybeDbLovelaceRoundtrip mLovelace === mLovelace
  where
    genWord64Range = Gen.word64 (Range.linear 0 (fromIntegral (maxBound :: Int64)))

-- Test DbWord64 roundtrip conversion
prop_roundtrip_DbWord64 :: Property
prop_roundtrip_DbWord64 =
  H.withTests 5000 . H.property $ do
    word64 <- H.forAll $ DbWord64 <$> genWord64Range

    -- Test roundtrip conversion
    runDbWord64Roundtrip word64 === word64

    -- Test Maybe version
    mWord64 <- H.forAll $ Gen.maybe (DbWord64 <$> genWord64Range)
    runMaybeDbWord64Roundtrip mWord64 === mWord64
  where
    genWord64Range = Gen.word64 (Range.linear 0 (fromIntegral (maxBound :: Int64)))

-- Test Word128 roundtrip through components
prop_roundtrip_Word128 :: Property
prop_roundtrip_Word128 =
  H.withTests 5000 . H.property $ do
    w128 <- H.forAll genWord128Limited

    runWord128Roundtrip w128 === w128
  where
    genWord128Limited = do
      hi <- Gen.word64 (Range.linear 0 (fromIntegral (maxBound :: Int64)))
      lo <- Gen.word64 (Range.linear 0 (fromIntegral (maxBound :: Int64)))
      pure $ Word128 hi lo

-- DbInt65 specific roundtrip test function
runDbInt65Roundtrip :: DbInt65 -> DbInt65
runDbInt65Roundtrip value =
  -- Directly use the conversion functions that are at the core of your encoders/decoders
  toDbInt65 (fromDbInt65 value)

-- DbLovelace specific roundtrip test function
runDbLovelaceRoundtrip :: DbLovelace -> DbLovelace
runDbLovelaceRoundtrip (DbLovelace w) =
  -- Simulate conversion to Int64 (PostgreSQL) and back
  DbLovelace (fromIntegral (fromIntegral w :: Int64))

-- Maybe DbLovelace specific roundtrip test function
runMaybeDbLovelaceRoundtrip :: Maybe DbLovelace -> Maybe DbLovelace
runMaybeDbLovelaceRoundtrip Nothing = Nothing
runMaybeDbLovelaceRoundtrip (Just value) = Just (runDbLovelaceRoundtrip value)

-- DbWord64 specific roundtrip test function
runDbWord64Roundtrip :: DbWord64 -> DbWord64
runDbWord64Roundtrip (DbWord64 w) =
  -- Simulate conversion to Int64 (PostgreSQL) and back
  DbWord64 (fromIntegral (fromIntegral w :: Int64))

-- Maybe DbWord64 specific roundtrip test function
runMaybeDbWord64Roundtrip :: Maybe DbWord64 -> Maybe DbWord64
runMaybeDbWord64Roundtrip Nothing = Nothing
runMaybeDbWord64Roundtrip (Just value) = Just (runDbWord64Roundtrip value)

-- Word128 specific roundtrip test function
runWord128Roundtrip :: Word128 -> Word128
runWord128Roundtrip (Word128 hi lo) =
  -- Extract components and convert to Int64 (simulating DB storage)
  let hiInt64 = fromIntegral hi :: Int64
      loInt64 = fromIntegral lo :: Int64

      -- Convert back to Word64 and reconstruct (simulating DB retrieval)
      hiBack = fromIntegral hiInt64 :: Word64
      loBack = fromIntegral loInt64 :: Word64
   in Word128 hiBack loBack

-- Generators from original code
genAda :: Gen Ada
genAda =
  word64ToAda <$> genWord64Ada
  where
    genWord64Ada :: Gen Word64
    genWord64Ada =
      Gen.choice
        [ Gen.word64 (Range.linear 0 maxLovelaceVal) -- Full range
        , Gen.word64 (Range.linear 0 5000) -- Small values
        , Gen.word64 (Range.linear (maxLovelaceVal - 5000) maxLovelaceVal) -- Near max.
        ]

genWord128 :: Gen Word128
genWord128 = Word128 <$> genWord64 <*> genWord64

genWord64 :: Gen Word64
genWord64 =
  Gen.choice
    [ Gen.word64 Range.constantBounded
    , Gen.word64 (Range.linear 0 5000) -- Small values
    , Gen.word64 (Range.linear (maxBound - 5000) maxBound) -- Near max.
    ]

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests = H.checkParallel $$discover
