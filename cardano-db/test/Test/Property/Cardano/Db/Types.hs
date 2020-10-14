{-# LANGUAGE TemplateHaskell #-}

module Test.Property.Cardano.Db.Types
  ( genAda
  , tests
  ) where

import           Cardano.Chain.Common (maxLovelaceVal)

import qualified Data.Aeson as Aeson
import           Data.Int (Int64)
import           Data.Ratio ((%))
import qualified Data.Text as Text
import           Data.WideWord.Word128 (Word128 (..))
import           Data.Word (Word64)

import           Database.Persist.Class (PersistField (..))
import           Database.Persist.Types (PersistValue (..))

import           Cardano.Db

import           Numeric.Natural (Natural)

import           Hedgehog (Gen, Property, discover)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Shelley.Spec.Ledger.PParams (ProtVer (..))


prop_roundtrip_Ada_via_JSON :: Property
prop_roundtrip_Ada_via_JSON =
  H.withTests 5000 . H.property $ do
    mv <- H.forAll genAda
    H.tripping mv Aeson.encode Aeson.eitherDecode

prop_roundtrip_DbWord64_PersistField :: Property
prop_roundtrip_DbWord64_PersistField =
    H.withTests 5000 . H.property $ do
      w64 <- H.forAll genDbWord64
      H.tripping w64 specialPersistDbWord64 fromPersistValue
  where
    specialPersistDbWord64 :: DbWord64 -> PersistValue
    specialPersistDbWord64 (DbWord64 w64) =
      if w64 > fromIntegral (maxBound :: Int64)
        then PersistRational (fromIntegral w64 % 1)
        else PersistText (Text.pack $ show w64)


prop_roundtrip_ProtVer_PersistField :: Property
prop_roundtrip_ProtVer_PersistField =
  H.withTests 5000 . H.property $ do
    pv <- H.forAll genProtVer
    H.tripping pv toPersistValue fromPersistValue

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

genNatural :: Gen Natural
genNatural = fromIntegral <$> Gen.word (Range.linear 0 5000)

genProtVer :: Gen ProtVer
genProtVer = ProtVer <$> genNatural <*> genNatural

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
