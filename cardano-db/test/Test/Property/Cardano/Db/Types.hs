{-# LANGUAGE TemplateHaskell #-}

module Test.Property.Cardano.Db.Types
  ( genAda
  , tests
  ) where

import           Cardano.Chain.Common (maxLovelaceVal)

import qualified Data.Aeson as Aeson
import           Data.Word (Word64)

import           Cardano.Db

import           Hedgehog (Gen, Property, discover)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


prop_roundtrip_Ada_via_JSON :: Property
prop_roundtrip_Ada_via_JSON =
  H.withTests 5000 . H.property $ do
    mv <- H.forAll genAda
    H.tripping mv Aeson.encode Aeson.eitherDecode

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

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests = H.checkParallel $$discover
