{-# LANGUAGE TemplateHaskell #-}

module Test.Property.Cardano.Db.Migration
  ( genMigrationVersion
  , tests
  ) where

import           Cardano.Db

import           Hedgehog (Gen, Property, (===), discover)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


prop_roundtrip_MigrationVersion :: Property
prop_roundtrip_MigrationVersion =
  H.property $ do
    mv <- H.forAll genMigrationVersion
    H.tripping mv renderMigrationVersionFile parseMigrationVersionFromFile

prop_roundtrip_renderMigrationVersion_no_spaces :: Property
prop_roundtrip_renderMigrationVersion_no_spaces =
  H.property $ do
    mv <- H.forAll genMigrationVersion
    any (== ' ') (renderMigrationVersionFile mv) === False

-- -----------------------------------------------------------------------------

genMigrationVersion :: Gen MigrationVersion
genMigrationVersion =
  MigrationVersion
    <$> Gen.int (Range.linear 0 10)
    <*> Gen.int (Range.linear 0 10000)
    <*> genDate

genDate :: Gen Int
genDate = do
  year <- Gen.int (Range.linear 2000 2100)
  month <- Gen.int (Range.linear 1 12)
  day <- Gen.int (Range.linear 1 12)
  pure $ year * 10000 + month * 100 + day

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests = H.checkParallel $$discover
