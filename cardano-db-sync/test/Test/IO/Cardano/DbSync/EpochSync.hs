{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
module Test.IO.Cardano.DbSync.EpochSync
  ( tests
  ) where

import           Cardano.Prelude

import           Cardano.DbSync.Plugin.Epoch

import           Test.Tasty (TestTree, testGroup)

import           Test.Tasty.Hedgehog (testProperty)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

tests :: TestTree
tests =
  testGroup "Update epoch logic"
    [ testProperty "Latest epoch following the chain" prop_syncedUpEpoch
    , testProperty "No cached epoch, block non-zero" prop_noCachedEpoch
    , testProperty "Diff from cached (DB) epoch more than 2" prop_diffFromCachedEpochMoreThanTwo
    , testProperty "Synced chain, DB out of sync" prop_syncedChainDBOutOfDate
    ]

prop_syncedUpEpoch :: Property
prop_syncedUpEpoch =
  withTests 100_000 $ property $ do
    epochNum <- forAll $ Gen.word64 (Range.linear 0 100_000)
    let mLatestCachedEpoch = Just epochNum -- The same value!

    updateEpochNumLogic epochNum mLatestCachedEpoch === DoNotUpdate

prop_noCachedEpoch :: Property
prop_noCachedEpoch =
  withTests 100_000 $ property $ do
    epochNum <- forAll $ Gen.word64 (Range.linear 1 100_000)
    let mLatestCachedEpoch = Nothing

    -- Update the first epoch since we have no cache from the DB.
    updateEpochNumLogic epochNum mLatestCachedEpoch === UpdateEpoch 0

prop_diffFromCachedEpochMoreThanTwo :: Property
prop_diffFromCachedEpochMoreThanTwo =
  withTests 100_000 $ property $ do
    cachedEpoch <- forAll $ Gen.word64 (Range.linear 0 100_000)
    epochNum <- forAll $ Gen.word64 (Range.linear (cachedEpoch + 2) 100_002)

    let mLatestCachedEpoch = Just cachedEpoch

    assert $ epochNum >= cachedEpoch + 2

    -- Update the next epoch since we have no cache from the DB.
    updateEpochNumLogic epochNum mLatestCachedEpoch === UpdateEpochs cachedEpoch (cachedEpoch + 1)

prop_syncedChainDBOutOfDate :: Property
prop_syncedChainDBOutOfDate =
  withTests 100_000 $ property $ do
    cachedEpoch <- forAll $ Gen.word64 (Range.linear 1 100_000)
    epochNum <- forAll $ Gen.word64 (Range.linear 0 (cachedEpoch - 1))

    let mLatestCachedEpoch = Just cachedEpoch

    -- Do not update, the
    updateEpochNumLogic epochNum mLatestCachedEpoch === DoNotUpdate

