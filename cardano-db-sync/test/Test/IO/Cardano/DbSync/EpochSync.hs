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
    , testProperty "DB synced up, epoch = tip epoch, follow chain closely" prop_followChainClosely
    , testProperty "Synced chain, DB out of sync" prop_syncedChainDBOutOfDate
    ]

testCaseNumber :: TestLimit
testCaseNumber = 10_000

prop_syncedUpEpoch :: Property
prop_syncedUpEpoch =
  withTests testCaseNumber $ property $ do
    epochNum <- forAll $ Gen.word64 (Range.linear 0 100_000)

    -- The assumption is that the tip is larger then the current epoch.
    tipEpochNum <- forAll . Gen.filter (> epochNum) $ Gen.word64 (Range.linear 0 100_000)
    let mLatestCachedEpoch = Just epochNum -- The same value!

    updateEpochNumLogic epochNum mLatestCachedEpoch tipEpochNum === DoNotUpdate

prop_noCachedEpoch :: Property
prop_noCachedEpoch =
  withTests testCaseNumber $ property $ do
    epochNum <- forAll $ Gen.word64 (Range.linear 1 100_000)

    -- The assumption is that the tip is larger then the current epoch.
    tipEpochNum <- forAll . Gen.filter (> epochNum) $ Gen.word64 (Range.linear 1 100_000)
    let mLatestCachedEpoch = Nothing

    -- Update the first epoch since we have no cache from the DB.
    updateEpochNumLogic epochNum mLatestCachedEpoch tipEpochNum === UpdateEpoch 0

prop_diffFromCachedEpochMoreThanTwo :: Property
prop_diffFromCachedEpochMoreThanTwo =
  withTests testCaseNumber $ property $ do
    cachedEpoch <- forAll $ Gen.word64 (Range.linear 0 100_000)
    epochNum <- forAll $ Gen.word64 (Range.linear (cachedEpoch + 2) 100_002)

    -- The assumption is that the tip is larger then the current epoch.
    tipEpochNum <- forAll . Gen.filter (> epochNum) $ Gen.word64 (Range.linear (cachedEpoch + 2) 100_000)

    let mLatestCachedEpoch = Just cachedEpoch

    assert $ epochNum >= cachedEpoch + 2

    -- Update the next epoch since we have no cache from the DB.
    updateEpochNumLogic epochNum mLatestCachedEpoch tipEpochNum === UpdateEpochs cachedEpoch (cachedEpoch + 1)

prop_followChainClosely :: Property
prop_followChainClosely =
  withTests 100_000 $ property $ do
    cachedEpoch <- forAll $ Gen.word64 (Range.linear 0 100_000)

    -- The assumption is that the tip is equal to the current epoch.
    let epochNum = cachedEpoch
    let tipEpochNum = epochNum

    let mLatestCachedEpoch = Just cachedEpoch

    -- Update the next epoch since we have no cache from the DB.
    updateEpochNumLogic epochNum mLatestCachedEpoch tipEpochNum === UpdateEpoch epochNum

prop_syncedChainDBOutOfDate :: Property
prop_syncedChainDBOutOfDate =
  withTests testCaseNumber $ property $ do
    cachedEpoch <- forAll $ Gen.word64 (Range.linear 1 100_000)
    epochNum <- forAll $ Gen.word64 (Range.linear 0 (cachedEpoch - 1))

    -- The assumption is that the tip is larger then the current epoch.
    tipEpochNum <- forAll . Gen.filter (> epochNum) $ Gen.word64 (Range.linear 1 100_000)

    let mLatestCachedEpoch = Just cachedEpoch

    -- Do not update, the
    updateEpochNumLogic epochNum mLatestCachedEpoch tipEpochNum === DoNotUpdate

