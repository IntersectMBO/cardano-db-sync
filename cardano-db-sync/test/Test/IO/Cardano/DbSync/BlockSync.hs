{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
module Test.IO.Cardano.DbSync.BlockSync
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
    , testProperty "Latest epoch following the chain, block diff large" prop_notSyncedUpEpoch
    , testProperty "No cached epoch, block non-zero" prop_noCachedEpoch
    , testProperty "Diff from cached (DB) epoch more than 2" prop_diffFromCachedEpochMoreThanTwo
    , testProperty "Synced chain, DB out of sync" prop_syncedChainDBOutOfDate
    --, testProperty "Current epoch greater than tip" prop_currentEpochGreaterThanTip
    ]

prop_syncedUpEpoch :: Property
prop_syncedUpEpoch =
  withTests 100_000 $ property $ do
    epochNum <- forAll $ Gen.word64 (Range.linear 0 100_000)
    let chainTipEpochNum = epochNum -- The same value!
    blockDiff <- forAll $ Gen.word64 (Range.linear 0 14)
    let mLatestCachedEpoch = Just epochNum -- The same value!

    updateEpochNumLogic epochNum mLatestCachedEpoch chainTipEpochNum blockDiff === UpdateEpoch epochNum

prop_notSyncedUpEpoch :: Property
prop_notSyncedUpEpoch =
  withTests 100_000 $ property $ do
    epochNum <- forAll $ Gen.word64 (Range.linear 0 100_000)
    let chainTipEpochNum = epochNum -- The same value!
    blockDiff <- forAll $ Gen.word64 (Range.linear 15 22000)
    let mLatestCachedEpoch = Just epochNum -- The same value!

    updateEpochNumLogic epochNum mLatestCachedEpoch chainTipEpochNum blockDiff === DoNotUpdate


prop_noCachedEpoch :: Property
prop_noCachedEpoch =
  withTests 100_000 $ property $ do
    epochNum <- forAll $ Gen.word64 (Range.linear 1 100_000)
    chainTipEpochNum <- forAll $ Gen.word64 (Range.linear 1 100_000)
    blockDiff <- forAll $ Gen.word64 (Range.linear 0 22000)
    let mLatestCachedEpoch = Nothing

    -- Update the first epoch since we have no cache from the DB.
    updateEpochNumLogic epochNum mLatestCachedEpoch chainTipEpochNum blockDiff === UpdateEpoch 0


prop_diffFromCachedEpochMoreThanTwo :: Property
prop_diffFromCachedEpochMoreThanTwo =
  withTests 100_000 $ property $ do
    chainTipEpochNum <- forAll $ Gen.word64 (Range.linear 0 100_000)
    epochNum <- forAll $ Gen.word64 (Range.linear (chainTipEpochNum + 2) 100_002)
    blockDiff <- forAll $ Gen.word64 (Range.linear 0 22000)
    let mLatestCachedEpoch = Just chainTipEpochNum -- epochNum >= lastCachedEpoch + 2

    assert $ epochNum >= chainTipEpochNum + 2

    -- Update the next epoch since we have no cache from the DB.
    updateEpochNumLogic epochNum mLatestCachedEpoch chainTipEpochNum blockDiff === UpdateEpoch (chainTipEpochNum + 1)


prop_syncedChainDBOutOfDate :: Property
prop_syncedChainDBOutOfDate =
  withTests 100_000 $ property $ do
    epochNum <- forAll $ Gen.word64 (Range.linear 1 100_000)
    let chainTipEpochNum = epochNum
    blockDiff <- forAll $ Gen.word64 (Range.linear 0 22000)

    latestCachedEpoch <- forAll $ Gen.word64 (Range.linear 0 (epochNum - 1))
    let mLatestCachedEpoch = Just latestCachedEpoch

    -- Update next epoch since we have no cache from the DB.
    updateEpochNumLogic epochNum mLatestCachedEpoch chainTipEpochNum blockDiff === UpdateEpoch (latestCachedEpoch + 1)

--prop_currentEpochGreaterThanTip :: Property
--prop_currentEpochGreaterThanTip =
--  withTests 100_000 $ property $ do
--    epochNum <- forAll $ Gen.word64 (Range.linear 1 100_000)
--    chainTipEpochNum <- forAll $ Gen.filter (\epochNum' -> (epochNum' - epochNum) < 2) $ Gen.word64 (Range.linear 0 epochNum)
--    blockDiff <- forAll $ Gen.word64 (Range.linear 0 22000)
--
--    --mLatestCachedEpoch <- forAll $ Gen.maybe $ Gen.word64 (Range.linear 0 epochNum) -- This hits the second condition
--    latestCachedEpoch <- forAll $ Gen.word64 (Range.linear 0 epochNum)
--    let mLatestCachedEpoch = Just latestCachedEpoch
--
--    -- Update next epoch since we have no cache from the DB.
--    updateEpochNumLogic epochNum mLatestCachedEpoch chainTipEpochNum blockDiff === UpdateEpoch epochNum


