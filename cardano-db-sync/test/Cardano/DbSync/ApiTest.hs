{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.ApiTest (tests) where

import Cardano.DbSync.Api
import Cardano.DbSync.Config.Types
import qualified Cardano.DbSync.Gen as Gen
import Cardano.Prelude
import Hedgehog

tests :: IO Bool
tests =
  checkParallel $
    Group
      "Cardano.DbSync.Api"
      [ ("extractInsertOptions", prop_extractInsertOptions)
      , ("extractInsertOptions rewards", prop_extractInsertOptionsRewards)
      ]

prop_extractInsertOptions :: Property
prop_extractInsertOptions = property $ do
  cfg <- forAll Gen.syncPreConfig

  let insertCfg = pcInsertConfig cfg
  coverInsertCfg insertCfg

  case insertCfg of
    SyncInsertConfig (Just FullInsertPreset) _ ->
      extractInsertOptions cfg === fullInsertOptions
    SyncInsertConfig (Just OnlyUTxOInsertPreset) _ ->
      extractInsertOptions cfg === onlyUTxOInsertOptions
    SyncInsertConfig (Just OnlyGovInsertPreset) _ ->
      extractInsertOptions cfg === onlyGovInsertOptions
    SyncInsertConfig (Just DisableAllInsertPreset) _ ->
      extractInsertOptions cfg === disableAllInsertOptions
    SyncInsertConfig Nothing opts ->
      extractInsertOptions cfg === opts

prop_extractInsertOptionsRewards :: Property
prop_extractInsertOptionsRewards = property $ do
  cfg <- forAll Gen.syncPreConfig

  let insertCfg = pcInsertConfig cfg
  coverInsertCfg insertCfg

  let areRewardsEnabled' = areRewardsEnabled $ sioRewards (extractInsertOptions cfg)

  case insertCfg of
    SyncInsertConfig (Just OnlyGovInsertPreset) _ ->
      assert $ not areRewardsEnabled'
    SyncInsertConfig (Just DisableAllInsertPreset) _ ->
      assert $ not areRewardsEnabled'
    _other -> assert areRewardsEnabled'

coverInsertCfg :: MonadTest m => SyncInsertConfig -> m ()
coverInsertCfg insertOpts = do
  let preset = sicPreset insertOpts
  cover 5 "full" (preset == Just FullInsertPreset)
  cover 5 "only utxo" (preset == Just OnlyUTxOInsertPreset)
  cover 5 "only gov" (preset == Just OnlyGovInsertPreset)
  cover 5 "disable all" (preset == Just DisableAllInsertPreset)
  cover 5 "config" isSyncInsertConfig
  where
    isSyncInsertConfig =
      case insertOpts of
        (SyncInsertConfig Nothing _) -> True
        _other -> False
