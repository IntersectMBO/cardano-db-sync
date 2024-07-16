{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.ApiTest (tests) where

import Cardano.DbSync.Api
import Cardano.DbSync.Config.Types
import qualified Cardano.DbSync.Gen as Gen
import Cardano.Prelude
import Control.Monad (MonadFail (..))
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
    SyncInsertConfig (Just "full") _ ->
      extractInsertOptions cfg === fullInsertOptions
    SyncInsertConfig (Just "only_utxo") _ ->
      extractInsertOptions cfg === onlyUTxOInsertOptions
    SyncInsertConfig (Just "only_gov") _ ->
      extractInsertOptions cfg === onlyGovInsertOptions
    SyncInsertConfig (Just "disable_all") _ ->
      extractInsertOptions cfg === disableAllInsertOptions
    SyncInsertConfig Nothing opts ->
      extractInsertOptions cfg === opts
    _other -> fail "Unexpected SyncInsertConfig" -- This case should not happen if all presets are covered

prop_extractInsertOptionsRewards :: Property
prop_extractInsertOptionsRewards = property $ do
  cfg <- forAll Gen.syncPreConfig

  let insertCfg = pcInsertConfig cfg
  coverInsertCfg insertCfg

  let areRewardsEnabled' = areRewardsEnabled $ sioRewards (extractInsertOptions cfg)

  case insertCfg of
    SyncInsertConfig (Just "only_gov") _ ->
      assert $ not areRewardsEnabled'
    SyncInsertConfig (Just "disable_all") _ ->
      assert $ not areRewardsEnabled'
    _other -> assert areRewardsEnabled'

coverInsertCfg :: SyncInsertConfig -> PropertyT IO ()
coverInsertCfg cfg = do
  cover 5 "full" $ isPreset "full" cfg
  cover 5 "only utxo" $ isPreset "only_utxo" cfg
  cover 5 "only gov" $ isPreset "only_gov" cfg
  cover 5 "disable all" $ isPreset "disable_all" cfg
  cover 5 "custom config" $ isCustomConfig cfg
  where
    isPreset :: Text -> SyncInsertConfig -> Bool
    isPreset preset (SyncInsertConfig (Just p) _) = p == preset
    isPreset _ _ = False

    isCustomConfig :: SyncInsertConfig -> Bool
    isCustomConfig (SyncInsertConfig Nothing _) = True
    isCustomConfig _ = False
