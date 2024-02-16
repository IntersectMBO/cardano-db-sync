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
      [("extractInsertOptions", prop_extractInsertOptions)]

prop_extractInsertOptions :: Property
prop_extractInsertOptions = property $ do
  cfg <- forAll Gen.syncPreConfig

  let insertOpts = pcInsertConfig cfg

      isSyncInsertConfig =
        case insertOpts of
          SyncInsertConfig _ -> True
          _ -> False

  cover 5 "full" (insertOpts == FullInsertOptions)
  cover 5 "only utxo" (insertOpts == OnlyUTxOInsertOptions)
  cover 5 "only gov" (insertOpts == OnlyGovInsertOptions)
  cover 5 "disable all" (insertOpts == DisableAllInsertOptions)
  cover 5 "config" isSyncInsertConfig

  case insertOpts of
    FullInsertOptions ->
      extractInsertOptions cfg === fullInsertOptions
    OnlyUTxOInsertOptions ->
      extractInsertOptions cfg === onlyUTxOInsertOptions
    OnlyGovInsertOptions ->
      extractInsertOptions cfg === onlyGovInsertOptions
    DisableAllInsertOptions ->
      extractInsertOptions cfg === disableAllInsertOptions
    SyncInsertConfig cfg' ->
      extractInsertOptions cfg === cfg'
