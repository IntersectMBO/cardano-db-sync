{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSyncTest (tests) where

import qualified Cardano.BM.Configuration.Model as Logging
import Cardano.DbSync
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncOptions (..))
import Cardano.DbSync.Config.Types
import qualified Cardano.DbSync.Gen as Gen
import Cardano.Prelude
import Hedgehog
import qualified Hedgehog.Gen as Gen

tests :: IO Bool
tests =
  checkParallel $
    Group
      "Cardano.DbSync"
      [
        ( "extractSyncOptions passes offchain pool data"
        , prop_extractSyncOptionsOffchainPoolData
        )
      , ("extractSyncOptions passes governance", prop_extractSyncOptionsGov)
      ]

prop_extractSyncOptionsOffchainPoolData :: Property
prop_extractSyncOptionsOffchainPoolData = property $ do
  loggingCfg <- liftIO Logging.empty
  syncNodeParams <- forAll Gen.syncNodeParams
  abortOnPanic <- forAll Gen.bool
  syncNodeConfig <-
    forAllWith
      (const "SyncNodeConfig") -- SyncNodeConfig does not have Show
      $ Gen.syncNodeConfig loggingCfg

  let syncOptions = extractSyncOptions syncNodeParams abortOnPanic syncNodeConfig

  ioOffChainPoolData (soptInsertOptions syncOptions)
    === isOffchainPoolDataEnabled (spcOffchainPoolData (dncInsertConfig syncNodeConfig))

prop_extractSyncOptionsGov :: Property
prop_extractSyncOptionsGov = property $ do
  loggingCfg <- liftIO Logging.empty
  syncNodeParams <- forAll Gen.syncNodeParams
  abortOnPanic <- forAll Gen.bool
  syncNodeConfig <-
    forAllWith
      (const "SyncNodeConfig") -- SyncNodeConfig does not have Show
      $ Gen.syncNodeConfig loggingCfg

  let syncOptions = extractSyncOptions syncNodeParams abortOnPanic syncNodeConfig

  ioGov (soptInsertOptions syncOptions)
    === isGovernanceEnabled (spcGovernance (dncInsertConfig syncNodeConfig))
