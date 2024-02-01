{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSyncTest (tests) where

import qualified Cardano.BM.Configuration.Model as Logging
import Cardano.Chain.Update (ProtocolVersion (..))
import Cardano.Crypto (RequiresNetworkMagic (..))
import Cardano.Crypto.Hash (Blake2b_256, Hash ())
import Cardano.Crypto.Hash.Class (HashAlgorithm (..), hashFromBytes)
import Cardano.Db (PGPassSource (..))
import Cardano.DbSync
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncOptions (..))
import Cardano.DbSync.Config.Types
import Cardano.DbSync.Config.TypesTest (genSyncInsertConfig)
import Cardano.Ledger.Slot (EpochNo (..))
import Cardano.Prelude
import Data.Maybe (fromJust)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Ouroboros.Consensus.Cardano.CanHardFork (TriggerHardFork (..))

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
  syncNodeParams <- forAll genSyncNodeParams
  abortOnPanic <- forAll Gen.bool
  syncNodeConfig <-
    forAllWith
      (const "SyncNodeConfig") -- SyncNodeConfig does not have Show
      $ Gen.syncNodeConfig loggingCfg

  let syncOptions = extractSyncOptions syncNodeParams abortOnPanic syncNodeConfig

  unless
    ( enpOnlyGov syncNodeParams
        || enpOnlyUTxO syncNodeParams
        || enpDisableAllMode syncNodeParams
        || enpFullMode syncNodeParams
    )
    ( ioOffChainPoolData (soptInsertOptions syncOptions)
        === isOffchainPoolDataEnabled (spcOffchainPoolData (dncInsertConfig syncNodeConfig))
    )

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

  unless
    ( enpOnlyGov syncNodeParams
        || enpOnlyUTxO syncNodeParams
        || enpDisableAllMode syncNodeParams
        || enpFullMode syncNodeParams
    )
    ( ioGov (soptInsertOptions syncOptions)
        === isGovernanceEnabled (spcGovernance (dncInsertConfig syncNodeConfig))
    )
