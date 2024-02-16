{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSyncTest (tests) where

import qualified Cardano.BM.Configuration.Model as Logging
import Cardano.DbSync
import Cardano.DbSync.Api (initPruneConsumeMigration)
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
      [ ("extractSyncOptions passes tx out", prop_extractSyncOptionsTxOut)
      ,
        ( "extractSyncOptions passes offchain pool data"
        , prop_extractSyncOptionsOffchainPoolData
        )
      , ("extractSyncOptions passes governance", prop_extractSyncOptionsGov)
      ]

prop_extractSyncOptionsTxOut :: Property
prop_extractSyncOptionsTxOut = property $ do
  loggingCfg <- liftIO Logging.empty
  syncNodeParams <- forAll Gen.syncNodeParams
  abortOnPanic <- forAll Gen.bool
  syncNodeConfig <-
    forAllWith
      (const "SyncNodeConfig") -- SyncNodeConfig does not have Show
      $ Gen.syncNodeConfig loggingCfg

  let isTxOutEnabled' = isTxOutEnabled . sioTxOut . dncInsertOptions $ syncNodeConfig
      isTxOutDisabled' = isTxOutEnabled . sioTxOut . dncInsertOptions $ syncNodeConfig
      isTxOutBootstrap' = isTxOutBootstrap . sioTxOut . dncInsertOptions $ syncNodeConfig
      isTxOutPrune' = isTxOutPrune . sioTxOut . dncInsertOptions $ syncNodeConfig
      isTxOutConsumed' = isTxOutConsumed . sioTxOut . dncInsertOptions $ syncNodeConfig

  cover 5 "tx out enabled" isTxOutEnabled'
  cover 5 "tx out disabled" isTxOutDisabled'
  cover 5 "tx out bootstrap" isTxOutBootstrap'
  cover 5 "tx out prune" isTxOutPrune'
  cover 5 "tx out consumed" isTxOutConsumed'

  let syncOptions = extractSyncOptions syncNodeParams abortOnPanic syncNodeConfig
      expectedPruneConsume =
        case sioTxOut (dncInsertOptions syncNodeConfig) of
          TxOutEnable -> initPruneConsumeMigration False False False False
          TxOutDisable -> initPruneConsumeMigration False False False False
          TxOutBootstrap (ForceTxIn f) -> initPruneConsumeMigration False False True f
          TxOutPrune (ForceTxIn f) -> initPruneConsumeMigration False True False f
          TxOutConsumed (ForceTxIn f) -> initPruneConsumeMigration True False False f

  soptPruneConsumeMigration syncOptions === expectedPruneConsume

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
    === isOffchainPoolDataEnabled (sioOffchainPoolData (dncInsertOptions syncNodeConfig))

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
    === isGovernanceEnabled (sioGovernance (dncInsertOptions syncNodeConfig))
