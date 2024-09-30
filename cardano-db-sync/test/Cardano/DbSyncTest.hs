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
      [
        ( "extractSyncOptions passes prune consume migration"
        , prop_extractSyncOptionsPruneConsumeMigration
        )
      , ("extractSyncOptions passes tx out disable", prop_extractSyncOptionsDisable)
      ,
        ( "extractSyncOptions passes offchain pool data"
        , prop_extractSyncOptionsOffchainPoolData
        )
      , ("extractSyncOptions passes governance", prop_extractSyncOptionsGov)
      ]

prop_extractSyncOptionsPruneConsumeMigration :: Property
prop_extractSyncOptionsPruneConsumeMigration = property $ do
  loggingCfg <- liftIO Logging.empty
  syncNodeParams <- forAll Gen.syncNodeParams
  abortOnPanic <- forAll Gen.bool
  syncNodeConfig <-
    forAllWith
      (const "SyncNodeConfig") -- SyncNodeConfig does not have Show
      $ Gen.syncNodeConfig loggingCfg

  coverTxOut syncNodeConfig

  let syncOptions = extractSyncOptions syncNodeParams abortOnPanic syncNodeConfig
      expectedPruneConsume =
        case sioTxOut (dncInsertOptions syncNodeConfig) of
          TxOutEnable _ -> initPruneConsumeMigration False False False False
          TxOutDisable -> initPruneConsumeMigration False False False False
          TxOutConsumedBootstrap (ForceTxIn f) _ -> initPruneConsumeMigration False False True f
          TxOutConsumedPrune (ForceTxIn f) _ -> initPruneConsumeMigration False True False f
          TxOutConsumed (ForceTxIn f) _ -> initPruneConsumeMigration True False False f

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

prop_extractSyncOptionsDisable :: Property
prop_extractSyncOptionsDisable = property $ do
  loggingCfg <- liftIO Logging.empty
  syncNodeParams <- forAll Gen.syncNodeParams
  abortOnPanic <- forAll Gen.bool
  syncNodeConfig <-
    forAllWith
      (const "SyncNodeConfig") -- SyncNodeConfig does not have Show
      $ Gen.syncNodeConfig loggingCfg

  coverTxOut syncNodeConfig

  let isTxOutDisabled' = isTxOutEnabled . sioTxOut . dncInsertOptions $ syncNodeConfig
      syncOptions = extractSyncOptions syncNodeParams abortOnPanic syncNodeConfig

  ioInOut (soptInsertOptions syncOptions) === isTxOutDisabled'

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

coverTxOut :: MonadTest m => SyncNodeConfig -> m ()
coverTxOut syncNodeConfig = do
  let isTxOutEnabled' = isTxOutEnabled . sioTxOut . dncInsertOptions $ syncNodeConfig
      isTxOutDisabled' = isTxOutEnabled . sioTxOut . dncInsertOptions $ syncNodeConfig
      isTxOutConsumedBootstrap' = isTxOutConsumedBootstrap . sioTxOut . dncInsertOptions $ syncNodeConfig
      isTxOutConsumedPrune' = isTxOutConsumedPrune . sioTxOut . dncInsertOptions $ syncNodeConfig
      isTxOutConsumed' = isTxOutConsumed . sioTxOut . dncInsertOptions $ syncNodeConfig

  cover 5 "tx out enabled" isTxOutEnabled'
  cover 5 "tx out disabled" isTxOutDisabled'
  cover 5 "tx out bootstrap" isTxOutConsumedBootstrap'
  cover 5 "tx out prune" isTxOutConsumedPrune'
  cover 5 "tx out consumed" isTxOutConsumed'
