{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Test.Cardano.Db.Mock.Unit.Babbage.Tx (
  addSimpleTx,
  addSimpleTxShelley,
  consumeSameBlock,
) where

import Cardano.Mock.ChainSync.Server (IOManager)
import qualified Cardano.Mock.Forging.Tx.Babbage as Babbage
import qualified Cardano.Mock.Forging.Tx.Shelley as Shelley
import Cardano.Mock.Forging.Types (UTxOIndex (..))
import Control.Monad (void)
import Data.Text (Text)
import Test.Cardano.Db.Mock.Config (babbageConfigDir, startDBSync, withFullConfig)
import Test.Cardano.Db.Mock.UnifiedApi (
  withBabbageFindLeaderAndSubmit,
  withBabbageFindLeaderAndSubmitTx,
  withShelleyFindLeaderAndSubmitTx,
 )
import Test.Cardano.Db.Mock.Validate (assertBlockNoBackoff)
import Test.Tasty.HUnit (Assertion)

addSimpleTx :: IOManager -> [(Text, Text)] -> Assertion
addSimpleTx =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSyncEnv -> do
    -- translate the block to a real Cardano block.
    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 500

    startDBSync dbSyncEnv
    assertBlockNoBackoff dbSyncEnv 1
  where
    testLabel = "addSimpleTx"

addSimpleTxShelley :: IOManager -> [(Text, Text)] -> Assertion
addSimpleTxShelley =
  withFullConfig "config-shelley" testLabel $ \interpreter mockServer dbSyncEnv -> do
    -- translate the block to a real Cardano block.
    void $
      withShelleyFindLeaderAndSubmitTx interpreter mockServer $
        Shelley.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 500

    -- start db-sync and let it sync
    startDBSync dbSyncEnv
    assertBlockNoBackoff dbSyncEnv 1
  where
    testLabel = "addSimpleTxShelley"

consumeSameBlock :: IOManager -> [(Text, Text)] -> Assertion
consumeSameBlock =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSyncEnv -> do
    startDBSync dbSyncEnv

    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Babbage.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 20000 20000 st
      let utxo0 = head (Babbage.mkUTxOBabbage tx0)
      tx1 <- Babbage.mkPaymentTx (UTxOPair utxo0) (UTxOIndex 2) 10000 500 st
      pure [tx0, tx1]
    assertBlockNoBackoff dbSyncEnv 1
  where
    testLabel = "consumeSameBlock"
