{-# LANGUAGE NumericUnderscores #-}

module Test.Cardano.Db.Mock.Unit.Conway.Tx (
  addSimpleTx,
  addSimpleTxShelley,
  consumeSameBlock,
) where

import Cardano.Mock.ChainSync.Server (IOManager ())
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import qualified Cardano.Mock.Forging.Tx.Shelley as Shelley
import Cardano.Mock.Forging.Types (UTxOIndex (..))
import Cardano.Prelude hiding (head)
import Test.Cardano.Db.Mock.Config
import qualified Test.Cardano.Db.Mock.UnifiedApi as UnifiedApi
import Test.Cardano.Db.Mock.Validate (assertBlockNoBackoff, assertTxCount)
import Test.Tasty.HUnit (Assertion ())
import Prelude (head)

addSimpleTx :: IOManager -> [(Text, Text)] -> Assertion
addSimpleTx =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    -- Forge a block
    void $
      UnifiedApi.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10_000 500

    startDBSync dbSync
    -- Verify it syncs
    assertBlockNoBackoff dbSync 1
    assertTxCount dbSync 12
  where
    testLabel = "conwayAddSimpleTx"

addSimpleTxShelley :: IOManager -> [(Text, Text)] -> Assertion
addSimpleTxShelley =
  withFullConfig shelleyConfigDir testLabel $ \interpreter mockServer dbSync -> do
    -- Forge a shelley block
    void $
      UnifiedApi.withShelleyFindLeaderAndSubmitTx interpreter mockServer $
        Shelley.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10_000 500

    startDBSync dbSync
    -- Verify it syncs
    assertBlockNoBackoff dbSync 1
    assertTxCount dbSync 12
  where
    testLabel = "conwayAddSimpleTxShelley"

consumeSameBlock :: IOManager -> [(Text, Text)] -> Assertion
consumeSameBlock =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge some transactions
    void $ UnifiedApi.withConwayFindLeaderAndSubmit interpreter mockServer $ \state' -> do
      tx0 <- Conway.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 20_000 20_000 state'
      let utxo0 = head (Conway.mkUTxOConway tx0)
      -- Create a transaction with UTxOs from tx0
      tx1 <- Conway.mkPaymentTx (UTxOPair utxo0) (UTxOIndex 2) 10_000 500 state'
      pure [tx0, tx1]

    -- Verify the new transaction count
    assertBlockNoBackoff dbSync 1
    assertTxCount dbSync 13
  where
    testLabel = "conwayConsumeSameBlock"
