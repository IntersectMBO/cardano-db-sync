{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Db.Mock.Unit.Conway.Config.TxOutConsumed (
  consumeTx,
  consumeTxConsumed,
) where

import qualified Cardano.Db as Db
import Cardano.Mock.ChainSync.Server (IOManager ())
import Cardano.Mock.Forging.Interpreter (withConwayLedgerState)
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import Cardano.Mock.Forging.Types (ForgingError (..), UTxOIndex (..))
import qualified Cardano.Mock.Query as Query
import Cardano.Prelude
import Test.Cardano.Db.Mock.Config
import qualified Test.Cardano.Db.Mock.UnifiedApi as Api
import Test.Cardano.Db.Mock.Validate
import Test.Tasty.HUnit (Assertion ())
import Prelude ()

consumeTx :: IOManager -> [(Text, Text)] -> Assertion
consumeTx =
  withFullConfigAndDropDB conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a payment transaction
    tx0 <-
      withConwayLedgerState interpreter $
        Conway.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 20_000 20_000 0

    -- Forge a block with a transaction
    void $ Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $ \_ -> Right tx0
    void $ Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $ \state' -> do
      utxo' <- maybeToEither CantFindUTxO (head $ Conway.mkUTxOConway tx0)
      Conway.mkPaymentTx (UTxOPair utxo') (UTxOIndex 2) 10_000 500 0 state'

    -- Verify the new transaction count
    assertBlockNoBackoff dbSync 2

    -- Should not have consumed_by_tx
    assertEqBackoff
      dbSync
      (Query.queryConsumedTxOutCount @'Db.TxOutCore)
      0
      []
      "Unexpected consumed_by_tx count"
  where
    testLabel = "conwayConsumeTx"

consumeTxConsumed :: IOManager -> [(Text, Text)] -> Assertion
consumeTxConsumed =
  withCustomConfigAndDropDB cmdLineArgs cfg conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a payment transaction
    tx0 <-
      withConwayLedgerState interpreter $
        Conway.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 20_000 20_000 0

    -- Forge a block with a transaction
    void $ Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $ \_ -> Right tx0
    void $ Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $ \state' -> do
      utxo' <- maybeToEither CantFindUTxO (head $ Conway.mkUTxOConway tx0)
      Conway.mkPaymentTx (UTxOPair utxo') (UTxOIndex 2) 10_000 500 0 state'

    -- Wait for it to sync
    assertBlockNoBackoff dbSync 2
    assertTxCount dbSync 13

    -- Should have consumed_by_tx
    assertEqBackoff
      dbSync
      (Query.queryConsumedTxOutCount @'Db.TxOutCore)
      2
      []
      "Unexpected consumed_by_tx count"
  where
    cmdLineArgs = initCommandLineArgs
    cfg = Just (configConsume False)
    testLabel = "conwayConsumeTxConsumed"
