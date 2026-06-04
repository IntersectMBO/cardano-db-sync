{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Test.Cardano.Db.Mock.Unit.Alonzo.Tx (
  addSimpleTx,
  consumeSameBlock,
) where

import qualified Cardano.Db as DB
import Cardano.Mock.ChainSync.Server (IOManager)
import qualified Cardano.Mock.Forging.Tx.Alonzo as Alonzo
import Cardano.Mock.Forging.Types (UTxOIndex (..))
import Control.Monad (void)
import Data.Text (Text)
import Test.Cardano.Db.Mock.Config (
  alonzoConfigDir,
  startDBSync,
  withFullConfig,
  withFullConfigDropDB,
 )
import Test.Cardano.Db.Mock.UnifiedApi (
  withAlonzoFindLeaderAndSubmit,
  withAlonzoFindLeaderAndSubmitTx,
 )
import Test.Cardano.Db.Mock.Validate (assertBlockNoBackoff)
import Test.Tasty.HUnit (Assertion)

addSimpleTx :: DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion
addSimpleTx source =
  withFullConfigDropDB source alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    -- translate the block to a real Cardano block.
    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 500

    startDBSync dbSync
    assertBlockNoBackoff dbSync 1
  where
    testLabel = "addSimpleTx-alonzo"

consumeSameBlock :: DB.PGPassSource -> IOManager -> [(Text, Text)] -> Assertion
consumeSameBlock source =
  withFullConfig source alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 20000 20000 st
      let utxo0 = head (Alonzo.mkUTxOAlonzo tx0)
      tx1 <- Alonzo.mkPaymentTx (UTxOPair utxo0) (UTxOIndex 2) 10000 500 st
      pure [tx0, tx1]
    assertBlockNoBackoff dbSync 1
  where
    testLabel = "consumeSameBlock-alonzo"
