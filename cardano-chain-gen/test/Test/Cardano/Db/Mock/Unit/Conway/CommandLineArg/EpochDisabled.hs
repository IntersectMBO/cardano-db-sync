{-# LANGUAGE NumericUnderscores #-}

module Test.Cardano.Db.Mock.Unit.Conway.CommandLineArg.EpochDisabled (
  checkEpochDisabledArg,
  checkEpochEnabled,
) where

import Cardano.Db as DB
import Cardano.Mock.ChainSync.Server (IOManager ())
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import Cardano.Mock.Forging.Types (UTxOIndex (..))
import Cardano.Prelude
import Test.Cardano.Db.Mock.Config
import Test.Cardano.Db.Mock.UnifiedApi (forgeAndSubmitBlocks, withConwayFindLeaderAndSubmitTx)
import Test.Cardano.Db.Mock.Validate (assertBlockNoBackoff, assertEqQuery)
import Test.Tasty.HUnit (Assertion ())
import Prelude ()

checkEpochDisabledArg :: IOManager -> [(Text, Text)] -> Assertion
checkEpochDisabledArg =
  withCustomConfigAndDropDB cliArgs Nothing conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge some blocks
    void $ forgeAndSubmitBlocks interpreter mockServer 50
    -- Add two blocks with transactions
    void $
      withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10_000 10_000 0
    void $
      withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx (UTxOIndex 1) (UTxOIndex 0) 10_000 10_000 0
    -- Add some more empty blocks
    void $ forgeAndSubmitBlocks interpreter mockServer 60

    -- Verify epoch didn't update
    assertBlockNoBackoff dbSync 112
    assertEqQuery dbSync DB.queryEpochCount 0 "epoch updated"
  where
    cliArgs = mkCommandLineArgs True
    testLabel = "conwayCLACheckEpochDisabledArg"

checkEpochEnabled :: IOManager -> [(Text, Text)] -> Assertion
checkEpochEnabled =
  withCustomConfig cliArgs Nothing conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge some blocks
    void $ forgeAndSubmitBlocks interpreter mockServer 50
    -- Add two blocks with transactions
    void $
      withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10_000 10_000 0
    void $
      withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx (UTxOIndex 1) (UTxOIndex 0) 10_000 10_000 0
    -- Add some more empty blocks
    void $ forgeAndSubmitBlocks interpreter mockServer 60

    -- Verify epoch updated
    assertBlockNoBackoff dbSync 112
    assertEqQuery dbSync DB.queryEpochCount 1 "epoch not updated"
  where
    cliArgs = mkCommandLineArgs False
    testLabel = "conwayCLACheckEpochDisabledArg"

mkCommandLineArgs :: Bool -> CommandLineArgs
mkCommandLineArgs epochDisabled = initCommandLineArgs {claEpochDisabled = epochDisabled}
