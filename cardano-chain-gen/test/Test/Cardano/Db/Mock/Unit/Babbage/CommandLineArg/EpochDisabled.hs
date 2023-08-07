module Test.Cardano.Db.Mock.Unit.Babbage.CommandLineArg.EpochDisabled (
  checkEpochDisabledArg,
  checkEpochEnabled,
)
where

import qualified Cardano.Db as DB
import Cardano.Mock.ChainSync.Server (IOManager)
import qualified Cardano.Mock.Forging.Tx.Babbage as Babbage
import Cardano.Mock.Forging.Types (UTxOIndex (..))
import Control.Monad (void)
import Data.Text (Text)
import Test.Cardano.Db.Mock.Config (CommandLineArgs (..), babbageConfigDir, initCommandLineArgs, startDBSync, withCustomConfig)
import Test.Cardano.Db.Mock.UnifiedApi (forgeAndSubmitBlocks, withBabbageFindLeaderAndSubmitTx)
import Test.Cardano.Db.Mock.Validate (assertBlockNoBackoff, assertEqQuery)
import Test.Tasty.HUnit (Assertion)

mkCommandLineArgs :: Bool -> CommandLineArgs
mkCommandLineArgs epochDisabled = initCommandLineArgs {claEpochDisabled = epochDisabled}

-- this test fails as incorrect configuration file given
checkEpochDisabledArg :: IOManager -> [(Text, Text)] -> Assertion
checkEpochDisabledArg =
  withCustomConfig (mkCommandLineArgs True) babbageConfigDir testLabel $ \interpreter mockServer dbSyncEnv -> do
    startDBSync dbSyncEnv
    b1 <- forgeAndSubmitBlocks interpreter mockServer 50
    -- add 2 blocks with tx
    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ Babbage.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000
    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ Babbage.mkPaymentTx (UTxOIndex 1) (UTxOIndex 0) 10000 10000
    b2 <- forgeAndSubmitBlocks interpreter mockServer 60

    assertBlockNoBackoff dbSyncEnv (fromIntegral $ length (b1 <> b2) + 2)
    assertEqQuery dbSyncEnv DB.queryEpochCount 0 "new epoch didn't prune tx_out column that are null"
  where
    testLabel = "CLAcheckEpochDisabledArg "

checkEpochEnabled :: IOManager -> [(Text, Text)] -> Assertion
checkEpochEnabled =
  withCustomConfig (mkCommandLineArgs False) babbageConfigDir testLabel $ \interpreter mockServer dbSyncEnv -> do
    startDBSync dbSyncEnv
    b1 <- forgeAndSubmitBlocks interpreter mockServer 50
    -- add 2 blocks with tx
    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ Babbage.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000
    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ Babbage.mkPaymentTx (UTxOIndex 1) (UTxOIndex 0) 10000 10000
    b2 <- forgeAndSubmitBlocks interpreter mockServer 60

    assertBlockNoBackoff dbSyncEnv (fromIntegral $ length (b1 <> b2) + 2)
    assertEqQuery dbSyncEnv DB.queryEpochCount 1 "new epoch didn't prune tx_out column that are null"
  where
    testLabel = "CLAcheckEpochDisabledArg "
