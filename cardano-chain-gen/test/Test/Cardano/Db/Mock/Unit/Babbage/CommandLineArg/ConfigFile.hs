module Test.Cardano.Db.Mock.Unit.Babbage.CommandLineArg.ConfigFile (
  checkConfigFileArg,
)
where

import Cardano.Mock.ChainSync.Server (IOManager)
import Data.Text (Text)
import Test.Cardano.Db.Mock.Config (CommandLineArgs (..), babbageConfigDir, withCustomConfigAndLogs)
import Test.Tasty.HUnit (Assertion)

mkCommandLineArgs :: Bool -> CommandLineArgs
mkCommandLineArgs hasConfigFile =
  CommandLineArgs
    { claHasConfigFile = hasConfigFile
    , claExtended = True
    , claHasCache = True
    , claShouldUseLedger = True
    , claSkipFix = True
    , claOnlyFix = False
    , claForceIndexes = False
    , claHasMultiAssets = True
    , claHasMetadata = True
    , claHasPlutusExtra = True
    , claHasOfflineData = True
    , claTurboMode = False
    , claFullMode = True
    , claMigrateConsumed = True
    , claPruneTxOut = True
    }

-- this test fails as incorrect configuration file given
checkConfigFileArg :: IOManager -> [(Text, Text)] -> Assertion
checkConfigFileArg =
  withCustomConfigAndLogs (mkCommandLineArgs False) babbageConfigDir testLabel $ \_ _ _ -> do
    pure ()
  where
    testLabel = "CLAcheckConfigFileArg"
