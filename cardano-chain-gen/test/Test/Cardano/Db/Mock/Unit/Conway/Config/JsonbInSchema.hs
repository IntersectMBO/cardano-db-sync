{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Db.Mock.Unit.Conway.Config.JsonbInSchema (
  configJsonbInSchemaTrue,
  configJsonbInSchemaFalse,
  configJsonbInSchemaShouldError,
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Config (SyncNodeConfig (..))
import Cardano.DbSync.Config.Types (AddJsonbToSchemaConfig (..), SyncInsertOptions (..))
import Cardano.Mock.ChainSync.Server (IOManager ())
import Cardano.Prelude hiding (head)
import Test.Cardano.Db.Mock.Config
import Test.Cardano.Db.Mock.Validate
import Test.Tasty.HUnit (Assertion ())

configJsonbInSchemaTrue :: IOManager -> [(Text, Text)] -> Assertion
configJsonbInSchemaTrue ioManager metadata = do
  syncNodeConfig <- mksNodeConfig
  withCustomConfigAndDropDB args (Just syncNodeConfig) cfgDir testLabel action ioManager metadata
  where
    action = \_interpreter _mockServer dbSync -> do
      startDBSync dbSync
      threadDelay 2_000_000
      assertEqQuery
        dbSync
        DB.queryJsonbInSchemaExists
        True
        "There should be jsonb data types in database if option is active"
      checkStillRuns dbSync

    args = initCommandLineArgs {claFullMode = False}
    testLabel = "conwayConfigJsonbInSchemaTrue"

    cfgDir = conwayConfigDir

    mksNodeConfig :: IO SyncNodeConfig
    mksNodeConfig = do
      initConfigFile <- mkSyncNodeConfig cfgDir args
      let dncInsertOptions' = dncInsertOptions initConfigFile
      pure $
        initConfigFile
          { dncInsertOptions = dncInsertOptions' {sioAddJsonbToSchema = AddJsonbToSchemaConfig True}
          }

configJsonbInSchemaFalse :: IOManager -> [(Text, Text)] -> Assertion
configJsonbInSchemaFalse ioManager metadata = do
  syncNodeConfig <- mksNodeConfig
  withCustomConfigAndDropDB args (Just syncNodeConfig) cfgDir testLabel action ioManager metadata
  where
    action = \_interpreter _mockServer dbSync -> do
      startDBSync dbSync
      threadDelay 2_000_000
      assertEqQuery
        dbSync
        DB.queryJsonbInSchemaExists
        False
        "There should be no jsonb types in database if option isn't active"
      checkStillRuns dbSync

    args = initCommandLineArgs {claFullMode = False}
    testLabel = "conwayConfigJsonbInSchemaFalse"

    cfgDir = conwayConfigDir

    mksNodeConfig :: IO SyncNodeConfig
    mksNodeConfig = do
      initConfigFile <- mkSyncNodeConfig cfgDir args
      let dncInsertOptions' = dncInsertOptions initConfigFile
      pure $
        initConfigFile
          { dncInsertOptions = dncInsertOptions' {sioAddJsonbToSchema = AddJsonbToSchemaConfig False}
          }

configJsonbInSchemaShouldError :: IOManager -> [(Text, Text)] -> Assertion
configJsonbInSchemaShouldError ioManager metadata = do
  syncNodeConfig <- mksNodeConfig
  withCustomConfigAndDropDB args (Just syncNodeConfig) cfgDir testLabel action ioManager metadata
  where
    action = \_interpreter _mockServer dbSync -> do
      startDBSync dbSync
      threadDelay 2_000_000
      stopDBSync dbSync
      let newDbSyncEnv =
            dbSync
              { dbSyncConfig =
                  (dbSyncConfig dbSync)
                    { dncInsertOptions =
                        (dncInsertOptions $ dbSyncConfig dbSync)
                          { sioAddJsonbToSchema = AddJsonbToSchemaConfig False
                          }
                    }
              }
      startDBSync newDbSyncEnv
      threadDelay 2_000_000
      -- Expected to fail
      checkStillRuns dbSync

    args = initCommandLineArgs {claFullMode = False}
    testLabel = "conwayConfigJsonbInSchemaShouldError"

    cfgDir = conwayConfigDir

    mksNodeConfig :: IO SyncNodeConfig
    mksNodeConfig = do
      initConfigFile <- mkSyncNodeConfig cfgDir args
      let dncInsertOptions' = dncInsertOptions initConfigFile
      pure $
        initConfigFile
          { dncInsertOptions = dncInsertOptions' {sioAddJsonbToSchema = AddJsonbToSchemaConfig True}
          }
