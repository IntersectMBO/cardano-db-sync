{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Db.Mock.Unit.Conway.Config.JsonbInSchema (
  configRemoveJsonbFromSchemaEnabled,
  configRemoveJsonbFromSchemaDisabled,
  configJsonbInSchemaShouldRemoveThenAdd,
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Config (SyncNodeConfig (..))
import Cardano.DbSync.Config.Types (RemoveJsonbFromSchemaConfig (..), SyncInsertOptions (..))
import Cardano.Mock.ChainSync.Server (IOManager ())
import Cardano.Prelude hiding (head)
import Test.Cardano.Db.Mock.Config
import Test.Cardano.Db.Mock.Validate
import Test.Tasty.HUnit (Assertion ())

configRemoveJsonbFromSchemaEnabled :: IOManager -> [(Text, Text)] -> Assertion
configRemoveJsonbFromSchemaEnabled = do
  withCustomConfigDropDB args (Just configRemoveJsonFromSchema) cfgDir testLabel $ \_interpreter _mockServer dbSync -> do
    startDBSync dbSync
    assertEqQuery
      dbSync
      DB.queryJsonbInSchemaExistsTest
      False
      "There should be no jsonb data types in database if option is enabled"
    checkStillRuns dbSync
  where
    args = initCommandLineArgs {claFullMode = False}
    testLabel = "conwayConfigRemoveJsonbFromSchemaEnabled"

    cfgDir = conwayConfigDir

configRemoveJsonbFromSchemaDisabled :: IOManager -> [(Text, Text)] -> Assertion
configRemoveJsonbFromSchemaDisabled = do
  withCustomConfigDropDB args (Just configRemoveJsonFromSchemaFalse) cfgDir testLabel $
    \_interpreter _mockServer dbSync -> do
      startDBSync dbSync
      assertEqQuery
        dbSync
        DB.queryJsonbInSchemaExistsTest
        True
        "There should be jsonb types in database if option is disabled"
      checkStillRuns dbSync
  where
    args = initCommandLineArgs {claFullMode = False}
    testLabel = "conwayConfigRemoveJsonbFromSchemaDisabled"
    cfgDir = conwayConfigDir

configJsonbInSchemaShouldRemoveThenAdd :: IOManager -> [(Text, Text)] -> Assertion
configJsonbInSchemaShouldRemoveThenAdd =
  withCustomConfigDropDB args (Just configRemoveJsonFromSchema) cfgDir testLabel $ \_interpreter _mockServer dbSyncEnv -> do
    startDBSync dbSyncEnv
    assertEqQuery
      dbSyncEnv
      DB.queryJsonbInSchemaExistsTest
      False
      "There should be no jsonb types in database if option has been enabled"
    stopDBSync dbSyncEnv
    let newDbSyncEnv =
          dbSyncEnv
            { dbSyncConfig =
                (dbSyncConfig dbSyncEnv)
                  { dncInsertOptions =
                      (dncInsertOptions $ dbSyncConfig dbSyncEnv)
                        { sioRemoveJsonbFromSchema = RemoveJsonbFromSchemaConfig False
                        }
                  }
            }
    startDBSync newDbSyncEnv
    assertEqQuery
      dbSyncEnv
      DB.queryJsonbInSchemaExistsTest
      True
      "There should be jsonb types in database if option has been disabled"
    -- Expected to fail
    checkStillRuns dbSyncEnv
  where
    args = initCommandLineArgs {claFullMode = False}
    testLabel = "configJsonbInSchemaShouldRemoveThenAdd"
    cfgDir = conwayConfigDir
