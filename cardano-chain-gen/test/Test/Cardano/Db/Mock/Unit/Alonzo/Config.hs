module Test.Cardano.Db.Mock.Unit.Alonzo.Config (
  defaultInsertConfig,
  insertConfig,
) where

import Cardano.DbSync.Config
import Cardano.DbSync.Config.Types
import Cardano.Prelude
import Data.Default.Class (Default (..))
import Test.Cardano.Db.Mock.Config
import Test.Tasty.HUnit (Assertion (), (@?=))
import Prelude ()

defaultInsertConfig :: Assertion
defaultInsertConfig = do
  cfg <- mkSyncNodeConfig alonzoConfigDir initCommandLineArgs
  dncInsertConfig cfg @?= def

insertConfig :: Assertion
insertConfig = do
  cfg <- mkSyncNodeConfig configDir initCommandLineArgs
  let expected =
        SyncInsertConfig
          { spcTxOut = TxOutDisable
          , spcLedger = LedgerDisable
          , spcShelley = ShelleyDisable
          , spcMultiAsset = MultiAssetDisable
          , spcMetadata = MetadataDisable
          , spcPlutus = PlutusDisable
          , spcGovernance = GovernanceConfig False
          , spcOffchainPoolData = OffchainPoolDataConfig False
          , spcJsonType = JsonTypeDisable
          }

  dncInsertConfig cfg @?= expected
  where
    configDir = "config-alonzo-insert-options"
