{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Db.Mock.Unit.Conway.Config.Parse (
  conwayGenesis,
  missingConwayGenesis,
  noConwayGenesis,
  noConwayGenesisHash,
  wrongConwayGenesisHash,
  insertConfig,
  defaultInsertConfig,
) where

import Cardano.DbSync.Config
import Cardano.DbSync.Config.Types
import Cardano.DbSync.Error
import Cardano.Prelude hiding (from, isNothing)
import qualified Data.Aeson as Aeson
import Data.Default.Class (Default (..))
import Test.Cardano.Db.Mock.Config
import Test.Tasty.HUnit (Assertion (), assertBool, (@?=))
import Prelude ()

conwayGenesis :: Assertion
conwayGenesis =
  mkSyncNodeConfig configDir initCommandLineArgs
    >>= void . mkConfig configDir mutableDir cmdLineArgs
  where
    configDir = "config-conway"
    mutableDir = mkMutableDir "conwayConfigSimple"
    cmdLineArgs = initCommandLineArgs

missingConwayGenesis :: Assertion
missingConwayGenesis = do
  res <- try $ do
    cfg <- mkSyncNodeConfig configDir initCommandLineArgs
    mkConfig configDir mutableDir cmdLineArgs cfg
  assertBool "Not a SyncNodeError" (isConwayConfigError res)
  where
    configDir = "config-conway-missing-genesis"
    mutableDir = mkMutableDir "conwayConfigMissingGenesis"
    cmdLineArgs = initCommandLineArgs

noConwayGenesis :: Assertion
noConwayGenesis = do
  cfg <- mkSyncNodeConfig configDir initCommandLineArgs
  let cfg' = cfg {dncConwayGenesisFile = Nothing}
  void $
    mkConfig configDir mutableDir cmdLineArgs cfg'
  where
    configDir = "config-conway"
    mutableDir = mkMutableDir "conwayConfigNoGenesis"
    cmdLineArgs = initCommandLineArgs

noConwayGenesisHash :: Assertion
noConwayGenesisHash = do
  cfg <- mkSyncNodeConfig configDir initCommandLineArgs
  let cfg' = cfg {dncConwayGenesisHash = Nothing}
  void $
    mkConfig configDir mutableDir initCommandLineArgs cfg'
  where
    configDir = "config-conway"
    mutableDir = mkMutableDir "conwayConfigNoGenesis"

wrongConwayGenesisHash :: Assertion
wrongConwayGenesisHash = do
  cfg <- mkSyncNodeConfig configDir initCommandLineArgs
  hash <- Aeson.throwDecode "\"0000000000000000000000000000000000000000000000000000000000000000\""
  let cfg' = cfg {dncConwayGenesisHash = Just hash}

  res <- try (mkConfig configDir mutableDir initCommandLineArgs cfg')
  assertBool "Not a SyncNodeError" (isConwayConfigError res)
  where
    configDir = "config-conway"
    mutableDir = mkMutableDir "configConwayWrongGenesis"

isConwayConfigError :: Either SyncNodeError a -> Bool
isConwayConfigError = either isConwayConfigError' (const False)
  where
    isConwayConfigError' (SNErrConwayConfig _ _) = True
    isConwayConfigError' _ = False

defaultInsertConfig :: Assertion
defaultInsertConfig = do
  cfg <- mkSyncNodeConfig configDir initCommandLineArgs
  dncInsertOptions cfg @?= def
  where
    configDir = "config-conway"

insertConfig :: Assertion
insertConfig = do
  cfg <- mkSyncNodeConfig configDir initCommandLineArgs
  let expected =
        SyncInsertOptions
          { sioTxOut = TxOutDisable
          , sioLedger = LedgerDisable
          , sioShelley = ShelleyDisable
          , sioRewards = RewardsConfig True
          , sioMultiAsset = MultiAssetDisable
          , sioMetadata = MetadataDisable
          , sioPlutus = PlutusDisable
          , sioGovernance = GovernanceConfig False
          , sioOffchainPoolData = OffchainPoolDataConfig False
          , sioJsonType = JsonTypeDisable
          }

  dncInsertOptions cfg @?= expected
  where
    configDir = "config-conway-insert-options"
