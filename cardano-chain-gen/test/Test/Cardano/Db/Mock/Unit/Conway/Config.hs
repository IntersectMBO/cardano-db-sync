{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Db.Mock.Unit.Conway.Config (
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
import Cardano.Prelude
import qualified Data.Aeson as Aeson
import Data.Default.Class (Default (..))
import Test.Cardano.Db.Mock.Config
import Test.Tasty.HUnit (Assertion (), assertBool, (@?=))
import Prelude ()

conwayGenesis :: Assertion
conwayGenesis =
  mkSyncNodeConfig configDir
    >>= void . mkConfig (mkConfigDir configDir) mutableDir cmdLineArgs
  where
    configDir = "config-conway"
    mutableDir = mkMutableDir "conwayConfigSimple"
    cmdLineArgs = initCommandLineArgs

missingConwayGenesis :: Assertion
missingConwayGenesis = do
  res <- try $ mkConfig (mkConfigDir configDir) mutableDir cmdLineArgs =<< mkSyncNodeConfig configDir
  assertBool "Not a SyncNodeError" (isConwayConfigError res)
  where
    configDir = "config-conway-missing-genesis"
    mutableDir = mkMutableDir "conwayConfigMissingGenesis"
    cmdLineArgs = initCommandLineArgs

noConwayGenesis :: Assertion
noConwayGenesis = do
  cfg <- mkSyncNodeConfig configDir
  let cfg' = cfg {dncConwayGenesisFile = Nothing}
  void $
    mkConfig (mkConfigDir configDir) mutableDir cmdLineArgs cfg'
  where
    configDir = "config-conway"
    mutableDir = mkMutableDir "conwayConfigNoGenesis"
    cmdLineArgs = initCommandLineArgs

noConwayGenesisHash :: Assertion
noConwayGenesisHash = do
  cfg <- mkSyncNodeConfig configDir
  let cfg' = cfg {dncConwayGenesisHash = Nothing}
  void $
    mkConfig (mkConfigDir configDir) mutableDir initCommandLineArgs cfg'
  where
    configDir = "config-conway"
    mutableDir = mkMutableDir "conwayConfigNoGenesis"

wrongConwayGenesisHash :: Assertion
wrongConwayGenesisHash = do
  cfg <- mkSyncNodeConfig configDir
  hash <- Aeson.throwDecode "\"0000000000000000000000000000000000000000000000000000000000000000\""
  let cfg' = cfg {dncConwayGenesisHash = Just hash}

  res <- try (mkConfig (mkConfigDir configDir) mutableDir initCommandLineArgs cfg')
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
  cfg <- mkSyncNodeConfig conwayConfigDir
  dncInsertConfig cfg @?= def

insertConfig :: Assertion
insertConfig = do
  cfg <- mkSyncNodeConfig configDir
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
    configDir = "config-conway-insert-options"
