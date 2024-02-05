{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Db.Mock.Unit.Conway.Config (
  conwayGenesis,
  missingConwayGenesis,
  noConwayGenesis,
  noConwayGenesisHash,
  wrongConwayGenesisHash,
) where

import Cardano.DbSync.Config
import Cardano.DbSync.Error
import Cardano.Prelude
import qualified Data.Aeson as Aeson
import Test.Cardano.Db.Mock.Config
import Test.Tasty.HUnit (Assertion (), assertBool)
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
