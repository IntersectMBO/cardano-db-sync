{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Db.Mock.Unit.Conway.Config.Parse (
  conwayGenesis,
  missingConwayGenesis,
  noConwayGenesis,
  noConwayGenesisHash,
  wrongConwayGenesisHash,
  insertConfig,
  defaultInsertConfig,
  invalidShelleyStkAddrHash,
  invalidMultiAssetPoliciesHash,
  invalidPlutusScriptHash,
)
where

import Cardano.DbSync.Config
import Cardano.DbSync.Config.Types
import Cardano.DbSync.Error
import Cardano.Prelude hiding (from, isNothing)
import qualified Data.Aeson as Aeson
import Data.Default.Class (Default (..))
import Data.String (String)
import Data.Text (pack)
import Test.Cardano.Db.Mock.Config
import Test.Tasty.HUnit (Assertion (), assertBool, (@?=))
import Prelude ()

conwayGenesis :: Assertion
conwayGenesis =
  mkSyncNodeConfig configDir initCommandLineArgs
    >>= void
      . mkConfig configDir mutableDir cmdLineArgs
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
          { sioTxCBOR = TxCBORConfig False
          , sioTxOut = TxOutDisable
          , sioLedger = LedgerDisable
          , sioShelley = ShelleyDisable
          , sioRewards = RewardsConfig True
          , sioMultiAsset = MultiAssetDisable
          , sioMetadata = MetadataDisable
          , sioPlutus = PlutusDisable
          , sioGovernance = GovernanceConfig False
          , sioOffchainPoolData = OffchainPoolDataConfig False
          , sioPoolStats = PoolStatsConfig False
          , sioJsonType = JsonTypeDisable
          , sioRemoveJsonbFromSchema = RemoveJsonbFromSchemaConfig False
          }

  dncInsertOptions cfg @?= expected
  where
    configDir = "config-conway-insert-options"

invalidShelleyStkAddrHash :: Assertion
invalidShelleyStkAddrHash =
  let invalidJson = "{ \"enable\": true, \"stake_addresses\": " <> invalidHash <> " }"
      decodedResult :: Either String ShelleyInsertConfig
      decodedResult = Aeson.eitherDecodeStrict $ encodeUtf8 $ pack invalidJson
   in assertBool "Decoding should fail for invalid Shelley stake address hash" (isLeft decodedResult)

invalidMultiAssetPoliciesHash :: Assertion
invalidMultiAssetPoliciesHash =
  let invalidJson = "{ \"enable\": true, \"policies\": " <> invalidHash <> " }"
      decodedResult :: Either String MultiAssetConfig
      decodedResult = Aeson.eitherDecodeStrict $ encodeUtf8 $ pack invalidJson
   in assertBool "Decoding should fail for invalid MultiAsset policies hash" (isLeft decodedResult)

invalidPlutusScriptHash :: Assertion
invalidPlutusScriptHash =
  let invalidJson = "{ \"enable\": true, \"script_hashes\": " <> invalidHash <> " }"
      decodedResult :: Either String PlutusConfig
      decodedResult = Aeson.eitherDecodeStrict $ encodeUtf8 $ pack invalidJson
   in assertBool "Decoding should fail for invalid Plutus script hash" (isLeft decodedResult)

invalidHash :: String
invalidHash = "[\"\\xe0758b08dea05dabd1cd3510689ebd9efb6a49316acb30eead750e2e9e\"]"
