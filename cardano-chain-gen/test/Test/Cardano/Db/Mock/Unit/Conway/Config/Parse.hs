{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Db.Mock.Unit.Conway.Config.Parse (
  conwayGenesis,
  missingConwayGenesis,
  noConwayGenesisHash,
  wrongConwayGenesisHash,
  insertConfig,
  defaultInsertConfig,
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Config
import Cardano.DbSync.Config.Types
import Cardano.DbSync.Error
import Cardano.Prelude hiding (from, isNothing)
import qualified Data.Aeson as Aeson
import Data.Default.Class (Default (..))
import Test.Cardano.Db.Mock.Config
import Test.Tasty.HUnit (Assertion (), assertBool, (@?=))
import Prelude ()

conwayGenesis :: DB.PGPassSource -> Assertion
conwayGenesis source = do
  cfg <- mkSyncNodeConfig configDir initCommandLineArgs
  withConfig source configDir mutableDir cmdLineArgs cfg (\_ -> pure ())
  where
    configDir = "config-conway"
    mutableDir = mkMutableDir "conwayConfigSimple"
    cmdLineArgs = initCommandLineArgs

missingConwayGenesis :: DB.PGPassSource -> Assertion
missingConwayGenesis source = do
  res <- try $ do
    cfg <- mkSyncNodeConfig configDir initCommandLineArgs
    withConfig source configDir mutableDir cmdLineArgs cfg pure
  assertBool "Not a SyncNodeError" (isConwayConfigError res)
  where
    configDir = "config-conway-missing-genesis"
    mutableDir = mkMutableDir "conwayConfigMissingGenesis"
    cmdLineArgs = initCommandLineArgs

noConwayGenesisHash :: DB.PGPassSource -> Assertion
noConwayGenesisHash source = do
  cfg <- mkSyncNodeConfig configDir initCommandLineArgs
  let cfg' = cfg {dncConwayGenesisHash = Nothing}
  withConfig source configDir mutableDir initCommandLineArgs cfg' (\_ -> pure ())
  where
    configDir = "config-conway"
    mutableDir = mkMutableDir "conwayConfigNoGenesis"

wrongConwayGenesisHash :: DB.PGPassSource -> Assertion
wrongConwayGenesisHash source = do
  cfg <- mkSyncNodeConfig configDir initCommandLineArgs
  hash <- Aeson.throwDecode "\"0000000000000000000000000000000000000000000000000000000000000000\""
  let cfg' = cfg {dncConwayGenesisHash = Just hash}
  res <- try (withConfig source configDir mutableDir initCommandLineArgs cfg' (\_ -> pure ()))
  assertBool "Not a SyncNodeError" (isConwayConfigError res)
  where
    configDir = "config-conway"
    mutableDir = mkMutableDir "configConwayWrongGenesis"

isConwayConfigError :: Either SyncNodeError a -> Bool
isConwayConfigError = either isConwayConfigError' (const False)
  where
    isConwayConfigError' (SNErrConwayConfig _ _) = True
    isConwayConfigError' _ = False

defaultInsertConfig :: DB.PGPassSource -> Assertion
defaultInsertConfig _source = do
  cfg <- mkSyncNodeConfig configDir initCommandLineArgs
  dncInsertOptions cfg @?= def
  where
    configDir = "config-conway"

insertConfig :: DB.PGPassSource -> Assertion
insertConfig _source = do
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
          , sioOffchainVoteData = OffchainVoteDataConfig False
          , sioPoolStats = PoolStatsConfig False
          , sioJsonType = JsonTypeDisable
          , sioRemoveJsonbFromSchema = RemoveJsonbFromSchemaConfig False
          , sioEpoch = EpochConfig False
          , sioStopAtBlock = Nothing
          }

  dncInsertOptions cfg @?= expected
  where
    configDir = "config-conway-insert-options"
