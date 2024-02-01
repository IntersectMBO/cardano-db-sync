{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Db.Mock.Unit.Conway.Config.Parse (
  conwayGenesis,
  missingConwayGenesis,
  noConwayGenesis,
  noConwayGenesisHash,
  wrongConwayGenesisHash,
  insertConfig,
  defaultInsertConfig,
  basicPrune,
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Config
import Cardano.DbSync.Config.Types
import Cardano.DbSync.Error
import Cardano.Mock.ChainSync.Server (IOManager ())
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import Cardano.Mock.Forging.Types (UTxOIndex (..))
import Cardano.Prelude hiding (from, isNothing)
import qualified Data.Aeson as Aeson
import Data.Default.Class (Default (..))
import Test.Cardano.Db.Mock.Config
import Test.Cardano.Db.Mock.UnifiedApi
import Test.Cardano.Db.Mock.Validate
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
  dncInsertConfig cfg @?= def
  where
    configDir = "config-conway"

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
    configDir = "config-conway-insert-options"

basicPrune :: IOManager -> [(Text, Text)] -> Assertion
basicPrune = do
  withCustomConfigAndDropDB args Nothing cfgDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Add some blocks
    blks <- forgeAndSubmitBlocks interpreter mockServer 50

    -- Add blocks with transactions
    void $
      withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10_000 10_000
    void $
      withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx (UTxOIndex 1) (UTxOIndex 0) 10_000 10_000

    -- Check tx-out count before pruning
    assertBlockNoBackoff dbSync (fullBlockSize blks)
    assertEqQuery dbSync DB.queryTxOutCount 14 "new epoch didn't prune tx_out column that are null"

    blks' <- forgeAndSubmitBlocks interpreter mockServer 48
    assertBlockNoBackoff dbSync (fullBlockSize $ blks <> blks')

    -- Check that tx_out was pruned
    assertEqQuery dbSync DB.queryTxOutCount 12 "the pruning didn't work correctly as the tx-out count is incorrect"
    -- Check unspent tx
    assertUnspentTx dbSync
  where
    args =
      initCommandLineArgs
        { claConfigFilename = "test-db-sync-config-prune.json"
        , claMigrateConsumed = True
        , claPruneTxOut = True
        }
    testLabel = "conwayConfigPrune"
    fullBlockSize b = fromIntegral $ length b + 2
    cfgDir = conwayConfigDir
