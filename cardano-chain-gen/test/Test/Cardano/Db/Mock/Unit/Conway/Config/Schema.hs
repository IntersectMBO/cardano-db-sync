{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Db.Mock.Unit.Conway.Config.Schema where

import qualified Cardano.Db as DB
import qualified Cardano.Db.Schema.Variants.TxOutAddress as DB
import qualified Cardano.Db.Schema.Variants.TxOutCore as DB
import Cardano.Mock.ChainSync.Server (IOManager (), ServerHandle)
import Cardano.Mock.Forging.Interpreter (Interpreter)
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import Cardano.Mock.Forging.Types (CardanoBlock, UTxOIndex (..))
import Cardano.Prelude hiding (putStrLn, show)
import qualified Data.Text as Text
import Test.Cardano.Db.Mock.Config
import Test.Cardano.Db.Mock.UnifiedApi
import Test.Cardano.Db.Mock.Validate
import Test.Tasty.HUnit (Assertion, assertBool)

------------------------------------------------------------------------------
-- Main Schema Validation Test
------------------------------------------------------------------------------

-- | Test all table schemas for column compatibility
validateSchemaColumns :: IOManager -> [(Text, Text)] -> Assertion
validateSchemaColumns =
  withFullConfigDropDB conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Setup test data
    setupTestData interpreter mockServer dbSync

    -- Cardano.Db.Schema.Core.Base
    validateCall dbSync (Proxy @DB.Block)
    validateCall dbSync (Proxy @DB.Tx)
    validateCall dbSync (Proxy @DB.TxMetadata)
    validateCall dbSync (Proxy @DB.TxIn)
    validateCall dbSync (Proxy @DB.CollateralTxIn)
    validateCall dbSync (Proxy @DB.ReferenceTxIn)
    validateCall dbSync (Proxy @DB.ReverseIndex)
    validateCall dbSync (Proxy @DB.TxCbor)
    validateCall dbSync (Proxy @DB.Datum)
    validateCall dbSync (Proxy @DB.Script)
    validateCall dbSync (Proxy @DB.Redeemer)
    validateCall dbSync (Proxy @DB.RedeemerData)
    validateCall dbSync (Proxy @DB.ExtraKeyWitness)
    validateCall dbSync (Proxy @DB.SlotLeader)
    validateCall dbSync (Proxy @DB.SchemaVersion)
    validateCall dbSync (Proxy @DB.Meta)
    validateCall dbSync (Proxy @DB.Withdrawal)
    validateCall dbSync (Proxy @DB.ExtraMigrations)

    -- Cardano.Db.Schema.Core.Pool
    validateCall dbSync (Proxy @DB.PoolHash)
    validateCall dbSync (Proxy @DB.PoolStat)
    validateCall dbSync (Proxy @DB.PoolUpdate)
    validateCall dbSync (Proxy @DB.PoolMetadataRef)
    validateCall dbSync (Proxy @DB.PoolOwner)
    validateCall dbSync (Proxy @DB.PoolRetire)
    validateCall dbSync (Proxy @DB.PoolRelay)
    validateCall dbSync (Proxy @DB.DelistedPool)
    validateCall dbSync (Proxy @DB.ReservedPoolTicker)

    -- Cardano.Db.Schema.Core.OffChain
    validateCall dbSync (Proxy @DB.OffChainPoolData)
    validateCall dbSync (Proxy @DB.OffChainPoolFetchError)
    validateCall dbSync (Proxy @DB.OffChainVoteData)
    validateCall dbSync (Proxy @DB.OffChainVoteGovActionData)
    validateCall dbSync (Proxy @DB.OffChainVoteDrepData)
    validateCall dbSync (Proxy @DB.OffChainVoteAuthor)
    validateCall dbSync (Proxy @DB.OffChainVoteReference)
    validateCall dbSync (Proxy @DB.OffChainVoteExternalUpdate)
    validateCall dbSync (Proxy @DB.OffChainVoteFetchError)

    -- Cardano.Db.Schema.Core.MultiAsset
    validateCall dbSync (Proxy @DB.MultiAsset)
    validateCall dbSync (Proxy @DB.MaTxMint)

    -- Cardano.Db.Schema.Core.StakeDelegation
    validateCall dbSync (Proxy @DB.StakeAddress)
    validateCall dbSync (Proxy @DB.StakeRegistration)
    validateCall dbSync (Proxy @DB.StakeDeregistration)
    validateCall dbSync (Proxy @DB.Delegation)
    validateCall dbSync (Proxy @DB.Reward)
    validateCall dbSync (Proxy @DB.RewardRest)
    validateCall dbSync (Proxy @DB.EpochStake)
    validateCall dbSync (Proxy @DB.EpochStakeProgress)

    -- Cardano.Db.Schema.Core.EpochAndProtocol
    validateCall dbSync (Proxy @DB.Epoch)
    validateCall dbSync (Proxy @DB.EpochParam)
    validateCall dbSync (Proxy @DB.EpochState)
    validateCall dbSync (Proxy @DB.AdaPots)
    validateCall dbSync (Proxy @DB.PotTransfer)
    validateCall dbSync (Proxy @DB.Treasury)
    validateCall dbSync (Proxy @DB.Reserve)
    validateCall dbSync (Proxy @DB.CostModel)

    -- Cardano.Db.Schema.Variants.TxOutCore
    validateCall dbSync (Proxy @DB.TxOutCore)
    validateCall dbSync (Proxy @DB.CollateralTxOutCore)
    validateCall dbSync (Proxy @DB.MaTxOutCore)
  where
    testLabel = "validateSchemaColumns"

validateVariantAddressSchemaColumns :: IOManager -> [(Text, Text)] -> Assertion
validateVariantAddressSchemaColumns =
  withCustomConfigDropDB args (Just $ configPruneForceTxIn True) cfgDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Setup test data
    setupTestData interpreter mockServer dbSync

    --  Cardano.Db.Schema.Variants.TxOutAddress
    validateCall dbSync (Proxy @DB.TxOutAddress)
    validateCall dbSync (Proxy @DB.CollateralTxOutAddress)
    validateCall dbSync (Proxy @DB.Address)
    validateCall dbSync (Proxy @DB.MaTxOutAddress)
  where
    args = initCommandLineArgs
    cfgDir = conwayConfigDir
    testLabel = "validateVariantAddressSchemaColumns"

-- | Setup minimal test data for validation
setupTestData :: Interpreter -> ServerHandle IO CardanoBlock -> DBSyncEnv -> IO ()
setupTestData interpreter mockServer dbSync = do
  void $ forgeAndSubmitBlocks interpreter mockServer 5
  void $
    withConwayFindLeaderAndSubmitTx interpreter mockServer $
      Conway.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10_000 10_000 0
  assertBlockNoBackoff dbSync 6

------------------------------------------------------------------------------
-- Individual Table Validation Functions
------------------------------------------------------------------------------

-- | Validate TxOutCore table column order
validateCall :: forall a. DB.DbInfo a => DBSyncEnv -> Proxy a -> IO ()
validateCall dbSync proxy = do
  result <- queryDBSync dbSync $ DB.queryTableColumns proxy
  assertColumnsMatch result

------------------------------------------------------------------------------
-- Helper Functions
------------------------------------------------------------------------------

-- | Print column mismatch details (only called on failure)
printColumnMismatch :: DB.ColumnComparisonResult -> IO ()
printColumnMismatch result = do
  let tableName = Text.unpack $ DB.ccrTableName result
  let typeName = Text.unpack $ DB.ccrTypeName result
  let expectedCols = Text.unpack <$> DB.ccrExpectedColumns result
  let dbCols = Text.unpack <$> DB.ccrDatabaseColumns result

  putStrLn $ "Column mismatch for table " <> tableName <> " (type: " <> typeName <> "):"
  putStrLn $ "Expected: " <> show expectedCols
  putStrLn $ "Database: " <> show dbCols

-- | Assert that columns match (with output only on failure)
assertColumnsMatch :: DB.ColumnComparisonResult -> IO ()
assertColumnsMatch result = do
  let expected = DB.ccrExpectedColumns result
  let actual = DB.ccrDatabaseColumns result
  let tableName = Text.unpack $ DB.ccrTableName result
  let typeName = Text.unpack $ DB.ccrTypeName result

  unless (expected == actual) $ do
    printColumnMismatch result

  assertBool
    ("Column mismatch for table " <> tableName <> " (type: " <> typeName <> ")")
    (expected == actual)
