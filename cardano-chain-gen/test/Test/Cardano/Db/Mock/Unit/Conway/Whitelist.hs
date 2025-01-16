{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Test.Cardano.Db.Mock.Unit.Conway.Whitelist (
  addTxMultiAssetsWhitelist,
  addTxMetadataWhitelist,
  addTxMetadataWhitelistMultiple,
  addSimpleTxStakeAddrsWhitelist,
  fullTxStakeAddressWhitelist,
)
where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.Shelley.TxAuxData (Metadatum (..))
import Cardano.Mock.ChainSync.Server (IOManager ())
import qualified Cardano.Mock.Forging.Tx.Alonzo.ScriptsExamples as Examples
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import Cardano.Mock.Forging.Types
import qualified Cardano.Mock.Query as MockQ
import Cardano.Prelude hiding (head)
import Data.ByteString.Short (toShort)
import Data.List.NonEmpty (fromList)
import qualified Data.Map as Map
import Test.Cardano.Db.Mock.Config
import Test.Cardano.Db.Mock.UnifiedApi (withConwayFindLeaderAndSubmit)
import qualified Test.Cardano.Db.Mock.UnifiedApi as Api
import qualified Test.Cardano.Db.Mock.UnifiedApi as UnifiedApi
import Test.Cardano.Db.Mock.Validate
import Test.Tasty.HUnit (Assertion ())
import Prelude (head, (!!))

addTxMultiAssetsWhitelist :: IOManager -> [(Text, Text)] -> Assertion
addTxMultiAssetsWhitelist = do
  withCustomConfig args (Just $ configMulitiAssetPoliciesKeys maPolicyShortBs) cfgDir testLabel $
    \interpreter mockServer dbSyncEnv -> do
      startDBSync dbSyncEnv
      -- Forge a block with multiple multi-asset scripts
      void $ Api.withConwayFindLeaderAndSubmit interpreter mockServer $ \state' -> do
        let assetsMinted =
              Map.fromList [(head Examples.assetNames, 10), (Examples.assetNames !! 1, 4)]
            policy0 = PolicyID $ Examples.alwaysMintScriptHashRandomPolicyVal 1
            policy1 = PolicyID $ Examples.alwaysMintScriptHashRandomPolicyVal 2
            mintValue =
              MultiAsset $
                Map.fromList [(policy0, assetsMinted), (policy1, assetsMinted)]
            assets =
              Map.fromList [(head Examples.assetNames, 5), (Examples.assetNames !! 1, 2)]
            outValue =
              MaryValue (Coin 20) $
                MultiAsset $
                  Map.fromList [(policy0, assets), (policy1, assets)]

        -- Forge a multi-asset script
        tx0 <-
          Conway.mkMultiAssetsScriptTx
            [UTxOIndex 0]
            (UTxOIndex 1)
            [ (UTxOAddress Examples.alwaysSucceedsScriptAddr, outValue)
            , (UTxOAddress Examples.alwaysMintScriptAddr, outValue)
            ]
            []
            mintValue
            True
            100
            state'

        -- Consume the outputs from tx0
        let utxos = Conway.mkUTxOConway tx0
        tx1 <-
          Conway.mkMultiAssetsScriptTx
            [UTxOPair (head utxos), UTxOPair (utxos !! 1), UTxOIndex 2]
            (UTxOIndex 3)
            [ (UTxOAddress Examples.alwaysSucceedsScriptAddr, outValue)
            , (UTxOAddress Examples.alwaysMintScriptAddr, outValue)
            , (UTxOAddressNew 0, outValue)
            , (UTxOAddressNew 0, outValue)
            ]
            []
            mintValue
            True
            200
            state'
        pure [tx0, tx1]
      -- Verify script counts
      assertBlockNoBackoff dbSyncEnv 1
      assertAlonzoCounts dbSyncEnv (2, 4, 1, 2, 4, 2, 0, 0)
      -- create 4 multi-assets but only 2 should be added due to the whitelist
      assertEqBackoff dbSyncEnv MockQ.queryMultiAssetCount 2 [] "Expected 2 multi-assets"
      -- do the policy match the whitelist
      assertEqBackoff dbSyncEnv MockQ.queryMultiAssetMetadataPolicy (Just maPolicyShortBs) [] "Expected correct policy in db"
  where
    args = initCommandLineArgs {claFullMode = False}
    testLabel = "conwayConfigMultiAssetsWhitelist"
    maPolicyShortBs = toShort "4509cdddad21412c22c9164e10bc6071340ba235562f1575a35ded4d"
    cfgDir = conwayConfigDir

-- 2 blocks each with 4 metadata entries.
-- The whitelist has one tx metadata key which is in the first block
-- so only the TX in the first block should have tx metadata kept.
addTxMetadataWhitelist :: IOManager -> [(Text, Text)] -> Assertion
addTxMetadataWhitelist = do
  withCustomConfigAndDropDB args (Just configMetadataKeys) cfgDir testLabel $
    \interpreter mockServer dbSyncEnv -> do
      startDBSync dbSyncEnv
      -- Add transactions with metadata
      void $ do
        UnifiedApi.withConwayFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
          let txBody = Conway.mkDummyTxBodyWithFee $ Coin 1_000
              auxData = Map.fromList [(1, I 1), (2, I 2), (3, I 3), (4, I 4)]
           in Right (Conway.mkAuxDataTx True txBody auxData)
      void $ do
        UnifiedApi.withConwayFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
          let txBody = Conway.mkDummyTxBodyWithFee $ Coin 2_000
              auxData = Map.fromList [(5, I 5), (6, I 6), (7, I 7), (8, I 8)]
           in Right (Conway.mkAuxDataTx True txBody auxData)

      assertBlockNoBackoff dbSyncEnv 2
      -- Should have first block's tx metadata
      assertEqBackoff dbSyncEnv MockQ.queryTxMetadataCount 4 [] "Expected tx metadata"
  where
    args = initCommandLineArgs {claFullMode = False}
    testLabel = "conwayConfigMetadataWhitelist"

    cfgDir = conwayConfigDir

-- 2 blocks each with 4 metadata entries
-- The whitelist is set to keys [1,6] each key in in different TX
-- so all TxMetadata should be kept from both blocks.
addTxMetadataWhitelistMultiple :: IOManager -> [(Text, Text)] -> Assertion
addTxMetadataWhitelistMultiple = do
  withCustomConfigAndDropDB args (Just configMultipleMetadataKeys) cfgDir testLabel $
    \interpreter mockServer dbSyncEnv -> do
      startDBSync dbSyncEnv
      -- Add transactions with metadata
      void $ do
        UnifiedApi.withConwayFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
          let txBody = Conway.mkDummyTxBodyWithFee $ Coin 1_000
              auxData = Map.fromList [(1, I 1), (2, I 2), (3, I 3), (4, I 4)]
           in Right (Conway.mkAuxDataTx True txBody auxData)
      void $ do
        UnifiedApi.withConwayFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
          let txBody = Conway.mkDummyTxBodyWithFee $ Coin 2_000
              auxData = Map.fromList [(5, I 5), (6, I 6), (7, I 7), (8, I 8)]
           in Right (Conway.mkAuxDataTx True txBody auxData)

      assertBlockNoBackoff dbSyncEnv 2
      -- Should have both block's tx metadata
      assertEqBackoff dbSyncEnv MockQ.queryTxMetadataCount 8 [] "Expected tx metadata"
  where
    args = initCommandLineArgs {claFullMode = False}
    testLabel = "conwayConfigMetadataWhitelist"
    cfgDir = conwayConfigDir

addSimpleTxStakeAddrsWhitelist :: IOManager -> [(Text, Text)] -> Assertion
addSimpleTxStakeAddrsWhitelist = do
  withCustomConfigAndDropDB args (Just $ configShelleyStakeAddrKeys shelleyStakeAddrShortBs) cfgDir testLabel $
    \interpreter mockServer dbSyncEnv -> do
      -- Forge a block
      void $
        UnifiedApi.withConwayFindLeaderAndSubmitTx interpreter mockServer $
          Conway.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10_000 500 1_000

      startDBSync dbSyncEnv
      -- Verify it syncs
      assertBlockNoBackoff dbSyncEnv 1
      assertTxCount dbSyncEnv 12

      assertEqBackoff dbSyncEnv MockQ.queryStakeAddressHashRaw (Just shelleyStakeAddrShortBs) [] "Expected matching stake address"
  where
    testLabel = "conwayAddSimpleTx"
    args = initCommandLineArgs {claFullMode = False}
    cfgDir = conwayConfigDir
    shelleyStakeAddrShortBs = toShort "e0921c25093b263793a1baf36166b819543f5822c62f72571111111111"

fullTxStakeAddressWhitelist :: IOManager -> [(Text, Text)] -> Assertion
fullTxStakeAddressWhitelist = do
  withCustomConfig args (Just $ configMultipleShelleyStakeAddrKeys listShelleyStakeAddrs) cfgDir testLabel $
    \interpreter mockServer dbSyncEnv -> do
      let txOutTableType = txOutTableTypeFromConfig dbSyncEnv
      startDBSync dbSyncEnv
      -- Add some blocks with transactions
      void $ withConwayFindLeaderAndSubmit interpreter mockServer $ \state' ->
        sequence
          [ Conway.mkFullTx 0 100 state'
          , Conway.mkFullTx 1 200 state'
          ]
      -- Wait for them to sync
      assertBlockNoBackoff dbSyncEnv 1
      assertTxCount dbSyncEnv 13
      -- Check all tables that stake addresses effect
      assertEqBackoff dbSyncEnv MockQ.queryStakeAddressCount 5 [] "Expected 5 stake addresses"
      assertEqBackoff dbSyncEnv MockQ.queryCollateralTxOutCount 2 [] "Expected 1 collateral tx out"
      assertEqBackoff dbSyncEnv MockQ.queryPoolUpdateCount 5 [] "Expected 3 pool updates"
      assertEqBackoff dbSyncEnv MockQ.queryStakeDeRegCount 2 [] "Expected 1 stake deregistration"
      assertEqBackoff dbSyncEnv MockQ.queryStakeRegCount 2 [] "Expected 1 stake registration"
      assertEqBackoff dbSyncEnv (MockQ.countTxOutNonNullStakeAddrIds txOutTableType) 2 [] "Expected 1 non-null stake address id"
  where
    -- TODO: Cmdv: Missing tables checks that are currently blank in tests:
    -- delegation_vote, gov_action_proposal, instant_reward, reserve,
    -- treasury, treasury_withdrawl.

    testLabel = "fullTxStakeAddressWhitelist"
    args = initCommandLineArgs {claFullMode = True}
    cfgDir = conwayConfigDir
    shelleyStakeAddr0 = toShort "e0addfa484e8095ff53f45b25cf337923cf79abe6ec192fdf288d621f9"
    shelleyStakeAddr1 = toShort "e0921c25093b263793a1baf36166b819543f5822c62f72571111111111"
    shelleyStakeAddr2 = toShort "e0921c25093b263793a1baf36166b819543f5822c62f72573333333333"
    shelleyStakeAddr3 = toShort "e0000131350ac206583290486460934394208654903261221230945870"
    shelleyStakeAddr4 = toShort "e022236827154873624578632414768234573268457923654973246472"
    listShelleyStakeAddrs = fromList [shelleyStakeAddr0, shelleyStakeAddr1, shelleyStakeAddr2, shelleyStakeAddr3, shelleyStakeAddr4]
