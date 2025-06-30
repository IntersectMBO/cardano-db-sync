{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Db.Mock.Validate (
  assertBlocksCount,
  assertBlocksCountDetailed,
  assertTxCount,
  assertTxOutCount,
  assertTxInCount,
  assertUnspentTx,
  assertRewardCount,
  assertRewardRestCount,
  assertBlockNoBackoff,
  assertBlockNoBackoffTimes,
  expectFailSilent,
  assertEqQuery,
  assertEqBackoff,
  assertBackoff,
  assertQuery,
  assertBabbageCounts,
  assertPoolLayerCounters,
  runQuery,
  addPoolCounters,
  assertCurrentEpoch,
  assertAddrValues,
  assertRight,
  assertCertCounts,
  assertRewardCounts,
  assertEpochStake,
  assertEpochStakeEpoch,
  assertNonZeroFeesContract,
  assertDatumCBOR,
  assertAlonzoCounts,
  assertScriptCert,
  assertPoolCounters,
  poolCountersQuery,
  checkStillRuns,
  defaultDelays,
) where

import Control.Concurrent
import Control.Exception
import Control.Monad (forM_)
import Control.Monad.Logger (NoLoggingT)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Either (isRight)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

import qualified Cardano.Db as DB
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Shelley.Generic.Util
import qualified Cardano.Ledger.Address as Ledger
import Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Shelley.LedgerState (EraCertState)
import Cardano.Mock.Forging.Tx.Generic
import Cardano.Mock.Forging.Types
import Cardano.Prelude (MonadIO)
import Cardano.SMASH.Server.PoolDataLayer
import Cardano.SMASH.Server.Types
import Test.Cardano.Db.Mock.Config

{- HLINT ignore "Reduce duplication" -}

assertBlocksCount :: DBSyncEnv -> Word64 -> IO ()
assertBlocksCount env n = do
  assertEqBackoff env DB.queryBlockCount n defaultDelays "Unexpected block count"

assertBlocksCountDetailed :: DBSyncEnv -> Word64 -> [Int] -> IO ()
assertBlocksCountDetailed env n delays = do
  assertEqBackoff env DB.queryBlockCount n delays "Unexpected block count"

assertTxCount :: DBSyncEnv -> Word64 -> IO ()
assertTxCount env n = do
  assertEqBackoff env DB.queryTxCount n defaultDelays "Unexpected tx count"

assertTxOutCount :: DBSyncEnv -> Word64 -> IO ()
assertTxOutCount env n = do
  assertEqBackoff env (DB.queryTxOutCount DB.TxOutVariantCore) n defaultDelays "Unexpected txOut count"

assertTxInCount :: DBSyncEnv -> Word64 -> IO ()
assertTxInCount env n = do
  assertEqBackoff env DB.queryTxInCount n defaultDelays "Unexpected txIn count"

assertRewardCount :: DBSyncEnv -> Word64 -> IO ()
assertRewardCount env n =
  assertEqBackoff env DB.queryRewardCount n defaultDelays "Unexpected rewards count"

assertRewardRestCount :: DBSyncEnv -> Word64 -> IO ()
assertRewardRestCount env n =
  assertEqBackoff env DB.queryRewardRestCount n defaultDelays "Unexpected instant rewards count"

assertBlockNoBackoff :: DBSyncEnv -> Int -> IO ()
assertBlockNoBackoff = assertBlockNoBackoffTimes defaultDelays

assertBlockNoBackoffTimes :: [Int] -> DBSyncEnv -> Int -> IO ()
assertBlockNoBackoffTimes times env blockNo =
  assertEqBackoff env DB.queryBlockHeight (Just $ fromIntegral blockNo) times "Unexpected BlockNo"

expectFailSilent :: String -> Assertion -> TestTree
expectFailSilent name action = testCase name $ do
  result <- catch (Right <$> action) (\(_ :: SomeException) -> pure $ Left ())
  case result of
    Left _ -> pure () -- Test failed as expected, do nothing
    Right _ -> assertFailure "Expected test to fail but it succeeded"

-- checking that unspent count matches from tx_in to tx_out
assertUnspentTx :: DBSyncEnv -> IO ()
assertUnspentTx dbSyncEnv = do
  let txOutVariantType = txOutVariantTypeFromConfig dbSyncEnv
  unspentTxCount <- queryDBSync dbSyncEnv $ DB.queryTxOutConsumedNullCount txOutVariantType
  consumedNullCount <- queryDBSync dbSyncEnv $ DB.queryTxOutUnspentCount txOutVariantType
  assertEqual "Unexpected tx unspent count between tx-in & tx-out" unspentTxCount consumedNullCount

defaultDelays :: [Int]
defaultDelays = [1, 2, 4, 8, 16, 32, 64, 128, 256]

assertEqQuery :: (Eq a, Show a) => DBSyncEnv -> DB.DbAction (NoLoggingT IO) a -> a -> String -> IO ()
assertEqQuery env query a msg = do
  assertEqBackoff env query a defaultDelays msg

assertEqBackoff :: (Eq a, Show a) => DBSyncEnv -> DB.DbAction (NoLoggingT IO) a -> a -> [Int] -> String -> IO ()
assertEqBackoff env query a delays msg = do
  checkStillRuns env
  assertBackoff env query delays (== a) (\a' -> msg <> ": got " <> show a' <> " expected " <> show a)

assertBackoff :: DBSyncEnv -> DB.DbAction (NoLoggingT IO) a -> [Int] -> (a -> Bool) -> (a -> String) -> IO ()
assertBackoff env query delays check errMsg = go delays
  where
    go ds = do
      q <- assertQuery env query check errMsg
      case (q, ds) of
        (Nothing, _) -> pure ()
        (Just err, []) -> assertFailure err
        (Just _err, dl : rest) -> do
          threadDelay $ dl * 100_000
          go rest

assertQuery :: DBSyncEnv -> DB.DbAction (NoLoggingT IO) a -> (a -> Bool) -> (a -> String) -> IO (Maybe String)
assertQuery env query check errMsg = do
  ma <- try @DB.DbError $ queryDBSync env query
  case ma of
    Left dbErr | migrationNotDoneYet (DB.dbErrorMessage dbErr) -> do
      threadDelay 1_000_000
      pure $ Just $ Text.unpack $ DB.dbErrorMessage dbErr
    Left err -> throwIO err
    Right a | not (check a) -> pure $ Just $ errMsg a
    _ -> pure Nothing

runQuery :: DBSyncEnv -> DB.DbAction (NoLoggingT IO) a -> IO a
runQuery env query = do
  ma <- try @DB.DbError $ queryDBSync env query
  case ma of
    Left dbErr | migrationNotDoneYet (DB.dbErrorMessage dbErr) -> do
      threadDelay 1_000_000
      runQuery env query
    Left err -> throwIO err
    Right a -> pure a

checkStillRuns :: DBSyncEnv -> IO ()
checkStillRuns env = do
  ret <- pollDBSync env
  case ret of
    Nothing -> pure ()
    Just (Right ()) -> throwIO $ userError "dbsync has stopped"
    Just (Left err) -> throwIO err

migrationNotDoneYet :: Text -> Bool
migrationNotDoneYet =
  Text.isPrefixOf "relation"

assertCurrentEpoch :: DBSyncEnv -> Word64 -> IO ()
assertCurrentEpoch env expected =
  assertEqBackoff env q (Just expected) defaultDelays "Unexpected epoch stake counts"
  where
    q = DB.queryBlocksForCurrentEpochNo

assertAddrValues ::
  (EraCertState era, Core.EraTxOut era) =>
  DBSyncEnv ->
  UTxOIndex era ->
  DB.DbLovelace ->
  LedgerState (ShelleyBlock p era) ->
  IO ()
assertAddrValues env ix expected sta = do
  addr <- assertRight $ resolveAddress ix sta
  let address = Generic.renderAddress addr
      q = DB.queryAddressOutputs DB.TxOutVariantCore address
  assertEqBackoff env q expected defaultDelays "Unexpected Balance"

assertRight :: Show err => Either err a -> IO a
assertRight ei =
  case ei of
    Right a -> pure a
    Left err -> assertFailure (show err)

assertCertCounts :: DBSyncEnv -> (Word64, Word64, Word64, Word64) -> IO ()
assertCertCounts env expected =
  assertEqBackoff env q expected defaultDelays "Unexpected Cert counts"
  where
    q = do
      registr <- DB.queryStakeRegistrationCount
      deregistr <- DB.queryStakeDeregistrationCount
      deleg <- DB.queryDelegationCount
      withdrawal <- DB.queryWithdrawalCount
      -- We deduct the initial registration and delegation in the genesis
      pure (registr - 5, deregistr, deleg - 5, withdrawal)

assertRewardCounts ::
  EraCertState era =>
  DBSyncEnv ->
  LedgerState (ShelleyBlock p era) ->
  Bool ->
  Maybe Word64 ->
  [(StakeIndex, (Word64, Word64, Word64, Word64, Word64))] ->
  IO ()
assertRewardCounts env st filterAddr mEpoch expected = do
  assertEqBackoff env (groupByAddress <$> DB.queryRewardsAndRestsWithStakeAddr mEpoch) expectedMap defaultDelays "Unexpected rewards count"
  where
    expectedMap :: Map ByteString (Word64, Word64, Word64, Word64, Word64)
    expectedMap = Map.fromList $ fmap (first mkDBStakeAddress) expected

    groupByAddress :: [(DB.RewardSource, ByteString)] -> Map ByteString (Word64, Word64, Word64, Word64, Word64)
    groupByAddress rewards =
      let res = foldr updateMap Map.empty rewards
       in if filterAddr
            then Map.filterWithKey (\k _ -> Map.member k expectedMap) res
            else res

    mkDBStakeAddress :: StakeIndex -> ByteString
    mkDBStakeAddress stIx = case resolveStakeCreds stIx st of
      Left _ -> error "could not resolve StakeIndex"
      Right cred -> Ledger.serialiseRewardAccount $ Ledger.RewardAccount Testnet cred

    updateAddrCounters ::
      DB.RewardSource ->
      Maybe (Word64, Word64, Word64, Word64, Word64) ->
      (Word64, Word64, Word64, Word64, Word64)
    updateAddrCounters rs Nothing = updateCounters rs (0, 0, 0, 0, 0)
    updateAddrCounters rs (Just cs) = updateCounters rs cs

    updateCounters ::
      DB.RewardSource ->
      (Word64, Word64, Word64, Word64, Word64) ->
      (Word64, Word64, Word64, Word64, Word64)
    updateCounters rs (a, b, c, d, e) = case rs of
      DB.RwdLeader -> (a + 1, b, c, d, e)
      DB.RwdMember -> (a, b + 1, c, d, e)
      DB.RwdReserves -> (a, b, c + 1, d, e)
      DB.RwdTreasury -> (a, b, c, d + 1, e)
      DB.RwdDepositRefund -> (a, b, c, d, e + 1)
      _ -> (a, b, c, d, e)

    updateMap ::
      (DB.RewardSource, ByteString) ->
      Map ByteString (Word64, Word64, Word64, Word64, Word64) ->
      Map ByteString (Word64, Word64, Word64, Word64, Word64)
    updateMap (rs, addr) = Map.alter (Just . updateAddrCounters rs) addr

assertEpochStake :: DBSyncEnv -> Word64 -> IO ()
assertEpochStake env expected =
  assertEqBackoff env DB.queryEpochStakeCountGen expected defaultDelays "Unexpected epoch stake counts"

assertEpochStakeEpoch :: DBSyncEnv -> Word64 -> Word64 -> IO ()
assertEpochStakeEpoch env e expected =
  assertEqBackoff env (DB.queryEpochStakeByEpochCount e) expected defaultDelays "Unexpected epoch stake counts"

assertNonZeroFeesContract :: DBSyncEnv -> IO ()
assertNonZeroFeesContract env =
  assertEqBackoff env DB.queryZeroFeeInvalidTxCount 0 defaultDelays "Found contract tx with zero fees"

assertDatumCBOR :: DBSyncEnv -> ByteString -> IO ()
assertDatumCBOR env bs =
  assertEqBackoff env (DB.queryDatumByBytesCount bs) 1 defaultDelays "Datum bytes not found"

assertAlonzoCounts :: DBSyncEnv -> (Word64, Word64, Word64, Word64, Word64, Word64, Word64, Word64) -> IO ()
assertAlonzoCounts env expected =
  assertEqBackoff env q expected defaultDelays "Unexpected Alonzo counts"
  where
    q = do
      scripts <- DB.queryScriptCount
      redeemers <- DB.queryRedeemerCount
      datums <- DB.queryDatumCount
      colInputs <- DB.queryCollateralTxInCount
      scriptOutputs <- fromIntegral . length <$> DB.queryScriptOutputs DB.TxOutVariantCore
      redeemerTxIn <- fromIntegral . length <$> DB.queryTxInRedeemer
      invalidTx <- fromIntegral . length <$> DB.queryInvalidTx
      txIninvalidTx <- fromIntegral . length <$> DB.queryTxInFailedTx

      pure
        ( scripts
        , redeemers
        , datums
        , colInputs
        , scriptOutputs
        , redeemerTxIn
        , invalidTx
        , txIninvalidTx
        )

assertBabbageCounts :: DBSyncEnv -> (Word64, Word64, Word64, Word64, Word64, Word64, Word64, Word64, Word64, Word64, Word64, Word64, Word64) -> IO ()
assertBabbageCounts env expected =
  assertEqBackoff env q expected defaultDelays "Unexpected Babbage counts"
  where
    q = do
      scripts <- DB.queryScriptCount
      redeemers <- DB.queryRedeemerCount
      datums <- DB.queryDatumCount
      colInputs <- DB.queryCollateralTxInCount
      scriptOutputs <- fromIntegral . length <$> DB.queryScriptOutputs DB.TxOutVariantCore
      redeemerTxIn <- fromIntegral . length <$> DB.queryTxInRedeemer
      invalidTx <- fromIntegral . length <$> DB.queryInvalidTx
      txIninvalidTx <- fromIntegral . length <$> DB.queryTxInFailedTx
      redeemerData <- DB.queryRedeemerDataCount
      referenceTxIn <- DB.queryReferenceTxInCount

      collTxOut <- case txOutVariantTypeFromConfig env of
        DB.TxOutVariantCore -> DB.queryCollateralTxOutCoreCount
        DB.TxOutVariantAddress -> DB.queryCollateralTxOutAddressCount

      inlineDatum <- case txOutVariantTypeFromConfig env of
        DB.TxOutVariantCore -> DB.queryInlineDatumCoreCount
        DB.TxOutVariantAddress -> DB.queryInlineDatumAddressCount

      referenceScript <- case txOutVariantTypeFromConfig env of
        DB.TxOutVariantCore -> DB.queryReferenceScriptCoreCount
        DB.TxOutVariantAddress -> DB.queryReferenceScriptAddressCount

      pure
        ( scripts
        , redeemers
        , datums
        , colInputs
        , scriptOutputs
        , redeemerTxIn
        , invalidTx
        , txIninvalidTx
        , redeemerData
        , referenceTxIn
        , collTxOut
        , inlineDatum
        , referenceScript
        )

assertScriptCert :: DBSyncEnv -> (Word64, Word64, Word64, Word64) -> IO ()
assertScriptCert env expected =
  assertEqBackoff env q expected defaultDelays "Unexpected Script Stake counts"
  where
    q = do
      deregistrScript <- fromIntegral . length <$> DB.queryDeregistrationScript
      delegScript <- fromIntegral . length <$> DB.queryDelegationScript
      withdrawalScript <- fromIntegral . length <$> DB.queryWithdrawalScript
      stakeAddressScript <- fromIntegral . length <$> DB.queryStakeAddressScript
      pure (deregistrScript, delegScript, withdrawalScript, stakeAddressScript)

assertPoolCounters :: DBSyncEnv -> (Word64, Word64, Word64, Word64, Word64, Word64) -> IO ()
assertPoolCounters env expected =
  assertEqBackoff env poolCountersQuery expected defaultDelays "Unexpected Pool counts"

poolCountersQuery :: MonadIO m => DB.DbAction m (Word64, Word64, Word64, Word64, Word64, Word64)
poolCountersQuery = do
  poolHash <- DB.queryPoolHashCount
  poolMetadataRef <- DB.queryPoolMetadataRefCount
  poolUpdate <- DB.queryPoolUpdateCount
  poolOwner <- DB.queryPoolOwnerCount
  poolRetire <- DB.queryPoolRetireCount
  poolRelay <- DB.queryPoolRelayCount
  pure (poolHash, poolMetadataRef, poolUpdate, poolOwner, poolRetire, poolRelay)

addPoolCounters :: Num a => (a, a, a, a, a, a) -> (a, a, a, a, a, a) -> (a, a, a, a, a, a)
addPoolCounters (a, b, c, d, e, f) (a', b', c', d', e', f') = (a + a', b + b', c + c', d + d', e + e', f + f')

assertPoolLayerCounters ::
  EraCertState era =>
  DBSyncEnv ->
  (Word64, Word64) ->
  [(PoolIndex, (Either DBFail Bool, Bool, Bool))] ->
  LedgerState (ShelleyBlock p era) ->
  IO ()
assertPoolLayerCounters env (expectedRetired, expectedDelisted) expResults st = do
  poolLayer <- getPoolLayer env
  retiredPools <- dlGetRetiredPools poolLayer
  assertEqual "Unexpected retired pools counter" (Right expectedRetired) (fromIntegral . length <$> retiredPools)

  delistedPools <- dlGetDelistedPools poolLayer
  assertEqual "Unexpected delisted pools counter" expectedDelisted (fromIntegral $ length delistedPools)

  forM_ expResults $ \(poolIndex, expected) -> do
    let poolKeyHash = resolvePool poolIndex st
    let poolHashBs = unKeyHashRaw poolKeyHash
    let servantPoolId = toDbPoolId poolHashBs
    isRetired <- dlCheckRetiredPool poolLayer servantPoolId
    isDelisted <- dlCheckDelistedPool poolLayer servantPoolId
    isGetPool <- isRight <$> dlGetPool poolLayer servantPoolId
    assertEqual ("Unexpected result for pool " ++ show servantPoolId) expected (isRetired, isDelisted, isGetPool)
