{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Db.Mock.Validate (
  assertBlocksCount,
  assertBlocksCountDetailed,
  assertTxCount,
  assertUnspentTx,
  assertRewardCount,
  assertBlockNoBackoff,
  assertBlockNoBackoffTimes,
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
) where

import Cardano.Db
import qualified Cardano.Db as DB
import Cardano.DbSync.Era.Shelley.Generic.Util
import qualified Cardano.Ledger.Address as Ledger
import Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era
import Cardano.Mock.Forging.Tx.Generic
import Cardano.Mock.Forging.Types
import Cardano.SMASH.Server.PoolDataLayer
import Cardano.SMASH.Server.Types
import Control.Concurrent
import Control.Exception
import Control.Monad (forM_)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Bifunctor (bimap, first)
import Data.ByteString (ByteString)
import Data.Either (isRight)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding
import Data.Word (Word64)
import Database.Esqueleto.Legacy (
  InnerJoin (..),
  SqlExpr,
  countRows,
  from,
  on,
  select,
  unValue,
  val,
  where_,
  (==.),
  (^.),
 )
import Database.Persist.Sql (Entity, SqlBackend, entityVal)
import Database.PostgreSQL.Simple (SqlError (..))
import Ouroboros.Consensus.Cardano.Block
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import Test.Cardano.Db.Mock.Config
import Test.Tasty.HUnit (assertEqual, assertFailure)

{- HLINT ignore "Reduce duplication" -}

assertBlocksCount :: DBSyncEnv -> Word -> IO ()
assertBlocksCount env n = do
  assertEqBackoff env queryBlockCount n defaultDelays "Unexpected block count"

assertBlocksCountDetailed :: DBSyncEnv -> Word -> [Int] -> IO ()
assertBlocksCountDetailed env n delays = do
  assertEqBackoff env queryBlockCount n delays "Unexpected block count"

assertTxCount :: DBSyncEnv -> Word -> IO ()
assertTxCount env n = do
  assertEqBackoff env queryTxCount n defaultDelays "Unexpected tx count"

assertRewardCount :: DBSyncEnv -> Word64 -> IO ()
assertRewardCount env n =
  assertEqBackoff env queryRewardCount n defaultDelays "Unexpected rewards count"

assertBlockNoBackoff :: DBSyncEnv -> Int -> IO ()
assertBlockNoBackoff = assertBlockNoBackoffTimes defaultDelays

assertBlockNoBackoffTimes :: [Int] -> DBSyncEnv -> Int -> IO ()
assertBlockNoBackoffTimes times env blockNo =
  assertEqBackoff env DB.queryBlockHeight (Just $ fromIntegral blockNo) times "Unexpected BlockNo"

-- checking that unspent count matches from tx_in to tx_out
assertUnspentTx :: DBSyncEnv -> IO ()
assertUnspentTx syncEnv = do
  unspentTxCount <- queryDBSync syncEnv DB.queryTxOutConsumedNullCount
  consumedNullCount <- queryDBSync syncEnv DB.queryTxOutUnspentCount
  assertEqual "Unexpected tx unspent count between tx-in & tx-out" unspentTxCount consumedNullCount

defaultDelays :: [Int]
defaultDelays = [1, 2, 4, 8, 16, 32, 64, 128, 256]

assertEqQuery :: (Eq a, Show a) => DBSyncEnv -> ReaderT SqlBackend (NoLoggingT IO) a -> a -> String -> IO ()
assertEqQuery env query a msg = do
  assertEqBackoff env query a defaultDelays msg

assertEqBackoff :: (Eq a, Show a) => DBSyncEnv -> ReaderT SqlBackend (NoLoggingT IO) a -> a -> [Int] -> String -> IO ()
assertEqBackoff env query a delays msg = do
  checkStillRuns env
  assertBackoff env query delays (== a) (\a' -> msg <> ": got " <> show a' <> " expected " <> show a)

assertBackoff :: DBSyncEnv -> ReaderT SqlBackend (NoLoggingT IO) a -> [Int] -> (a -> Bool) -> (a -> String) -> IO ()
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

assertQuery :: DBSyncEnv -> ReaderT SqlBackend (NoLoggingT IO) a -> (a -> Bool) -> (a -> String) -> IO (Maybe String)
assertQuery env query check errMsg = do
  ma <- try $ queryDBSync env query
  case ma of
    Left sqlErr | migrationNotDoneYet (decodeUtf8 $ sqlErrorMsg sqlErr) -> do
      threadDelay 1_000_000
      pure $ Just $ show sqlErr
    Left err -> throwIO err
    Right a | not (check a) -> pure $ Just $ errMsg a
    _ -> pure Nothing

runQuery :: DBSyncEnv -> ReaderT SqlBackend (NoLoggingT IO) a -> IO a
runQuery env query = do
  ma <- try $ queryDBSync env query
  case ma of
    Left sqlErr | migrationNotDoneYet (decodeUtf8 $ sqlErrorMsg sqlErr) -> do
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
    q = queryCurrentEpochNo

assertAddrValues ::
  (EraCrypto era ~ StandardCrypto, Core.EraTxOut era) =>
  DBSyncEnv ->
  UTxOIndex era ->
  DbLovelace ->
  LedgerState (ShelleyBlock p era) ->
  IO ()
assertAddrValues env ix expected sta = do
  addr <- assertRight $ resolveAddress ix sta
  let addrBs = Ledger.serialiseAddr addr
      q = queryAddressOutputs addrBs
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
      registr <-
        maybe 0 unValue . listToMaybe
          <$> (select . from $ \(_a :: SqlExpr (Entity StakeRegistration)) -> pure countRows)
      deregistr <-
        maybe 0 unValue . listToMaybe
          <$> (select . from $ \(_a :: SqlExpr (Entity StakeDeregistration)) -> pure countRows)
      deleg <-
        maybe 0 unValue . listToMaybe
          <$> (select . from $ \(_a :: SqlExpr (Entity Delegation)) -> pure countRows)
      withdrawal <-
        maybe 0 unValue . listToMaybe
          <$> (select . from $ \(_a :: SqlExpr (Entity Withdrawal)) -> pure countRows)
      -- We deduct the initial registration and delegation in the genesis
      pure (registr - 5, deregistr, deleg - 5, withdrawal)

assertRewardCounts ::
  EraCrypto era ~ StandardCrypto =>
  DBSyncEnv ->
  LedgerState (ShelleyBlock p era) ->
  Bool ->
  Maybe Word64 ->
  [(StakeIndex, (Word64, Word64, Word64, Word64, Word64))] ->
  IO ()
assertRewardCounts env st filterAddr mEpoch expected = do
  assertEqBackoff env (groupByAddress <$> q) expectedMap defaultDelays "Unexpected rewards count"
  where
    expectedMap :: Map ByteString (Word64, Word64, Word64, Word64, Word64)
    expectedMap = Map.fromList $ fmap (first mkDBStakeAddress) expected

    groupByAddress :: [(Reward, ByteString)] -> Map ByteString (Word64, Word64, Word64, Word64, Word64)
    groupByAddress rewards =
      let res = foldr updateMap Map.empty rewards
       in if filterAddr
            then Map.filterWithKey (\k _ -> Map.member k expectedMap) res
            else res

    mkDBStakeAddress :: StakeIndex -> ByteString
    mkDBStakeAddress stIx = case resolveStakeCreds stIx st of
      Left _ -> error "could not resolve StakeIndex"
      Right cred -> Ledger.serialiseRewardAcnt $ Ledger.RewardAcnt Testnet cred

    updateAddrCounters ::
      Reward ->
      Maybe (Word64, Word64, Word64, Word64, Word64) ->
      (Word64, Word64, Word64, Word64, Word64)
    updateAddrCounters reward Nothing = updateCounters reward (0, 0, 0, 0, 0)
    updateAddrCounters reward (Just cs) = updateCounters reward cs

    updateCounters ::
      Reward ->
      (Word64, Word64, Word64, Word64, Word64) ->
      (Word64, Word64, Word64, Word64, Word64)
    updateCounters reward (a, b, c, d, e) = case rewardType reward of
      RwdLeader -> (a + 1, b, c, d, e)
      RwdMember -> (a, b + 1, c, d, e)
      RwdReserves -> (a, b, c + 1, d, e)
      RwdTreasury -> (a, b, c, d + 1, e)
      RwdDepositRefund -> (a, b, c, d, e + 1)

    updateMap ::
      (Reward, ByteString) ->
      Map ByteString (Word64, Word64, Word64, Word64, Word64) ->
      Map ByteString (Word64, Word64, Word64, Word64, Word64)
    updateMap (rew, addr) = Map.alter (Just . updateAddrCounters rew) addr

    filterEpoch rw = case mEpoch of
      Nothing -> val True
      Just e -> rw ^. RewardSpendableEpoch ==. val e

    q = do
      res <- select . from $ \(reward `InnerJoin` stake_addr) -> do
        on (reward ^. RewardAddrId ==. stake_addr ^. StakeAddressId)
        where_ (filterEpoch reward)
        pure (reward, stake_addr ^. StakeAddressHashRaw)
      pure $ fmap (bimap entityVal unValue) res

assertEpochStake :: DBSyncEnv -> Word64 -> IO ()
assertEpochStake env expected =
  assertEqBackoff env q expected defaultDelays "Unexpected epoch stake counts"
  where
    q =
      maybe 0 unValue . listToMaybe
        <$> (select . from $ \(_a :: SqlExpr (Entity EpochStake)) -> pure countRows)

assertEpochStakeEpoch :: DBSyncEnv -> Word64 -> Word64 -> IO ()
assertEpochStakeEpoch env e expected =
  assertEqBackoff env q expected defaultDelays "Unexpected epoch stake counts"
  where
    q =
      maybe 0 unValue . listToMaybe
        <$> ( select . from $ \(a :: SqlExpr (Entity EpochStake)) -> do
                where_ (a ^. EpochStakeEpochNo ==. val e)
                pure countRows
            )

assertNonZeroFeesContract :: DBSyncEnv -> IO ()
assertNonZeroFeesContract env =
  assertEqBackoff env q 0 defaultDelays "Found contract tx with zero fees"
  where
    q :: ReaderT SqlBackend (NoLoggingT IO) Word64
    q =
      maybe 0 unValue . listToMaybe
        <$> ( select . from $ \tx -> do
                where_ (tx ^. TxFee ==. val (DbLovelace 0))
                where_ (tx ^. TxValidContract ==. val False)
                pure countRows
            )

assertDatumCBOR :: DBSyncEnv -> ByteString -> IO ()
assertDatumCBOR env bs =
  assertEqBackoff env q 1 defaultDelays "Datum bytes not found"
  where
    q :: ReaderT SqlBackend (NoLoggingT IO) Word64
    q =
      maybe 0 unValue . listToMaybe
        <$> ( select . from $ \datum -> do
                where_ (datum ^. DatumBytes ==. val bs)
                pure countRows
            )

assertAlonzoCounts :: DBSyncEnv -> (Word64, Word64, Word64, Word64, Word64, Word64, Word64, Word64) -> IO ()
assertAlonzoCounts env expected =
  assertEqBackoff env q expected defaultDelays "Unexpected Alonzo counts"
  where
    q = do
      scripts <-
        maybe 0 unValue . listToMaybe
          <$> (select . from $ \(_a :: SqlExpr (Entity Script)) -> pure countRows)
      redeemers <-
        maybe 0 unValue . listToMaybe
          <$> (select . from $ \(_a :: SqlExpr (Entity Redeemer)) -> pure countRows)
      datums <-
        maybe 0 unValue . listToMaybe
          <$> (select . from $ \(_a :: SqlExpr (Entity Datum)) -> pure countRows)
      colInputs <-
        maybe 0 unValue . listToMaybe
          <$> (select . from $ \(_a :: SqlExpr (Entity CollateralTxIn)) -> pure countRows)
      scriptOutputs <- fromIntegral . length <$> queryScriptOutputs
      redeemerTxIn <- fromIntegral . length <$> queryTxInRedeemer
      invalidTx <- fromIntegral . length <$> queryInvalidTx
      txIninvalidTx <- fromIntegral . length <$> queryTxInFailedTx

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
      scripts <-
        maybe 0 unValue . listToMaybe
          <$> (select . from $ \(_a :: SqlExpr (Entity Script)) -> pure countRows)
      redeemers <-
        maybe 0 unValue . listToMaybe
          <$> (select . from $ \(_a :: SqlExpr (Entity Redeemer)) -> pure countRows)
      datums <-
        maybe 0 unValue . listToMaybe
          <$> (select . from $ \(_a :: SqlExpr (Entity Datum)) -> pure countRows)
      colInputs <-
        maybe 0 unValue . listToMaybe
          <$> (select . from $ \(_a :: SqlExpr (Entity CollateralTxIn)) -> pure countRows)
      scriptOutputs <- fromIntegral . length <$> queryScriptOutputs
      redeemerTxIn <- fromIntegral . length <$> queryTxInRedeemer
      invalidTx <- fromIntegral . length <$> queryInvalidTx
      txIninvalidTx <- fromIntegral . length <$> queryTxInFailedTx
      redeemerData <-
        maybe 0 unValue . listToMaybe
          <$> (select . from $ \(_a :: SqlExpr (Entity RedeemerData)) -> pure countRows)
      referenceTxIn <-
        maybe 0 unValue . listToMaybe
          <$> (select . from $ \(_a :: SqlExpr (Entity ReferenceTxIn)) -> pure countRows)
      collTxOut <-
        maybe 0 unValue . listToMaybe
          <$> (select . from $ \(_a :: SqlExpr (Entity CollateralTxOut)) -> pure countRows)
      inlineDatum <-
        maybe 0 unValue . listToMaybe
          <$> (select . from $ \txOut -> where_ (isJust (txOut ^. TxOutInlineDatumId)) >> pure countRows)
      referenceScript <-
        maybe 0 unValue . listToMaybe
          <$> (select . from $ \txOut -> where_ (isJust (txOut ^. TxOutReferenceScriptId)) >> pure countRows)
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
      deregistrScript <- fromIntegral . length <$> queryDeregistrationScript
      delegScript <- fromIntegral . length <$> queryDelegationScript
      withdrawalScript <- fromIntegral . length <$> queryWithdrawalScript
      stakeAddressScript <- fromIntegral . length <$> queryStakeAddressScript
      pure (deregistrScript, delegScript, withdrawalScript, stakeAddressScript)

assertPoolCounters :: DBSyncEnv -> (Word64, Word64, Word64, Word64, Word64, Word64) -> IO ()
assertPoolCounters env expected =
  assertEqBackoff env poolCountersQuery expected defaultDelays "Unexpected Pool counts"

poolCountersQuery :: ReaderT SqlBackend (NoLoggingT IO) (Word64, Word64, Word64, Word64, Word64, Word64)
poolCountersQuery = do
  poolHash <-
    maybe 0 unValue . listToMaybe
      <$> (select . from $ \(_a :: SqlExpr (Entity PoolHash)) -> pure countRows)
  poolMetadataRef <-
    maybe 0 unValue . listToMaybe
      <$> (select . from $ \(_a :: SqlExpr (Entity PoolMetadataRef)) -> pure countRows)
  poolUpdate <-
    maybe 0 unValue . listToMaybe
      <$> (select . from $ \(_a :: SqlExpr (Entity PoolUpdate)) -> pure countRows)
  poolOwner <-
    maybe 0 unValue . listToMaybe
      <$> (select . from $ \(_a :: SqlExpr (Entity PoolOwner)) -> pure countRows)
  poolRetire <-
    maybe 0 unValue . listToMaybe
      <$> (select . from $ \(_a :: SqlExpr (Entity PoolRetire)) -> pure countRows)
  poolRelay <-
    maybe 0 unValue . listToMaybe
      <$> (select . from $ \(_a :: SqlExpr (Entity PoolRelay)) -> pure countRows)
  pure (poolHash, poolMetadataRef, poolUpdate, poolOwner, poolRetire, poolRelay)

addPoolCounters :: Num a => (a, a, a, a, a, a) -> (a, a, a, a, a, a) -> (a, a, a, a, a, a)
addPoolCounters (a, b, c, d, e, f) (a', b', c', d', e', f') = (a + a', b + b', c + c', d + d', e + e', f + f')

assertPoolLayerCounters ::
  EraCrypto era ~ StandardCrypto =>
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
    let servantPoolId = dbToServantPoolId poolHashBs
    isRetired <- dlCheckRetiredPool poolLayer servantPoolId
    isDelisted <- dlCheckDelistedPool poolLayer servantPoolId
    isGetPool <- isRight <$> dlGetPool poolLayer servantPoolId
    assertEqual ("Unexpected result for pool " ++ show servantPoolId) expected (isRetired, isDelisted, isGetPool)
