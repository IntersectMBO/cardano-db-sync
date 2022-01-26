{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Db.Mock.Validate where

import           Cardano.Db
import           Control.Concurrent
import           Control.Exception
import           Control.Monad (forM_)
import           Control.Monad.Logger (NoLoggingT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Bifunctor (bimap, first)
import           Data.ByteString (ByteString)
import           Data.Either (isRight)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding
import           Data.Word (Word64)
import           GHC.Records (HasField (..))

import           Database.Esqueleto.Legacy (InnerJoin (..), SqlExpr, countRows, from, on, select,
                   unValue, (==.), (^.))
import           Database.Persist.Sql (Entity, SqlBackend, entityVal)
import           Database.PostgreSQL.Simple (SqlError (..))

import qualified Cardano.Ledger.Address as Ledger
import           Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.Core as Core
import           Cardano.Ledger.Era

import           Cardano.DbSync.Era.Shelley.Generic.Util

import           Cardano.SMASH.Server.PoolDataLayer

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)

import           Cardano.Mock.Forging.Tx.Generic
import           Cardano.Mock.Forging.Types

import           Test.Cardano.Db.Mock.Config

import           Test.Tasty.HUnit

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

assertBlockNoBackoff :: DBSyncEnv -> Word64 -> IO ()
assertBlockNoBackoff env blockNo =
    assertEqBackoff env queryBlockHeight (Just blockNo) defaultDelays "Unexpected BlockNo"

defaultDelays :: [Int]
defaultDelays = [1,2,4,8,16,32,64,128]

assertEqQuery :: (Eq a, Show a) => DBSyncEnv -> ReaderT SqlBackend (NoLoggingT IO) a -> a -> String -> IO ()
assertEqQuery env query a msg = do
    assertEqBackoff env query a [] msg

assertEqBackoff :: (Eq a, Show a) => DBSyncEnv -> ReaderT SqlBackend (NoLoggingT IO) a -> a -> [Int] -> String -> IO ()
assertEqBackoff env query a delays msg = do
    checkStillRuns env
    assertBackoff env query delays (== a) (\a' -> msg <> ": " <> show a' <> " /= " <> show a)

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
    Left sqlErr | migrationNotDoneYet (decodeUtf8 $ sqlErrorMsg sqlErr) -> pure $ Just $ show sqlErr
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
migrationNotDoneYet txt =
    Text.isPrefixOf "relation" txt && Text.isSuffixOf "does not exist" txt

assertAddrValues :: (Crypto era ~ StandardCrypto, HasField "address" (Core.TxOut era) (Ledger.Addr (Crypto era)))
                 => DBSyncEnv -> UTxOIndex era -> DbLovelace
                 -> LedgerState (ShelleyBlock era) -> IO ()
assertAddrValues env ix expected sta = do
    addr <- assertRight $ resolveAddress ix sta
    let addrBs = Ledger.serialiseAddr addr
        q = queryAddressOutputs addrBs
    assertEqBackoff env q expected defaultDelays "Unexpected Balance"

assertRight :: Show err => Either err a -> IO a
assertRight ei = case ei of
  Right a -> pure a
  Left err -> assertFailure (show err)

assertCertCounts :: DBSyncEnv -> (Word64, Word64, Word64, Word64) -> IO ()
assertCertCounts env expected =
    assertEqBackoff env q expected defaultDelays "Unexpected Cert counts"
  where
    q = do
      registr <- maybe 0 unValue . listToMaybe <$>
                    (select . from $ \(_a :: SqlExpr (Entity StakeRegistration)) -> pure countRows)
      deregistr <- maybe 0 unValue . listToMaybe <$>
                    (select . from $ \(_a :: SqlExpr (Entity StakeDeregistration)) -> pure countRows)
      deleg <- maybe 0 unValue . listToMaybe <$>
                    (select . from $ \(_a :: SqlExpr (Entity Delegation)) -> pure countRows)
      withdrawal <- maybe 0 unValue . listToMaybe <$>
                    (select . from $ \(_a :: SqlExpr (Entity Withdrawal)) -> pure countRows)
      -- We deduct the initial delegation in the genesis
      pure (registr, deregistr, deleg - 5, withdrawal)

assertRewardCounts :: (Crypto era ~ StandardCrypto)
                   => DBSyncEnv -> LedgerState (ShelleyBlock era) -> Bool
                   -> [(StakeIndex, (Word64, Word64, Word64, Word64, Word64))] -> IO ()
assertRewardCounts env st filterAddr expected = do
    assertEqBackoff env (groupByAddress <$> q) expectedMap defaultDelays "Unexpected rewards count"
  where
    expectedMap :: Map ByteString (Word64, Word64, Word64, Word64, Word64)
    expectedMap = Map.fromList $ fmap (first mkDBStakeAddress) expected

    groupByAddress :: [(Reward, ByteString)] -> Map ByteString (Word64, Word64, Word64, Word64, Word64)
    groupByAddress rewards =
      let res = foldr updateMap Map.empty rewards
      in if filterAddr then Map.filterWithKey (\k _ -> Map.member k expectedMap) res
         else res

    mkDBStakeAddress :: StakeIndex -> ByteString
    mkDBStakeAddress stIx = case resolveStakeCreds stIx st of
        Left _ -> error "could not resolve StakeIndex"
        Right cred -> Ledger.serialiseRewardAcnt $ Ledger.RewardAcnt Testnet cred

    updateAddrCounters :: Reward
                       -> Maybe (Word64, Word64, Word64, Word64, Word64)
                       -> (Word64, Word64, Word64, Word64, Word64)
    updateAddrCounters reward Nothing = updateCounters reward (0,0,0,0,0)
    updateAddrCounters reward (Just cs) = updateCounters reward cs

    updateCounters :: Reward
                   -> (Word64, Word64, Word64, Word64, Word64)
                   -> (Word64, Word64, Word64, Word64, Word64)
    updateCounters reward (a,b,c,d,e) = case rewardType reward of
        RwdLeader ->        (a+1, b,   c  , d   , e  )
        RwdMember ->        (a  , b+1, c  , d   , e  )
        RwdReserves ->      (a  , b,   c+1, d   , e  )
        RwdTreasury ->      (a  , b,   c  , d+1 , e  )
        RwdDepositRefund -> (a  , b,   c  , d   , e+1)

    updateMap :: (Reward, ByteString)
              -> Map ByteString (Word64, Word64, Word64, Word64, Word64)
              -> Map ByteString (Word64, Word64, Word64, Word64, Word64)
    updateMap (rew, addr) res = Map.alter (Just . updateAddrCounters rew) addr res

    q = do
      res <- select . from $ \ (reward `InnerJoin` stake_addr) -> do
                on (reward ^. RewardAddrId ==. stake_addr ^. StakeAddressId)
                pure (reward, stake_addr ^. StakeAddressHashRaw)
      pure $ fmap (bimap entityVal unValue) res


assertAlonzoCounts :: DBSyncEnv -> (Word64, Word64, Word64, Word64, Word64, Word64, Word64, Word64) -> IO ()
assertAlonzoCounts env expected =
    assertEqBackoff env q expected defaultDelays "Unexpected Alonzo counts"
  where
    q = do
      scripts <- maybe 0 unValue . listToMaybe <$>
                    (select . from $ \(_a :: SqlExpr (Entity Script)) -> pure countRows)
      redeemers <- maybe 0 unValue . listToMaybe <$>
                    (select . from $ \(_a :: SqlExpr (Entity Redeemer)) -> pure countRows)
      datums <- maybe 0 unValue . listToMaybe <$>
                    (select . from $ \(_a :: SqlExpr (Entity Datum)) -> pure countRows)
      colInputs <- maybe 0 unValue . listToMaybe <$>
              (select . from $ \(_a :: SqlExpr (Entity CollateralTxIn)) -> pure countRows)
      scriptOutputs <- fromIntegral . length <$> queryScriptOutputs
      redeemerTxIn <- fromIntegral . length <$> queryTxInRedeemer
      invalidTx <- fromIntegral . length <$> queryInvalidTx
      txIninvalidTx <- fromIntegral . length <$> queryTxInFailedTx

      pure ( scripts, redeemers, datums, colInputs, scriptOutputs, redeemerTxIn, invalidTx
           , txIninvalidTx)

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
      poolHash <- maybe 0 unValue . listToMaybe <$>
                      (select . from $ \(_a :: SqlExpr (Entity PoolHash)) -> pure countRows)
      poolMetadataRef <- maybe 0 unValue . listToMaybe <$>
                  (select . from $ \(_a :: SqlExpr (Entity PoolMetadataRef)) -> pure countRows)
      poolUpdate <- maybe 0 unValue . listToMaybe <$>
                  (select . from $ \(_a :: SqlExpr (Entity PoolUpdate)) -> pure countRows)
      poolOwner <- maybe 0 unValue . listToMaybe <$>
                  (select . from $ \(_a :: SqlExpr (Entity PoolOwner)) -> pure countRows)
      poolRetire <- maybe 0 unValue . listToMaybe <$>
                  (select . from $ \(_a :: SqlExpr (Entity PoolRetire)) -> pure countRows)
      poolRelay <- maybe 0 unValue . listToMaybe <$>
                  (select . from $ \(_a :: SqlExpr (Entity PoolRelay)) -> pure countRows)
      pure (poolHash, poolMetadataRef, poolUpdate, poolOwner, poolRetire, poolRelay)

addPoolCounters :: Num a => (a,a,a,a,a,a) -> (a,a,a,a,a,a) -> (a,a,a,a,a,a)
addPoolCounters (a,b,c,d,e,f) (a',b',c',d',e',f') = (a + a',b + b',c + c',d + d',e + e',f + f')

assertPoolLayerCounters :: Crypto era ~ StandardCrypto
                        => DBSyncEnv -> (Word64, Word64)
                        -> [(PoolIndex, (Bool, Bool, Bool))]
                        -> LedgerState (ShelleyBlock era)
                        -> IO ()
assertPoolLayerCounters env (expectedRetired, expectedDelisted) expResults st = do
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
  where
    poolLayer = getPoolLayer env


