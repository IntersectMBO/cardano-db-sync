{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Db.Mock.Unit.Alonzo.Reward (
  simpleRewards,
  rewardsDeregistration,
  rewardsReregistration,
  mirReward,
  mirRewardRollback,
  mirRewardDereg,
  rollbackBoundary,
  singleMIRCertMultiOut,
) where

import Cardano.Ledger.Coin (Coin (Coin), DeltaCoin (DeltaCoin))
import Cardano.Ledger.Keys (KeyHash (KeyHash))
import Cardano.Ledger.Shelley.TxBody (Withdrawals (Withdrawals))
import Cardano.Ledger.Shelley.TxCert
import Cardano.Mock.ChainSync.Server (IOManager, addBlock, rollback)
import qualified Cardano.Mock.Forging.Tx.Alonzo as Alonzo
import Cardano.Mock.Forging.Tx.Alonzo.Scenarios (delegateAndSendBlocks)
import Cardano.Mock.Forging.Tx.Generic (resolvePool, resolveStakeCreds)
import Cardano.Mock.Forging.Types (
  PoolIndex (PoolIndex, PoolIndexId),
  StakeIndex (
    StakeIndex,
    StakeIndexNew,
    StakeIndexPoolLeader,
    StakeIndexPoolMember
  ),
  UTxOIndex (UTxOAddressNewWithStake, UTxOIndex),
 )
import Control.Concurrent.Class.MonadSTM.Strict (MonadSTM (atomically))
import Control.Monad (forM_, void)
import qualified Data.Map as Map
import Data.Text (Text)
import Ouroboros.Network.Block (blockPoint)
import Test.Cardano.Db.Mock.Config (
  alonzoConfigDir,
  startDBSync,
  stopDBSync,
  withFullConfig,
 )
import Test.Cardano.Db.Mock.UnifiedApi (
  fillEpochPercentage,
  fillEpochs,
  fillUntilNextEpoch,
  forgeAndSubmitBlocks,
  getAlonzoLedgerState,
  registerAllStakeCreds,
  skipUntilNextEpoch,
  withAlonzoFindLeaderAndSubmit,
  withAlonzoFindLeaderAndSubmitTx,
 )
import Test.Cardano.Db.Mock.Validate (assertBlockNoBackoff, assertRewardCount, assertRewardCounts)
import Test.Tasty.HUnit (Assertion)

simpleRewards :: IOManager -> [(Text, Text)] -> Assertion
simpleRewards =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    -- Pools are not registered yet, this takes 2 epochs. So fees of this tx
    -- should not create any rewards.
    void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ Alonzo.mkPaymentTx (UTxOIndex 2) (UTxOIndex 1) 10000 10000

    a <- fillEpochs interpreter mockServer 3
    assertBlockNoBackoff dbSync (fromIntegral $ 2 + length a)

    -- The pool leaders take leader rewards with value 0
    assertRewardCount dbSync 3

    st <- getAlonzoLedgerState interpreter
    -- False indicates that we provide the full expected list of addresses with rewards.
    assertRewardCounts
      dbSync
      st
      False
      (Just 3)
      [ (StakeIndexPoolLeader (PoolIndex 0), (1, 0, 0, 0, 0))
      , (StakeIndexPoolLeader (PoolIndex 1), (1, 0, 0, 0, 0))
      , (StakeIndexPoolLeader (PoolIndex 2), (1, 0, 0, 0, 0))
      ]

    -- Now that pools are registered, we add a tx to fill the fees pot.
    -- Rewards will be distributed.
    void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ Alonzo.mkPaymentTx (UTxOIndex 2) (UTxOIndex 1) 10000 10000

    b <- fillEpochs interpreter mockServer 2

    assertBlockNoBackoff dbSync (fromIntegral $ 1 + length a + 2 + length b)
    assertRewardCount dbSync 14
    assertRewardCounts
      dbSync
      st
      True
      (Just 5)
      -- 2 pool leaders also delegate to pools.
      [ (StakeIndexPoolLeader (PoolIndexId $ KeyHash "9f1b441b9b781b3c3abb43b25679dc17dbaaf116dddca1ad09dc1de0"), (1, 0, 0, 0, 0))
      , (StakeIndexPoolLeader (PoolIndexId $ KeyHash "5af582399de8c226391bfd21424f34d0b053419c4d93975802b7d107"), (1, 1, 0, 0, 0))
      , (StakeIndexPoolLeader (PoolIndexId $ KeyHash "58eef2925db2789f76ea057c51069e52c5e0a44550f853c6cdf620f8"), (1, 1, 0, 0, 0))
      , (StakeIndexPoolMember 0 (PoolIndex 0), (0, 1, 0, 0, 0))
      , (StakeIndexPoolMember 0 (PoolIndex 1), (0, 1, 0, 0, 0))
      ]
  where
    testLabel = "simpleRewards-alonzo"

rewardsDeregistration :: IOManager -> [(Text, Text)] -> Assertion
rewardsDeregistration =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkDepositTxPools (UTxOIndex 1) 20000

    -- first move to treasury from reserves
    void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
      Alonzo.mkDCertTx [ShelleyTxCertMir $ MIRCert ReservesMIR (SendToOppositePotMIR (Coin 100000))] (Withdrawals mempty)

    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      -- register the stake address and delegate to a pool
      let poolId = resolvePool (PoolIndex 0) st
      tx1 <-
        Alonzo.mkSimpleDCertTx
          [ (StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)
          , (StakeIndexNew 1, \stCred -> ShelleyTxCertDelegCert $ ShelleyDelegCert stCred poolId)
          ]
          st
      -- send some funds to the address so
      tx2 <- Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOAddressNewWithStake 0 (StakeIndexNew 1)) 100000 5000 st
      Right [tx1, tx2]

    a <- fillEpochs interpreter mockServer 3
    assertBlockNoBackoff dbSync (fromIntegral $ 3 + length a)

    st <- getAlonzoLedgerState interpreter

    -- Now that pools are registered, we add a tx to fill the fees pot.
    -- Rewards will be distributed.
    void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000

    b <- fillEpochs interpreter mockServer 2

    assertBlockNoBackoff dbSync (fromIntegral $ 4 + length a + length b)
    assertRewardCounts dbSync st True Nothing [(StakeIndexNew 1, (0, 1, 0, 0, 0))]

    void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ Alonzo.mkPaymentTx (UTxOIndex 1) (UTxOIndex 0) 10000 10000

    c <- fillEpochs interpreter mockServer 2

    assertBlockNoBackoff dbSync (fromIntegral $ 5 + length a + length b + length c)
    assertRewardCounts dbSync st True Nothing [(StakeIndexNew 1, (0, 2, 0, 0, 0))]

    d <- fillEpochs interpreter mockServer 1
    e <- fillEpochPercentage interpreter mockServer 85
    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyUnRegCert)]

    f <- fillUntilNextEpoch interpreter mockServer

    assertBlockNoBackoff dbSync (fromIntegral $ 6 + length (a <> b <> c <> d <> e <> f))
    -- stays at 2, since it's deregistered.
    assertRewardCounts dbSync st True Nothing [(StakeIndexNew 1, (0, 2, 0, 0, 0))]

    g <- fillEpochs interpreter mockServer 2
    assertBlockNoBackoff dbSync (fromIntegral $ 6 + length (a <> b <> c <> d <> e <> f <> g))
    assertRewardCounts dbSync st True Nothing [(StakeIndexNew 1, (0, 2, 0, 0, 0))]
  where
    testLabel = "rewardsDeregistration-alonzo"

-- This is a fix of the reward issue fix in Babbage described in the Babbage specs
-- If a stake address is deregistered during the reward computation initialisation,
-- and is registered later it doesn't receive rewards before Babbage. It does receive
-- on Babbage. See the same test on Alonzo.
rewardsReregistration :: IOManager -> [(Text, Text)] -> Assertion
rewardsReregistration =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkDepositTxPools (UTxOIndex 1) 20000

    -- first move to treasury from reserves
    void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
      Alonzo.mkDCertTx [ShelleyTxCertMir $ MIRCert ReservesMIR (SendToOppositePotMIR (Coin 100000))] (Withdrawals mempty)

    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      -- register the stake address and delegate to a pool
      let poolId = resolvePool (PoolIndex 0) st
      tx1 <-
        Alonzo.mkSimpleDCertTx
          [ (StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)
          , (StakeIndexNew 1, \stCred -> ShelleyTxCertDelegCert $ ShelleyDelegCert stCred poolId)
          ]
          st
      -- send some funds to the address so
      tx2 <- Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOAddressNewWithStake 0 (StakeIndexNew 1)) 100000 5000 st
      Right [tx1, tx2]

    a <- fillEpochs interpreter mockServer 3
    assertBlockNoBackoff dbSync (fromIntegral $ 3 + length a)

    st <- getAlonzoLedgerState interpreter

    -- Now that pools are registered, we add a tx to fill the fees pot.
    -- Rewards will be distributed.
    void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000

    b <- fillEpochs interpreter mockServer 2

    assertBlockNoBackoff dbSync (fromIntegral $ 4 + length a + length b)
    assertRewardCounts dbSync st True Nothing [(StakeIndexNew 1, (0, 1, 0, 0, 0))]

    void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ Alonzo.mkPaymentTx (UTxOIndex 1) (UTxOIndex 0) 10000 10000

    b' <- fillEpochs interpreter mockServer 1
    c <- fillEpochPercentage interpreter mockServer 10
    -- deregister before the 40% of the epoch
    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyUnRegCert)]
    d <- fillEpochPercentage interpreter mockServer 60
    -- register after 40% and before epoch boundary.
    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)]
    e <- fillUntilNextEpoch interpreter mockServer
    assertBlockNoBackoff dbSync (fromIntegral $ 7 + length (a <> b <> b' <> c <> d <> e))
    -- This is 2 in Babbage
    assertRewardCounts dbSync st True Nothing [(StakeIndexNew 1, (0, 1, 0, 0, 0))]
  where
    testLabel = "rewardsReregistration-Alonzo"

mirReward :: IOManager -> [(Text, Text)] -> Assertion
mirReward =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    -- first move to treasury from reserves
    void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
      Alonzo.mkDCertTx [ShelleyTxCertMir $ MIRCert ReservesMIR (SendToOppositePotMIR (Coin 100000))] (Withdrawals mempty)

    void $ fillEpochPercentage interpreter mockServer 50

    -- mir from treasury
    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx1 <-
        Alonzo.mkSimpleDCertTx
          [
            ( StakeIndex 1
            , \cred -> ShelleyTxCertMir $ MIRCert TreasuryMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 100)))
            )
          ]
          st
      tx2 <-
        Alonzo.mkSimpleDCertTx
          [
            ( StakeIndex 1
            , \cred -> ShelleyTxCertMir $ MIRCert ReservesMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 200)))
            )
          ]
          st
      tx3 <-
        Alonzo.mkSimpleDCertTx
          [
            ( StakeIndex 1
            , \cred -> ShelleyTxCertMir $ MIRCert TreasuryMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 300)))
            )
          ]
          st
      pure [tx1, tx2, tx3]

    void $ fillUntilNextEpoch interpreter mockServer

    st <- getAlonzoLedgerState interpreter
    -- 2 mir rewards from treasury are sumed
    assertRewardCounts dbSync st True Nothing [(StakeIndex 1, (0, 0, 1, 1, 0))]
  where
    testLabel = "mirReward-alonzo"

mirRewardRollback :: IOManager -> [(Text, Text)] -> Assertion
mirRewardRollback =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    -- first move to treasury from reserves
    void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
      Alonzo.mkDCertTx [ShelleyTxCertMir $ MIRCert ReservesMIR (SendToOppositePotMIR (Coin 100000))] (Withdrawals mempty)

    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)]

    a <- fillUntilNextEpoch interpreter mockServer
    b <- fillEpochPercentage interpreter mockServer 5
    -- mir from treasury
    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx
          [
            ( StakeIndexNew 1
            , \cred -> ShelleyTxCertMir $ MIRCert TreasuryMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 1000)))
            )
          ]
    c <- fillEpochPercentage interpreter mockServer 50
    d <- fillUntilNextEpoch interpreter mockServer

    st <- getAlonzoLedgerState interpreter
    assertBlockNoBackoff dbSync (fromIntegral $ 4 + length (a <> b <> c <> d))
    assertRewardCounts dbSync st True Nothing [(StakeIndexNew 1, (0, 0, 0, 1, 0))]

    atomically $ rollback mockServer (blockPoint $ last c)
    assertBlockNoBackoff dbSync (fromIntegral $ 4 + length (a <> b <> c <> d))
    assertRewardCounts dbSync st True Nothing [(StakeIndexNew 1, (0, 0, 0, 1, 0))]
    stopDBSync dbSync
    startDBSync dbSync
    assertBlockNoBackoff dbSync (fromIntegral $ 4 + length (a <> b <> c <> d))
    assertRewardCounts dbSync st True Nothing [(StakeIndexNew 1, (0, 0, 0, 1, 0))]

    forM_ d $ atomically . addBlock mockServer
    e <- fillEpochPercentage interpreter mockServer 5
    assertBlockNoBackoff dbSync (fromIntegral $ 4 + length (a <> b <> c <> d <> e))
    assertRewardCounts dbSync st True Nothing [(StakeIndexNew 1, (0, 0, 0, 1, 0))]
  where
    testLabel = "mirRewardRollback-alonzo"

mirRewardDereg :: IOManager -> [(Text, Text)] -> Assertion
mirRewardDereg =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    -- first move to treasury from reserves
    void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
      Alonzo.mkDCertTx [ShelleyTxCertMir $ MIRCert ReservesMIR (SendToOppositePotMIR (Coin 100000))] (Withdrawals mempty)

    a <- fillUntilNextEpoch interpreter mockServer

    -- mir from treasury
    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx1 <-
        Alonzo.mkSimpleDCertTx
          [
            ( StakeIndex 1
            , \cred -> ShelleyTxCertMir $ MIRCert TreasuryMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 100)))
            )
          ]
          st
      tx2 <-
        Alonzo.mkSimpleDCertTx
          [
            ( StakeIndex 1
            , \cred -> ShelleyTxCertMir $ MIRCert ReservesMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 200)))
            )
          ]
          st
      tx3 <-
        Alonzo.mkSimpleDCertTx
          [
            ( StakeIndex 1
            , \cred -> ShelleyTxCertMir $ MIRCert TreasuryMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 300)))
            )
          ]
          st
      pure [tx1, tx2, tx3]

    b <- fillEpochPercentage interpreter mockServer 20
    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [(StakeIndex 1, ShelleyTxCertDelegCert . ShelleyUnRegCert)]

    assertBlockNoBackoff dbSync (fromIntegral $ 4 + length (a <> b))
    -- deregistration means empty rewards
    st <- getAlonzoLedgerState interpreter
    assertRewardCounts dbSync st False Nothing []
  where
    testLabel = "mirRewardDereg-alonzo"

_rewardsEmptyChainLast :: IOManager -> [(Text, Text)] -> Assertion
_rewardsEmptyChainLast =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    a <- fillEpochs interpreter mockServer 3
    assertRewardCount dbSync 3

    -- Now that pools are registered, we add a tx to fill the fees pot.
    -- Rewards will be distributed.
    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000

    b <- fillUntilNextEpoch interpreter mockServer
    assertRewardCount dbSync 6

    c <- fillEpochPercentage interpreter mockServer 68

    -- Skip a percentage of the epoch epoch
    void $ skipUntilNextEpoch interpreter mockServer []
    d <- fillUntilNextEpoch interpreter mockServer
    assertBlockNoBackoff dbSync (fromIntegral $ 1 + length a + 1 + length b + length c + 1 + length d)
    assertRewardCount dbSync 17
  where
    testLabel = "rewardsEmptyChainLast-alonzo"

_rewardsDelta :: IOManager -> [(Text, Text)] -> Assertion
_rewardsDelta =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    -- These delegation push the computation of the 3 leader
    -- rewards toward the 8k/f slot, so it can be delayed even more
    -- with the missing blocks and create the delta reward.
    -- This trick may break at some point in the future.
    a <- delegateAndSendBlocks 1000 interpreter
    forM_ a $ atomically . addBlock mockServer
    void $ registerAllStakeCreds interpreter mockServer
    b <- fillEpochs interpreter mockServer 3
    assertRewardCount dbSync 3

    c <- fillUntilNextEpoch interpreter mockServer
    assertRewardCount dbSync 6

    d <- fillEpochPercentage interpreter mockServer 68
    assertRewardCount dbSync 6

    -- Skip a percentage of the epoch epoch
    void $ skipUntilNextEpoch interpreter mockServer []
    assertBlockNoBackoff dbSync (fromIntegral $ 1 + length a + length b + length c + 1 + length d)
    -- These are delta rewards aka rewards that were added at the epoch boundary, because the reward
    -- update was not complete on time, due to missing blocks.
    assertRewardCount dbSync 9
  where
    testLabel = "rewardsDelta-alonzo"

rollbackBoundary :: IOManager -> [(Text, Text)] -> Assertion
rollbackBoundary =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer
    a <- fillEpochs interpreter mockServer 2

    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000

    blks <- forgeAndSubmitBlocks interpreter mockServer 50
    blks' <- fillUntilNextEpoch interpreter mockServer

    assertRewardCount dbSync 3
    atomically $ rollback mockServer (blockPoint $ last blks)
    assertBlockNoBackoff dbSync (2 + length a + length blks + length blks')
    forM_ blks' $ atomically . addBlock mockServer
    assertBlockNoBackoff dbSync (2 + length a + length blks + length blks')
    assertRewardCount dbSync 3
    blks'' <- fillUntilNextEpoch interpreter mockServer
    assertBlockNoBackoff dbSync (2 + length a + length blks + length blks' + length blks'')
  where
    testLabel = "rollbackBoundary-alonzo"

singleMIRCertMultiOut :: IOManager -> [(Text, Text)] -> Assertion
singleMIRCertMultiOut =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
      Alonzo.mkDCertTx [ShelleyTxCertMir $ MIRCert ReservesMIR (SendToOppositePotMIR (Coin 100000))] (Withdrawals mempty)

    a <- fillUntilNextEpoch interpreter mockServer

    void $ withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ \state -> do
      stakeAddr0 <- resolveStakeCreds (StakeIndex 0) state
      stakeAddr1 <- resolveStakeCreds (StakeIndex 1) state
      let saMIR = StakeAddressesMIR (Map.fromList [(stakeAddr0, DeltaCoin 10), (stakeAddr1, DeltaCoin 20)])
      Alonzo.mkDCertTx [ShelleyTxCertMir $ MIRCert ReservesMIR saMIR, ShelleyTxCertMir $ MIRCert TreasuryMIR saMIR] (Withdrawals mempty)

    b <- fillUntilNextEpoch interpreter mockServer

    assertBlockNoBackoff dbSync (2 + length a + length b)
    assertRewardCount dbSync 4
  where
    testLabel = "singleMIRCertMultiOut-alonzo"
