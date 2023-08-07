{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Db.Mock.Unit.Babbage.Reward (
  simpleRewards,
  rewardsShelley,
  rewardsDeregistration,
  rewardsReregistration,
  mirReward,
  mirRewardShelley,
  mirRewardDereg,
  rollbackBoundary,
  singleMIRCertMultiOut,
) where

import Cardano.Ledger.Coin (Coin (Coin), DeltaCoin (DeltaCoin))
import Cardano.Ledger.Keys (KeyHash (KeyHash))
import Cardano.Ledger.Shelley.TxBody (
  Withdrawals (..),
 )
import Cardano.Ledger.Shelley.TxCert
import Cardano.Mock.ChainSync.Server (IOManager, addBlock, rollback)
import Cardano.Mock.Forging.Interpreter (withShelleyLedgerState)
import qualified Cardano.Mock.Forging.Tx.Babbage as Babbage
import Cardano.Mock.Forging.Tx.Babbage.Scenarios (delegateAndSendBlocks)
import Cardano.Mock.Forging.Tx.Generic (resolvePool, resolveStakeCreds)
import qualified Cardano.Mock.Forging.Tx.Shelley as Shelley
import Cardano.Mock.Forging.Types (
  PoolIndex (..),
  StakeIndex (..),
  UTxOIndex (..),
 )
import Control.Concurrent.Class.MonadSTM.Strict (MonadSTM (atomically))
import Control.Monad (forM_, void)
import qualified Data.Map as Map
import Data.Text (Text)
import Ouroboros.Network.Block (blockPoint)
import Test.Cardano.Db.Mock.Config (babbageConfigDir, startDBSync, stopDBSync, withFullConfig)
import Test.Cardano.Db.Mock.UnifiedApi (
  fillEpochPercentage,
  fillEpochs,
  fillUntilNextEpoch,
  forgeAndSubmitBlocks,
  getBabbageLedgerState,
  registerAllStakeCreds,
  rollbackTo,
  skipUntilNextEpoch,
  withBabbageFindLeaderAndSubmit,
  withBabbageFindLeaderAndSubmitTx,
  withShelleyFindLeaderAndSubmitTx,
 )
import Test.Cardano.Db.Mock.Validate (
  assertBlockNoBackoff,
  assertCurrentEpoch,
  assertRewardCount,
  assertRewardCounts,
 )
import Test.Tasty.HUnit (Assertion)

simpleRewards :: IOManager -> [(Text, Text)] -> Assertion
simpleRewards =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    -- Pools are not registered yet, this takes 2 epochs. So fees of this tx
    -- should not create any rewards.
    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ Babbage.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000

    a <- fillEpochs interpreter mockServer 3
    assertBlockNoBackoff dbSync (fromIntegral $ 2 + length a)

    -- The pool leaders take leader rewards with value 0
    assertRewardCount dbSync 3

    st <- getBabbageLedgerState interpreter
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
    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ Babbage.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000

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
    testLabel = "simpleRewards"

-- This test is the same as the previous, but in Shelley era. Rewards result
-- should be different because of the old Shelley bug.
-- https://github.com/input-output-hk/cardano-db-sync/issues/959
--
-- The differenece in rewards is triggered when a reward address of a pool A
-- delegates to a pool B and is not an owner of pool B. In this case it receives
-- leader rewards from pool A and member rewards from pool B. In this test, we
-- have 2 instances of this case, one where A = B and one where A /= B.
rewardsShelley :: IOManager -> [(Text, Text)] -> Assertion
rewardsShelley =
  withFullConfig "config-shelley" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    void $
      withShelleyFindLeaderAndSubmitTx interpreter mockServer $
        Shelley.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000

    a <- fillEpochs interpreter mockServer 3
    assertRewardCount dbSync 3

    void $
      withShelleyFindLeaderAndSubmitTx interpreter mockServer $
        Shelley.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000

    b <- fillEpochs interpreter mockServer 2

    assertBlockNoBackoff dbSync (fromIntegral $ length a + length b + 3)
    st <- withShelleyLedgerState interpreter Right
    -- Note we have 2 rewards less compared to other era
    assertRewardCount dbSync 12
    assertRewardCounts
      dbSync
      st
      True
      (Just 5)
      -- Here we dont' have both leader and member rewards.
      [ (StakeIndexPoolLeader (PoolIndexId $ KeyHash "9f1b441b9b781b3c3abb43b25679dc17dbaaf116dddca1ad09dc1de0"), (1, 0, 0, 0, 0))
      , (StakeIndexPoolLeader (PoolIndexId $ KeyHash "5af582399de8c226391bfd21424f34d0b053419c4d93975802b7d107"), (1, 0, 0, 0, 0))
      , (StakeIndexPoolLeader (PoolIndexId $ KeyHash "58eef2925db2789f76ea057c51069e52c5e0a44550f853c6cdf620f8"), (1, 0, 0, 0, 0))
      , (StakeIndexPoolMember 0 (PoolIndex 0), (0, 1, 0, 0, 0))
      , (StakeIndexPoolMember 0 (PoolIndex 1), (0, 1, 0, 0, 0))
      ]
  where
    testLabel = "rewardsShelley"

rewardsDeregistration :: IOManager -> [(Text, Text)] -> Assertion
rewardsDeregistration =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkDepositTxPools (UTxOIndex 1) 20000

    -- first move to treasury from reserves
    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
      Babbage.mkDCertTx [ShelleyTxCertMir $ MIRCert ReservesMIR (SendToOppositePotMIR (Coin 100000))] (Withdrawals mempty) Nothing

    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      -- register the stake address and delegate to a pool
      let poolId = resolvePool (PoolIndex 0) st
      tx1 <-
        Babbage.mkSimpleDCertTx
          [ (StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)
          , (StakeIndexNew 1, \stCred -> ShelleyTxCertDelegCert $ ShelleyDelegCert stCred poolId)
          ]
          st
      -- send some funds to the address so
      tx2 <- Babbage.mkPaymentTx (UTxOIndex 0) (UTxOAddressNewWithStake 0 (StakeIndexNew 1)) 100000 5000 st
      Right [tx1, tx2]

    a <- fillEpochs interpreter mockServer 3
    assertBlockNoBackoff dbSync (fromIntegral $ 3 + length a)

    st <- getBabbageLedgerState interpreter

    -- Now that pools are registered, we add a tx to fill the fees pot.
    -- Rewards will be distributed.
    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ Babbage.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000

    b <- fillEpochs interpreter mockServer 2

    assertBlockNoBackoff dbSync (fromIntegral $ 4 + length a + length b)
    assertRewardCounts dbSync st True Nothing [(StakeIndexNew 1, (0, 1, 0, 0, 0))]

    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ Babbage.mkPaymentTx (UTxOIndex 1) (UTxOIndex 0) 10000 10000

    c <- fillEpochs interpreter mockServer 2

    assertBlockNoBackoff dbSync (fromIntegral $ 5 + length a + length b + length c)
    assertRewardCounts dbSync st True Nothing [(StakeIndexNew 1, (0, 2, 0, 0, 0))]

    d <- fillEpochs interpreter mockServer 1
    e <- fillEpochPercentage interpreter mockServer 85
    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyUnRegCert)]

    f <- fillUntilNextEpoch interpreter mockServer

    assertBlockNoBackoff dbSync (fromIntegral $ 6 + length (a <> b <> c <> d <> e <> f))
    -- stays at 2, since it's deregistered.
    assertRewardCounts dbSync st True Nothing [(StakeIndexNew 1, (0, 2, 0, 0, 0))]

    g <- fillEpochs interpreter mockServer 2
    assertBlockNoBackoff dbSync (fromIntegral $ 6 + length (a <> b <> c <> d <> e <> f <> g))
    assertRewardCounts dbSync st True Nothing [(StakeIndexNew 1, (0, 2, 0, 0, 0))]
  where
    testLabel = "rewardsDeregistration"

-- This is a fix of the reward issue fix in Babbage described in the Babbage specs
-- If a stake address is deregistered during the reward computation initialisation,
-- and is registered later it doesn't receive rewards before Babbage. It does receive
-- on Babbage. See the same test on Alonzo.
rewardsReregistration :: IOManager -> [(Text, Text)] -> Assertion
rewardsReregistration =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkDepositTxPools (UTxOIndex 1) 20000

    -- first move to treasury from reserves
    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
      Babbage.mkDCertTx [ShelleyTxCertMir $ MIRCert ReservesMIR (SendToOppositePotMIR (Coin 100000))] (Withdrawals mempty) Nothing

    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      -- register the stake address and delegate to a pool
      let poolId = resolvePool (PoolIndex 0) st
      tx1 <-
        Babbage.mkSimpleDCertTx
          [ (StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)
          , (StakeIndexNew 1, \stCred -> ShelleyTxCertDelegCert $ ShelleyDelegCert stCred poolId)
          ]
          st
      -- send some funds to the address so
      tx2 <- Babbage.mkPaymentTx (UTxOIndex 0) (UTxOAddressNewWithStake 0 (StakeIndexNew 1)) 100000 5000 st
      Right [tx1, tx2]

    a <- fillEpochs interpreter mockServer 3
    assertBlockNoBackoff dbSync (fromIntegral $ 3 + length a)

    st <- getBabbageLedgerState interpreter

    -- Now that pools are registered, we add a tx to fill the fees pot.
    -- Rewards will be distributed.
    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ Babbage.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000

    b <- fillEpochs interpreter mockServer 2

    assertBlockNoBackoff dbSync (fromIntegral $ 4 + length a + length b)
    assertRewardCounts dbSync st True Nothing [(StakeIndexNew 1, (0, 1, 0, 0, 0))]

    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ Babbage.mkPaymentTx (UTxOIndex 1) (UTxOIndex 0) 10000 10000

    b' <- fillEpochs interpreter mockServer 1
    c <- fillEpochPercentage interpreter mockServer 10
    -- deregister before the 40% of the epoch
    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyUnRegCert)]
    d <- fillEpochPercentage interpreter mockServer 60
    -- register after 40% and before epoch boundary.
    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)]
    e <- fillUntilNextEpoch interpreter mockServer
    assertBlockNoBackoff dbSync (fromIntegral $ 7 + length (a <> b <> b' <> c <> d <> e))
    -- This is 1 in Alonzo
    assertRewardCounts dbSync st True Nothing [(StakeIndexNew 1, (0, 2, 0, 0, 0))]
  where
    testLabel = "rewardsReregistration"

mirReward :: IOManager -> [(Text, Text)] -> Assertion
mirReward =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    -- first move to treasury from reserves
    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
      Babbage.mkDCertTx [ShelleyTxCertMir $ MIRCert ReservesMIR (SendToOppositePotMIR (Coin 100000))] (Withdrawals mempty) Nothing

    void $ fillEpochPercentage interpreter mockServer 50

    -- mir from treasury
    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx1 <-
        Babbage.mkSimpleDCertTx
          [
            ( StakeIndex 1
            , \cred -> ShelleyTxCertMir $ MIRCert TreasuryMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 100)))
            )
          ]
          st
      tx2 <-
        Babbage.mkSimpleDCertTx
          [
            ( StakeIndex 1
            , \cred -> ShelleyTxCertMir $ MIRCert ReservesMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 200)))
            )
          ]
          st
      tx3 <-
        Babbage.mkSimpleDCertTx
          [
            ( StakeIndex 1
            , \cred -> ShelleyTxCertMir $ MIRCert TreasuryMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 300)))
            )
          ]
          st
      pure [tx1, tx2, tx3]

    void $ fillUntilNextEpoch interpreter mockServer

    st <- getBabbageLedgerState interpreter
    -- 2 mir rewards from treasury are sumed
    assertRewardCounts dbSync st True Nothing [(StakeIndex 1, (0, 0, 1, 1, 0))]
  where
    testLabel = "mirReward"

_mirRewardRollback :: IOManager -> [(Text, Text)] -> Assertion
_mirRewardRollback =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    -- first move to treasury from reserves
    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
      Babbage.mkDCertTx [ShelleyTxCertMir $ MIRCert ReservesMIR (SendToOppositePotMIR (Coin 100000))] (Withdrawals mempty) Nothing

    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)]

    a <- fillUntilNextEpoch interpreter mockServer
    b <- fillEpochPercentage interpreter mockServer 5
    -- mir from treasury
    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkSimpleDCertTx
          [
            ( StakeIndexNew 1
            , \cred -> ShelleyTxCertMir $ MIRCert TreasuryMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 1000)))
            )
          ]
    c <- fillEpochPercentage interpreter mockServer 50
    d <- fillUntilNextEpoch interpreter mockServer

    st <- getBabbageLedgerState interpreter
    assertBlockNoBackoff dbSync (fromIntegral $ 4 + length (a <> b <> c <> d))
    assertRewardCounts dbSync st True Nothing [(StakeIndexNew 1, (0, 0, 0, 1, 0))]

    rollbackTo interpreter mockServer (blockPoint $ last c)
    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
      Babbage.mkDummyRegisterTx 1 1
    d' <- fillUntilNextEpoch interpreter mockServer
    stopDBSync dbSync
    startDBSync dbSync
    assertBlockNoBackoff dbSync (fromIntegral $ 5 + length (a <> b <> c <> d'))
    assertRewardCounts dbSync st True Nothing [(StakeIndexNew 1, (0, 0, 0, 1, 0))]

    e <- fillEpochPercentage interpreter mockServer 5
    assertBlockNoBackoff dbSync (fromIntegral $ 5 + length (a <> b <> c <> d' <> e))
    assertRewardCounts dbSync st True Nothing [(StakeIndexNew 1, (0, 0, 0, 1, 0))]
  where
    testLabel = "mirRewardRollback"

mirRewardShelley :: IOManager -> [(Text, Text)] -> Assertion
mirRewardShelley =
  withFullConfig "config-shelley" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    -- TODO test that this has no effect. You can't send funds between reserves and
    -- treasury before protocol version 5.
    void $
      withShelleyFindLeaderAndSubmitTx interpreter mockServer $
        const $
          Shelley.mkDCertTx
            [ShelleyTxCertMir $ MIRCert ReservesMIR (SendToOppositePotMIR (Coin 100000))]
            (Withdrawals mempty)

    a <- fillEpochPercentage interpreter mockServer 50

    -- mir from reserves
    void $
      withShelleyFindLeaderAndSubmitTx interpreter mockServer $
        Shelley.mkSimpleDCertTx
          [(StakeIndex 1, \cred -> ShelleyTxCertMir $ MIRCert ReservesMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 100))))]

    b <- fillUntilNextEpoch interpreter mockServer

    st <- withShelleyLedgerState interpreter Right
    assertBlockNoBackoff dbSync (fromIntegral $ 3 + length a + length b)
    assertRewardCounts dbSync st False Nothing [(StakeIndex 1, (0, 0, 1, 0, 0))]
  where
    testLabel = "mirRewardShelley"

mirRewardDereg :: IOManager -> [(Text, Text)] -> Assertion
mirRewardDereg =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    -- first move to treasury from reserves
    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
      Babbage.mkDCertTx [ShelleyTxCertMir $ MIRCert ReservesMIR (SendToOppositePotMIR (Coin 100000))] (Withdrawals mempty) Nothing

    a <- fillUntilNextEpoch interpreter mockServer

    -- mir from treasury
    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx1 <-
        Babbage.mkSimpleDCertTx
          [
            ( StakeIndex 1
            , \cred -> ShelleyTxCertMir $ MIRCert TreasuryMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 100)))
            )
          ]
          st
      tx2 <-
        Babbage.mkSimpleDCertTx
          [
            ( StakeIndex 1
            , \cred -> ShelleyTxCertMir $ MIRCert ReservesMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 200)))
            )
          ]
          st
      tx3 <-
        Babbage.mkSimpleDCertTx
          [
            ( StakeIndex 1
            , \cred -> ShelleyTxCertMir $ MIRCert TreasuryMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 300)))
            )
          ]
          st
      pure [tx1, tx2, tx3]

    b <- fillEpochPercentage interpreter mockServer 20
    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkSimpleDCertTx [(StakeIndex 1, ShelleyTxCertDelegCert . ShelleyUnRegCert)]

    assertBlockNoBackoff dbSync (fromIntegral $ 4 + length (a <> b))
    -- deregistration means empty rewards
    st <- getBabbageLedgerState interpreter
    assertRewardCounts dbSync st False Nothing []
  where
    testLabel = "mirRewardDereg"

_rewardsEmptyChainLast :: IOManager -> [(Text, Text)] -> Assertion
_rewardsEmptyChainLast =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer

    a <- fillEpochs interpreter mockServer 3
    assertRewardCount dbSync 3
    assertCurrentEpoch dbSync 3

    -- Now that pools are registered, we add a tx to fill the fees pot.
    -- Rewards will be distributed.
    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000

    b <- fillUntilNextEpoch interpreter mockServer
    assertRewardCount dbSync 6
    assertCurrentEpoch dbSync 4

    c <- fillEpochPercentage interpreter mockServer 90
    assertCurrentEpoch dbSync 4
    -- Skip a percentage of the epoch epoch
    void $ skipUntilNextEpoch interpreter mockServer []
    assertCurrentEpoch dbSync 5
    d <- fillUntilNextEpoch interpreter mockServer
    assertCurrentEpoch dbSync 6
    assertBlockNoBackoff dbSync (fromIntegral $ 1 + length a + 1 + length b + length c + 1 + length d)
    assertRewardCount dbSync 17
  where
    testLabel = "rewardsEmptyChainLast"

-- It is almost impossible to create a delta event. This event is created when there is
-- a big gap in the chain. But with current changes to ledger such big gaps cannot exist.
-- So we disable this test.
_rewardsDelta :: IOManager -> [(Text, Text)] -> Assertion
_rewardsDelta =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
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
    testLabel = "rewardsDelta"

rollbackBoundary :: IOManager -> [(Text, Text)] -> Assertion
rollbackBoundary =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $ registerAllStakeCreds interpreter mockServer
    a <- fillEpochs interpreter mockServer 2

    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000

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
    testLabel = "rollbackBoundary"

singleMIRCertMultiOut :: IOManager -> [(Text, Text)] -> Assertion
singleMIRCertMultiOut =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
      Babbage.mkDCertTx [ShelleyTxCertMir $ MIRCert ReservesMIR (SendToOppositePotMIR (Coin 100000))] (Withdrawals mempty) Nothing

    a <- fillUntilNextEpoch interpreter mockServer

    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ \state -> do
      stakeAddr0 <- resolveStakeCreds (StakeIndex 0) state
      stakeAddr1 <- resolveStakeCreds (StakeIndex 1) state
      let saMIR = StakeAddressesMIR (Map.fromList [(stakeAddr0, DeltaCoin 10), (stakeAddr1, DeltaCoin 20)])
      Babbage.mkDCertTx [ShelleyTxCertMir $ MIRCert ReservesMIR saMIR, ShelleyTxCertMir $ MIRCert TreasuryMIR saMIR] (Withdrawals mempty) Nothing

    b <- fillUntilNextEpoch interpreter mockServer

    assertBlockNoBackoff dbSync (2 + length a + length b)
    assertRewardCount dbSync 4
  where
    testLabel = "singleMIRCertMultiOut"
