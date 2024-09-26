{-# LANGUAGE NumericUnderscores #-}

module Test.Cardano.Db.Mock.Unit.Babbage.Stake (
  -- stake address
  registrationTx,
  registrationsSameBlock,
  registrationsSameTx,
  stakeAddressPtr,
  stakeAddressPtrDereg,
  stakeAddressPtrUseBefore,
  -- stake distribution
  stakeDistGenesis,
  delegations2000,
  delegations2001,
  delegations8000,
  delegationsMany,
  delegationsManyNotDense,
)
where

import qualified Cardano.Db as DB
import Cardano.Ledger.BaseTypes (CertIx (CertIx), TxIx (TxIx))
import Cardano.Ledger.Credential (Ptr (..))
import Cardano.Ledger.Shelley.TxCert
import Cardano.Mock.ChainSync.Server (IOManager, addBlock)
import qualified Cardano.Mock.Forging.Tx.Babbage as Babbage
import Cardano.Mock.Forging.Tx.Babbage.Scenarios (delegateAndSendBlocks)
import Cardano.Mock.Forging.Types (StakeIndex (..), UTxOIndex (..))
import Control.Concurrent.Class.MonadSTM.Strict (MonadSTM (..))
import Control.Monad (forM_, void)
import Data.Text (Text)
import Ouroboros.Network.Block (blockSlot)
import Test.Cardano.Db.Mock.Config (babbageConfigDir, startDBSync, withFullConfig, withFullConfigAndDropDB)
import Test.Cardano.Db.Mock.UnifiedApi (
  fillEpochs,
  fillUntilNextEpoch,
  forgeAndSubmitBlocks,
  forgeNextFindLeaderAndSubmit,
  getBabbageLedgerState,
  withBabbageFindLeaderAndSubmit,
  withBabbageFindLeaderAndSubmitTx,
 )
import Test.Cardano.Db.Mock.Validate (
  assertAddrValues,
  assertBlockNoBackoff,
  assertBlockNoBackoffTimes,
  assertCertCounts,
  assertEpochStake,
  assertEpochStakeEpoch,
 )
import Test.Tasty.HUnit (Assertion)

----------------------------------------------------------------------------------------------------------
-- Stake Address
----------------------------------------------------------------------------------------------------------

registrationTx :: IOManager -> [(Text, Text)] -> Assertion
registrationTx =
  withFullConfigAndDropDB babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)]

    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyUnRegCert)]

    -- We add interval or else the txs would have the same id
    void $
      withBabbageFindLeaderAndSubmitTx
        interpreter
        mockServer
        ( fmap (Babbage.addValidityInterval 1000)
            . Babbage.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)]
        )

    void $
      withBabbageFindLeaderAndSubmitTx
        interpreter
        mockServer
        ( fmap (Babbage.addValidityInterval 2000)
            . Babbage.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyUnRegCert)]
        )

    assertBlockNoBackoff dbSync 4
    assertCertCounts dbSync (2, 2, 0, 0)
  where
    testLabel = "registrationTx"

registrationsSameBlock :: IOManager -> [(Text, Text)] -> Assertion
registrationsSameBlock =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Babbage.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)] st
      tx1 <- Babbage.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyUnRegCert)] st
      tx2 <- Babbage.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)] st
      tx3 <- Babbage.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyUnRegCert)] st
      Right [tx0, tx1, Babbage.addValidityInterval 1000 tx2, Babbage.addValidityInterval 2000 tx3]

    assertBlockNoBackoff dbSync 1
    assertCertCounts dbSync (2, 2, 0, 0)
  where
    testLabel = "registrationsSameBlock"

registrationsSameTx :: IOManager -> [(Text, Text)] -> Assertion
registrationsSameTx =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkSimpleDCertTx
          [ (StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)
          , (StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyUnRegCert)
          , (StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)
          , (StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyUnRegCert)
          ]

    assertBlockNoBackoff dbSync 1
    assertCertCounts dbSync (2, 2, 0, 0)
  where
    testLabel = "registrationsSameTx"

stakeAddressPtr :: IOManager -> [(Text, Text)] -> Assertion
stakeAddressPtr =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    blk <-
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)]

    let ptr = Ptr (blockSlot blk) (TxIx 0) (CertIx 0)

    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkPaymentTx (UTxOIndex 0) (UTxOAddressNewWithPtr 0 ptr) 20000 20000

    assertBlockNoBackoff dbSync 2
    assertCertCounts dbSync (1, 0, 0, 0)
  where
    testLabel = "stakeAddressPtr"

stakeAddressPtrDereg :: IOManager -> [(Text, Text)] -> Assertion
stakeAddressPtrDereg =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    blk <-
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkSimpleDCertTx [(StakeIndexNew 0, ShelleyTxCertDelegCert . ShelleyRegCert)]

    let ptr0 = Ptr (blockSlot blk) (TxIx 0) (CertIx 0)

    blk' <- withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Babbage.mkPaymentTx (UTxOIndex 0) (UTxOAddressNewWithPtr 0 ptr0) 20000 20000 st
      tx1 <-
        Babbage.mkSimpleDCertTx
          [ (StakeIndexNew 0, ShelleyTxCertDelegCert . ShelleyUnRegCert)
          , (StakeIndexNew 0, ShelleyTxCertDelegCert . ShelleyRegCert)
          ]
          st
      pure [tx0, tx1]

    let ptr1 = Ptr (blockSlot blk') (TxIx 1) (CertIx 1)

    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Babbage.mkPaymentTx (UTxOIndex 1) (UTxOAddressNewWithPtr 0 ptr1) 20000 20000 st
      tx1 <- Babbage.mkPaymentTx (UTxOIndex 2) (UTxOAddressNewWithPtr 0 ptr0) 20000 20000 st
      pure [tx0, tx1]

    st <- getBabbageLedgerState interpreter
    assertBlockNoBackoff dbSync 3
    assertCertCounts dbSync (2, 1, 0, 0)
    -- The 2 addresses have the same payment credentials and they reference the same
    -- stake credentials, however they have
    assertAddrValues dbSync (UTxOAddressNewWithPtr 0 ptr0) (DB.DbLovelace 40000) st
    assertAddrValues dbSync (UTxOAddressNewWithPtr 0 ptr1) (DB.DbLovelace 20000) st
  where
    testLabel = "stakeAddressPtrDereg"

stakeAddressPtrUseBefore :: IOManager -> [(Text, Text)] -> Assertion
stakeAddressPtrUseBefore =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- first use this stake credential
    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkPaymentTx (UTxOIndex 1) (UTxOAddressNewWithStake 0 (StakeIndexNew 1)) 10000 500

    -- and then register it
    blk <-
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)]

    let ptr = Ptr (blockSlot blk) (TxIx 0) (CertIx 0)

    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkPaymentTx (UTxOIndex 0) (UTxOAddressNewWithPtr 0 ptr) 20000 20000

    assertBlockNoBackoff dbSync 3
    assertCertCounts dbSync (1, 0, 0, 0)
  where
    testLabel = "stakeAddressPtrUseBefore"

----------------------------------------------------------------------------------------------------------
-- Stake Distribution
----------------------------------------------------------------------------------------------------------
stakeDistGenesis :: IOManager -> [(Text, Text)] -> Assertion
stakeDistGenesis =
  withFullConfigAndDropDB babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    blks <- fillUntilNextEpoch interpreter mockServer
    assertBlockNoBackoff dbSync (fromIntegral $ length blks)
    -- There are 10 delegations in genesis
    assertEpochStakeEpoch dbSync 1 5
    assertEpochStakeEpoch dbSync 2 5
  where
    testLabel = "stakeDistGenesis"

delegations2000 :: IOManager -> [(Text, Text)] -> Assertion
delegations2000 =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    blks <- delegateAndSendBlocks 1995 interpreter
    forM_ blks (atomically . addBlock mockServer)
    -- Fill the rest of the epoch
    epoch <- fillUntilNextEpoch interpreter mockServer
    -- Wait for them to sync
    assertBlockNoBackoff dbSync (length blks + length epoch)
    assertEpochStakeEpoch dbSync 1 5
    -- Add some more blocks
    blks' <- forgeAndSubmitBlocks interpreter mockServer 10
    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length blks + length epoch + length blks')
    assertEpochStakeEpoch dbSync 2 2000
    -- Forge another block
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length blks + length epoch + length blks' + 1)
    -- There are still 2000 entries
    assertEpochStakeEpoch dbSync 2 2000
  where
    testLabel = "delegations2000"

delegations2001 :: IOManager -> [(Text, Text)] -> Assertion
delegations2001 =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    -- We want exactly 2001 delegations, 5 from genesis and 1996 manually added
    blks <- delegateAndSendBlocks 1996 interpreter
    forM_ blks (atomically . addBlock mockServer)
    -- Fill the rest of the epoch
    epoch <- fillUntilNextEpoch interpreter mockServer
    -- Add some more blocks
    blks' <- forgeAndSubmitBlocks interpreter mockServer 9
    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length blks + length epoch + length blks')
    assertEpochStakeEpoch dbSync 1 5
    -- The next 2000 entries is inserted on the next block
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync (length blks + length epoch + length blks' + 1)
    assertEpochStakeEpoch dbSync 2 2001
    -- The remaining entry is inserted on the next block
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync (length blks + length epoch + length blks' + 2)
    assertEpochStakeEpoch dbSync 2 2001
  where
    testLabel = "delegations2001"

delegations8000 :: IOManager -> [(Text, Text)] -> Assertion
delegations8000 =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    -- We want exactly 8000 delegations, 5 from genesis and 7995 manually added
    blks <- delegateAndSendBlocks 7995 interpreter
    forM_ blks (atomically . addBlock mockServer)
    -- Fill the rest of the epoch
    epoch <- fillEpochs interpreter mockServer 2
    -- Add some more blocks
    blks' <- forgeAndSubmitBlocks interpreter mockServer 10
    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length blks + length epoch + length blks')
    assertEpochStakeEpoch dbSync 1 5
    assertEpochStakeEpoch dbSync 2 8000
  where
    testLabel = "delegations8000"

delegationsMany :: IOManager -> [(Text, Text)] -> Assertion
delegationsMany =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    -- Forge many delegations
    blks <- delegateAndSendBlocks 40_000 interpreter
    forM_ blks (atomically . addBlock mockServer)
    -- Fill some epochs
    epochs <- fillEpochs interpreter mockServer 4
    -- Add some more blocks
    blks' <- forgeAndSubmitBlocks interpreter mockServer 10
    -- We can't use default delays because this takes too long
    assertBlockNoBackoffTimes
      (repeat 10)
      dbSync
      (length blks + length epochs + length blks')
    assertEpochStakeEpoch dbSync 6 40_005
    assertEpochStakeEpoch dbSync 7 40_005
  where
    testLabel = "delegationsMany"

delegationsManyNotDense :: IOManager -> [(Text, Text)] -> Assertion
delegationsManyNotDense =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    -- Forge many delegations
    blks <- delegateAndSendBlocks 40_000 interpreter
    forM_ blks (atomically . addBlock mockServer)
    -- Fill some epochs
    epochs <- fillEpochs interpreter mockServer 4
    -- Add some more blocks
    blks' <- forgeAndSubmitBlocks interpreter mockServer 10
    -- We can't use default delays because this takes too long
    assertBlockNoBackoffTimes
      (repeat 10)
      dbSync
      (length blks + length epochs + length blks')
    -- check the stake distribution for each epoch
    assertEpochStakeEpoch dbSync 1 5
    assertEpochStakeEpoch dbSync 2 6005
    assertEpochStakeEpoch dbSync 3 40_005
    assertEpochStakeEpoch dbSync 4 40_005
    assertEpochStakeEpoch dbSync 5 40_005
    assertEpochStakeEpoch dbSync 6 40_005
    assertEpochStakeEpoch dbSync 7 40_005
    -- check the sum of stake distribution for all epochs
    assertEpochStake dbSync 206_035
  where
    testLabel = "delegationsManyNotDense"
