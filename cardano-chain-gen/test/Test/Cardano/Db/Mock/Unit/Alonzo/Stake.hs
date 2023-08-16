module Test.Cardano.Db.Mock.Unit.Alonzo.Stake (
  -- stake addresses
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
) where

import qualified Cardano.Db as DB
import Cardano.Ledger.BaseTypes (CertIx (CertIx), TxIx (TxIx))
import Cardano.Ledger.Shelley.TxBody (Ptr (..))
import Cardano.Ledger.Shelley.TxCert
import Cardano.Mock.ChainSync.Server (IOManager, addBlock)
import qualified Cardano.Mock.Forging.Tx.Alonzo as Alonzo
import Cardano.Mock.Forging.Tx.Alonzo.Scenarios (delegateAndSendBlocks)
import Cardano.Mock.Forging.Types (StakeIndex (..), UTxOIndex (..))
import Control.Concurrent.Class.MonadSTM.Strict (MonadSTM (atomically))
import Control.Monad (forM_, replicateM_, void)
import Data.Text (Text)
import Ouroboros.Network.Block (blockSlot)
import Test.Cardano.Db.Mock.Config (alonzoConfigDir, startDBSync, withFullConfig)
import Test.Cardano.Db.Mock.UnifiedApi (
  fillEpochs,
  fillUntilNextEpoch,
  forgeAndSubmitBlocks,
  forgeNextFindLeaderAndSubmit,
  forgeNextSkipSlotsFindLeaderAndSubmit,
  getAlonzoLedgerState,
  withAlonzoFindLeaderAndSubmit,
  withAlonzoFindLeaderAndSubmitTx,
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
-- Stake Addresses
----------------------------------------------------------------------------------------------------------

registrationTx :: IOManager -> [(Text, Text)] -> Assertion
registrationTx =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)]

    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyUnRegCert)]

    -- We add interval or else the txs would have the same id
    void $
      withAlonzoFindLeaderAndSubmitTx
        interpreter
        mockServer
        ( fmap (Alonzo.addValidityInterval 1000)
            . Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)]
        )

    void $
      withAlonzoFindLeaderAndSubmitTx
        interpreter
        mockServer
        ( fmap (Alonzo.addValidityInterval 2000)
            . Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyUnRegCert)]
        )

    assertBlockNoBackoff dbSync 4
    assertCertCounts dbSync (2, 2, 0, 0)
  where
    testLabel = "registrationTx-alonzo"

registrationsSameBlock :: IOManager -> [(Text, Text)] -> Assertion
registrationsSameBlock =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)] st
      tx1 <- Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyUnRegCert)] st
      tx2 <- Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)] st
      tx3 <- Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyUnRegCert)] st
      Right [tx0, tx1, Alonzo.addValidityInterval 1000 tx2, Alonzo.addValidityInterval 2000 tx3]

    assertBlockNoBackoff dbSync 1
    assertCertCounts dbSync (2, 2, 0, 0)
  where
    testLabel = "registrationsSameBlock-alonzo"

registrationsSameTx :: IOManager -> [(Text, Text)] -> Assertion
registrationsSameTx =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx
          [ (StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)
          , (StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyUnRegCert)
          , (StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)
          , (StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyUnRegCert)
          ]

    assertBlockNoBackoff dbSync 1
    assertCertCounts dbSync (2, 2, 0, 0)
  where
    testLabel = "registrationsSameTx-alonzo"

stakeAddressPtr :: IOManager -> [(Text, Text)] -> Assertion
stakeAddressPtr =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    blk <-
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)]

    let ptr = Ptr (blockSlot blk) (TxIx 0) (CertIx 0)

    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOAddressNewWithPtr 0 ptr) 20000 20000

    assertBlockNoBackoff dbSync 2
    assertCertCounts dbSync (1, 0, 0, 0)
  where
    testLabel = "stakeAddressPtr-alonzo"

stakeAddressPtrDereg :: IOManager -> [(Text, Text)] -> Assertion
stakeAddressPtrDereg =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    blk <-
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [(StakeIndexNew 0, ShelleyTxCertDelegCert . ShelleyRegCert)]

    let ptr0 = Ptr (blockSlot blk) (TxIx 0) (CertIx 0)

    blk' <- withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOAddressNewWithPtr 0 ptr0) 20000 20000 st
      tx1 <-
        Alonzo.mkSimpleDCertTx
          [ (StakeIndexNew 0, ShelleyTxCertDelegCert . ShelleyUnRegCert)
          , (StakeIndexNew 0, ShelleyTxCertDelegCert . ShelleyRegCert)
          ]
          st
      pure [tx0, tx1]

    let ptr1 = Ptr (blockSlot blk') (TxIx 1) (CertIx 1)

    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkPaymentTx (UTxOIndex 1) (UTxOAddressNewWithPtr 0 ptr1) 20000 20000 st
      tx1 <- Alonzo.mkPaymentTx (UTxOIndex 2) (UTxOAddressNewWithPtr 0 ptr0) 20000 20000 st
      pure [tx0, tx1]

    st <- getAlonzoLedgerState interpreter
    assertBlockNoBackoff dbSync 3
    assertCertCounts dbSync (2, 1, 0, 0)
    -- The 2 addresses have the same payment credentials and they reference the same
    -- stake credentials, however they have
    assertAddrValues dbSync (UTxOAddressNewWithPtr 0 ptr0) (DB.DbLovelace 40000) st
    assertAddrValues dbSync (UTxOAddressNewWithPtr 0 ptr1) (DB.DbLovelace 20000) st
  where
    testLabel = "stakeAddressPtrDereg-alonzo"

stakeAddressPtrUseBefore :: IOManager -> [(Text, Text)] -> Assertion
stakeAddressPtrUseBefore =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- first use this stake credential
    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkPaymentTx (UTxOIndex 1) (UTxOAddressNewWithStake 0 (StakeIndexNew 1)) 10000 500

    -- and then register it
    blk <-
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)]

    let ptr = Ptr (blockSlot blk) (TxIx 0) (CertIx 0)

    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOAddressNewWithPtr 0 ptr) 20000 20000

    assertBlockNoBackoff dbSync 3
    assertCertCounts dbSync (1, 0, 0, 0)
  where
    testLabel = "stakeAddressPtrUseBefore-alonzo"

----------------------------------------------------------------------------------------------------------
-- Stake Distribution
----------------------------------------------------------------------------------------------------------

stakeDistGenesis :: IOManager -> [(Text, Text)] -> Assertion
stakeDistGenesis =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    a <- fillUntilNextEpoch interpreter mockServer
    assertBlockNoBackoff dbSync (fromIntegral $ length a)
    -- There are 5 delegations in genesis
    assertEpochStake dbSync 5
  where
    testLabel = "stakeDistGenesis-alonzo"

delegations2000 :: IOManager -> [(Text, Text)] -> Assertion
delegations2000 =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    a <- delegateAndSendBlocks 1995 interpreter
    forM_ a $ atomically . addBlock mockServer
    b <- fillUntilNextEpoch interpreter mockServer
    c <- forgeAndSubmitBlocks interpreter mockServer 10

    assertBlockNoBackoff dbSync (fromIntegral $ length a + length b + length c)
    -- There are exactly 2000 entries on the second epoch, 5 from genesis and 1995 manually added
    assertEpochStakeEpoch dbSync 2 2000

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync (fromIntegral $ length a + length b + length c + 1)
    assertEpochStakeEpoch dbSync 2 2000
  where
    testLabel = "delegations2000-alonzo"

delegations2001 :: IOManager -> [(Text, Text)] -> Assertion
delegations2001 =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    a <- delegateAndSendBlocks 1996 interpreter
    forM_ a $ atomically . addBlock mockServer
    b <- fillUntilNextEpoch interpreter mockServer
    c <- forgeAndSubmitBlocks interpreter mockServer 9

    assertBlockNoBackoff dbSync (fromIntegral $ length a + length b + length c)
    assertEpochStakeEpoch dbSync 2 0
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync (fromIntegral $ length a + length b + length c + 1)
    assertEpochStakeEpoch dbSync 2 2000
    -- The remaining entry is inserted on the next block.
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync (fromIntegral $ length a + length b + length c + 2)
    assertEpochStakeEpoch dbSync 2 2001
  where
    testLabel = "delegations2001-alonzo"

delegations8000 :: IOManager -> [(Text, Text)] -> Assertion
delegations8000 =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    a <- delegateAndSendBlocks 7995 interpreter
    forM_ a $ atomically . addBlock mockServer
    b <- fillEpochs interpreter mockServer 2
    c <- forgeAndSubmitBlocks interpreter mockServer 10

    assertBlockNoBackoff dbSync (fromIntegral $ length a + length b + length c)
    assertEpochStakeEpoch dbSync 3 2000

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertEpochStakeEpoch dbSync 3 4000

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertEpochStakeEpoch dbSync 3 6000

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertEpochStakeEpoch dbSync 3 8000

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertEpochStakeEpoch dbSync 3 8000
  where
    testLabel = "delegations8000-alonzo"

delegationsMany :: IOManager -> [(Text, Text)] -> Assertion
delegationsMany =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    a <- delegateAndSendBlocks 40000 interpreter
    forM_ a $ atomically . addBlock mockServer
    b <- fillEpochs interpreter mockServer 4
    c <- forgeAndSubmitBlocks interpreter mockServer 10

    -- too long. We cannot use default delays
    assertBlockNoBackoffTimes (repeat 10) dbSync (fromIntegral $ length a + length b + length c)
    -- The slice size here is
    -- 1 + div (delegationsLen * 5) expectedBlocks = 2001
    -- instead of 2000, because there are many delegations
    assertEpochStakeEpoch dbSync 7 2001

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertEpochStakeEpoch dbSync 7 4002

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertEpochStakeEpoch dbSync 7 6003
  where
    testLabel = "delegationsMany-alonzo"

delegationsManyNotDense :: IOManager -> [(Text, Text)] -> Assertion
delegationsManyNotDense =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    a <- delegateAndSendBlocks 40000 interpreter
    forM_ a $ atomically . addBlock mockServer
    b <- fillEpochs interpreter mockServer 4
    c <- forgeAndSubmitBlocks interpreter mockServer 10

    -- too long. We cannot use default delays
    assertBlockNoBackoffTimes (repeat 10) dbSync (fromIntegral $ length a + length b + length c)
    -- The slice size here is
    -- 1 + div (delegationsLen * 5) expectedBlocks = 2001
    -- instead of 2000, because there are many delegations
    assertEpochStakeEpoch dbSync 7 2001

    -- Blocks come on average every 5 slots. If we skip 15 slots before each block,
    -- we are expected to get only 1/4 of the expected blocks. The adjusted slices
    -- should still be long enough to cover everything.
    replicateM_ 40 $
      forgeNextSkipSlotsFindLeaderAndSubmit interpreter mockServer 15 []

    -- Even if the chain is sparse, all distributions are inserted.
    assertEpochStakeEpoch dbSync 7 40005
  where
    testLabel = "delegationsManyNotDense-alonzo"
