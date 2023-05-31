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
  delegationsManyNotDense
)
where

import qualified Cardano.Db as DB
import Cardano.Ledger.BaseTypes (CertIx (CertIx), TxIx (TxIx))
import Cardano.Ledger.Credential (Ptr (..))
import Cardano.Ledger.Shelley.TxBody (
  DCert (..),
  DelegCert (..),
 )
import Cardano.Mock.ChainSync.Server (IOManager, addBlock)
import qualified Cardano.Mock.Forging.Tx.Babbage as Babbage
import Cardano.Mock.Forging.Tx.Babbage.Scenarios (delegateAndSendBlocks)
import Cardano.Mock.Forging.Types (StakeIndex (..), UTxOIndex (..))
import Control.Concurrent.Class.MonadSTM.Strict (MonadSTM (..))
import Control.Monad (forM_, replicateM_, void)
import Data.Text (Text)
import Ouroboros.Network.Block (blockSlot)
import Test.Cardano.Db.Mock.Config (startDBSync, withFullConfig)
import Test.Cardano.Db.Mock.UnifiedApi (
  fillEpochs,
  fillUntilNextEpoch,
  forgeNextFindLeaderAndSubmit,
  forgeNextSkipSlotsFindLeaderAndSubmit,
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

babbageConfig :: FilePath
babbageConfig = "config"

----------------------------------------------------------------------------------------------------------
-- Stake Address
----------------------------------------------------------------------------------------------------------

registrationTx :: IOManager -> [(Text, Text)] -> Assertion
registrationTx =
  withFullConfig babbageConfig testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . RegKey)]

    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . DeRegKey)]

    -- We add interval or else the txs would have the same id
    void $
      withBabbageFindLeaderAndSubmitTx
        interpreter
        mockServer
        ( fmap (Babbage.addValidityInterval 1000)
            . Babbage.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . RegKey)]
        )

    void $
      withBabbageFindLeaderAndSubmitTx
        interpreter
        mockServer
        ( fmap (Babbage.addValidityInterval 2000)
            . Babbage.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . DeRegKey)]
        )

    assertBlockNoBackoff dbSync 4
    assertCertCounts dbSync (2, 2, 0, 0)
  where
    testLabel = "registrationTx"

registrationsSameBlock :: IOManager -> [(Text, Text)] -> Assertion
registrationsSameBlock =
  withFullConfig babbageConfig testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Babbage.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . RegKey)] st
      tx1 <- Babbage.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . DeRegKey)] st
      tx2 <- Babbage.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . RegKey)] st
      tx3 <- Babbage.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . DeRegKey)] st
      Right [tx0, tx1, Babbage.addValidityInterval 1000 tx2, Babbage.addValidityInterval 2000 tx3]

    assertBlockNoBackoff dbSync 1
    assertCertCounts dbSync (2, 2, 0, 0)
  where
    testLabel = "registrationsSameBlock"

registrationsSameTx :: IOManager -> [(Text, Text)] -> Assertion
registrationsSameTx =
  withFullConfig babbageConfig testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkSimpleDCertTx
          [ (StakeIndexNew 1, DCertDeleg . RegKey)
          , (StakeIndexNew 1, DCertDeleg . DeRegKey)
          , (StakeIndexNew 1, DCertDeleg . RegKey)
          , (StakeIndexNew 1, DCertDeleg . DeRegKey)
          ]

    assertBlockNoBackoff dbSync 1
    assertCertCounts dbSync (2, 2, 0, 0)
  where
    testLabel = "registrationsSameTx"

stakeAddressPtr :: IOManager -> [(Text, Text)] -> Assertion
stakeAddressPtr =
  withFullConfig babbageConfig testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    blk <-
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . RegKey)]

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
  withFullConfig babbageConfig testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    blk <-
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkSimpleDCertTx [(StakeIndexNew 0, DCertDeleg . RegKey)]

    let ptr0 = Ptr (blockSlot blk) (TxIx 0) (CertIx 0)

    blk' <- withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Babbage.mkPaymentTx (UTxOIndex 0) (UTxOAddressNewWithPtr 0 ptr0) 20000 20000 st
      tx1 <-
        Babbage.mkSimpleDCertTx
          [ (StakeIndexNew 0, DCertDeleg . DeRegKey)
          , (StakeIndexNew 0, DCertDeleg . RegKey)
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
  withFullConfig babbageConfig testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- first use this stake credential
    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkPaymentTx (UTxOIndex 1) (UTxOAddressNewWithStake 0 (StakeIndexNew 1)) 10000 500

    -- and then register it
    blk <-
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . RegKey)]

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
  withFullConfig babbageConfig testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    a <- fillUntilNextEpoch interpreter mockServer
    assertBlockNoBackoff dbSync (fromIntegral $ length a)
    -- There are 5 delegations in genesis
    assertEpochStake dbSync 5
  where
    testLabel = "stakeDistGenesis"

delegations2000 :: IOManager -> [(Text, Text)] -> Assertion
delegations2000 =
  withFullConfig babbageConfig testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    a <- delegateAndSendBlocks 1995 interpreter
    forM_ a $ atomically . addBlock mockServer
    b <- fillUntilNextEpoch interpreter mockServer
    c <- fillUntilNextEpoch interpreter mockServer

    assertBlockNoBackoff dbSync (fromIntegral $ length a + length b + length c)
    -- There are exactly 2000 entries on the second epoch, 5 from genesis and 1995 manually added
    assertEpochStakeEpoch dbSync 2 2000

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync (fromIntegral $ length a + length b + length c + 1)
    assertEpochStakeEpoch dbSync 2 2000
  where
    testLabel = "delegations2000"

delegations2001 :: IOManager -> [(Text, Text)] -> Assertion
delegations2001 =
  withFullConfig babbageConfig testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    a <- delegateAndSendBlocks 1996 interpreter
    forM_ a $ atomically . addBlock mockServer
    b <- fillUntilNextEpoch interpreter mockServer
    c <- fillUntilNextEpoch interpreter mockServer

    assertBlockNoBackoff dbSync (fromIntegral $ length a + length b + length c)
    -- The first block of epoch inserts 2000 out of 2001 epoch distribution.
    assertEpochStakeEpoch dbSync 2 2000
    -- The remaining entry is inserted on the next block.
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync (fromIntegral $ length a + length b + length c + 1)
    assertEpochStakeEpoch dbSync 2 2001
  where
    testLabel = "delegations2001"

delegations8000 :: IOManager -> [(Text, Text)] -> Assertion
delegations8000 =
  withFullConfig babbageConfig testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    a <- delegateAndSendBlocks 7995 interpreter
    forM_ a $ atomically . addBlock mockServer
    b <- fillEpochs interpreter mockServer 3

    assertBlockNoBackoff dbSync (fromIntegral $ length a + length b)
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
    testLabel = "delegations8000"

delegationsMany :: IOManager -> [(Text, Text)] -> Assertion
delegationsMany =
  withFullConfig babbageConfig testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    a <- delegateAndSendBlocks 40000 interpreter
    forM_ a $ atomically . addBlock mockServer
    b <- fillEpochs interpreter mockServer 5

    -- too long. We cannot use default delays
    assertBlockNoBackoffTimes (repeat 10) dbSync (fromIntegral $ length a + length b)
    -- The slice size here is
    -- 1 + div (delegationsLen * 5) expectedBlocks = 2001
    -- instead of 2000, because there are many delegations
    assertEpochStakeEpoch dbSync 7 2001

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertEpochStakeEpoch dbSync 7 4002

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertEpochStakeEpoch dbSync 7 6003
  where
    testLabel = "delegationsMany"

delegationsManyNotDense :: IOManager -> [(Text, Text)] -> Assertion
delegationsManyNotDense =
  withFullConfig babbageConfig testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    a <- delegateAndSendBlocks 40000 interpreter
    forM_ a $ atomically . addBlock mockServer
    b <- fillEpochs interpreter mockServer 5

    -- too long. We cannot use default delays
    assertBlockNoBackoffTimes (repeat 10) dbSync (fromIntegral $ length a + length b)
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
    testLabel = "delegationsManyNotDense"
