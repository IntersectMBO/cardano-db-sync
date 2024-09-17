{-# LANGUAGE NumericUnderscores #-}

module Test.Cardano.Db.Mock.Unit.Conway.Stake (
  -- * Stake Address
  registrationTx,
  registrationsSameBlock,
  registrationsSameTx,
  stakeAddressPtr,
  stakeAddressPtrDereg,
  stakeAddressPtrUseBefore,
  registerStakeCreds,
  registerStakeCredsNoShelley,

  -- * Stake Distribution
  stakeDistGenesis,
  delegations2000,
  delegations2001,
  delegations8000,
  delegationsMany,
  delegationsManyNotDense,
) where

import qualified Cardano.Db as DB
import Cardano.Ledger.BaseTypes (CertIx (..), TxIx (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Ptr (..))
import Cardano.Mock.ChainSync.Server (IOManager (), addBlock)
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import qualified Cardano.Mock.Forging.Tx.Conway.Scenarios as Conway
import Cardano.Mock.Forging.Types (StakeIndex (..), UTxOIndex (..))
import Cardano.Prelude
import Data.Maybe.Strict (StrictMaybe (..))
import Ouroboros.Network.Block (blockSlot)
import Test.Cardano.Db.Mock.Config
import qualified Test.Cardano.Db.Mock.UnifiedApi as Api
import Test.Cardano.Db.Mock.Validate
import Test.Tasty.HUnit (Assertion ())
import Prelude ()

registrationTx :: IOManager -> [(Text, Text)] -> Assertion
registrationTx =
  withFullConfigAndDropDB conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge some registration txs
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkSimpleDCertTx [(StakeIndexNew 1, Conway.mkRegTxCert SNothing)]
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkSimpleDCertTx [(StakeIndexNew 1, Conway.mkUnRegTxCert SNothing)]

    -- Add interval so txs don't have the same ID
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        fmap (Conway.addValidityInterval 1000)
          . Conway.mkSimpleDCertTx [(StakeIndexNew 1, Conway.mkRegTxCert SNothing)]
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        fmap (Conway.addValidityInterval 2000)
          . Conway.mkSimpleDCertTx [(StakeIndex 1, Conway.mkUnRegTxCert SNothing)]

    -- Wait for it to sync and verify counts
    assertBlockNoBackoff dbSync 4
    assertCertCounts dbSync (2, 2, 0, 0)
  where
    testLabel = "conwayRegistrationTx"

registrationsSameBlock :: IOManager -> [(Text, Text)] -> Assertion
registrationsSameBlock =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with some registrations
    void $ Api.withConwayFindLeaderAndSubmit interpreter mockServer $ \state' -> do
      tx0 <-
        Conway.mkSimpleDCertTx
          [(StakeIndexNew 1, Conway.mkRegTxCert SNothing)]
          state'
      tx1 <-
        Conway.mkSimpleDCertTx
          [(StakeIndexNew 1, Conway.mkUnRegTxCert SNothing)]
          state'
      pure
        [ tx0
        , tx1
        , Conway.addValidityInterval 1000 tx0
        , Conway.addValidityInterval 2000 tx1
        ]

    -- Wait for it to sync and verify counts
    assertBlockNoBackoff dbSync 1
    assertCertCounts dbSync (2, 2, 0, 0)
  where
    testLabel = "conwayRegistrationsSameBlock"

registrationsSameTx :: IOManager -> [(Text, Text)] -> Assertion
registrationsSameTx =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a transaction with some registrations
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkSimpleDCertTx
          [ (StakeIndexNew 1, Conway.mkRegTxCert SNothing)
          , (StakeIndexNew 1, Conway.mkUnRegTxCert SNothing)
          , -- The certificates need to be unique, otherwise they'll be deduplicated
            (StakeIndexNew 1, Conway.mkRegTxCert (SJust $ Coin 0))
          , (StakeIndexNew 1, Conway.mkUnRegTxCert (SJust $ Coin 0))
          ]

    -- Wait for it to sync and verify counts
    assertBlockNoBackoff dbSync 1
    assertCertCounts dbSync (2, 2, 0, 0)
  where
    testLabel = "conwayRegistrationsSameTx"

stakeAddressPtr :: IOManager -> [(Text, Text)] -> Assertion
stakeAddressPtr =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with a cert
    blk <-
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkSimpleDCertTx [(StakeIndexNew 1, Conway.mkRegTxCert SNothing)]

    -- Forge a block pointing to the cert
    let ptr = Ptr (blockSlot blk) (TxIx 0) (CertIx 0)
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx (UTxOIndex 0) (UTxOAddressNewWithPtr 0 ptr) 20_000 20_000 0

    -- Wait for it to sync and verify counts
    assertBlockNoBackoff dbSync 2
    assertCertCounts dbSync (1, 0, 0, 0)
  where
    testLabel = "conwayStakeAddressPtr"

stakeAddressPtrDereg :: IOManager -> [(Text, Text)] -> Assertion
stakeAddressPtrDereg =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with a registration
    blk <-
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkSimpleDCertTx [(StakeIndexNew 0, Conway.mkRegTxCert SNothing)]
    -- Forge a block with a pointer
    let ptr0 = Ptr (blockSlot blk) (TxIx 0) (CertIx 0)
    blk' <- Api.withConwayFindLeaderAndSubmit interpreter mockServer $ \state' ->
      sequence
        [ Conway.mkPaymentTx
            (UTxOIndex 0)
            (UTxOAddressNewWithPtr 0 ptr0)
            20_000
            20_000
            0
            state'
        , Conway.mkSimpleDCertTx
            [ (StakeIndexNew 0, Conway.mkUnRegTxCert SNothing)
            , (StakeIndexNew 0, Conway.mkRegTxCert SNothing)
            ]
            state'
        ]

    -- Forge a block with a pointers to the certs
    let ptr1 = Ptr (blockSlot blk') (TxIx 1) (CertIx 1)
    void $ Api.withConwayFindLeaderAndSubmit interpreter mockServer $ \state' ->
      sequence
        [ Conway.mkPaymentTx
            (UTxOIndex 1)
            (UTxOAddressNewWithPtr 0 ptr1)
            20_000
            20_000
            0
            state'
        , Conway.mkPaymentTx
            (UTxOIndex 2)
            (UTxOAddressNewWithPtr 0 ptr0)
            20_000
            20_000
            0
            state'
        ]

    -- Wait for it to sync
    assertBlockNoBackoff dbSync 3
    assertCertCounts dbSync (2, 1, 0, 0)
    -- The 2 addresses have the same payment credentials and they reference the same
    -- stake credentials, however they have different values
    state' <- Api.getConwayLedgerState interpreter
    assertAddrValues dbSync (UTxOAddressNewWithPtr 0 ptr0) (DB.DbLovelace 40_000) state'
    assertAddrValues dbSync (UTxOAddressNewWithPtr 0 ptr1) (DB.DbLovelace 20_000) state'
  where
    testLabel = "conwayStakeAddressPtrDereg"

stakeAddressPtrUseBefore :: IOManager -> [(Text, Text)] -> Assertion
stakeAddressPtrUseBefore =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Use a stake credential
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx
          (UTxOIndex 1)
          (UTxOAddressNewWithStake 0 $ StakeIndexNew 1)
          10_000
          500
          0
    -- Register it
    blk <-
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkSimpleDCertTx [(StakeIndexNew 1, Conway.mkRegTxCert SNothing)]
    -- Create a pointer to it
    let ptr = Ptr (blockSlot blk) (TxIx 0) (CertIx 0)
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx (UTxOIndex 0) (UTxOAddressNewWithPtr 0 ptr) 20_000 20_000 0

    -- Wait for it to sync and verify count
    assertBlockNoBackoff dbSync 3
    assertCertCounts dbSync (1, 0, 0, 0)
  where
    testLabel = "conwayStakeAddressPtrUseBefore"

stakeDistGenesis :: IOManager -> [(Text, Text)] -> Assertion
stakeDistGenesis =
  withFullConfigAndDropDB conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge an entire epoch
    blks <- Api.fillUntilNextEpoch interpreter mockServer

    -- Wait for it to sync
    assertBlockNoBackoff dbSync (fromIntegral $ length blks)
    -- There are 5 delegations in genesis
    assertEpochStake dbSync 5
  where
    testLabel = "conwayStakeDistGenesis"

delegations2000 :: IOManager -> [(Text, Text)] -> Assertion
delegations2000 =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- We want exactly 2000 delegations, 5 from genesis and 1995 manually added
    blks <- Conway.delegateAndSendBlocks 1995 interpreter
    forM_ blks (atomically . addBlock mockServer)
    -- Fill the rest of the epoch
    epoch <- Api.fillUntilNextEpoch interpreter mockServer
    -- Add some more blocks
    blks' <- Api.forgeAndSubmitBlocks interpreter mockServer 10

    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length blks + length epoch + length blks')
    -- There are exactly 2000 entries on the second epoch, 5 from genesis and 1995
    -- manually added
    assertEpochStakeEpoch dbSync 2 2000

    -- Forge another block
    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer []
    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length blks + length epoch + length blks' + 1)
    -- There are still 2000 entries
    assertEpochStakeEpoch dbSync 2 2000
  where
    testLabel = "conwayDelegations2000"

delegations2001 :: IOManager -> [(Text, Text)] -> Assertion
delegations2001 =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- We want exactly 2001 delegations, 5 from genesis and 1996 manually added
    blks <- Conway.delegateAndSendBlocks 1996 interpreter
    forM_ blks (atomically . addBlock mockServer)
    -- Fill the rest of the epoch
    epoch <- Api.fillUntilNextEpoch interpreter mockServer
    -- Add some more blocks
    blks' <- Api.forgeAndSubmitBlocks interpreter mockServer 9

    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length blks + length epoch + length blks')
    assertEpochStakeEpoch dbSync 2 0
    -- The next 2000 entries is inserted on the next block
    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync (length blks + length epoch + length blks' + 1)
    assertEpochStakeEpoch dbSync 2 2000
    -- The remaining entry is inserted on the next block
    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync (length blks + length epoch + length blks' + 2)
    assertEpochStakeEpoch dbSync 2 2001
  where
    testLabel = "conwayDelegations2001"

delegations8000 :: IOManager -> [(Text, Text)] -> Assertion
delegations8000 =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- We want exactly 8000 delegations, 5 from genesis and 7995 manually added
    blks <- Conway.delegateAndSendBlocks 7995 interpreter
    forM_ blks (atomically . addBlock mockServer)
    -- Fill the rest of the epoch
    epoch <- Api.fillEpochs interpreter mockServer 2
    -- Add some more blocks
    blks' <- Api.forgeAndSubmitBlocks interpreter mockServer 10

    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length blks + length epoch + length blks')
    assertEpochStakeEpoch dbSync 3 2000

    -- Each block will add 2000 more
    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertEpochStakeEpoch dbSync 3 4000

    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertEpochStakeEpoch dbSync 3 6000

    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertEpochStakeEpoch dbSync 3 8000

    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertEpochStakeEpoch dbSync 3 8000
  where
    testLabel = "conwayDelegations8000"

delegationsMany :: IOManager -> [(Text, Text)] -> Assertion
delegationsMany =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge many delegations
    blks <- Conway.delegateAndSendBlocks 40_000 interpreter
    forM_ blks (atomically . addBlock mockServer)
    -- Fill some epochs
    epochs <- Api.fillEpochs interpreter mockServer 4
    -- Add some more blocks
    blks' <- Api.forgeAndSubmitBlocks interpreter mockServer 10

    -- We can't use default delays because this takes too long
    assertBlockNoBackoffTimes
      (repeat 10)
      dbSync
      (length blks + length epochs + length blks')
    -- The slice size here is 1 + div (delegationsLen * 5) expectedBlocks = 2001
    -- instead of 2000, because there are many delegations
    assertEpochStakeEpoch dbSync 7 2001

    -- Each block will add 2001 more
    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertEpochStakeEpoch dbSync 7 4002

    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertEpochStakeEpoch dbSync 7 6003
  where
    testLabel = "conwayDelegationsMany"

delegationsManyNotDense :: IOManager -> [(Text, Text)] -> Assertion
delegationsManyNotDense =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge many delegations
    blks <- Conway.delegateAndSendBlocks 40_000 interpreter
    forM_ blks (atomically . addBlock mockServer)
    -- Fill some epochs
    epochs <- Api.fillEpochs interpreter mockServer 4
    -- Add some more blocks
    blks' <- Api.forgeAndSubmitBlocks interpreter mockServer 10

    -- We can't use default delays because this takes too long
    assertBlockNoBackoffTimes
      (repeat 10)
      dbSync
      (length blks + length epochs + length blks')
    -- The slice size here is 1 + div (delegationsLen * 5) expectedBlocks = 2001
    -- instead of 2000, because there are many delegations
    assertEpochStakeEpoch dbSync 7 2001

    -- Blocks come on average every 5 slots. If we skip 15 slots before each block,
    -- we are expected to get only 1/4 of the expected blocks. The adjusted slices
    -- should still be long enough to cover everything.
    replicateM_ 40 $
      Api.forgeNextSkipSlotsFindLeaderAndSubmit interpreter mockServer 15 []

    -- Even if the chain is sparse, all distributions are inserted.
    assertEpochStakeEpoch dbSync 7 40_005
  where
    testLabel = "conwayDelegationsManyNotDense"

registerStakeCreds :: IOManager -> [(Text, Text)] -> Assertion
registerStakeCreds = do
  withCustomConfig args Nothing cfgDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- These will be saved
    void $ Api.registerAllStakeCreds interpreter mockServer

    -- Wait for it to sync
    assertBlockNoBackoff dbSync 1

    -- Verify stake registrations
    let expectedRegCount = 4
    assertCertCounts dbSync (expectedRegCount, 0, 0, 0)
  where
    args =
      initCommandLineArgs
        { claFullMode = False
        }
    testLabel = "conwayConfigShelleyEnabled"
    cfgDir = conwayConfigDir

registerStakeCredsNoShelley :: IOManager -> [(Text, Text)] -> Assertion
registerStakeCredsNoShelley = do
  withCustomConfig args (Just configShelleyDisable) cfgDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- These should not be saved when shelley is disabled
    void $ Api.registerAllStakeCreds interpreter mockServer

    -- Wait for it to sync
    assertBlockNoBackoff dbSync 1

    -- Verify stake registrations
    let expectedRegCount = 0
    assertCertCounts dbSync (expectedRegCount, 0, 0, 0)
  where
    args =
      initCommandLineArgs
        { claFullMode = False
        }
    testLabel = "conwayConfigShelleyDisabled"
    cfgDir = conwayConfigDir
