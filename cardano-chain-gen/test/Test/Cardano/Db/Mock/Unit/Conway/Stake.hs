{-# LANGUAGE NumericUnderscores #-}

module Test.Cardano.Db.Mock.Unit.Conway.Stake (
  registrationTx,
  registrationsSameBlock,
  registrationsSameTx,
  stakeAddressPtr,
  stakeAddressPtrDereg,
  stakeAddressPtrUseBefore,
) where

import qualified Cardano.Db as DB
import Cardano.Ledger.BaseTypes (CertIx (..), TxIx (..))
import Cardano.Ledger.Credential (Ptr (..))
import Cardano.Mock.ChainSync.Server (IOManager ())
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import Cardano.Mock.Forging.Types (StakeIndex (..), UTxOIndex (..))
import Cardano.Prelude
import Data.Maybe.Strict (StrictMaybe (..))
import Ouroboros.Network.Block (blockSlot)
import Test.Cardano.Db.Mock.Config
import qualified Test.Cardano.Db.Mock.UnifiedApi as Api
import Test.Cardano.Db.Mock.Validate (assertAddrValues, assertBlockNoBackoff, assertCertCounts)
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
    testLabel = "registrationTx"

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
          , (StakeIndexNew 1, Conway.mkRegTxCert SNothing)
          , (StakeIndexNew 1, Conway.mkUnRegTxCert SNothing)
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
        Conway.mkPaymentTx (UTxOIndex 0) (UTxOAddressNewWithPtr 0 ptr) 20_000 20_000

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
            state'
        , Conway.mkPaymentTx
            (UTxOIndex 2)
            (UTxOAddressNewWithPtr 0 ptr0)
            20_000
            20_000
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
    -- Register it
    blk <-
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkSimpleDCertTx [(StakeIndexNew 1, Conway.mkRegTxCert SNothing)]
    -- Create a pointer to it
    let ptr = Ptr (blockSlot blk) (TxIx 0) (CertIx 0)
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkPaymentTx (UTxOIndex 0) (UTxOAddressNewWithPtr 0 ptr) 20_000 20_000

    -- Wait for it to sync and verify count
    assertBlockNoBackoff dbSync 3
    assertCertCounts dbSync (1, 0, 0, 0)
  where
    testLabel = "conwayStakeAddressPtrUseBefore"
