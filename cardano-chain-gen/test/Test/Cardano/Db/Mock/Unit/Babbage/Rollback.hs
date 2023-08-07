{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Cardano.Db.Mock.Unit.Babbage.Rollback (
  simpleRollback,
  bigChain,
  restartAndRollback,
  lazyRollback,
  lazyRollbackRestart,
  doubleRollback,
  stakeAddressRollback,
  rollbackChangeTxOrder,
  rollbackFullTx,
)
where

import Cardano.Ledger.Shelley.TxCert
import Cardano.Mock.ChainSync.Server (IOManager, addBlock, rollback)
import Cardano.Mock.Forging.Interpreter (forgeNext)
import qualified Cardano.Mock.Forging.Tx.Babbage as Babbage
import Cardano.Mock.Forging.Tx.Generic (resolvePool)
import Cardano.Mock.Forging.Types (PoolIndex (..), StakeIndex (..), UTxOIndex (..))
import Control.Concurrent.Class.MonadSTM.Strict (atomically)
import Control.Monad (forM, forM_, void)
import Data.Text (Text)
import Ouroboros.Network.Block (blockPoint)
import Test.Cardano.Db.Mock.Config (babbageConfigDir, startDBSync, stopDBSync, withFullConfig)
import Test.Cardano.Db.Mock.Examples (mockBlock0, mockBlock1, mockBlock2)
import Test.Cardano.Db.Mock.UnifiedApi (forgeAndSubmitBlocks, forgeNextAndSubmit, forgeNextFindLeaderAndSubmit, getBabbageLedgerState, rollbackTo, withBabbageFindLeaderAndSubmit, withBabbageFindLeaderAndSubmitTx)
import Test.Cardano.Db.Mock.Validate (assertBlockNoBackoff, assertTxCount)
import Test.Tasty.HUnit (Assertion)

simpleRollback :: IOManager -> [(Text, Text)] -> Assertion
simpleRollback = do
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    blk0 <- forgeNext interpreter mockBlock0
    blk1 <- forgeNext interpreter mockBlock1
    blk2 <- forgeNext interpreter mockBlock2
    atomically $ addBlock mockServer blk0
    startDBSync dbSync
    atomically $ addBlock mockServer blk1
    atomically $ addBlock mockServer blk2
    assertBlockNoBackoff dbSync 3

    atomically $ rollback mockServer (blockPoint blk1)
    assertBlockNoBackoff dbSync 3 -- rollbacks effects are now delayed
  where
    testLabel = "simpleRollback"

bigChain :: IOManager -> [(Text, Text)] -> Assertion
bigChain =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    forM_ (replicate 101 mockBlock0) (forgeNextAndSubmit interpreter mockServer)
    startDBSync dbSync
    assertBlockNoBackoff dbSync 101

    blks' <- forM (replicate 100 mockBlock1) (forgeNextAndSubmit interpreter mockServer)
    assertBlockNoBackoff dbSync 201

    forM_ (replicate 5 mockBlock2) (forgeNextAndSubmit interpreter mockServer)
    assertBlockNoBackoff dbSync 206

    atomically $ rollback mockServer (blockPoint $ last blks')
    assertBlockNoBackoff dbSync 206
  where
    testLabel = "bigChain"

restartAndRollback :: IOManager -> [(Text, Text)] -> Assertion
restartAndRollback =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    forM_ (replicate 101 mockBlock0) (forgeNextAndSubmit interpreter mockServer)
    startDBSync dbSync
    assertBlockNoBackoff dbSync 101

    blks <- forM (replicate 100 mockBlock0) (forgeNextAndSubmit interpreter mockServer)
    assertBlockNoBackoff dbSync 201

    forM_ (replicate 5 mockBlock2) (forgeNextAndSubmit interpreter mockServer)
    assertBlockNoBackoff dbSync 206

    stopDBSync dbSync
    atomically $ rollback mockServer (blockPoint $ last blks)
    startDBSync dbSync
    assertBlockNoBackoff dbSync 206
  where
    testLabel = "restartAndRollback"

-- wibble
{-}
rollbackFurther :: IOManager -> [(Text, Text)] -> Assertion
rollbackFurther =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    blks <- replicateM 80 (forgeNextFindLeaderAndSubmit interpreter mockServer [])
    startDBSync dbSync
    assertBlockNoBackoff dbSync 80

    -- We want to test that db-sync rollbacks temporarily to block 34
    -- and then syncs further. We add references to blocks 34 and 35, to
    -- validate later that one is deleted through cascade, but the other was not
    -- because a checkpoint was found.
    let blockHash1 = hfBlockHash (blks !! 33)
    Right bid1 <- queryDBSync dbSync $ DB.queryBlockId blockHash1
    cm1 <- queryDBSync dbSync $ DB.insertAdaPots $
      DB.AdaPots 0 1 (DB.DbLovelace 0) (DB.DbLovelace 0) (DB.DbLovelace 0) (DB.DbLovelace 0) (DB.DbLovelace 0) (DB.DbLovelace 0) bid1

    let blockHash2 = hfBlockHash (blks !! 34)
    Right bid2 <- queryDBSync dbSync $ DB.queryBlockId blockHash2
    cm2 <- queryDBSync dbSync $ DB.insertAdaPots $
      DB.AdaPots 0 1 (DB.DbLovelace 0) (DB.DbLovelace 0) (DB.DbLovelace 0) (DB.DbLovelace 0) (DB.DbLovelace 0) (DB.DbLovelace 0) bid2

    -- Note that there is no epoch change, which would add a new entry, since we have
    -- 80 blocks and not 100, which is the expected blocks/epoch. This also means there
    -- no epoch snapshots
    assertEqQuery dbSync DB.queryCostModel [cm1, cm2] "Unexpected CostModels"

    -- server tells db-sync to rollback to point 50. However db-sync only has
    -- a snapshot at block 34, so it will go there first. There is no proper way
    -- to test that db-sync temporarily is there, that's why we have this trick
    -- with references.
    atomically $ rollback mockServer (blockPoint $ blks !! 50)
    assertBlockNoBackoff dbSync 51

    assertEqQuery dbSync DB.queryCostModel [cm1] "Unexpected CostModel"
  where
    testLabel = "rollbackFurther"
-}

lazyRollback :: IOManager -> [(Text, Text)] -> Assertion
lazyRollback =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    lastBlk <- last <$> forgeAndSubmitBlocks interpreter mockServer 200
    void $ forgeAndSubmitBlocks interpreter mockServer 70
    assertBlockNoBackoff dbSync 270
    rollbackTo interpreter mockServer (blockPoint lastBlk)
    -- Here we create the fork.
    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)]
    void $ forgeAndSubmitBlocks interpreter mockServer 40
    assertBlockNoBackoff dbSync 241
  where
    testLabel = "lazyRollback"

lazyRollbackRestart :: IOManager -> [(Text, Text)] -> Assertion
lazyRollbackRestart =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    lastBlk <- last <$> forgeAndSubmitBlocks interpreter mockServer 220
    void $ forgeAndSubmitBlocks interpreter mockServer 60
    assertBlockNoBackoff dbSync 280

    stopDBSync dbSync
    rollbackTo interpreter mockServer (blockPoint lastBlk)

    startDBSync dbSync
    -- Here we create the fork.
    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)]
    void $ forgeAndSubmitBlocks interpreter mockServer 30
    assertBlockNoBackoff dbSync 251
  where
    testLabel = "lazyRollbackRestart"

doubleRollback :: IOManager -> [(Text, Text)] -> Assertion
doubleRollback =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    lastBlk1 <- last <$> forgeAndSubmitBlocks interpreter mockServer 150
    lastBlk2 <- last <$> forgeAndSubmitBlocks interpreter mockServer 100
    void $ forgeAndSubmitBlocks interpreter mockServer 100
    assertBlockNoBackoff dbSync 350

    rollbackTo interpreter mockServer (blockPoint lastBlk2)
    -- Here we create the fork.
    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkSimpleDCertTx [(StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)]
    void $ forgeAndSubmitBlocks interpreter mockServer 50

    rollbackTo interpreter mockServer (blockPoint lastBlk1)
    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkSimpleDCertTx [(StakeIndexNew 0, ShelleyTxCertDelegCert . ShelleyRegCert)]
    void $ forgeAndSubmitBlocks interpreter mockServer 50

    assertBlockNoBackoff dbSync 201
  where
    testLabel = "doubleRollback"

stakeAddressRollback :: IOManager -> [(Text, Text)] -> Assertion
stakeAddressRollback =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    blk <- forgeNextFindLeaderAndSubmit interpreter mockServer []
    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      let poolId = resolvePool (PoolIndex 0) st
      tx1 <-
        Babbage.mkSimpleDCertTx
          [ (StakeIndexNew 1, ShelleyTxCertDelegCert . ShelleyRegCert)
          , (StakeIndexNew 1, \stCred -> ShelleyTxCertDelegCert $ ShelleyDelegCert stCred poolId)
          ]
          st
      Right [tx1]
    assertBlockNoBackoff dbSync 2
    rollbackTo interpreter mockServer (blockPoint blk)
    void $ withBabbageFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
      Babbage.mkDummyRegisterTx 1 2
    assertBlockNoBackoff dbSync 2
    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync 3
  where
    testLabel = "stakeAddressRollback"

rollbackChangeTxOrder :: IOManager -> [(Text, Text)] -> Assertion
rollbackChangeTxOrder =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    blk0 <- forgeNextFindLeaderAndSubmit interpreter mockServer []
    st <- getBabbageLedgerState interpreter
    let Right tx0 = Babbage.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 500 st
    let Right tx1 = Babbage.mkPaymentTx (UTxOIndex 2) (UTxOIndex 3) 10000 500 st
    let Right tx2 = Babbage.mkPaymentTx (UTxOIndex 4) (UTxOIndex 5) 10000 500 st

    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \_st ->
      Right [tx0, tx1]
    assertBlockNoBackoff dbSync 2
    assertTxCount dbSync 13
    rollbackTo interpreter mockServer $ blockPoint blk0
    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \_st ->
      Right [tx1, tx0, tx2]
    assertBlockNoBackoff dbSync 2
    assertTxCount dbSync 14
  where
    testLabel = "rollbackChangeTxOrder"

rollbackFullTx :: IOManager -> [(Text, Text)] -> Assertion
rollbackFullTx =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    blk0 <- forgeNextFindLeaderAndSubmit interpreter mockServer []
    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Babbage.mkFullTx 0 100 st
      tx1 <- Babbage.mkFullTx 1 200 st
      pure [tx0, tx1]
    assertBlockNoBackoff dbSync 2
    assertTxCount dbSync 13
    rollbackTo interpreter mockServer $ blockPoint blk0
    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Babbage.mkFullTx 0 100 st
      tx1 <- Babbage.mkFullTx 1 200 st
      tx2 <- Babbage.mkFullTx 2 200 st
      pure [tx1, tx2, tx0]
    assertBlockNoBackoff dbSync 2
    assertTxCount dbSync 14
  where
    testLabel = "rollbackFullTx"
