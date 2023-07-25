{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Cardano.Db.Mock.Unit.Babbage.Other (
  -- different configs
  configNoPools,
  configNoStakes,
  -- pools and smash
  poolReg,
  nonexistantPoolQuery,
  poolDeReg,
  poolDeRegMany,
  poolDelist,
  -- hard fork
  forkFixedEpoch,
  rollbackFork,
) where

import Cardano.DbSync.Era.Shelley.Generic.Util (unKeyHashRaw)
import Cardano.Ledger.BaseTypes (EpochNo)
import Cardano.Ledger.Credential (StakeCredential)
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool))
import Cardano.Ledger.Shelley.TxCert
import Cardano.Mock.ChainSync.Server (IOManager, addBlock, rollback)
import Cardano.Mock.Forging.Interpreter (forgeNext)
import qualified Cardano.Mock.Forging.Tx.Alonzo as Alonzo
import qualified Cardano.Mock.Forging.Tx.Babbage as Babbage
import Cardano.Mock.Forging.Tx.Generic (resolvePool)
import Cardano.Mock.Forging.Types (
  ForgingError (..),
  PoolIndex (..),
  StakeIndex (..),
  UTxOIndex (..),
 )
import Cardano.SMASH.Server.PoolDataLayer (PoolDataLayer (..), dbToServantPoolId)
import Cardano.SMASH.Server.Types (DBFail (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Class.MonadSTM.Strict (MonadSTM (atomically))
import Control.Exception (try)
import Control.Monad (forM_, void)
import Data.Text (Text)
import Ouroboros.Consensus.Cardano.Block (StandardBabbage, StandardCrypto)
import Ouroboros.Network.Block (blockPoint)
import Test.Cardano.Db.Mock.Config (babbageConfigDir, getPoolLayer, startDBSync, stopDBSync, withFullConfig)
import Test.Cardano.Db.Mock.Examples (mockBlock0)
import Test.Cardano.Db.Mock.UnifiedApi (
  fillEpochPercentage,
  fillEpochs,
  fillUntilNextEpoch,
  forgeNextFindLeaderAndSubmit,
  getBabbageLedgerState,
  withAlonzoFindLeaderAndSubmitTx,
  withBabbageFindLeaderAndSubmit,
  withBabbageFindLeaderAndSubmitTx,
 )
import Test.Cardano.Db.Mock.Validate (
  addPoolCounters,
  assertBlockNoBackoff,
  assertBlocksCount,
  assertPoolCounters,
  assertPoolLayerCounters,
  assertTxCount,
  checkStillRuns,
  poolCountersQuery,
  runQuery,
 )
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure)

{- HLINT ignore "Use underscore" -}

----------------------------------------------------------------------------------------------------------
-- Different Configs
----------------------------------------------------------------------------------------------------------

configNoPools :: IOManager -> [(Text, Text)] -> Assertion
configNoPools =
  withFullConfig "config2" testLabel $ \_ _ dbSync -> do
    startDBSync dbSync
    assertBlocksCount dbSync 2
    assertTxCount dbSync 6
    stopDBSync dbSync
    startDBSync dbSync
    -- Nothing changes, so polling assertions doesn't help here
    -- We have to pause and check if anything crashed.
    threadDelay 3_000_000
    checkStillRuns dbSync
    assertBlocksCount dbSync 2 -- 2 genesis blocks
    assertTxCount dbSync 6
  where
    testLabel = "configNoPools"

configNoStakes :: IOManager -> [(Text, Text)] -> Assertion
configNoStakes =
  withFullConfig "config3" testLabel $ \interpreter _ dbSync -> do
    startDBSync dbSync
    assertBlocksCount dbSync 2
    assertTxCount dbSync 7
    stopDBSync dbSync
    startDBSync dbSync
    -- Nothing changes, so polling assertions don't help here
    -- We have to pause and check if anything crashed.
    threadDelay 3_000_000
    checkStillRuns dbSync
    assertBlocksCount dbSync 2
    assertTxCount dbSync 7
    -- A pool with no stakes can't create a block.
    eblk <- try $ forgeNext interpreter mockBlock0
    case eblk of
      Right _ -> assertFailure "should fail"
      Left WentTooFar {} -> pure ()
      -- TODO add an option to disable fingerprint validation for tests like this.
      Left (EmptyFingerprint _ _) -> pure ()
      Left err -> assertFailure $ "got " <> show err <> " instead of WentTooFar"
  where
    testLabel = "configNoStakes"

----------------------------------------------------------------------------------------------------------
-- Pools and Smash
----------------------------------------------------------------------------------------------------------

poolReg :: IOManager -> [(Text, Text)] -> Assertion
poolReg =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync 1
    initCounter <- runQuery dbSync poolCountersQuery
    assertEqual "Unexpected init pool counter" (3, 0, 3, 2, 0, 0) initCounter

    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkDCertPoolTx
          [
            ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Babbage.consPoolParamsTwoOwners
            )
          ]

    assertBlockNoBackoff dbSync 2
    assertPoolCounters dbSync (addPoolCounters (1, 1, 1, 2, 0, 1) initCounter)
    st <- getBabbageLedgerState interpreter
    assertPoolLayerCounters dbSync (0, 0) [(PoolIndexNew 0, (Right False, False, True))] st
  where
    testLabel = "poolReg"

-- Issue https://github.com/input-output-hk/cardano-db-sync/issues/997
nonexistantPoolQuery :: IOManager -> [(Text, Text)] -> Assertion
nonexistantPoolQuery =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync 1

    st <- getBabbageLedgerState interpreter
    assertPoolLayerCounters dbSync (0, 0) [(PoolIndexNew 0, (Left RecordDoesNotExist, False, False))] st
  where
    testLabel = "nonexistantPoolQuery"

poolDeReg :: IOManager -> [(Text, Text)] -> Assertion
poolDeReg =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync 1
    initCounter <- runQuery dbSync poolCountersQuery
    assertEqual "Unexpected init pool counter" (3, 0, 3, 2, 0, 0) initCounter

    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkDCertPoolTx
          [
            ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Babbage.consPoolParamsTwoOwners
            )
          , ([], PoolIndexNew 0, \_ poolId -> ShelleyTxCertPool $ RetirePool poolId 1)
          ]

    assertBlockNoBackoff dbSync 2
    assertPoolCounters dbSync (addPoolCounters (1, 1, 1, 2, 1, 1) initCounter)

    st <- getBabbageLedgerState interpreter
    -- Not retired yet, because epoch has not changed
    assertPoolLayerCounters dbSync (0, 0) [(PoolIndexNew 0, (Right False, False, True))] st

    -- change epoch
    a <- fillUntilNextEpoch interpreter mockServer

    assertBlockNoBackoff dbSync (fromIntegral $ length a + 2)
    -- these counters are the same
    assertPoolCounters dbSync (addPoolCounters (1, 1, 1, 2, 1, 1) initCounter)

    -- the pool is now retired, since the epoch changed.
    assertPoolLayerCounters dbSync (1, 0) [(PoolIndexNew 0, (Right True, False, False))] st
  where
    testLabel = "poolDeReg"

poolDeRegMany :: IOManager -> [(Text, Text)] -> Assertion
poolDeRegMany =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync 1
    initCounter <- runQuery dbSync poolCountersQuery
    assertEqual "Unexpected init pool counter" (3, 0, 3, 2, 0, 0) initCounter

    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkDCertPoolTx
          [ -- register

            ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Babbage.consPoolParamsTwoOwners
            )
          , -- de register
            ([], PoolIndexNew 0, mkPoolDereg 4)
          , -- register

            ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Babbage.consPoolParamsTwoOwners
            )
          , -- register with different owner and reward address

            ( [StakeIndexNew 2, StakeIndexNew 1, StakeIndexNew 0]
            , PoolIndexNew 0
            , Babbage.consPoolParamsTwoOwners
            )
          ]

    void $ withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <-
        Babbage.mkDCertPoolTx
          [ -- register

            ( [StakeIndexNew 2, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Babbage.consPoolParamsTwoOwners
            )
          ]
          st

      tx1 <-
        Babbage.mkDCertPoolTx
          [ -- deregister
            ([] :: [StakeIndex], PoolIndexNew 0, mkPoolDereg 4)
          , -- register

            ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Babbage.consPoolParamsTwoOwners
            )
          , -- deregister
            ([] :: [StakeIndex], PoolIndexNew 0, mkPoolDereg 1)
          ]
          st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 3
    -- TODO fix PoolOwner and PoolRelay unique key
    assertPoolCounters dbSync (addPoolCounters (1, 1, 5, 10, 3, 5) initCounter)

    st <- getBabbageLedgerState interpreter
    -- Not retired yet, because epoch has not changed
    assertPoolLayerCounters dbSync (0, 0) [(PoolIndexNew 0, (Right False, False, True))] st

    -- change epoch
    a <- fillUntilNextEpoch interpreter mockServer

    assertBlockNoBackoff dbSync (fromIntegral $ length a + 3)
    -- these counters are the same
    assertPoolCounters dbSync (addPoolCounters (1, 1, 5, 10, 3, 5) initCounter)

    -- from all these certificates only the latest matters. So it will retire
    -- on epoch 0
    assertPoolLayerCounters dbSync (1, 0) [(PoolIndexNew 0, (Right True, False, False))] st
  where
    testLabel = "poolDeRegMany"
    mkPoolDereg ::
      EpochNo ->
      [StakeCredential StandardCrypto] ->
      KeyHash 'StakePool StandardCrypto ->
      ShelleyTxCert StandardBabbage
    mkPoolDereg epochNo _creds keyHash = ShelleyTxCertPool $ RetirePool keyHash epochNo

poolDelist :: IOManager -> [(Text, Text)] -> Assertion
poolDelist =
  withFullConfig babbageConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync 1
    initCounter <- runQuery dbSync poolCountersQuery
    assertEqual "Unexpected init pool counter" (3, 0, 3, 2, 0, 0) initCounter

    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkDCertPoolTx
          [
            ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Babbage.consPoolParamsTwoOwners
            )
          ]

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync 3
    st <- getBabbageLedgerState interpreter
    assertPoolLayerCounters dbSync (0, 0) [(PoolIndexNew 0, (Right False, False, True))] st

    let poolKeyHash = resolvePool (PoolIndexNew 0) st
    let poolId = dbToServantPoolId $ unKeyHashRaw poolKeyHash
    poolLayer <- getPoolLayer dbSync
    void $ dlAddDelistedPool poolLayer poolId

    -- This is not async, so we don't need to do exponential backoff
    -- delisted not retired
    assertPoolLayerCounters dbSync (0, 1) [(PoolIndexNew 0, (Right False, True, True))] st

    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkDCertPoolTx
          [([], PoolIndexNew 0, \_ poolHash -> ShelleyTxCertPool $ RetirePool poolHash 1)]

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync 5
    -- delisted and pending retirement
    assertPoolLayerCounters dbSync (0, 1) [(PoolIndexNew 0, (Right False, True, True))] st

    a <- fillUntilNextEpoch interpreter mockServer

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync (fromIntegral $ 5 + length a + 1)
    -- delisted and retired
    assertPoolLayerCounters dbSync (1, 1) [(PoolIndexNew 0, (Right True, True, False))] st
  where
    testLabel = "poolDelist"

----------------------------------------------------------------------------------------------------------
-- Hard Fork
----------------------------------------------------------------------------------------------------------

forkFixedEpoch :: IOManager -> [(Text, Text)] -> Assertion
forkFixedEpoch =
  withFullConfig "config-hf-epoch1" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 500

    a <- fillEpochs interpreter mockServer 2
    void $
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 500

    b <- fillUntilNextEpoch interpreter mockServer

    assertBlockNoBackoff dbSync $ 2 + length (a <> b)
  where
    testLabel = "forkFixedEpoch"

rollbackFork :: IOManager -> [(Text, Text)] -> Assertion
rollbackFork =
  withFullConfig "config-hf-epoch1" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync
    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 500
    a <- fillUntilNextEpoch interpreter mockServer
    b <- fillEpochPercentage interpreter mockServer 85
    c <- fillUntilNextEpoch interpreter mockServer
    blk <-
      withBabbageFindLeaderAndSubmitTx interpreter mockServer $
        Babbage.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 500

    assertBlockNoBackoff dbSync $ 2 + length (a <> b <> c)
    atomically $ rollback mockServer (blockPoint $ last b)

    forM_ (c <> [blk]) $ atomically . addBlock mockServer

    assertBlockNoBackoff dbSync $ 2 + length (a <> b <> c)
  where
    testLabel = "rollbackFork"
