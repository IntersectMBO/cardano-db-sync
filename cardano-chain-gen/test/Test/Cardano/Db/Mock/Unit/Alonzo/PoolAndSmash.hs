{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Db.Mock.Unit.Alonzo.PoolAndSmash (
  poolReg,
  nonexistantPoolQuery,
  poolDeReg,
  poolDeRegMany,
  poolDelist,
) where

import Cardano.DbSync.Era.Shelley.Generic.Util (unKeyHashRaw)
import Cardano.Ledger.BaseTypes (EpochNo)
import Cardano.Ledger.Credential (StakeCredential)
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool))
import Cardano.Ledger.Shelley.TxCert
import Cardano.Mock.ChainSync.Server (IOManager)
import qualified Cardano.Mock.Forging.Tx.Alonzo as Alonzo
import Cardano.Mock.Forging.Tx.Generic (resolvePool)
import Cardano.Mock.Forging.Types (PoolIndex (..), StakeIndex (..))
import Cardano.SMASH.Server.PoolDataLayer (PoolDataLayer (..), dbToServantPoolId)
import Cardano.SMASH.Server.Types (DBFail (RecordDoesNotExist))
import Control.Monad (void)
import Data.Text (Text)
import Ouroboros.Consensus.Cardano.Block (StandardAlonzo, StandardCrypto)
import Test.Cardano.Db.Mock.Config (
  alonzoConfigDir,
  getPoolLayer,
  startDBSync,
  withFullConfig,
 )
import Test.Cardano.Db.Mock.UnifiedApi (
  fillUntilNextEpoch,
  forgeNextFindLeaderAndSubmit,
  getAlonzoLedgerState,
  withAlonzoFindLeaderAndSubmit,
  withAlonzoFindLeaderAndSubmitTx,
 )
import Test.Cardano.Db.Mock.Validate (
  addPoolCounters,
  assertBlockNoBackoff,
  assertPoolCounters,
  assertPoolLayerCounters,
  poolCountersQuery,
  runQuery,
 )
import Test.Tasty.HUnit (Assertion, assertEqual)

poolReg :: IOManager -> [(Text, Text)] -> Assertion
poolReg =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync 1
    initCounter <- runQuery dbSync poolCountersQuery
    assertEqual "Unexpected init pool counter" (3, 0, 3, 2, 0, 0) initCounter

    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkDCertPoolTx
          [
            ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Alonzo.consPoolParamsTwoOwners
            )
          ]

    assertBlockNoBackoff dbSync 2
    assertPoolCounters dbSync (addPoolCounters (1, 1, 1, 2, 0, 1) initCounter)
    st <- getAlonzoLedgerState interpreter
    assertPoolLayerCounters dbSync (0, 0) [(PoolIndexNew 0, (Right False, False, True))] st
  where
    testLabel = "poolReg-alonzo"

-- Issue https://github.com/input-output-hk/cardano-db-sync/issues/997
nonexistantPoolQuery :: IOManager -> [(Text, Text)] -> Assertion
nonexistantPoolQuery =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync 1

    st <- getAlonzoLedgerState interpreter
    assertPoolLayerCounters dbSync (0, 0) [(PoolIndexNew 0, (Left RecordDoesNotExist, False, False))] st
  where
    testLabel = "nonexistantPoolQuery-alonzo"

poolDeReg :: IOManager -> [(Text, Text)] -> Assertion
poolDeReg =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync 1
    initCounter <- runQuery dbSync poolCountersQuery
    assertEqual "Unexpected init pool counter" (3, 0, 3, 2, 0, 0) initCounter

    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkDCertPoolTx
          [
            ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Alonzo.consPoolParamsTwoOwners
            )
          , ([], PoolIndexNew 0, \_ poolId -> ShelleyTxCertPool $ RetirePool poolId 1)
          ]

    assertBlockNoBackoff dbSync 2
    assertPoolCounters dbSync (addPoolCounters (1, 1, 1, 2, 1, 1) initCounter)

    st <- getAlonzoLedgerState interpreter
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
    testLabel = "poolDeReg-alonzo"

poolDeRegMany :: IOManager -> [(Text, Text)] -> Assertion
poolDeRegMany =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync 1
    initCounter <- runQuery dbSync poolCountersQuery
    assertEqual "Unexpected init pool counter" (3, 0, 3, 2, 0, 0) initCounter

    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkDCertPoolTx
          [ -- register

            ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Alonzo.consPoolParamsTwoOwners
            )
          , -- de register
            ([], PoolIndexNew 0, mkPoolDereg 4)
          , -- register

            ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Alonzo.consPoolParamsTwoOwners
            )
          , -- register with different owner and reward address

            ( [StakeIndexNew 2, StakeIndexNew 1, StakeIndexNew 0]
            , PoolIndexNew 0
            , Alonzo.consPoolParamsTwoOwners
            )
          ]

    void $ withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <-
        Alonzo.mkDCertPoolTx
          [ -- register

            ( [StakeIndexNew 2, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Alonzo.consPoolParamsTwoOwners
            )
          ]
          st

      tx1 <-
        Alonzo.mkDCertPoolTx
          [ -- deregister
            ([] :: [StakeIndex], PoolIndexNew 0, mkPoolDereg 4)
          , -- register

            ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Alonzo.consPoolParamsTwoOwners
            )
          , -- deregister
            ([] :: [StakeIndex], PoolIndexNew 0, mkPoolDereg 1)
          ]
          st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 3
    -- TODO fix PoolOwner and PoolRelay unique key
    assertPoolCounters dbSync (addPoolCounters (1, 1, 5, 10, 3, 5) initCounter)

    st <- getAlonzoLedgerState interpreter
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
    testLabel = "poolDeRegMany-alonzo"
    mkPoolDereg ::
      EpochNo ->
      [StakeCredential StandardCrypto] ->
      KeyHash 'StakePool StandardCrypto ->
      ShelleyTxCert StandardAlonzo
    mkPoolDereg epochNo _creds keyHash = ShelleyTxCertPool $ RetirePool keyHash epochNo

poolDelist :: IOManager -> [(Text, Text)] -> Assertion
poolDelist =
  withFullConfig alonzoConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync 1
    initCounter <- runQuery dbSync poolCountersQuery
    assertEqual "Unexpected init pool counter" (3, 0, 3, 2, 0, 0) initCounter

    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkDCertPoolTx
          [
            ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Alonzo.consPoolParamsTwoOwners
            )
          ]

    void $ forgeNextFindLeaderAndSubmit interpreter mockServer []
    assertBlockNoBackoff dbSync 3
    st <- getAlonzoLedgerState interpreter
    assertPoolLayerCounters dbSync (0, 0) [(PoolIndexNew 0, (Right False, False, True))] st

    let poolKeyHash = resolvePool (PoolIndexNew 0) st
    let poolId = dbToServantPoolId $ unKeyHashRaw poolKeyHash
    poolLayer <- getPoolLayer dbSync
    void $ dlAddDelistedPool poolLayer poolId

    -- This is not async, so we don't need to do exponential backoff
    -- delisted not retired
    assertPoolLayerCounters dbSync (0, 1) [(PoolIndexNew 0, (Right False, True, True))] st

    void $
      withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkDCertPoolTx
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
    testLabel = "poolDelist-alonzo"
