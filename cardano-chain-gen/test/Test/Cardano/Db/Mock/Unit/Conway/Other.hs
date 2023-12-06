{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}

module Test.Cardano.Db.Mock.Unit.Conway.Other (
  -- * Different configs
  configNoPools,
  configNoStakes,

  -- * Pools and smash
  poolReg,
  nonexistentPoolQuery,
  poolDeReg,
  poolDeRegMany,
  poolDelist,
) where

import Cardano.DbSync.Era.Shelley.Generic.Util (unKeyHashRaw)
import Cardano.Ledger.BaseTypes (EpochNo ())
import Cardano.Ledger.Conway.TxCert (ConwayTxCert (..))
import Cardano.Ledger.Core (PoolCert (..))
import Cardano.Ledger.Credential (StakeCredential ())
import Cardano.Ledger.Crypto (StandardCrypto ())
import Cardano.Ledger.Keys (KeyHash (), KeyRole (..))
import Cardano.Mock.ChainSync.Server (IOManager ())
import Cardano.Mock.Forging.Interpreter (forgeNext)
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import Cardano.Mock.Forging.Tx.Generic (resolvePool)
import Cardano.Mock.Forging.Types (ForgingError (..), PoolIndex (..), StakeIndex (..))
import Cardano.Prelude
import Cardano.SMASH.Server.PoolDataLayer (PoolDataLayer (..), dbToServantPoolId)
import Cardano.SMASH.Server.Types (DBFail (..))
import Ouroboros.Consensus.Shelley.Eras (StandardConway ())
import Test.Cardano.Db.Mock.Config
import Test.Cardano.Db.Mock.Examples (mockBlock0)
import qualified Test.Cardano.Db.Mock.UnifiedApi as Api
import Test.Cardano.Db.Mock.Validate
import Test.Tasty.HUnit (Assertion (), assertEqual, assertFailure)
import Prelude ()

configNoPools :: IOManager -> [(Text, Text)] -> Assertion
configNoPools =
  withFullConfig "config-conway-no-pools" testLabel $ \_ _ dbSync -> do
    startDBSync dbSync

    -- Wait for it to sync
    assertBlocksCount dbSync 2
    assertTxCount dbSync 6

    -- Restart
    stopDBSync dbSync
    startDBSync dbSync
    -- Nothing changes, so polling assertions doesn't help here We have to pause
    -- and check if anything crashed.
    threadDelay 3_000_000

    -- Verify it's still running
    checkStillRuns dbSync
    -- Verify the initial blocks again
    assertBlocksCount dbSync 2
    assertTxCount dbSync 6
  where
    testLabel = "conwayConfigNoPools"

configNoStakes :: IOManager -> [(Text, Text)] -> Assertion
configNoStakes =
  withFullConfig "config-conway-no-stakes" testLabel $ \interpreter _ dbSync -> do
    startDBSync dbSync

    -- Wait for it to sync
    assertBlocksCount dbSync 2
    assertTxCount dbSync 7

    -- Restart
    stopDBSync dbSync
    startDBSync dbSync
    -- Nothing changes, so polling assertions doesn't help here We have to pause
    -- and check if anything crashed.
    threadDelay 3_000_000

    -- Verify it's still running
    checkStillRuns dbSync
    -- Verify the initial blocks again
    assertBlocksCount dbSync 2
    assertTxCount dbSync 7

    -- Try to forge a block (expected to fail)
    errBlk <- try $ forgeNext interpreter mockBlock0
    case errBlk of
      Right _ -> assertFailure "expected to fail"
      -- A pool with no stakes can't create blocks
      Left WentTooFar {} -> pure ()
      -- TODO add an option to disable fingerprint validation for tests like this.
      Left (EmptyFingerprint _ _) -> pure ()
      Left err -> assertFailure $ "Expected WentTooFar, got " <> show err
  where
    testLabel = "conwayConfigNoStakes"

poolReg :: IOManager -> [(Text, Text)] -> Assertion
poolReg =
  withFullConfigAndDropDB conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block
    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer []
    -- Wait for it to sync
    assertBlockNoBackoff dbSync 1
    -- Verify initial pool counts:
    -- (poolHashes, poolMetadataRefs, poolUpdates, poolOwners, poolRetires, poolRelays)
    initCounter <- runQuery dbSync poolCountersQuery
    assertEqual "Unexpected init pool counter" (3, 0, 3, 2, 0, 0) initCounter

    -- Forge a pool registration
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkDCertPoolTx
          [
            ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Conway.consTxCertPool
            )
          ]

    -- Verify pool counts
    assertBlockNoBackoff dbSync 2
    -- We should have added two owners
    assertPoolCounters dbSync (addPoolCounters (1, 1, 1, 2, 0, 1) initCounter)
    state' <- Api.getConwayLedgerState interpreter
    -- Should not be retired or delisted
    assertPoolLayerCounters dbSync (0, 0) [(PoolIndexNew 0, (Right False, False, True))] state'
  where
    testLabel = "conwayPoolReg"

-- Issue https://github.com/input-output-hk/cardano-db-sync/issues/997
nonexistentPoolQuery :: IOManager -> [(Text, Text)] -> Assertion
nonexistentPoolQuery =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block
    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer []
    -- Wait for it to sync
    assertBlockNoBackoff dbSync 1

    -- Verify pool layer counts
    state' <- Api.getConwayLedgerState interpreter
    -- Should not exist
    assertPoolLayerCounters
      dbSync
      (0, 0)
      [(PoolIndex 0, (Left RecordDoesNotExist, False, False))]
      state'
  where
    testLabel = "conwayNonexistentPoolQuery"

poolDeReg :: IOManager -> [(Text, Text)] -> Assertion
poolDeReg =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block
    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer []
    -- Wait for it to sync
    assertBlockNoBackoff dbSync 1
    -- Verify initial pool counts:
    -- (poolHashes, poolMetadataRefs, poolUpdates, poolOwners, poolRetires, poolRelays)
    initCounter <- runQuery dbSync poolCountersQuery
    assertEqual "Unexpected init pool counter" (3, 0, 3, 2, 0, 0) initCounter

    -- Forge a registration/deregistration
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkDCertPoolTx
          [ -- Register a pool

            ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Conway.consTxCertPool
            )
          , -- Retire it
            ([], PoolIndexNew 0, \_ poolId -> ConwayTxCertPool $ RetirePool poolId 1)
          ]
    -- Wait for it to sync
    assertBlockNoBackoff dbSync 2
    -- Should have added two pool owners
    assertPoolCounters dbSync (addPoolCounters (1, 1, 1, 2, 1, 1) initCounter)

    state' <- Api.getConwayLedgerState interpreter
    -- Not retired yet, because epoch hasn't changed
    assertPoolLayerCounters
      dbSync
      (0, 0)
      [(PoolIndexNew 0, (Right False, False, True))]
      state'

    -- Get to the next epoch
    blks <- Api.fillUntilNextEpoch interpreter mockServer
    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length blks + 2)
    -- Should not have changed
    assertPoolCounters dbSync (addPoolCounters (1, 1, 1, 2, 1, 1) initCounter)
    -- Should now be retired
    assertPoolLayerCounters
      dbSync
      (1, 0)
      [(PoolIndexNew 0, (Right True, False, False))]
      state'
  where
    testLabel = "conwayPoolDeReg"

poolDeRegMany :: IOManager -> [(Text, Text)] -> Assertion
poolDeRegMany =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block
    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer []
    -- Wait for it to sync
    assertBlockNoBackoff dbSync 1
    -- Verify initial pool counts
    -- (poolHashes, poolMetadataRefs, poolUpdates, poolOwners, poolRetires, poolRelays)
    initCounter <- runQuery dbSync poolCountersQuery
    assertEqual "Unexpected init pool counter" (3, 0, 3, 2, 0, 0) initCounter

    -- Forge pool registrations and deregistrations
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkDCertPoolTx
          [ -- Register

            ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Conway.consTxCertPool
            )
          , -- Deregister
            ([], PoolIndexNew 0, mkPoolDereg 4)
          , -- Register--this will be deduplicated by the ledger, so counts
            -- below will not include this cert

            ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Conway.consTxCertPool
            )
          , -- Register with different owner and reward address

            ( [StakeIndexNew 2, StakeIndexNew 1, StakeIndexNew 0]
            , PoolIndexNew 0
            , Conway.consTxCertPool
            )
          ]

    -- Forge another block with more reg/dereg
    void $ Api.withConwayFindLeaderAndSubmit interpreter mockServer $ \state' ->
      sequence
        [ Conway.mkDCertPoolTx
            [ -- Register

              ( [StakeIndexNew 2, StakeIndexNew 1, StakeIndexNew 2]
              , PoolIndexNew 0
              , Conway.consTxCertPool
              )
            ]
            state'
        , Conway.mkDCertPoolTx
            [ -- Deregister
              ([], PoolIndexNew 0, mkPoolDereg 4)
            , -- Register

              ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
              , PoolIndexNew 0
              , Conway.consTxCertPool
              )
            , -- Deregister
              ([], PoolIndexNew 0, mkPoolDereg 1)
            ]
            state'
        ]

    -- Wait for it to sync
    assertBlockNoBackoff dbSync 3
    -- Verify pool counts
    -- (poolHashes, poolMetadataRefs, poolUpdates, poolOwners, poolRetires, poolRelays)
    -- TODO fix PoolOwner and PoolRelay unique key
    assertPoolCounters dbSync (addPoolCounters (1, 1, 4, 8, 3, 4) initCounter)

    state' <- Api.getConwayLedgerState interpreter
    -- Not retired yet, because epoch hasn't changed
    assertPoolLayerCounters
      dbSync
      (0, 0)
      [(PoolIndexNew 0, (Right False, False, True))]
      state'

    -- Get to the next epoch
    blks <- Api.fillUntilNextEpoch interpreter mockServer
    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length blks + 3)
    -- Pool counts should not have changed
    assertPoolCounters dbSync (addPoolCounters (1, 1, 4, 8, 3, 4) initCounter)
    -- Only the latest certificate matters, so it should be retired in epoch 0
    assertPoolLayerCounters
      dbSync
      (1, 0)
      [(PoolIndexNew 0, (Right True, False, False))]
      state'
  where
    testLabel = "conwayPoolDeRegMany"

poolDelist :: IOManager -> [(Text, Text)] -> Assertion
poolDelist =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block
    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer []
    -- Wait for it to sync
    assertBlockNoBackoff dbSync 1
    -- Verify initial pool counts
    -- (poolHashes, poolMetadataRefs, poolUpdates, poolOwners, poolRetires, poolRelays)
    initCounter <- runQuery dbSync poolCountersQuery
    assertEqual "Unexpected init pool counter" (3, 0, 3, 2, 0, 0) initCounter

    -- Register a new pool
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkDCertPoolTx
          [ -- Register

            ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Conway.consTxCertPool
            )
          ]

    -- Forge another block
    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer []
    -- Wait for it to sync
    assertBlockNoBackoff dbSync 3
    -- Should not be retired/delisted
    state' <- Api.getConwayLedgerState interpreter
    assertPoolLayerCounters
      dbSync
      (0, 0)
      [(PoolIndexNew 0, (Right False, False, True))]
      state'

    -- Delist the pool
    let poolKeyHash = resolvePool (PoolIndexNew 0) state'
        poolId = dbToServantPoolId (unKeyHashRaw poolKeyHash)
    poolLayer <- getPoolLayer dbSync
    void $ dlAddDelistedPool poolLayer poolId

    -- This is not async, so we don't need to do exponential backoff
    -- Should now be delisted
    assertPoolLayerCounters
      dbSync
      (0, 1)
      [(PoolIndexNew 0, (Right False, True, True))]
      state'

    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkDCertPoolTx [([], PoolIndexNew 0, mkPoolDereg 1)]

    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer []
  where
    testLabel = "poolDelist"

mkPoolDereg ::
  EpochNo ->
  [StakeCredential StandardCrypto] ->
  KeyHash 'StakePool StandardCrypto ->
  ConwayTxCert StandardConway
mkPoolDereg epochNo _ keyHash = ConwayTxCertPool (RetirePool keyHash epochNo)
