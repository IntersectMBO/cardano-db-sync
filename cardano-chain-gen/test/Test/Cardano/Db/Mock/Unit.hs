{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Db.Mock.Unit where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Class.MonadSTM.Strict
import           Data.ByteString (ByteString)
import qualified Data.Map as Map
import           Data.Text (Text)

import           Ouroboros.Consensus.Cardano.Block hiding (CardanoBlock)

import           Ouroboros.Network.Block (blockNo, blockPoint, blockSlot)

import qualified Cardano.Db as DB

import qualified Cardano.Crypto.Hash as Crypto

import           Cardano.Ledger.Alonzo.Data
import           Cardano.Ledger.Coin
import           Cardano.Ledger.Credential
import           Cardano.Ledger.Keys
import           Cardano.Ledger.Mary.Value
import           Cardano.Ledger.SafeHash
import           Cardano.Ledger.Shelley.TxBody
import           Cardano.Ledger.Slot (BlockNo (..), EpochNo)

import           Cardano.DbSync.Era.Shelley.Generic.Block (blockHash)
import           Cardano.DbSync.Era.Shelley.Generic.Util

import           Cardano.SMASH.Server.PoolDataLayer
import           Cardano.SMASH.Server.Types

import           Cardano.Mock.ChainSync.Server
import           Cardano.Mock.Db.Validate
import           Cardano.Mock.Db.Config hiding (withFullConfig)
import qualified Cardano.Mock.Db.Config as Config
import           Cardano.Mock.Forging.Interpreter
import qualified Cardano.Mock.Forging.Tx.Alonzo as Alonzo
import           Cardano.Mock.Forging.Tx.Alonzo.ScriptsExamples
import           Cardano.Mock.Forging.Tx.Generic
import qualified Cardano.Mock.Forging.Tx.Shelley as Shelley
import           Cardano.Mock.Forging.Types
import           Cardano.Mock.UnifiedApi

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

import           Test.Cardano.Db.Mock.Examples

unitTests :: IOManager -> [(Text, Text)] -> TestTree
unitTests iom knownMigrations =
    testGroup "unit tests"
      [ testGroup "simple"
          [ test "simple forge blocks" forgeBlocks
          , test "sync one block" addSimple
          , test "restart db-sync" restartDBSync
          , test "sync small chain" addSimpleChain
          ]
      , testGroup "rollbacks"
          [ test "simple rollback" simpleRollback
          , test "sync bigger chain" bigChain
          , test "rollback while db-sync is off" restartAndRollback
          , test "rollback further" rollbackFurther
          ]
      , testGroup "different configs"
          [ test "genesis config without pool" configNoPools
          , test "genesis config without stakes" configNoStakes
          ]
      , testGroup "blocks with txs"
          [ test "simple tx" addSimpleTx
          , test "simple tx in Shelley era" addSimpleTxShelley
          , test "consume utxo same block" consumeSameBlock
          ]
      , testGroup "stake addresses"
          [ test "(de)registrations" registrationTx
          , test "(de)registrations in same block" registrationsSameBlock
          , test "(de)registrations in same tx" registrationsSameTx
          , test "stake address pointers" stakeAddressPtr
          , test "stake address pointers deregistration" stakeAddressPtrDereg
          , test "stake address pointers. Use before registering." stakeAddressPtrUseBefore
          ]
      , testGroup "rewards"
          [ test "rewards" simpleRewards
          , test "shelley rewards from multiple sources" rewardsShelley
          , test "rewards with deregistration" rewardsDeregistration
          , test "Mir Cert" mirReward
          , test "Mir rollback" mirRewardRollback
          , test "Mir Cert Shelley" mirRewardShelley
          , test "Mir Cert deregistration" mirRewardDereg
          , test "test rewards empty last part of epoch" rewardsEmptyChainLast
          , test "rollback on epoch boundary" rollbackBoundary
          , test "single MIR Cert multiple outputs" singleMIRCertMultiOut
          ]
      , testGroup "plutus spend scripts"
          [ test "simple script lock" simpleScript
          , test "unlock script in same block" unlockScriptSameBlock
          , test "failed script" failedScript
          , test "failed script in same block" failedScriptSameBlock
          , test "multiple scripts unlocked" multipleScripts
          , test "multiple scripts unlocked same block" multipleScriptsSameBlock
          , test "multiple scripts failed" multipleScriptsFailed
          , test "multiple scripts failed same block" multipleScriptsFailedSameBlock
          ]
      , testGroup "plutus cert scripts"
          [ test "stake scripts" registrationScriptTx
          , test "stake scripts deregistration" deregistrationScriptTx
          , test "multiple stake scripts deregistration" deregistrationsScriptTxs
          , test "multiple stake scripts deregistration in same tx" deregistrationsScriptTx
          , test "multiple stake scripts deregistration in same tx missing redeemer 1" deregistrationsScriptTx'
          , test "multiple stake scripts deregistration in same tx missing redeemer 2" deregistrationsScriptTx''
          ]
      , testGroup "MultiAssets plutus scripts"
          [ test "mint simple multi asset" mintMultiAsset
          , test "mint many multi assets" mintMultiAssets
          , test "swap many multi assets" swapMultiAssets
          ]
      , testGroup "pools and smash"
          [ test "pool registration" poolReg
          , test "quey pool that's not registered" nonexistantPoolQuery
          , test "pool deregistration" poolDeReg
          , test "pool multiple deregistration" poolDeRegMany
          , test "delist pool" poolDelist
          ]
      ]
  where
    test :: String -> (IOManager -> [(Text, Text)] -> Assertion) -> TestTree
    test str action = testCase str (action iom knownMigrations)

defaultConfigDir ::  FilePath
defaultConfigDir = "config"

rootTestDir :: FilePath
rootTestDir = "test/testfiles"

withFullConfig :: FilePath -> FilePath
               -> (Interpreter -> ServerHandle IO CardanoBlock -> DBSyncEnv -> IO ())
               -> IOManager -> [(Text, Text)] -> IO ()
withFullConfig = Config.withFullConfig rootTestDir

forgeBlocks :: IOManager -> [(Text, Text)] -> Assertion
forgeBlocks = do
    withFullConfig defaultConfigDir testLabel $ \interpreter _mockServer _dbSync -> do
      _block0 <- forgeNext interpreter mockBlock0
      _block1 <- forgeNext interpreter mockBlock1
      block2 <- forgeNext interpreter mockBlock2
      let blkNo = blockNo block2
      assertBool (show blkNo <> " /= " <> "2")
        $ blkNo == BlockNo 2
  where
    testLabel = "forgeBlocks"

addSimple :: IOManager -> [(Text, Text)] -> Assertion
addSimple =
    withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
      -- Given a mock block, translate it into a real block and submit it to the
      -- chainsync server
      _ <- forgeNextAndSubmit interpreter mockServer mockBlock0
      -- start db-sync and let it sync
      startDBSync dbSync
      assertBlockNoBackoff dbSync 0
  where
    testLabel = "addSimple"

addSimpleChain :: IOManager -> [(Text, Text)] -> Assertion
addSimpleChain =
    withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
      -- translate the blocks to real Cardano blocks.
      blk0 <- forgeNext interpreter mockBlock0
      blk1 <- forgeNext interpreter mockBlock1
      blk2 <- forgeNext interpreter mockBlock2
      atomically $ addBlock mockServer blk0
      -- start db-sync and let it sync
      startDBSync dbSync
      -- add more blocks
      atomically $ addBlock mockServer blk1
      atomically $ addBlock mockServer blk2
      assertBlockNoBackoff dbSync 2
  where
    testLabel = "addSimpleChain"

restartDBSync :: IOManager -> [(Text, Text)] -> Assertion
restartDBSync =
    withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
      _ <- forgeNextAndSubmit interpreter mockServer mockBlock0
      -- start db-sync and let it sync
      startDBSync dbSync
      assertBlockNoBackoff dbSync 0

      stopDBSync dbSync
      -- The server sees a separate client here
      startDBSync dbSync
      assertBlockNoBackoff dbSync 0
  where
    testLabel = "restartDBSync"

simpleRollback :: IOManager -> [(Text, Text)] -> Assertion
simpleRollback = do
    withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
      blk0 <- forgeNext interpreter mockBlock0
      blk1 <- forgeNext interpreter mockBlock1
      blk2 <- forgeNext interpreter mockBlock2
      atomically $ addBlock mockServer blk0
      startDBSync dbSync
      atomically $ addBlock mockServer blk1
      atomically $ addBlock mockServer blk2
      assertBlockNoBackoff dbSync 2

      atomically $ rollback mockServer (blockPoint blk1)
      assertBlockNoBackoff dbSync 1
  where
    testLabel = "simpleRollback"

bigChain :: IOManager -> [(Text, Text)] -> Assertion
bigChain =
    withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
      forM_ (replicate 101 mockBlock0) (forgeNextAndSubmit interpreter mockServer)
      startDBSync dbSync
      assertBlockNoBackoff dbSync 100

      blks' <- forM (replicate 100 mockBlock1) (forgeNextAndSubmit interpreter mockServer)
      assertBlockNoBackoff dbSync 200

      forM_ (replicate 5 mockBlock2) (forgeNextAndSubmit interpreter mockServer)
      assertBlockNoBackoff dbSync 205

      atomically $ rollback mockServer (blockPoint $ last blks')
      assertBlockNoBackoff dbSync 200
  where
    testLabel = "bigChain"

restartAndRollback :: IOManager -> [(Text, Text)] -> Assertion
restartAndRollback =
    withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
      forM_ (replicate 101 mockBlock0) (forgeNextAndSubmit interpreter mockServer)
      startDBSync dbSync
      assertBlockNoBackoff dbSync 100

      blks <- forM (replicate 100 mockBlock0) (forgeNextAndSubmit interpreter mockServer)
      assertBlockNoBackoff dbSync 200

      forM_ (replicate 5 mockBlock2) (forgeNextAndSubmit interpreter mockServer)
      assertBlockNoBackoff dbSync 205

      stopDBSync dbSync
      atomically $ rollback mockServer (blockPoint $ last blks)
      startDBSync dbSync
      assertBlockNoBackoff dbSync 200
  where
    testLabel = "restartAndRollback"

rollbackFurther :: IOManager -> [(Text, Text)] -> Assertion
rollbackFurther =
    withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
    blks <- replicateM 81 (forgeNextFindLeaderAndSubmit interpreter mockServer [])
    startDBSync dbSync
    assertBlockNoBackoff dbSync 80

    -- We want to test that db-sync rollbacks temporarily to block 34
    -- and then syncs further. We add references to blocks 34 and 35, to
    -- validate later that one is deleted through cascade, but the other was not
    -- because a checkpoint was found.
    let blockHash1 = hfBlockHash (blks !! 34)
    Right bid1 <- queryDBSync dbSync $ DB.queryBlockId blockHash1
    cm1 <- queryDBSync dbSync $ DB.insertCostModel $ DB.CostModel "{\"1\" : 1}" bid1

    let blockHash2 = hfBlockHash (blks !! 35)
    Right bid2 <- queryDBSync dbSync $ DB.queryBlockId blockHash2
    cm2 <- queryDBSync dbSync $ DB.insertCostModel $ DB.CostModel "{\"2\" : 2}" bid2

    -- Note that there is no epoch change, which would add a new entry, since we have
    -- 80 blocks and not 100, which is the expected blocks/epoch. This also means there
    -- no epoch snapshots
    assertEqQuery dbSync DB.queryCostModel [cm1, cm2] "Unexpected CostModels"

    -- server tells db-sync to rollback to point 50. However db-sync only has
    -- a snapshot at block 34, so it will go there first. There is no proper way
    -- to test that db-sync temporarily is there, that's why we have this trick
    -- with references.
    atomically $ rollback mockServer (blockPoint $ blks !! 50)
    assertBlockNoBackoff dbSync 50

    assertEqQuery dbSync DB.queryCostModel [cm1] "Unexpected CostModel"
  where
    testLabel = "rollbackFurther"

configNoPools :: IOManager -> [(Text, Text)] -> Assertion
configNoPools =
    withFullConfig "config2" testLabel $ \_ _ dbSync -> do
      startDBSync  dbSync
      assertBlocksCount dbSync 2
      assertTxCount dbSync 6
      stopDBSync dbSync
      startDBSync  dbSync
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
      startDBSync  dbSync
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
        Left WentTooFar -> pure ()
        -- TODO add an option to disable fingerprint validation for tests like this.
        Left (EmptyFingerprint _ _) -> pure ()
        Left err -> assertFailure $ "got " <> show err <> " instead of WentTooFar"
  where
    testLabel = "configNoStakes"

addSimpleTx :: IOManager -> [(Text, Text)] -> Assertion
addSimpleTx =
    withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
      -- translate the block to a real Cardano block.
      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
              Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 500

      startDBSync  dbSync
      assertBlockNoBackoff dbSync 0
  where
    testLabel = "addSimpleTx"

addSimpleTxShelley :: IOManager -> [(Text, Text)] -> Assertion
addSimpleTxShelley =
    withFullConfig "config-shelley" testLabel $ \interpreter mockServer dbSync -> do
      -- translate the block to a real Cardano block.
      _ <- withShelleyFindLeaderAndSubmitTx interpreter mockServer $
            Shelley.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 500

      -- start db-sync and let it sync
      startDBSync  dbSync
      assertBlockNoBackoff dbSync 0
  where
    testLabel = "addSimpleTxShelley"

registrationTx :: IOManager -> [(Text, Text)] -> Assertion
registrationTx =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . RegKey)]

    _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . DeRegKey)]

    -- We add interval or else the txs would have the same id
    _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer
        (fmap (Alonzo.addValidityInterval 1000)
           . Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . RegKey)])

    _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer
        (fmap (Alonzo.addValidityInterval 2000)
           . Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . DeRegKey)])

    assertBlockNoBackoff dbSync 3
    assertCertCounts dbSync (2,2,0,0)
  where
    testLabel = "registrationTx"

registrationsSameBlock :: IOManager -> [(Text, Text)] -> Assertion
registrationsSameBlock =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    _ <- withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
        tx0 <- Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . RegKey)] st
        tx1 <- Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . DeRegKey)] st
        tx2 <- Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . RegKey)] st
        tx3 <- Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . DeRegKey)] st
        Right [tx0, tx1, Alonzo.addValidityInterval 1000 tx2, Alonzo.addValidityInterval 2000 tx3]

    assertBlockNoBackoff dbSync 0
    assertCertCounts dbSync (2,2,0,0)
  where
    testLabel = "registrationsSameBlock"

registrationsSameTx :: IOManager -> [(Text, Text)] -> Assertion
registrationsSameTx =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [ (StakeIndexNew 1, DCertDeleg . RegKey)
                               , (StakeIndexNew 1, DCertDeleg . DeRegKey)
                               , (StakeIndexNew 1, DCertDeleg . RegKey)
                               , (StakeIndexNew 1, DCertDeleg . DeRegKey)]

    assertBlockNoBackoff dbSync 0
    assertCertCounts dbSync (2,2,0,0)
  where
    testLabel = "registrationsSameTx"

stakeAddressPtr :: IOManager -> [(Text, Text)] -> Assertion
stakeAddressPtr =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    blk <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [ (StakeIndexNew 1, DCertDeleg . RegKey)]

    let ptr = Ptr (blockSlot blk) 0 0

    _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
      Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOAddressNewWithPtr 0 ptr) 20000 20000

    assertBlockNoBackoff dbSync 1
    assertCertCounts dbSync (1,0,0,0)
  where
    testLabel = "stakeAddressPtr"

stakeAddressPtrDereg :: IOManager -> [(Text, Text)] -> Assertion
stakeAddressPtrDereg =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    blk <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [ (StakeIndexNew 0, DCertDeleg . RegKey)]

    let ptr0 = Ptr (blockSlot blk) 0 0

    blk' <- withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOAddressNewWithPtr 0 ptr0) 20000 20000 st
      tx1 <- Alonzo.mkSimpleDCertTx [ (StakeIndexNew 0, DCertDeleg . DeRegKey)
                                    , (StakeIndexNew 0, DCertDeleg . RegKey) ]
                                    st
      pure [tx0, tx1]

    let ptr1 = Ptr (blockSlot blk') 1 1

    _ <- withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
        tx0 <- Alonzo.mkPaymentTx (UTxOIndex 1) (UTxOAddressNewWithPtr 0 ptr1) 20000 20000 st
        tx1 <- Alonzo.mkPaymentTx (UTxOIndex 2) (UTxOAddressNewWithPtr 0 ptr0) 20000 20000 st
        pure [tx0, tx1]

    st <- getAlonzoLedgerState interpreter
    assertBlockNoBackoff dbSync 2
    assertCertCounts dbSync (2,1,0,0)
    -- The 2 addresses have the same payment credentials and they reference the same
    -- stake credentials, however they have
    assertAddrValues dbSync (UTxOAddressNewWithPtr 0 ptr0) (DB.DbLovelace 40000) st
    assertAddrValues dbSync (UTxOAddressNewWithPtr 0 ptr1) (DB.DbLovelace 20000) st
  where
    testLabel = "stakeAddressPtrDereg"

stakeAddressPtrUseBefore :: IOManager -> [(Text, Text)] -> Assertion
stakeAddressPtrUseBefore =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync

      -- first use this stake credential
      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkPaymentTx (UTxOIndex 1) (UTxOAddressNewWithStake 0 (StakeIndexNew 1)) 10000 500

        -- and then register it
      blk <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [ (StakeIndexNew 1, DCertDeleg . RegKey)]

      let ptr = Ptr (blockSlot blk) 0 0

      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOAddressNewWithPtr 0 ptr) 20000 20000

      assertBlockNoBackoff dbSync 2
      assertCertCounts dbSync (1,0,0,0)
  where
    testLabel = "stakeAddressPtrUseBefore"

consumeSameBlock :: IOManager -> [(Text, Text)] -> Assertion
consumeSameBlock =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync

      _ <- withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
        tx0 <- Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 20000 20000 st
        let utxo0 = head (Alonzo.mkUTxOAlonzo tx0)
        tx1 <- Alonzo.mkPaymentTx (UTxOPair utxo0) (UTxOIndex 2) 10000 500 st
        pure [tx0, tx1]
      assertBlockNoBackoff dbSync 0
  where
    testLabel = "consumeSameBlock"

simpleRewards :: IOManager -> [(Text, Text)] -> Assertion
simpleRewards =
    withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      _ <- registerAllStakeCreds interpreter mockServer

      -- Pools are not registered yet, this takes 2 epochs. So fees of this tx
      -- should not create any rewards.
      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000

      a <- fillEpochs interpreter mockServer 3
      assertBlockNoBackoff dbSync (fromIntegral $ 2 + length a - 1)

      -- The pool leaders take leader rewards with value 0
      assertRewardCount dbSync 3

      st <- getAlonzoLedgerState interpreter
      -- False indicates that we provide the full expected list of addresses with rewards.
      assertRewardCounts dbSync st False
          [ (StakeIndexPoolLeader (PoolIndex 0), (1,0,0,0,0))
          , (StakeIndexPoolLeader (PoolIndex 1), (1,0,0,0,0))
          , (StakeIndexPoolLeader (PoolIndex 2), (1,0,0,0,0))]

      -- Now that pools are registered, we add a tx to fill the fees pot.
      -- Rewards will be distributed.
      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000

      b <- fillEpochs interpreter mockServer 3

      assertBlockNoBackoff dbSync (fromIntegral $ 1 + length a + 1 + length b)
      assertRewardCount dbSync 17
      assertRewardCounts dbSync st True
          -- 2 pool leaders also delegate to pools.
          [ (StakeIndexPoolLeader (PoolIndexId $ KeyHash "9f1b441b9b781b3c3abb43b25679dc17dbaaf116dddca1ad09dc1de0"), (4,0,0,0,0))
          , (StakeIndexPoolLeader (PoolIndexId $ KeyHash "5af582399de8c226391bfd21424f34d0b053419c4d93975802b7d107"), (4,1,0,0,0))
          , (StakeIndexPoolLeader (PoolIndexId $ KeyHash "58eef2925db2789f76ea057c51069e52c5e0a44550f853c6cdf620f8"), (4,1,0,0,0))
          , (StakeIndexPoolMember 0 (PoolIndex 0), (0,1,0,0,0))
          , (StakeIndexPoolMember 0 (PoolIndex 1), (0,1,0,0,0))
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
      startDBSync  dbSync
      _ <- registerAllStakeCreds interpreter mockServer

      _ <- withShelleyFindLeaderAndSubmitTx interpreter mockServer $
        Shelley.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000

      a <- fillEpochs interpreter mockServer 3
      assertRewardCount dbSync 3

      _ <- withShelleyFindLeaderAndSubmitTx interpreter mockServer $
        Shelley.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000

      b <- fillEpochs interpreter mockServer 3

      assertBlockNoBackoff dbSync (fromIntegral $ length a + length b + 2)
      st <- withShelleyLedgerState interpreter Right
      -- Note we have 2 rewards less compared to Alonzo era
      assertRewardCount dbSync 15
      assertRewardCounts dbSync st True
          -- Here we dont' have both leader and member rewards.
          [ (StakeIndexPoolLeader (PoolIndexId $ KeyHash "9f1b441b9b781b3c3abb43b25679dc17dbaaf116dddca1ad09dc1de0"), (4,0,0,0,0))
          , (StakeIndexPoolLeader (PoolIndexId $ KeyHash "5af582399de8c226391bfd21424f34d0b053419c4d93975802b7d107"), (4,0,0,0,0))
          , (StakeIndexPoolLeader (PoolIndexId $ KeyHash "58eef2925db2789f76ea057c51069e52c5e0a44550f853c6cdf620f8"), (4,0,0,0,0))
          , (StakeIndexPoolMember 0 (PoolIndex 0), (0,1,0,0,0))
          , (StakeIndexPoolMember 0 (PoolIndex 1), (0,1,0,0,0))
          ]

  where
    testLabel = "rewardsShelley"

rewardsDeregistration :: IOManager -> [(Text, Text)] -> Assertion
rewardsDeregistration =
    withFullConfig defaultConfigDir testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkDepositTxPools (UTxOIndex 1) 20000

      -- first move to treasury from reserves
      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
        Alonzo.mkDCertTx [DCertMir $ MIRCert ReservesMIR (SendToOppositePotMIR (Coin 100000))] (Wdrl mempty)

      _ <- withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
            -- register the stake address and delegate to a pool
        let poolId = resolvePool (PoolIndex 0) st
        tx1 <- Alonzo.mkSimpleDCertTx
                    [ (StakeIndexNew 1, DCertDeleg . RegKey)
                    , (StakeIndexNew 1, \stCred -> DCertDeleg $ Delegate $ Delegation stCred poolId) ]
                    st
            -- send some funds to the address so
        tx2 <- Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOAddressNewWithStake 0 (StakeIndexNew 1)) 100000 5000 st
        Right [tx1, tx2]

      a <- fillEpochs interpreter mockServer 3
      assertBlockNoBackoff dbSync (fromIntegral $ 3 + length a - 1)

      st <- getAlonzoLedgerState interpreter

      -- Now that pools are registered, we add a tx to fill the fees pot.
      -- Rewards will be distributed.
      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000

      b <- fillEpochs interpreter mockServer 2

      assertBlockNoBackoff dbSync (fromIntegral $ 4 + length a + length b - 1)
      assertRewardCounts dbSync st True [(StakeIndexNew 1, (0,1,0,0,0))]

      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ Alonzo.mkPaymentTx (UTxOIndex 1) (UTxOIndex 0) 10000 10000

      c <- fillEpochs interpreter mockServer 2

      assertBlockNoBackoff dbSync (fromIntegral $ 5 + length a + length b + length c - 1)
      assertRewardCounts dbSync st True [(StakeIndexNew 1, (0,2,0,0,0))]

      d <- fillEpochs interpreter mockServer 1
      e <- fillEpochPercentage interpreter mockServer 85
      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
          Alonzo.mkSimpleDCertTx [(StakeIndexNew 1, DCertDeleg . DeRegKey)]

      f <- fillUntilNextEpoch interpreter mockServer

      assertBlockNoBackoff dbSync (fromIntegral $ 6 + length (a <> b <> c <> d <> e <> f) - 1)
      assertRewardCounts dbSync st True [(StakeIndexNew 1, (0,2,0,0,0))]

      g <- fillEpochs interpreter mockServer 2
      -- TODO: the last field should be 1. There should be some deposit refund
      assertBlockNoBackoff dbSync (fromIntegral $ 6 + length (a <> b <> c <> d <> e <> f <> g) - 1)
      assertRewardCounts dbSync st True [(StakeIndexNew 1, (0,2,0,0,0))]

  where
    testLabel = "rewardsDeregistration"

mirReward :: IOManager -> [(Text, Text)] -> Assertion
mirReward =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      _ <- registerAllStakeCreds interpreter mockServer

      -- first move to treasury from reserves
      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
        Alonzo.mkDCertTx [DCertMir $ MIRCert ReservesMIR (SendToOppositePotMIR (Coin 100000))] (Wdrl mempty)

      _ <- fillEpochPercentage interpreter mockServer 50

      -- mir from treasury
      _ <- withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
        tx1 <- Alonzo.mkSimpleDCertTx [(StakeIndex 1,
          \cred -> DCertMir $ MIRCert TreasuryMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 100))))] st
        tx2 <- Alonzo.mkSimpleDCertTx [(StakeIndex 1,
          \cred -> DCertMir $ MIRCert ReservesMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 200))))] st
        tx3 <- Alonzo.mkSimpleDCertTx [(StakeIndex 1,
          \cred -> DCertMir $ MIRCert TreasuryMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 300))))] st
        pure [tx1, tx2, tx3]

      _ <- fillUntilNextEpoch interpreter mockServer

      st <- getAlonzoLedgerState interpreter
      -- 2 mir rewards from treasury are sumed
      assertRewardCounts dbSync st True [(StakeIndex 1, (0,0,1,1,0))]
  where
    testLabel = "mirReward"

mirRewardRollback :: IOManager -> [(Text, Text)] -> Assertion
mirRewardRollback =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      _ <- registerAllStakeCreds interpreter mockServer

      -- first move to treasury from reserves
      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
        Alonzo.mkDCertTx [DCertMir $ MIRCert ReservesMIR (SendToOppositePotMIR (Coin 100000))] (Wdrl mempty)

      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [ (StakeIndexNew 1, DCertDeleg . RegKey) ]

      a <- fillUntilNextEpoch interpreter mockServer
      b <- fillEpochPercentage interpreter mockServer 5
      -- mir from treasury
      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [(StakeIndexNew 1,
          \cred -> DCertMir $ MIRCert TreasuryMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 1000))))]
      c <- fillEpochPercentage interpreter mockServer 50
      d <- fillUntilNextEpoch interpreter mockServer

      st <- getAlonzoLedgerState interpreter
      assertBlockNoBackoff dbSync (fromIntegral $ 4 + length (a <> b <> c <> d) - 1)
      assertRewardCounts dbSync st True [(StakeIndexNew 1, (0,0,0,1,0))]

      atomically $ rollback mockServer (blockPoint $ last c)
      assertBlockNoBackoff dbSync (fromIntegral $ 4 + length (a <> b <> c) - 1)
      assertRewardCounts dbSync st True [(StakeIndexNew 1, (0,0,0,1,0))]
      stopDBSync dbSync
      startDBSync dbSync
      assertBlockNoBackoff dbSync (fromIntegral $ 4 + length (a <> b <> c) - 1)
      assertRewardCounts dbSync st True [(StakeIndexNew 1, (0,0,0,1,0))]

      forM_ d $ atomically . addBlock mockServer
      e <- fillEpochPercentage interpreter mockServer 5
      assertBlockNoBackoff dbSync (fromIntegral $ 4 + length (a <> b <> c <> d <> e) - 1)
      assertRewardCounts dbSync st True [(StakeIndexNew 1, (0,0,0,1,0))]
  where
    testLabel = "mirRewardRollback"

mirRewardShelley :: IOManager -> [(Text, Text)] -> Assertion
mirRewardShelley =
    withFullConfig "config-shelley" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      _ <- registerAllStakeCreds interpreter mockServer

      -- first move to treasury from reserves
      _ <- withShelleyFindLeaderAndSubmitTx interpreter mockServer $
        const $ Shelley.mkDCertTx [DCertMir $ MIRCert ReservesMIR (SendToOppositePotMIR (Coin 100000))]
                         (Wdrl mempty)

      a <- fillEpochPercentage interpreter mockServer 50

      -- mir from treasury
      _ <- withShelleyFindLeaderAndSubmitTx  interpreter mockServer $ Shelley.mkSimpleDCertTx
        [(StakeIndex 1, \cred -> DCertMir $ MIRCert TreasuryMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 100))))]

      b <- fillUntilNextEpoch interpreter mockServer

      st <- withShelleyLedgerState interpreter Right
      -- TODO: is this correct? It looks like there are no rewards.
      assertBlockNoBackoff dbSync (fromIntegral $ 3 + length a + length b - 1)
      assertRewardCounts dbSync st True []
  where
    testLabel = "mirRewardShelley"


mirRewardDereg :: IOManager -> [(Text, Text)] -> Assertion
mirRewardDereg =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      _ <- registerAllStakeCreds interpreter mockServer

      -- first move to treasury from reserves
      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
        Alonzo.mkDCertTx [DCertMir $ MIRCert ReservesMIR (SendToOppositePotMIR (Coin 100000))] (Wdrl mempty)

      a <- fillUntilNextEpoch interpreter mockServer

      -- mir from treasury
      _ <- withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
        tx1 <- Alonzo.mkSimpleDCertTx [(StakeIndex 1,
          \cred -> DCertMir $ MIRCert TreasuryMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 100))))] st
        tx2 <- Alonzo.mkSimpleDCertTx [(StakeIndex 1,
          \cred -> DCertMir $ MIRCert ReservesMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 200))))] st
        tx3 <- Alonzo.mkSimpleDCertTx [(StakeIndex 1,
          \cred -> DCertMir $ MIRCert TreasuryMIR (StakeAddressesMIR (Map.singleton cred (DeltaCoin 300))))] st
        pure [tx1, tx2, tx3]

      b <- fillEpochPercentage interpreter mockServer 20
      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
          Alonzo.mkSimpleDCertTx [(StakeIndex 1, DCertDeleg . DeRegKey)]


      assertBlockNoBackoff dbSync (fromIntegral $ 4 + length (a <> b) - 1)
      -- deregistration means empty rewards
      st <- getAlonzoLedgerState interpreter
      assertRewardCounts dbSync st False []
  where
    testLabel = "mirRewardDereg"

rewardsEmptyChainLast :: IOManager -> [(Text, Text)] -> Assertion
rewardsEmptyChainLast =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      _ <- registerAllStakeCreds interpreter mockServer

      a <- fillEpochs interpreter mockServer 3
      assertRewardCount dbSync 0

      -- Now that pools are registered, we add a tx to fill the fees pot.
      -- Rewards will be distributed.
      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000

      b <- fillUntilNextEpoch interpreter mockServer
      assertRewardCount dbSync 6

      c <- fillEpochPercentage interpreter mockServer 80

      -- Skip half an epoch
      _ <- skipUntilNextEpoch interpreter mockServer []
      d <- fillUntilNextEpoch interpreter mockServer
      assertBlockNoBackoff dbSync (fromIntegral $ 1 + length a + 1 + length b + length c + 1 + length d - 1)
      assertRewardCount dbSync 17
  where
    testLabel = "rewardsEmptyChainLast"

rollbackBoundary :: IOManager -> [(Text, Text)] -> Assertion
rollbackBoundary =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      _ <- registerAllStakeCreds interpreter mockServer
      a <- fillEpochs interpreter mockServer 2

      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10000 10000

      blks <- forgeAndSubmitBlocks interpreter mockServer 50
      blks' <- fillUntilNextEpoch interpreter mockServer

      assertRewardCount dbSync 3
      atomically $ rollback mockServer (blockPoint $ last blks)
      assertBlockNoBackoff dbSync (2 + length a + length blks - 1)
      forM_ blks' $ atomically . addBlock mockServer
      assertBlockNoBackoff dbSync (2 + length a + length blks + length blks' - 1)
      assertRewardCount dbSync 3
  where
    testLabel = "rollbackBoundary"

singleMIRCertMultiOut :: IOManager -> [(Text, Text)] -> Assertion
singleMIRCertMultiOut =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync dbSync

      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
            Alonzo.mkDCertTx [DCertMir $ MIRCert ReservesMIR (SendToOppositePotMIR (Coin 100000))] (Wdrl mempty)

      a <- fillUntilNextEpoch interpreter mockServer

      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ \ state -> do
            stakeAddr0 <- resolveStakeCreds (StakeIndex 0) state
            stakeAddr1 <- resolveStakeCreds (StakeIndex 1) state
            let saMIR = StakeAddressesMIR (Map.fromList [(stakeAddr0, DeltaCoin 10), (stakeAddr1, DeltaCoin 20)])
            Alonzo.mkDCertTx [DCertMir $ MIRCert ReservesMIR saMIR, DCertMir $ MIRCert TreasuryMIR saMIR] (Wdrl mempty)

      b <- fillUntilNextEpoch interpreter mockServer

      assertBlockNoBackoff dbSync (1 + length a + length b)
      assertRewardCount dbSync 4
  where
    testLabel = "singleMIRCertMultiOut"

simpleScript :: IOManager -> [(Text, Text)] -> Assertion
simpleScript =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      _ <- registerAllStakeCreds interpreter mockServer

      a <- fillUntilNextEpoch interpreter mockServer

      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkLockByScriptTx (UTxOIndex 0) [True] 20000 20000

      assertBlockNoBackoff dbSync (fromIntegral $ length a + 2 - 1)
      assertEqQuery dbSync (fmap getOutFields <$> DB.queryScriptOutputs) [expectedFields] "Unexpected script outputs"
  where
    testLabel = "simpleScript"
    getOutFields txOut = (DB.txOutAddress txOut, DB.txOutAddressHasScript txOut, DB.txOutValue txOut, DB.txOutDataHash txOut)
    expectedFields = ( "addr_test1wpnlxv2xv9a9ucvnvzqakwepzl9ltx7jzgm53av2e9ncv4sysemm8" :: Text
                     , True, DB.DbLovelace 20000
                     , Just $ Crypto.hashToBytes (extractHash $ hashData plutusDataList))

unlockScript :: IOManager -> [(Text, Text)] -> Assertion
unlockScript =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      _ <- registerAllStakeCreds interpreter mockServer

      -- We don't use withAlonzoFindLeaderAndSubmitTx here, because we want access to the tx.
      tx0 <- withAlonzoLedgerState interpreter $ Alonzo.mkLockByScriptTx (UTxOIndex 0) [True] 20000 20000
      _ <- forgeNextAndSubmit interpreter mockServer $ MockBlock [TxAlonzo tx0] (NodeId 1)

      let utxo0 = head (Alonzo.mkUTxOAlonzo tx0)
      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkUnlockScriptTx [UTxOPair utxo0] (UTxOIndex 1) (UTxOIndex 2) True 10000 500

      assertBlockNoBackoff dbSync 2
      assertAlonzoCounts dbSync (1,1,1,1,1,1,0,0)
  where
    testLabel = "unlockScriptSameBlock"

unlockScriptSameBlock :: IOManager -> [(Text, Text)] -> Assertion
unlockScriptSameBlock =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      _ <- registerAllStakeCreds interpreter mockServer

      _ <- withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
        tx0 <- Alonzo.mkLockByScriptTx (UTxOIndex 0) [True] 20000 20000 st
        let utxo0 = head (Alonzo.mkUTxOAlonzo tx0)
        tx1 <- Alonzo.mkUnlockScriptTx [UTxOPair utxo0] (UTxOIndex 1) (UTxOIndex 2) True 10000 500 st
        pure [tx0, tx1]

      assertBlockNoBackoff dbSync 1
      assertAlonzoCounts dbSync (1,1,1,1,1,1,0,0)

  where
    testLabel = "unlockScriptSameBlock"

failedScript :: IOManager -> [(Text, Text)] -> Assertion
failedScript =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync

      tx0 <- withAlonzoLedgerState interpreter $ Alonzo.mkLockByScriptTx (UTxOIndex 0) [False] 20000 20000
      _ <- forgeNextAndSubmit interpreter mockServer $ MockBlock [TxAlonzo tx0] (NodeId 1)

      let utxo0 = head (Alonzo.mkUTxOAlonzo tx0)
      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkUnlockScriptTx [UTxOPair utxo0] (UTxOIndex 1) (UTxOIndex 2) False 10000 500

      assertBlockNoBackoff dbSync 1
      assertAlonzoCounts dbSync (0,0,0,0,1,0,1,1)
  where
    testLabel = "failedScript"

failedScriptSameBlock :: IOManager -> [(Text, Text)] -> Assertion
failedScriptSameBlock =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      _ <- registerAllStakeCreds interpreter mockServer

      _ <- withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
        tx0 <- Alonzo.mkLockByScriptTx (UTxOIndex 0) [False] 20000 20000 st
        let utxo0 = head (Alonzo.mkUTxOAlonzo tx0)
        tx1 <- Alonzo.mkUnlockScriptTx [UTxOPair utxo0] (UTxOIndex 1) (UTxOIndex 2) False 10000 500 st
        pure [tx0, tx1]

      assertBlockNoBackoff dbSync 1
      assertAlonzoCounts dbSync (0,0,0,0,1,0,1,1)
  where
    testLabel = "failedScriptSameBlock"

multipleScripts :: IOManager -> [(Text, Text)] -> Assertion
multipleScripts =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    tx0 <- withAlonzoLedgerState interpreter $ Alonzo.mkLockByScriptTx (UTxOIndex 0) [True, False, True] 20000 20000
    let utxo = Alonzo.mkUTxOAlonzo tx0
        pair1 = head utxo
        pair2 = utxo !! 2
    tx1 <- withAlonzoLedgerState interpreter $
        Alonzo.mkUnlockScriptTx [UTxOPair pair1, UTxOPair pair2] (UTxOIndex 1) (UTxOIndex 2) True 10000 500

    _ <- forgeNextAndSubmit interpreter mockServer $ MockBlock [TxAlonzo tx0] (NodeId 1)
    _ <- forgeNextAndSubmit interpreter mockServer $ MockBlock [TxAlonzo tx1] (NodeId 1)

    assertBlockNoBackoff dbSync 1
    assertAlonzoCounts dbSync (1,2,1,1,3,2,0,0)
  where
    testLabel = "multipleScripts"

multipleScriptsSameBlock :: IOManager -> [(Text, Text)] -> Assertion
multipleScriptsSameBlock =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    _ <- withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkLockByScriptTx (UTxOIndex 0) [True, False, True] 20000 20000 st
      let utxo =  Alonzo.mkUTxOAlonzo tx0
          pair1 = head utxo
          pair2 = utxo !! 2
      tx1 <- Alonzo.mkUnlockScriptTx [UTxOPair pair1, UTxOPair pair2] (UTxOIndex 1) (UTxOIndex 2) True 10000 500 st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 0
    assertAlonzoCounts dbSync (1,2,1,1,3,2,0,0)
  where
    testLabel = "multipleScriptsSameBlock"

multipleScriptsFailed :: IOManager -> [(Text, Text)] -> Assertion
multipleScriptsFailed =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    tx0 <- withAlonzoLedgerState interpreter $ Alonzo.mkLockByScriptTx (UTxOIndex 0) [True, False, True] 20000 20000
    _ <- forgeNextAndSubmit interpreter mockServer $ MockBlock [TxAlonzo tx0] (NodeId 1)

    let utxos = Alonzo.mkUTxOAlonzo tx0
    tx1 <- withAlonzoLedgerState interpreter $
        Alonzo.mkUnlockScriptTx (UTxOPair <$> [head utxos, utxos !! 1, utxos !! 2]) (UTxOIndex 1) (UTxOIndex 2) False 10000 500
    _ <- forgeNextAndSubmit interpreter mockServer $ MockBlock [TxAlonzo tx1] (NodeId 1)

    assertBlockNoBackoff dbSync 1
    assertAlonzoCounts dbSync (0,0,0,0,3,0,1,1)
  where
    testLabel = "multipleScriptsFailed"

multipleScriptsFailedSameBlock :: IOManager -> [(Text, Text)] -> Assertion
multipleScriptsFailedSameBlock =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    _ <- withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkLockByScriptTx (UTxOIndex 0) [True, False, True] 20000 20000 st

      let utxos = tail $ Alonzo.mkUTxOAlonzo tx0
      tx1 <- Alonzo.mkUnlockScriptTx (UTxOPair <$> [head utxos, utxos !! 1, utxos !! 2]) (UTxOIndex 1) (UTxOIndex 2) False 10000 500 st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 0
    assertAlonzoCounts dbSync (0,0,0,0,3,0,1,1)
  where
    testLabel = "multipleScriptsFailedSameBlock"

registrationScriptTx :: IOManager -> [(Text, Text)] -> Assertion
registrationScriptTx =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkSimpleDCertTx [(StakeIndexScript True, DCertDeleg . RegKey)]
    assertBlockNoBackoff dbSync 0
    assertScriptCert dbSync (0,0,0,1)
  where
    testLabel = "registrationScriptTx"

deregistrationScriptTx :: IOManager -> [(Text, Text)] -> Assertion
deregistrationScriptTx =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    _ <- withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkSimpleDCertTx [(StakeIndexScript True, DCertDeleg . RegKey)] st
      tx1 <- Alonzo.mkScriptDCertTx [(StakeIndexScript True, True, DCertDeleg . DeRegKey)] True st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 0
    assertScriptCert dbSync (1,0,0,1)
  where
    testLabel = "deregistrationScriptTx"

deregistrationsScriptTxs :: IOManager -> [(Text, Text)] -> Assertion
deregistrationsScriptTxs =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    _ <- withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkSimpleDCertTx [(StakeIndexScript True, DCertDeleg . RegKey)] st
      tx1 <- Alonzo.mkScriptDCertTx [(StakeIndexScript True, True, DCertDeleg . DeRegKey)] True st
      tx2 <- Alonzo.mkSimpleDCertTx [(StakeIndexScript True, DCertDeleg .   RegKey)] st
      tx3 <- Alonzo.mkScriptDCertTx [(StakeIndexScript True, True, DCertDeleg . DeRegKey)] True st
      pure [tx0, tx1, Alonzo.addValidityInterval 1000 tx2, Alonzo.addValidityInterval 2000 tx3]

    assertBlockNoBackoff dbSync 0
    assertScriptCert dbSync (2,0,0,1)
    assertAlonzoCounts dbSync (1,2,1,0,0,0,0,0)
  where
    testLabel = "deregistrationsScriptTxs"

deregistrationsScriptTx :: IOManager -> [(Text, Text)] -> Assertion
deregistrationsScriptTx =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    _ <- withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkSimpleDCertTx [(StakeIndexScript True, DCertDeleg . RegKey)] st
      tx1 <- Alonzo.mkScriptDCertTx [ (StakeIndexScript True, True, DCertDeleg . DeRegKey)
                                    , (StakeIndexScript True, False, DCertDeleg .  RegKey)
                                    , (StakeIndexScript True, True, DCertDeleg . DeRegKey)]
                                    True st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 0
    assertScriptCert dbSync (2,0,0,1)
    assertAlonzoCounts dbSync (1,2,1,0,0,0,0,0)
  where
    testLabel = "deregistrationsScriptTx"

-- Like previous but missing a redeemer. This is a known ledger issue
deregistrationsScriptTx' :: IOManager -> [(Text, Text)] -> Assertion
deregistrationsScriptTx' =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    _ <- withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkSimpleDCertTx [(StakeIndexScript True, DCertDeleg . RegKey)] st
      tx1 <- Alonzo.mkScriptDCertTx [ (StakeIndexScript True, False, DCertDeleg . DeRegKey)
                               , (StakeIndexScript True, False, DCertDeleg .   RegKey)
                               , (StakeIndexScript True, True, DCertDeleg . DeRegKey)]
                               True st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 0
    -- TODO: This is a bug! The first field should be 2. However the deregistrations
    -- are missing the redeemers
    assertScriptCert dbSync (0,0,0,1)
    assertAlonzoCounts dbSync (1,1,1,0,0,0,0,0)
  where
    testLabel = "deregistrationsScriptTx'"

-- Like previous but missing the other redeemer. This is a known ledger issue
deregistrationsScriptTx'' :: IOManager -> [(Text, Text)] -> Assertion
deregistrationsScriptTx'' =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
    startDBSync  dbSync

    _ <- withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
      tx0 <- Alonzo.mkSimpleDCertTx [(StakeIndexScript True, DCertDeleg . RegKey)] st
      tx1 <- Alonzo.mkScriptDCertTx [ (StakeIndexScript True, True, DCertDeleg . DeRegKey)
                               , (StakeIndexScript True, False, DCertDeleg .   RegKey)
                               , (StakeIndexScript True, False, DCertDeleg . DeRegKey)]
                               True st
      pure [tx0, tx1]

    assertBlockNoBackoff dbSync 0
    assertScriptCert dbSync (2,0,0,1)
    assertAlonzoCounts dbSync (1,1,1,0,0,0,0,0)
  where
    testLabel = "deregistrationsScriptTx''"

mintMultiAsset ::  IOManager -> [(Text, Text)] -> Assertion
mintMultiAsset =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $ \st -> do
        let val0 = Value 1 $ Map.singleton (PolicyID alwaysMintScriptHash) (Map.singleton (head assetNames) 1)
        Alonzo.mkMAssetsScriptTx [UTxOIndex 0] (UTxOIndex 1) [(UTxOAddressNew 0, Value 10000 mempty)] val0 True 100 st

      assertBlockNoBackoff dbSync 0
      assertAlonzoCounts dbSync (1,1,1,1,0,0,0,0)
  where
    testLabel = "mintMultiAsset"

mintMultiAssets ::  IOManager -> [(Text, Text)] -> Assertion
mintMultiAssets =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      _ <- withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
        let assets0 = Map.fromList [(head assetNames,10), (assetNames !! 1,4)]
        let policy0 = PolicyID alwaysMintScriptHash
        let policy1 = PolicyID alwaysSucceedsScriptHash
        let val1 = Value 1 $ Map.fromList [(policy0, assets0), (policy1, assets0)]
        tx0 <- Alonzo.mkMAssetsScriptTx [UTxOIndex 0] (UTxOIndex 1) [(UTxOAddressNew 0, Value 10000 mempty)] val1 True 100 st
        tx1 <- Alonzo.mkMAssetsScriptTx [UTxOIndex 2] (UTxOIndex 3) [(UTxOAddressNew 0, Value 10000 mempty)] val1 True 200 st
        pure [tx0, tx1]

      assertBlockNoBackoff dbSync 0
      assertAlonzoCounts dbSync (2,4,1,2,0,0,0,0)
  where
    testLabel = "mintMultiAssets"

swapMultiAssets ::  IOManager -> [(Text, Text)] -> Assertion
swapMultiAssets =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync
      _ <- withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
          let assetsMinted0 = Map.fromList [(head assetNames, 10), (assetNames !! 1, 4)]
          let policy0 = PolicyID alwaysMintScriptHash
          let policy1 = PolicyID alwaysSucceedsScriptHash
          let mintValue0 = Value 100 $ Map.fromList [(policy0, assetsMinted0), (policy1, assetsMinted0)]
          let assets0 = Map.fromList [(head assetNames, 5), (assetNames !! 1, 2)]
          let outValue0 = Value 20 $ Map.fromList [(policy0, assets0), (policy1, assets0)]

          tx0 <- Alonzo.mkMAssetsScriptTx [UTxOIndex 0] (UTxOIndex 1)
                  [(UTxOAddress alwaysSucceedsScriptAddr, outValue0), (UTxOAddress alwaysMintScriptAddr, outValue0)] mintValue0 True 100 st

          let utxos = Alonzo.mkUTxOAlonzo tx0
          tx1 <- Alonzo.mkMAssetsScriptTx
            [UTxOPair (head utxos), UTxOPair (utxos !! 1), UTxOIndex 2]
            (UTxOIndex 3)
            [ (UTxOAddress alwaysSucceedsScriptAddr, outValue0), (UTxOAddress alwaysMintScriptAddr, outValue0)
            , (UTxOAddressNew 0, outValue0), (UTxOAddressNew 0, outValue0)]
            mintValue0 True 200 st
          pure [tx0, tx1]

      assertBlockNoBackoff dbSync 0
      assertAlonzoCounts dbSync (2,6,1,2,4,2,0,0)
  where
    testLabel = "swapMultiAssets"

poolReg ::  IOManager -> [(Text, Text)] -> Assertion
poolReg =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync

      _ <- forgeNextFindLeaderAndSubmit interpreter mockServer []
      assertBlockNoBackoff dbSync 0
      initCounter <- runQuery dbSync poolCountersQuery
      assertEqual "Unexpected init pool counter" (3,0,3,2,0,0) initCounter

      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkDCertPoolTx
          [( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
           , PoolIndexNew 0
           , Alonzo.consPoolParamsTwoOwners)]

      assertBlockNoBackoff dbSync 1
      assertPoolCounters dbSync (addPoolCounters (1,1,1,2,0,1) initCounter)
      st <- getAlonzoLedgerState interpreter
      assertPoolLayerCounters dbSync (0,0) [(PoolIndexNew 0, (Right False, False, True))] st
  where
    testLabel = "poolReg"

-- Issue https://github.com/input-output-hk/cardano-db-sync/issues/997
nonexistantPoolQuery ::  IOManager -> [(Text, Text)] -> Assertion
nonexistantPoolQuery =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync

      _ <- forgeNextFindLeaderAndSubmit interpreter mockServer []
      assertBlockNoBackoff dbSync 0

      st <- getAlonzoLedgerState interpreter
      assertPoolLayerCounters dbSync (0,0) [(PoolIndexNew 0, (Left RecordDoesNotExist, False, False))] st

  where
    testLabel = "nonexistantPoolQuery"

poolDeReg ::  IOManager -> [(Text, Text)] -> Assertion
poolDeReg =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync

      _ <- forgeNextFindLeaderAndSubmit interpreter mockServer []
      assertBlockNoBackoff dbSync 0
      initCounter <- runQuery dbSync poolCountersQuery
      assertEqual "Unexpected init pool counter" (3,0,3,2,0,0) initCounter

      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkDCertPoolTx
          [ ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Alonzo.consPoolParamsTwoOwners)

          , ([], PoolIndexNew 0, \_ poolId -> DCertPool $ RetirePool poolId 1)
          ]

      assertBlockNoBackoff dbSync 1
      assertPoolCounters dbSync (addPoolCounters (1,1,1,2,1,1) initCounter)

      st <- getAlonzoLedgerState interpreter
      -- Not retired yet, because epoch has not changed
      assertPoolLayerCounters dbSync (0,0) [(PoolIndexNew 0, (Right False, False, True))] st

      -- change epoch
      a <- fillUntilNextEpoch interpreter mockServer

      assertBlockNoBackoff dbSync (fromIntegral $ length a + 1)
      -- these counters are the same
      assertPoolCounters dbSync (addPoolCounters (1,1,1,2,1,1) initCounter)

      -- the pool is now retired, since the epoch changed.
      assertPoolLayerCounters dbSync (1,0) [(PoolIndexNew 0, (Right True, False, False))] st

  where
    testLabel = "poolDeReg"

poolDeRegMany :: IOManager -> [(Text, Text)] -> Assertion
poolDeRegMany =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync

      _ <- forgeNextFindLeaderAndSubmit interpreter mockServer []
      assertBlockNoBackoff dbSync 0
      initCounter <- runQuery dbSync poolCountersQuery
      assertEqual "Unexpected init pool counter" (3,0,3,2,0,0) initCounter

      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkDCertPoolTx
          [
            -- register
            ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Alonzo.consPoolParamsTwoOwners)

            -- de register
          , ([], PoolIndexNew 0, mkPoolDereg 4)

            -- register
          , ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Alonzo.consPoolParamsTwoOwners)

            -- register with different owner and reward address
          , ( [StakeIndexNew 2, StakeIndexNew 1, StakeIndexNew 0]
            , PoolIndexNew 0
            , Alonzo.consPoolParamsTwoOwners)
          ]

      _ <- withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
        tx0 <- Alonzo.mkDCertPoolTx
          [ -- register
           ( [StakeIndexNew 2, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Alonzo.consPoolParamsTwoOwners)
          ] st

        tx1 <- Alonzo.mkDCertPoolTx
          [ -- deregister
            ([] :: [StakeIndex], PoolIndexNew 0, mkPoolDereg 4)

            -- register
          , ( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
            , PoolIndexNew 0
            , Alonzo.consPoolParamsTwoOwners)

             -- deregister
          ,  ([] :: [StakeIndex], PoolIndexNew 0, mkPoolDereg 1)
          ] st
        pure [tx0, tx1]

      assertBlockNoBackoff dbSync 2
      -- TODO fix PoolOwner and PoolRelay unique key
      assertPoolCounters dbSync (addPoolCounters (1,1,5,10,3,5) initCounter)

      st <- getAlonzoLedgerState interpreter
      -- Not retired yet, because epoch has not changed
      assertPoolLayerCounters dbSync (0,0) [(PoolIndexNew 0, (Right False, False, True))] st

      -- change epoch
      a <- fillUntilNextEpoch interpreter mockServer

      assertBlockNoBackoff dbSync (fromIntegral $ length a + 2)
      -- these counters are the same
      assertPoolCounters dbSync (addPoolCounters (1,1,5,10,3,5) initCounter)

      -- from all these certificates only the latest matters. So it will retire
      -- on epoch 0
      assertPoolLayerCounters dbSync (1,0) [(PoolIndexNew 0, (Right True, False, False))] st

  where
    testLabel = "poolDeRegMany"
    mkPoolDereg :: EpochNo
                -> [StakeCredential StandardCrypto]
                -> KeyHash 'StakePool StandardCrypto
                -> DCert StandardCrypto
    mkPoolDereg epochNo _creds keyHash = DCertPool $ RetirePool keyHash epochNo

poolDelist :: IOManager -> [(Text, Text)] -> Assertion
poolDelist =
    withFullConfig "config" testLabel $ \interpreter mockServer dbSync -> do
      startDBSync  dbSync

      _ <- forgeNextFindLeaderAndSubmit interpreter mockServer []
      assertBlockNoBackoff dbSync 0
      initCounter <- runQuery dbSync poolCountersQuery
      assertEqual "Unexpected init pool counter" (3,0,3,2,0,0) initCounter

      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkDCertPoolTx
          [( [StakeIndexNew 0, StakeIndexNew 1, StakeIndexNew 2]
           , PoolIndexNew 0
           , Alonzo.consPoolParamsTwoOwners)]

      _ <- forgeNextFindLeaderAndSubmit interpreter mockServer []
      assertBlockNoBackoff dbSync 2
      st <- getAlonzoLedgerState interpreter
      assertPoolLayerCounters dbSync (0,0) [(PoolIndexNew 0, (Right False, False, True))] st

      let poolKeyHash = resolvePool (PoolIndexNew 0) st
      let poolId = dbToServantPoolId $ unKeyHashRaw poolKeyHash
      _ <- dlAddDelistedPool (getPoolLayer dbSync) poolId

      -- This is not async, so we don't need to do exponential backoff
      -- delisted not retired
      assertPoolLayerCounters dbSync (0,1) [(PoolIndexNew 0, (Right False, True, True))] st

      _ <- withAlonzoFindLeaderAndSubmitTx interpreter mockServer $
        Alonzo.mkDCertPoolTx
          [ ([], PoolIndexNew 0, \_ poolHash -> DCertPool $ RetirePool poolHash 1)]

      _ <- forgeNextFindLeaderAndSubmit interpreter mockServer []
      assertBlockNoBackoff dbSync 4
      -- delisted and pending retirement
      assertPoolLayerCounters dbSync (0,1) [(PoolIndexNew 0, (Right False, True, True))] st

      a <- fillUntilNextEpoch interpreter mockServer

      _ <- forgeNextFindLeaderAndSubmit interpreter mockServer []
      assertBlockNoBackoff dbSync (fromIntegral $ 5 + length a)
      -- delisted and retired
      assertPoolLayerCounters dbSync (1,1) [(PoolIndexNew 0, (Right True, True, False))] st
  where
    testLabel = "poolDelist"


hfBlockHash :: CardanoBlock -> ByteString
hfBlockHash blk = case blk of
  BlockShelley sblk -> blockHash sblk
  BlockAlonzo ablk -> blockHash ablk
  _ -> error "not supported block type"

throwLeft :: Exception err => IO (Either err a) -> IO a
throwLeft action = do
  ma <- action
  case ma of
    Left err -> throwIO err
    Right a -> pure a
