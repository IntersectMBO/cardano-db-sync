{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Test.Cardano.Db.Mock.Unit.Conway.Governance (
  drepDistr,
  newCommittee,
  rollbackNewCommittee,
  rollbackNewCommitteeProposal,
  updateConstitution,
  treasuryWithdrawal,
  parameterChange,
  chainedNewCommittee,
  hardFork,
  hardForkPostBlock,
  rollbackHardFork,
  infoAction,
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Era.Shelley.Generic.Util (unCredentialHash, unTxHash)
import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.BaseTypes (AnchorData (..), Network (..), textToUrl)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Governance (GovActionId (..), GovActionIx (..))
import qualified Cardano.Ledger.Conway.Governance as Governance
import Cardano.Ledger.Core (hashAnnotated, txIdTx)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Hashes (SafeToHash (..))
import Cardano.Mock.ChainSync.Server (IOManager, ServerHandle)
import Cardano.Mock.Forging.Interpreter (Interpreter, getCurrentEpoch)
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import qualified Cardano.Mock.Forging.Tx.Generic as Forging
import Cardano.Mock.Forging.Types
import Cardano.Prelude (MonadIO (..), void)
import Cardano.Slotting.Slot (EpochNo (..))
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Text (Text)
import qualified Ouroboros.Consensus.Shelley.Eras as Consensus
import Ouroboros.Network.Block (blockPoint)
import Test.Cardano.Db.Mock.Config
import qualified Test.Cardano.Db.Mock.UnifiedApi as Api
import Test.Cardano.Db.Mock.Validate
import Test.Tasty.HUnit (Assertion, assertFailure)

drepDistr :: IOManager -> [(Text, Text)] -> Assertion
drepDistr =
  withFullConfigDropDB conwayConfigDir testLabel $ \interpreter server dbSync -> do
    startDBSync dbSync

    -- Register SPOs, DReps, and committee to vote
    epoch1 <- initGovernance interpreter server

    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length epoch1)

    -- Should now have a DRep distribution
    let drepId = Prelude.head Forging.unregisteredDRepIds
    assertEqQuery
      dbSync
      (DB.queryDRepDistrAmount (unCredentialHash drepId) 1)
      10_000
      "Unexpected drep distribution amount"
  where
    testLabel = "conwayDrepDistr"

initGovernance :: Interpreter -> ServerHandle IO CardanoBlock -> IO [CardanoBlock]
initGovernance interpreter server = do
  -- Add stake
  blk1 <- Api.registerAllStakeCreds interpreter server
  -- Register a DRep and delegate votes to it
  blk2 <- Api.registerDRepsAndDelegateVotes interpreter server
  -- DRep distribution is calculated at end of the current epoch
  epoch0 <- Api.fillUntilNextEpoch interpreter server
  -- Register committee hot credentials
  blk3 <- Api.registerCommitteeCreds interpreter server

  pure $ (blk1 : blk2 : epoch0) ++ [blk3]

newCommittee :: IOManager -> [(Text, Text)] -> Assertion
newCommittee =
  withFullConfig conwayConfigDir testLabel $ \interpreter server dbSync -> do
    startDBSync dbSync

    -- Register SPOs, DReps, and committee to vote
    epoch1 <- initGovernance interpreter server
    -- Propose, ratify, and enact a new committee member
    epoch3 <- enactNewCommittee interpreter server

    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length $ epoch1 <> epoch3)
    -- Should now have a committee member
    assertEqQuery
      dbSync
      DB.queryGovActionCounts
      (1, 1, 0, 0)
      "Unexpected governance action counts"
  where
    testLabel = "conwayNewCommittee"

rollbackNewCommittee :: IOManager -> [(Text, Text)] -> Assertion
rollbackNewCommittee =
  withFullConfig conwayConfigDir testLabel $ \interpreter server dbSync -> do
    startDBSync dbSync

    -- Register SPOs, DReps, and committee to vote
    epoch1 <- initGovernance interpreter server
    -- Propose, ratify, and enact a new committee member
    epoch3 <- enactNewCommittee interpreter server
    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length $ epoch1 <> epoch3)
    -- Should now have a committee member
    assertEqQuery
      dbSync
      DB.queryGovActionCounts
      (1, 1, 0, 0)
      "Unexpected governance action counts"

    -- Rollback the last 2 blocks
    epoch1' <- rollbackBlocks interpreter server 2 epoch3
    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length $ epoch1 <> epoch1')
    -- Should not have a new committee member
    assertEqQuery
      dbSync
      DB.queryGovActionCounts
      (1, 0, 0, 0)
      "Unexpected governance action counts"

    -- Fast forward to next epoch
    epoch2' <- Api.fillUntilNextEpoch interpreter server
    assertBlockNoBackoff dbSync (length $ epoch1 <> epoch1' <> epoch2')
    -- Should now have 2 identical committees
    assertEqQuery
      dbSync
      (DB.queryEpochStateCount 3)
      2
      "Unexpected epoch state count for epoch 3"
  where
    testLabel = "conwayRollbackNewCommittee"

chainedNewCommittee :: IOManager -> [(Text, Text)] -> Assertion
chainedNewCommittee =
  withFullConfig conwayConfigDir testLabel $ \interpreter server dbSync -> do
    startDBSync dbSync

    -- Register SPOs, DReps, and committee to vote
    epoch1 <- initGovernance interpreter server

    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length epoch1)
    -- Should start with 4 committee members
    assertEqQuery
      dbSync
      (DB.queryCommitteeMemberCountByTxHash Nothing)
      4
      "Unexpected committee member count"

    let
      -- Propose a new committee member
      committee1Cred = Prelude.head Forging.unregisteredCommitteeCreds
      proposal1 = Conway.mkAddCommitteeTx Nothing committee1Cred

      -- Propose another, using proposal1 as the prev governance action
      committee2Cred = Forging.unregisteredCommitteeCreds Prelude.!! 1
      proposal2 = Conway.mkAddCommitteeTx (Just prevGovActionId) committee2Cred
      proposal2TxHash = unTxHash (txIdTx proposal2)

      prevGovActionId =
        Governance.GovPurposeId $
          Governance.GovActionId
            { gaidTxId = txIdTx proposal1
            , gaidGovActionIx = GovActionIx 0
            }

    void $ Api.withConwayFindLeaderAndSubmit interpreter server $ \_ ->
      Right [proposal1, proposal2]

    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length epoch1 + 1)
    -- Should now have 6 members
    assertEqQuery
      dbSync
      (DB.queryCommitteeMemberCountByTxHash $ Just proposal2TxHash)
      6
      "Unexpected committee member count"
  where
    testLabel = "conwayChainedNewCommittee"

rollbackNewCommitteeProposal :: IOManager -> [(Text, Text)] -> Assertion
rollbackNewCommitteeProposal =
  withFullConfig conwayConfigDir testLabel $ \interpreter server dbSync -> do
    startDBSync dbSync

    -- Register SPOs, DReps, and committee to vote
    epoch1 <- initGovernance interpreter server

    -- Propose a new committee member
    let proposal = proposeNewCommittee
        proposalTxHash = unTxHash (txIdTx proposal)
    void $ Api.withConwayFindLeaderAndSubmit interpreter server $ \_ ->
      Right [proposal]

    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length epoch1 + 1)
    -- Should have a new committee
    assertBackoff
      dbSync
      (DB.queryCommitteeByTxHash proposalTxHash)
      defaultDelays
      isJust
      (const "Expected at least one new committee")

    -- Rollback one block
    epoch1' <- rollbackBlocks interpreter server 1 epoch1
    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length epoch1')
    -- Should NOT have a new committee
    assertBackoff
      dbSync
      (DB.queryCommitteeByTxHash proposalTxHash)
      defaultDelays
      isNothing
      (const "Unexpected new committee")
  where
    testLabel = "conwayRollbackNewCommitteeProposal"

enactNewCommittee :: Interpreter -> ServerHandle IO CardanoBlock -> IO [CardanoBlock]
enactNewCommittee interpreter server = do
  blk <-
    Api.withConwayFindLeaderAndSubmit interpreter server $ \ledger -> do
      let
        -- Create gov action tx
        addCcTx = proposeNewCommittee
        -- Create votes for all stake pools. We start in the Conway bootstrap phase, so
        -- DRep votes are not yet required.
        addVoteTx =
          Conway.mkGovVoteYesTx
            govActionId
            (Forging.drepVoters ++ Forging.resolveStakePoolVoters ledger)
        govActionId =
          GovActionId
            { gaidTxId = txIdTx addCcTx
            , gaidGovActionIx = GovActionIx 0
            }

      -- Create votes
      pure [addCcTx, addVoteTx]

  -- It takes 2 epochs to enact a proposal--ratification will happen on the next
  -- epoch and enacted on the following.
  epochs <- Api.fillEpochs interpreter server 2
  pure (blk : epochs)

proposeNewCommittee :: Core.Tx Consensus.ConwayEra
proposeNewCommittee =
  Conway.mkAddCommitteeTx Nothing committeeCred
  where
    committeeCred = Prelude.head Forging.unregisteredCommitteeCreds

rollbackBlocks ::
  Interpreter ->
  ServerHandle IO CardanoBlock ->
  Int ->
  [CardanoBlock] ->
  IO [CardanoBlock]
rollbackBlocks interpreter server n blocks = do
  (rollbackPoint, blocks') <-
    case drop n (reverse blocks) of
      (blk : blks) -> pure (blockPoint blk, blk : blks)
      [] -> assertFailure $ "Expected at least " <> show n <> " blocks"

  -- Rollback to the previous epoch
  Api.rollbackTo interpreter server rollbackPoint
  -- Create a fork
  newBlock <-
    Api.withConwayFindLeaderAndSubmitTx interpreter server $
      Conway.mkSimpleDCertTx [(StakeIndexNew 1, Conway.mkRegTxCert SNothing)]

  pure $ reverse (newBlock : blocks')

updateConstitution :: IOManager -> [(Text, Text)] -> Assertion
updateConstitution =
  withFullConfig conwayConfigDir testLabel $ \interpreter server dbSync -> do
    startDBSync dbSync

    -- Register SPOs, DReps, and committee to vote
    epoch1 <- initGovernance interpreter server

    let newUrl = fromJust (textToUrl 64 "constitution.new")
        dataHash = hashAnnotated (AnchorData "constitution content")
        anchor = Governance.Anchor newUrl dataHash

    -- Create and vote for a governance proposal
    void $
      Api.withConwayFindLeaderAndSubmit interpreter server $ \_ -> do
        let
          -- Create gov action tx
          proposalTx = Conway.mkNewConstitutionTx anchor

          -- Create votes
          addVoteTx =
            Conway.mkGovVoteYesTx
              govActionId
              (Forging.drepVoters ++ Forging.committeeVoters)
          govActionId =
            GovActionId
              { gaidTxId = txIdTx proposalTx
              , gaidGovActionIx = GovActionIx 0
              }

        pure [proposalTx, addVoteTx]

    -- It takes 2 epochs to enact a proposal--ratification will happen on the next
    -- epoch and enacted on the following.
    epoch3 <- Api.fillEpochs interpreter server 2

    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length (epoch1 <> epoch3) + 1)

    -- Constitution should now be updated
    (EpochNo epochNo) <- getCurrentEpoch interpreter
    assertEqQuery
      dbSync
      (DB.queryConstitutionAnchor epochNo)
      (Just ("constitution.new", originalBytes dataHash))
      "Unexpected constution voting anchor"
  where
    testLabel = "conwayUpdateConstitution"

treasuryWithdrawal :: IOManager -> [(Text, Text)] -> Assertion
treasuryWithdrawal =
  withFullConfig conwayConfigDir testLabel $ \interpreter server dbSync -> do
    startDBSync dbSync

    -- Register SPOs, DReps, and committee to vote
    epoch1 <- initGovernance interpreter server

    -- Make sure we have treasury to spend
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter server $ \_ ->
        Right $ Conway.mkDonationTx (Coin 50_000)

    -- Create and vote for a governance proposal
    void $
      Api.withConwayFindLeaderAndSubmit interpreter server $ \ledger -> do
        rewardAccount <-
          RewardAccount Testnet <$> Forging.resolveStakeCreds (StakeIndex 0) ledger

        let
          proposalTx =
            Conway.mkTreasuryWithdrawalTx
              rewardAccount
              (Coin 10_000)

          addVoteTx =
            Conway.mkGovVoteYesTx
              govActionId
              (Forging.drepVoters ++ Forging.committeeVoters)

          govActionId =
            GovActionId
              { gaidTxId = txIdTx proposalTx
              , gaidGovActionIx = GovActionIx 0
              }

        pure [proposalTx, addVoteTx]

    -- It takes 2 epochs to enact a proposal--ratification will happen on the next
    -- epoch and enacted on the following.
    epoch3 <- Api.fillEpochs interpreter server 2

    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length (epoch1 <> epoch3) + 2)

    -- Should now have a treasury reward
    assertEqQuery
      dbSync
      DB.queryRewardRests
      [(DB.RwdTreasury, 10_000)]
      "Unexpected constution voting anchor"
  where
    testLabel = "conwayTreasuryWithdrawal"

parameterChange :: IOManager -> [(Text, Text)] -> Assertion
parameterChange =
  withFullConfig conwayConfigDir testLabel $ \interpreter server dbSync -> do
    startDBSync dbSync

    -- Register SPOs, DReps, and committee to vote
    epoch1 <- initGovernance interpreter server

    -- Create and vote for gov action
    void $
      Api.withConwayFindLeaderAndSubmit interpreter server $ \ledger -> do
        let
          -- Create gov action tx
          govActionTx = Conway.mkParamChangeTx
          -- Create votes for all stake pools. We start in the Conway bootstrap phase, so
          -- DRep votes are not yet required.
          addVoteTx =
            Conway.mkGovVoteYesTx
              govActionId
              ( Forging.drepVoters
                  ++ Forging.committeeVoters
                  ++ Forging.resolveStakePoolVoters ledger
              )
          govActionId =
            GovActionId
              { gaidTxId = txIdTx govActionTx
              , gaidGovActionIx = GovActionIx 0
              }

        -- Create votes
        pure [govActionTx, addVoteTx]

    -- It takes 2 epochs to enact a proposal--ratification will happen on the next
    -- epoch and enacted on the following.
    epoch3 <- Api.fillEpochs interpreter server 2

    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length (epoch1 <> epoch3) + 1)
    -- Should now have a ratified/enacted governance action
    assertEqQuery
      dbSync
      DB.queryGovActionCounts
      (1, 1, 0, 0)
      "Unexpected governance action counts"
    -- Should have updated param
    assertEqQuery
      dbSync
      (queryMaxTxSize interpreter)
      (Just 32_000)
      "Unexpected protocol parameter (max tx size)"
  where
    testLabel = "conwayGovParameterChange"
    queryMaxTxSize interpreter = do
      epochNo <- getEpochNo interpreter
      param <- DB.queryParamWithEpochNo epochNo
      pure (DB.epochParamMaxTxSize <$> param)
    getEpochNo = fmap unEpochNo . liftIO . getCurrentEpoch

hardFork :: IOManager -> [(Text, Text)] -> Assertion
hardFork =
  withFullConfig conwayConfigDir testLabel $ \interpreter server dbSync -> do
    startDBSync dbSync

    -- Register SPOs, DReps, and committee to vote
    epoch1 <- initGovernance interpreter server
    -- Propose, ratify, and enact a hard fork
    epoch3 <- enactHardFork interpreter server

    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length $ epoch1 <> epoch3)
    -- Should now have a ratified/enacted governance action
    assertEqQuery
      dbSync
      DB.queryGovActionCounts
      (1, 1, 0, 0)
      "Unexpected governance action counts"
    -- Should have a new major protocol version
    assertEqQuery
      dbSync
      ( do
          epochNo <- getEpochNo interpreter
          mEpochParam <- DB.queryEpochParamWithEpochNo epochNo
          pure $ DB.epochParamProtocolMajor <$> mEpochParam
      )
      (Just 11)
      "Unexpected governance action counts"
  where
    testLabel = "conwayGovernanceHardFork"
    getEpochNo = fmap unEpochNo . liftIO . getCurrentEpoch

-- | Tests that db-sync continues to sync correctly after an intra-era hard fork
-- (PV 10 -> PV 11 within Conway). After enacting the HF, we forge additional blocks
-- containing payment transactions and governance actions, then verify db-sync
-- indexes them without errors.
hardForkPostBlock :: IOManager -> [(Text, Text)] -> Assertion
hardForkPostBlock =
  withFullConfig conwayConfigDir testLabel $ \interpreter server dbSync -> do
    startDBSync dbSync

    -- Register SPOs, DReps, and committee to vote
    epoch1 <- initGovernance interpreter server
    -- Propose, ratify, and enact a hard fork (PV 10 -> 11)
    epoch3 <- enactHardFork interpreter server

    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length $ epoch1 <> epoch3)
    -- Verify the HF was enacted
    assertEqQuery
      dbSync
      ( do
          epochNo <- getEpochNo interpreter
          mEpochParam <- DB.queryEpochParamWithEpochNo epochNo
          pure $ DB.epochParamProtocolMajor <$> mEpochParam
      )
      (Just 11)
      "Unexpected protocol major version after hard fork"

    -- === Post-HF: forge blocks with transactions under PV 11 ===

    -- 1. Simple payment transaction
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter server $
        Conway.mkPaymentTx (UTxOIndex 0) (UTxOIndex 1) 10_000 500 0
    assertBlockNoBackoff dbSync (length (epoch1 <> epoch3) + 1)

    -- 2. Forge a few empty blocks to ensure the chain progresses
    emptyBlks <- Api.forgeAndSubmitBlocks interpreter server 3
    assertBlockNoBackoff dbSync (length (epoch1 <> epoch3) + 1 + length emptyBlks)

    -- 3. A governance info action to verify governance still works post-HF
    void $
      Api.withConwayFindLeaderAndSubmit interpreter server $ \_ ->
        pure [Conway.mkInfoTx]
    assertBlockNoBackoff dbSync (length (epoch1 <> epoch3) + 1 + length emptyBlks + 1)

    -- 4. Fill to next epoch boundary to exercise epoch processing post-HF
    epochNext <- Api.fillUntilNextEpoch interpreter server
    let totalBlocks = length (epoch1 <> epoch3) + 1 + length emptyBlks + 1 + length epochNext
    assertBlockNoBackoff dbSync totalBlocks

    -- Verify the HF governance action is still ratified/enacted
    assertEqQuery
      dbSync
      DB.queryGovActionCounts
      (1, 1, 0, 0)
      "Unexpected governance action counts after post-HF blocks"

    -- Verify db-sync is still running (hasn't crashed)
    checkStillRuns dbSync
  where
    testLabel = "conwayGovernanceHardForkPostBlock"
    getEpochNo = fmap unEpochNo . liftIO . getCurrentEpoch

rollbackHardFork :: IOManager -> [(Text, Text)] -> Assertion
rollbackHardFork =
  withFullConfig conwayConfigDir testLabel $ \interpreter server dbSync -> do
    startDBSync dbSync

    -- Register SPOs, DReps, and committee to vote
    epoch1 <- initGovernance interpreter server
    -- Propose, ratify, and enact a hard fork
    epoch3 <- enactHardFork interpreter server

    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length $ epoch1 <> epoch3)
    -- Should now have a ratified/enacted governance action
    assertEqQuery
      dbSync
      DB.queryGovActionCounts
      (1, 1, 0, 0)
      "Unexpected governance action counts"
    -- Should have a new major protocol version
    assertEqQuery
      dbSync
      ( do
          epochNo <- getEpochNo interpreter
          mEpochParam <- DB.queryEpochParamWithEpochNo epochNo
          pure $ DB.epochParamProtocolMajor <$> mEpochParam
      )
      (Just 11)
      "Unexpected governance action counts"

    -- Rollback the last 2 blocks
    epoch2 <- rollbackBlocks interpreter server 2 epoch3
    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length $ epoch1 <> epoch2)
    -- Should not have a new committee member
    assertEqQuery
      dbSync
      DB.queryGovActionCounts
      (1, 0, 0, 0)
      "Unexpected governance action counts"
    -- Should have the old major protocol version
    assertEqQuery
      dbSync
      ( do
          epochNo <- getEpochNo interpreter
          mEpochParam <- DB.queryEpochParamWithEpochNo epochNo
          pure $ DB.epochParamProtocolMajor <$> mEpochParam
      )
      (Just 10)
      "Unexpected governance action counts"

    -- Fast forward to next epoch
    epoch3' <- Api.fillUntilNextEpoch interpreter server
    assertBlockNoBackoff dbSync (length $ epoch1 <> epoch2 <> epoch3')
    -- Should once again have a new major protocol version
    assertEqQuery
      dbSync
      ( do
          epochNo <- getEpochNo interpreter
          mEpochParam <- DB.queryEpochParamWithEpochNo epochNo
          pure $ DB.epochParamProtocolMajor <$> mEpochParam
      )
      (Just 11)
      "Unexpected governance action counts"
  where
    testLabel = "conwayRollbackHardFork"
    getEpochNo = fmap unEpochNo . liftIO . getCurrentEpoch

enactHardFork :: Interpreter -> ServerHandle IO CardanoBlock -> IO [CardanoBlock]
enactHardFork interpreter server = do
  blk <-
    Api.withConwayFindLeaderAndSubmit interpreter server $ \ledger -> do
      let
        -- Create gov action tx
        govActionTx = Conway.mkHardForkInitTx
        -- Crate vote tx
        addVoteTx =
          Conway.mkGovVoteYesTx
            govActionId
            ( Forging.drepVoters
                ++ Forging.committeeVoters
                ++ Forging.resolveStakePoolVoters ledger
            )
        govActionId =
          GovActionId
            { gaidTxId = txIdTx govActionTx
            , gaidGovActionIx = GovActionIx 0
            }

      -- Create votes
      pure [govActionTx, addVoteTx]

  -- It takes 2 epochs to enact a proposal--ratification will happen on the next
  -- epoch and enacted on the following.
  epoch2 <- Api.fillEpochs interpreter server 2

  pure (blk : epoch2)

infoAction :: IOManager -> [(Text, Text)] -> Assertion
infoAction =
  withFullConfig conwayConfigDir testLabel $ \interpreter server dbSync -> do
    startDBSync dbSync

    -- Register SPOs, DReps, and committee to vote
    epoch1 <- initGovernance interpreter server

    let
      -- Create gov action tx
      govActionTx = Conway.mkInfoTx

      -- Create vote tx
      addVoteTx =
        Conway.mkGovVoteTx
          govActionId
          ( Map.fromList
              [
                ( Governance.DRepVoter (Prelude.head Forging.unregisteredDRepIds)
                , Governance.VoteYes
                )
              ,
                ( Governance.CommitteeVoter (snd $ Prelude.head Forging.bootstrapCommitteeCreds)
                , Governance.VoteNo
                )
              ,
                ( Governance.CommitteeVoter (snd $ Forging.bootstrapCommitteeCreds Prelude.!! 1)
                , Governance.Abstain
                )
              ]
          )

      govActionId =
        GovActionId
          { gaidTxId = txIdTx govActionTx
          , gaidGovActionIx = GovActionIx 0
          }

    -- Submit them
    void $
      Api.withConwayFindLeaderAndSubmit interpreter server $ \_ ->
        pure [govActionTx, addVoteTx]

    -- There is no ratification/enactment for info actions, so let it expire
    epoch11 <- Api.fillEpochs interpreter server 10

    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length (epoch1 <> epoch11) + 1)
    -- Should now be expired and dropped
    assertEqQuery
      dbSync
      DB.queryGovActionCounts
      (0, 0, 1, 1)
      "Unexpected governance action counts"
    -- Should have votes
    assertEqQuery
      dbSync
      (DB.queryVoteCounts (unTxHash $ txIdTx addVoteTx) 0)
      (1, 1, 1)
      "Unexpected governance action counts"
  where
    testLabel = "conwayGovernanceInfo"
