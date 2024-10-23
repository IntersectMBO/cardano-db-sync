{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Test.Cardano.Db.Mock.Unit.Conway.Governance (
  drepDistr,
  newCommittee,
  updateConstitution,
  treasuryWithdrawal,
) where

import qualified Cardano.Db as Db
import Cardano.DbSync.Era.Shelley.Generic.Util (unCredentialHash)
import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.BaseTypes (AnchorData (..), Network (..), hashAnchorData, textToUrl)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Governance (GovActionId (..), GovActionIx (..), Voter (..))
import qualified Cardano.Ledger.Conway.Governance as Governance
import Cardano.Ledger.Core (txIdTx)
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Mock.ChainSync.Server (IOManager)
import Cardano.Mock.Forging.Interpreter (getCurrentEpoch)
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import qualified Cardano.Mock.Forging.Tx.Generic as Forging
import Cardano.Mock.Forging.Types
import qualified Cardano.Mock.Query as Query
import Cardano.Prelude
import Cardano.Slotting.Slot (EpochNo (..))
import Data.Maybe (fromJust)
import qualified Ouroboros.Consensus.Shelley.Eras as Consensus
import Test.Cardano.Db.Mock.Config
import qualified Test.Cardano.Db.Mock.UnifiedApi as Api
import Test.Cardano.Db.Mock.Validate
import Test.Tasty.HUnit (Assertion)
import qualified Prelude

drepDistr :: IOManager -> [(Text, Text)] -> Assertion
drepDistr =
  withFullConfigAndDropDB conwayConfigDir testLabel $ \interpreter server dbSync -> do
    startDBSync dbSync

    -- Add stake
    void (Api.registerAllStakeCreds interpreter server)

    -- Register DRep and delegate votes to it
    void (Api.registerDRepsAndDelegateVotes interpreter server)

    -- DRep distribution is calculated at end of the current epoch
    epoch1 <- Api.fillUntilNextEpoch interpreter server

    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length epoch1 + 2)

    -- Should now have a DRep distribution
    let drepId = Prelude.head Forging.unregisteredDRepIds
    assertEqQuery
      dbSync
      (Query.queryDRepDistrAmount (unCredentialHash drepId) 1)
      10_000
      "Unexpected drep distribution amount"
  where
    testLabel = "conwayDrepDistr"

newCommittee :: IOManager -> [(Text, Text)] -> Assertion
newCommittee =
  withFullConfigAndLogs conwayConfigDir testLabel $ \interpreter server dbSync -> do
    startDBSync dbSync

    -- Add stake
    void (Api.registerAllStakeCreds interpreter server)

    -- Register a DRep and delegate votes to it
    void (Api.registerDRepsAndDelegateVotes interpreter server)

    -- Create and vote for gov action
    let committeeHash = "e0a714319812c3f773ba04ec5d6b3ffcd5aad85006805b047b082541"
        committeeCred = KeyHashObj (KeyHash committeeHash)

    void $
      Api.withConwayFindLeaderAndSubmit interpreter server $ \ledger -> do
        let
          -- Create gov action tx
          addCcTx = Conway.mkAddCommitteeTx committeeCred
          -- Create votes for all stake pools. We start in the Conway bootstrap phase, so
          -- DRep votes are not yet required.
          addVoteTx =
            Conway.mkGovVoteTx
              govActionId
              [ DRepVoter (Prelude.head Forging.unregisteredDRepIds)
              , StakePoolVoter (Forging.resolvePool (PoolIndex 0) ledger)
              , StakePoolVoter (Forging.resolvePool (PoolIndex 1) ledger)
              , StakePoolVoter (Forging.resolvePool (PoolIndex 2) ledger)
              ]
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

    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length epochs + 3)
    -- Should now have a committee member
    assertEqQuery
      dbSync
      Query.queryGovActionCounts
      (1, 1, 0, 0)
      "Unexpected committee hashes"
  where
    testLabel = "conwayNewCommittee"

updateConstitution :: IOManager -> [(Text, Text)] -> Assertion
updateConstitution =
  withFullConfig conwayConfigDir testLabel $ \interpreter server dbSync -> do
    startDBSync dbSync

    -- Add stake
    void (Api.registerAllStakeCreds interpreter server)

    -- Register a DRep and delegate votes to it
    void (Api.registerDRepsAndDelegateVotes interpreter server)

    -- DRep distribution is calculated at end of the current epoch
    epoch0 <- Api.fillUntilNextEpoch interpreter server

    -- Register committee hot credentials
    void $
      Api.withConwayFindLeaderAndSubmit interpreter server $ \_ ->
        mapM (uncurry Conway.mkCommitteeAuthTx) Forging.bootstrapCommitteeCreds

    let newUrl = fromJust (textToUrl 64 "constitution.new")
        dataHash = hashAnchorData @Consensus.StandardCrypto (AnchorData "constitution content")
        anchor = Governance.Anchor newUrl dataHash

    -- Create and vote for a governance proposal
    void $
      Api.withConwayFindLeaderAndSubmit interpreter server $ \_ -> do
        let
          -- Create gov action tx
          proposalTx = Conway.mkNewConstitutionTx anchor

          -- Create votes
          addVoteTx =
            Conway.mkGovVoteTx
              govActionId
              ( DRepVoter (Prelude.head Forging.unregisteredDRepIds)
                  : map (CommitteeVoter . snd) Forging.bootstrapCommitteeCreds
              )
          govActionId =
            GovActionId
              { gaidTxId = txIdTx proposalTx
              , gaidGovActionIx = GovActionIx 0
              }

        pure [proposalTx, addVoteTx]

    -- It takes 2 epochs to enact a proposal--ratification will happen on the next
    -- epoch and enacted on the following.
    epoch1 <- Api.fillEpochs interpreter server 2

    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length (epoch0 <> epoch1) + 4)

    -- Constitution should now be updated
    (EpochNo epochNo) <- getCurrentEpoch interpreter
    assertEqQuery
      dbSync
      (Query.queryConstitutionAnchor epochNo)
      (Just ("constitution.new", originalBytes dataHash))
      "Unexpected constution voting anchor"
  where
    testLabel = "conwayUpdateConstitution"

treasuryWithdrawal :: IOManager -> [(Text, Text)] -> Assertion
treasuryWithdrawal =
  withFullConfigAndLogs conwayConfigDir testLabel $ \interpreter server dbSync -> do
    startDBSync dbSync

    -- Add stake
    void (Api.registerAllStakeCreds interpreter server)

    -- Register a DRep and delegate votes to it
    void (Api.registerDRepsAndDelegateVotes interpreter server)

    -- DRep distribution is calculated at end of the current epoch
    epoch0 <- Api.fillUntilNextEpoch interpreter server

    -- Register committee hot credentials
    -- TODO[sgillespie]: Let's get this in UnifiedApi or something
    void $
      Api.withConwayFindLeaderAndSubmit interpreter server $ \_ ->
        mapM (uncurry Conway.mkCommitteeAuthTx) Forging.bootstrapCommitteeCreds

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
            Conway.mkGovVoteTx
              govActionId
              ( DRepVoter (Prelude.head Forging.unregisteredDRepIds)
                  : map (CommitteeVoter . snd) Forging.bootstrapCommitteeCreds
              )

          govActionId =
            GovActionId
              { gaidTxId = txIdTx proposalTx
              , gaidGovActionIx = GovActionIx 0
              }

        pure [proposalTx, addVoteTx]

    -- It takes 2 epochs to enact a proposal--ratification will happen on the next
    -- epoch and enacted on the following.
    epoch1 <- Api.fillEpochs interpreter server 2

    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length (epoch0 <> epoch1) + 5)

    -- Should now have a treasury reward
    assertEqQuery
      dbSync
      Query.queryRewardRests
      [(Db.RwdTreasury, 10_000)]
      "Unexpected constution voting anchor"
  where
    testLabel = "conwayTreasuryWithdrawal"
