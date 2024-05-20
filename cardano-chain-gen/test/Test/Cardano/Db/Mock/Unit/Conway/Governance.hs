{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Db.Mock.Unit.Conway.Governance (
  drepDistr,
  newCommittee,
) where

import Cardano.DbSync.Era.Shelley.Generic.Util (unCredentialHash)
import Cardano.Ledger.Address (Withdrawals (..))
import Cardano.Ledger.Alonzo.Tx (AlonzoTx)
import Cardano.Ledger.Conway.Governance (GovActionId (..), GovActionIx (..), Voter (..))
import Cardano.Ledger.Conway.TxCert (Delegatee (..))
import Cardano.Ledger.Core (txIdTx)
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep (DRep (..))
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Mock.ChainSync.Server (IOManager)
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import qualified Cardano.Mock.Forging.Tx.Generic as Forging
import Cardano.Mock.Forging.Types
import qualified Cardano.Mock.Query as Query
import Cardano.Prelude
import Ouroboros.Consensus.Shelley.Eras (StandardConway, StandardCrypto)
import Test.Cardano.Db.Mock.Config
import qualified Test.Cardano.Db.Mock.UnifiedApi as Api
import Test.Cardano.Db.Mock.Validate
import Test.Tasty.HUnit (Assertion)
import Prelude ()

drepDistr :: IOManager -> [(Text, Text)] -> Assertion
drepDistr =
  withFullConfigAndDropDB conwayConfigDir testLabel $ \interpreter server dbSync -> do
    startDBSync dbSync

    -- Add stake
    void (Api.registerAllStakeCreds interpreter server)

    -- Register a DRep
    let drepHash = "0d94e174732ef9aae73f395ab44507bfa983d65023c11a951f0c32e4"
        drepId = KeyHashObj (KeyHash drepHash)

    -- Register DRep and delegate votes to it
    void $
      Api.withConwayFindLeaderAndSubmit interpreter server $ \ledger ->
        registerDRepAndDelegateVotes drepId (StakeIndex 4) ledger

    -- DRep distribution is calculated at end of the current epoch
    epoch1 <- Api.fillUntilNextEpoch interpreter server

    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length epoch1 + 2)

    -- Should now have a DRep distribution
    assertEqQuery
      dbSync
      (Query.queryDRepDistrAmount (unCredentialHash drepId) 1)
      10_000
      "Unexpected drep distribution amount"
  where
    testLabel = "conwayDrepDistr"

newCommittee :: IOManager -> [(Text, Text)] -> Assertion
newCommittee =
  withFullConfig conwayConfigDir testLabel $ \interpreter server dbSync -> do
    startDBSync dbSync

    -- Add stake
    void (Api.registerAllStakeCreds interpreter server)

    -- Register a DRep
    let drepHash = "0d94e174732ef9aae73f395ab44507bfa983d65023c11a951f0c32e4"
        drepId = KeyHashObj (KeyHash drepHash)

    -- Register DRep and delegate votes to it
    void $
      Api.withConwayFindLeaderAndSubmit interpreter server $ \ledger ->
        registerDRepAndDelegateVotes drepId (StakeIndex 4) ledger

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
              [ DRepVoter drepId
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
      (1, 1, 1, 0)
      "Unexpected committee hashes"
  where
    testLabel = "conwayNewCommittee"

registerDRepAndDelegateVotes ::
  Credential 'DRepRole StandardCrypto ->
  StakeIndex ->
  Conway.ConwayLedgerState ->
  Either ForgingError [AlonzoTx StandardConway]
registerDRepAndDelegateVotes drepId stakeIx ledger = do
  stakeCreds <- Forging.resolveStakeCreds stakeIx ledger

  let utxoStake = UTxOAddressNewWithStake 0 stakeIx
      regDelegCert =
        Conway.mkDelegTxCert (DelegVote $ DRepCredential drepId) stakeCreds

  paymentTx <- Conway.mkPaymentTx (UTxOIndex 0) utxoStake 10_000 500 ledger
  regTx <- Conway.mkRegisterDRepTx drepId
  delegTx <- Conway.mkDCertTx [regDelegCert] (Withdrawals mempty) Nothing

  pure [paymentTx, regTx, delegTx]
