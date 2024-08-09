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
  paramChange,
  hardFork,
  updateConstitution,
  treasuryWithdrawal,
) where

import qualified Cardano.Db as Db
import Cardano.DbSync.Era.Shelley.Generic.Util (unCredentialHash)
import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.BaseTypes (AnchorData (..), Network (..), ProtVer (..), hashAnchorData, textToUrl)
import Cardano.Ledger.Binary.Version (natVersion)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Governance (GovActionId (..), GovActionIx (..))
import qualified Cardano.Ledger.Conway.Governance as Governance
import Cardano.Ledger.Conway.PParams (ppuGovActionDepositL)
import Cardano.Ledger.Core (emptyPParamsUpdate, txIdTx)
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Mock.ChainSync.Server (IOManager)
import Cardano.Mock.Forging.Interpreter (getCurrentEpoch)
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import Cardano.Mock.Forging.Tx.Generic (committeeVoters, drepVoters, spoVoters)
import qualified Cardano.Mock.Forging.Tx.Generic as Forging
import Cardano.Mock.Forging.Types
import qualified Cardano.Mock.Query as Query
import Cardano.Prelude
import Cardano.Slotting.Slot (EpochNo (..))
import Data.Maybe (fromJust)
import Data.Maybe.Strict (StrictMaybe (..))
import Lens.Micro
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

    epoch0 <- Api.initGovernance interpreter server

    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length epoch0)

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
  withFullConfig conwayConfigDir testLabel $ \interpreter server dbSync -> do
    startDBSync dbSync

    epoch0 <- Api.initGovernance interpreter server

    -- Create and vote for gov action
    let committeeHash = "e0a714319812c3f773ba04ec5d6b3ffcd5aad85006805b047b082541"
        committeeCred = KeyHashObj (KeyHash committeeHash)

    void $
      Api.withConwayFindLeaderAndSubmit interpreter server $ \ledger -> do
        let
          -- Create gov action tx
          addCcTx = Conway.mkAddCommitteeTx committeeCred
          -- Create votes for all stake pools
          addVoteTx = Conway.mkGovVoteTx govActionId (drepVoters ++ spoVoters ledger)
          govActionId =
            GovActionId
              { gaidTxId = txIdTx addCcTx
              , gaidGovActionIx = GovActionIx 0
              }

        -- Create votes
        pure [addCcTx, addVoteTx]

    -- It takes 2 epochs to enact a proposal--ratification will happen on the next
    -- epoch and enacted on the following.
    epoch1 <- Api.fillEpochs interpreter server 2

    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length (epoch0 <> epoch1) + 1)
    -- Should now have a committee member
    assertEqQuery
      dbSync
      Query.queryGovActionCounts
      (1, 1, 1, 0)
      "Unexpected committee hashes"
  where
    testLabel = "conwayNewCommittee"

updateConstitution :: IOManager -> [(Text, Text)] -> Assertion
updateConstitution =
  withFullConfig conwayConfigDir testLabel $ \interpreter server dbSync -> do
    startDBSync dbSync

    epoch0 <- Api.initGovernance interpreter server

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
          addVoteTx = Conway.mkGovVoteTx govActionId (drepVoters ++ committeeVoters)
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
    assertBlockNoBackoff dbSync (length (epoch0 <> epoch1) + 1)

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
  withFullConfig conwayConfigDir testLabel $ \interpreter server dbSync -> do
    startDBSync dbSync

    epoch0 <- Api.initGovernance interpreter server

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

          addVoteTx = Conway.mkGovVoteTx govActionId (drepVoters ++ committeeVoters)

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
    assertBlockNoBackoff dbSync (length (epoch0 <> epoch1) + 2)

    -- Should now have a treasury reward
    assertEqQuery
      dbSync
      Query.queryRewardRests
      [(Db.RwdTreasury, 10_000)]
      "Unexpected constution voting anchor"
  where
    testLabel = "conwayTreasuryWithdrawal"

paramChange :: IOManager -> [(Text, Text)] -> Assertion
paramChange =
  withFullConfig conwayConfigDir testLabel $ \interpreter server dbSync -> do
    startDBSync dbSync

    epoch0 <- Api.initGovernance interpreter server

    -- Create and vote for a governance proposal
    void $
      Api.withConwayFindLeaderAndSubmit interpreter server $ \ledger -> do
        let proposalTx = Conway.mkParamChangeTx newParams
            newParams =
              emptyPParamsUpdate & ppuGovActionDepositL .~ SJust (Coin 100)

            addVoteTx =
              Conway.mkGovVoteTx
                govActionId
                (committeeVoters ++ drepVoters ++ spoVoters ledger)

            govActionId =
              GovActionId
                { gaidTxId = txIdTx proposalTx
                , gaidGovActionIx = GovActionIx 0
                }

        pure [proposalTx, addVoteTx]

    -- It takes 2 epochs to enact a proposal--ratification will happen on the next
    -- epoch and enacted on the following.
    epoch1 <- Api.fillEpochs interpreter server 2

    -- Wait for it to synch
    assertBlockNoBackoff dbSync (length (epoch0 <> epoch1) + 1)

    epochNo <- unEpochNo <$> getCurrentEpoch interpreter

    -- Should have updated epoch param
    assertEqQuery
      dbSync
      (join <$> Query.queryParamFromEpoch Db.EpochParamGovActionDeposit epochNo)
      (Just $ Db.DbWord64 100)
      "Unexpected constution voting anchor"
  where
    testLabel = "conwayParamChange"

hardFork :: IOManager -> [(Text, Text)] -> Assertion
hardFork =
  withFullConfig configDir testLabel $ \interpreter server dbSync -> do
    startDBSync dbSync

    epoch0 <- Api.initGovernance interpreter server

    -- Create and vote for a governance proposal
    void $
      Api.withConwayFindLeaderAndSubmit interpreter server $ \ledger -> do
        let proposalTx = Conway.mkHardForkTx version
            version = ProtVer (natVersion @10) 0

            addVoteTx =
              Conway.mkGovVoteTx
                govActionId
                (committeeVoters ++ spoVoters ledger)

            govActionId =
              GovActionId
                { gaidTxId = txIdTx proposalTx
                , gaidGovActionIx = GovActionIx 0
                }

        pure [proposalTx, addVoteTx]

    -- It takes 2 epochs to enact a proposal--ratification will happen on the next
    -- epoch and enacted on the following.
    epoch1 <- Api.fillEpochs interpreter server 2

    -- Wait for it to synch
    assertBlockNoBackoff dbSync (length (epoch0 <> epoch1) + 1)

    epochNo <- getCurrentEpoch interpreter

    -- Protocol major version should now be 10
    assertEqBackoff
      dbSync
      (Query.queryVersionMajorFromEpoch (unEpochNo epochNo))
      (Just 10)
      []
      "Unexpected protocol major version"
  where
    configDir = "config-conway-bootstrap"
    testLabel = "conwayHardFork"
