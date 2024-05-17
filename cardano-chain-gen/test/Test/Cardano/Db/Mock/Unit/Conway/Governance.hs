{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Db.Mock.Unit.Conway.Governance (
  drepDistr,
) where

import Cardano.DbSync.Era.Shelley.Generic.Util (unCredentialHash)
import Cardano.Ledger.Address (Withdrawals (..))
import Cardano.Ledger.Conway.TxCert (Delegatee (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep (DRep (..))
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Mock.ChainSync.Server (IOManager)
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import Cardano.Mock.Forging.Tx.Generic (resolveStakeCreds)
import Cardano.Mock.Forging.Types
import qualified Cardano.Mock.Query as Query
import Cardano.Prelude
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

    -- Delegate funds to a stake address
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter server $ \state' ->
        let utxoStake = UTxOAddressNewWithStake 0 (StakeIndex 4)
         in Conway.mkPaymentTx (UTxOIndex 0) utxoStake 10_000 500 state'

    -- Register a DRep
    let drepHash = "0d94e174732ef9aae73f395ab44507bfa983d65023c11a951f0c32e4"
        drepId = KeyHashObj (KeyHash drepHash)

    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter server $
        const (Conway.mkRegisterDRepTx drepId)

    -- Delegate votes to the drep above
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter server $ \state' -> do
        stakeCreds <- resolveStakeCreds (StakeIndex 4) state'
        let regDelegCert =
              Conway.mkDelegTxCert (DelegVote $ DRepCredential drepId) stakeCreds

        Conway.mkDCertTx [regDelegCert] (Withdrawals mempty) Nothing

    -- DRep distribution is calculated at end of the current epoch
    epoch1 <- Api.fillUntilNextEpoch interpreter server

    -- Wait for it to sync
    assertBlockNoBackoff dbSync (length epoch1 + 4)

    -- Should now have a DRep distribution
    assertEqQuery
      dbSync
      (Query.queryDRepDistrAmount (unCredentialHash drepId) 1)
      10_000
      "Unexpected drep distribution amount"

    pure ()
  where
    testLabel = "conwayDrepDistr"
