{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Statement.GovernanceAndVoting where

import Cardano.Db.Schema.Core (OffChainVoteReference(..))
import Cardano.Db.Schema.Ids (OffChainVoteDataId)
import Cardano.Db.Statement.Helpers (runDbT, mkDbTransaction, bulkInsertNoReturn)
import Cardano.Db.Types (DbAction, DbTxMode (..))
import Cardano.Prelude (MonadIO, Text)
import qualified Hasql.Transaction as HsqlT
import Cardano.Db.Schema.Core.OffChain (offChainVoteReferenceManyEncoder)


-- These tables manage governance-related data, including DReps, committees, and voting procedures.

-- drep_hash
-- drep_registration
-- drep_distr
-- delegation_vote
-- gov_action_proposal
-- voting_procedure
-- voting_anchor
-- constitution
-- committee
-- committee_hash
-- committee_member
-- committee_registration
-- committee_de_registration
-- new_committee
-- param_proposal
-- treasury_withdrawal
-- event_info
