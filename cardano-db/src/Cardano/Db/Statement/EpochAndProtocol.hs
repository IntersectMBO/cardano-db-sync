{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Statement.EpochAndProtocol where

import qualified Hasql.Decoders as HsqlD

import qualified Cardano.Db.Schema.Core.EpochAndProtocol as SEnP
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Types (DbAction, DbTransMode (..))
import Cardano.Prelude (MonadIO, Word64)
import Cardano.Db.Statement.Function.Core (runDbT, mkDbTransaction, ResultType (..), WithConstraint (..))
import Cardano.Db.Statement.Function.Insert (insert, insertCheckUnique, insertManyUnique)

--------------------------------------------------------------------------------
-- | CostModel
--------------------------------------------------------------------------------
insertCostModel :: MonadIO m => SEnP.CostModel -> DbAction m Id.CostModelId
insertCostModel costModel = runDbT TransWrite $ mkDbTransaction "insertCostModel" $
  insert
    SEnP.costModelEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.CostModelId))
    costModel

--------------------------------------------------------------------------------
-- | AdaPots
--------------------------------------------------------------------------------
insertAdaPots :: MonadIO m => SEnP.AdaPots -> DbAction m Id.AdaPotsId
insertAdaPots adaPots = runDbT TransWrite $ mkDbTransaction "insertAdaPots" $
  insert
    SEnP.adaPotsEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.AdaPotsId))
    adaPots

--------------------------------------------------------------------------------
-- | Epoch
--------------------------------------------------------------------------------
insertEpoch:: MonadIO m => SEnP.Epoch -> DbAction m Id.EpochId
insertEpoch epoch = runDbT TransWrite $ mkDbTransaction "insertEpoch" $
  insertCheckUnique
    SEnP.epochEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.EpochId))
    epoch

insertEpochParam :: MonadIO m => SEnP.EpochParam -> DbAction m Id.EpochParamId
insertEpochParam epochParam = runDbT TransWrite $ mkDbTransaction "insertEpochParam" $
  insert
    SEnP.epochParamEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.EpochParamId))
    epochParam

insertEpochSyncTime:: MonadIO m => SEnP.EpochSyncTime -> DbAction m Id.EpochSyncTimeId
insertEpochSyncTime epochSyncTime = runDbT TransWrite $ mkDbTransaction "insertEpochSyncTime" $
  insert
    SEnP.epochSyncTimeEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.EpochSyncTimeId))
    epochSyncTime

--------------------------------------------------------------------------------
-- | EpochState
--------------------------------------------------------------------------------
insertEpochState:: MonadIO m => SEnP.EpochState -> DbAction m Id.EpochStateId
insertEpochState epochState = runDbT TransWrite $ mkDbTransaction "insertEpochState" $
  insert
    SEnP.epochStateEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.EpochStateId))
    epochState

insertManyEpochState:: MonadIO m => [SEnP.EpochState] -> DbAction m ()
insertManyEpochState epochStates = runDbT TransWrite $ mkDbTransaction "insertManyEpochState" $
  insertManyUnique
    extractEpochState
    SEnP.epochStateManyEncoder
    NoConstraint
    epochStates
  where
    extractEpochState :: [SEnP.EpochState] -> ([Id.EpochStateId],[Maybe Id.CommitteeId], [Maybe Id.GovActionProposalId], [Maybe Id.ConstitutionId], [Word64])
    extractEpochState xs =
        ( map SEnP.epochStateId xs
        , map SEnP.epochStateCommitteeId xs
        , map SEnP.epochStateNoConfidenceId xs
        , map SEnP.epochStateConstitutionId xs
        , map SEnP.epochStateEpochNo xs
        )

--------------------------------------------------------------------------------
-- | PotTransfer
--------------------------------------------------------------------------------
insertPotTransfer:: MonadIO m => SEnP.PotTransfer -> DbAction m Id.PotTransferId
insertPotTransfer potTransfer = runDbT TransWrite $ mkDbTransaction "insertPotTransfer" $
  insert
    SEnP.potTransferEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.PotTransferId))
    potTransfer

--------------------------------------------------------------------------------
-- | Reserve
--------------------------------------------------------------------------------
insertRerved:: MonadIO m => SEnP.Reserve -> DbAction m Id.ReserveId
insertRerved reserve = runDbT TransWrite $ mkDbTransaction "insertRerved" $
  insert
    SEnP.reserveEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.ReserveId))
    reserve

-- Epoch And Protocol Parameters
-- These tables store epoch-specific data and protocol parameters.

-- ada_pots
-- cost_model
-- epoch
-- epoch_param
-- epoch_state
-- epoch_sync_time
-- pot_transfer
-- reserve
-- treasury
