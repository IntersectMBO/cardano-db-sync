{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Statement.EpochAndProtocol where

import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Session as HsqlS
import qualified Hasql.Statement as HsqlS

import qualified Cardano.Db.Schema.Core.EpochAndProtocol as SEnP
import qualified Cardano.Db.Schema.Core.StakeDeligation as SSD
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Statement.Function.Core (ResultType (..), ResultTypeBulk (..), mkCallInfo, runDbSession)
import Cardano.Db.Statement.Function.Insert (bulkInsert, insert)
import Cardano.Db.Statement.Function.Query (replace, selectByField)
import Cardano.Db.Statement.Types (Entity (..))
import Cardano.Db.Types (DbAction (..), DbLovelace)
import Cardano.Prelude (MonadIO (..), Word64, void)

--------------------------------------------------------------------------------

-- | CostModel

--------------------------------------------------------------------------------
costModelStmt :: HsqlS.Statement SEnP.CostModel (Entity SEnP.CostModel)
costModelStmt =
  insert
    SEnP.costModelEncoder
    (WithResult $ HsqlD.singleRow SEnP.entityCostModelDecoder)

insertCostModel :: MonadIO m => SEnP.CostModel -> DbAction m Id.CostModelId
insertCostModel costModel = do
  entity <- runDbSession (mkCallInfo "insertCostModel") $ HsqlS.statement costModel costModelStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------

-- | AdaPots

--------------------------------------------------------------------------------

-- | INSERT
insertAdaPotsStmt :: HsqlS.Statement SEnP.AdaPots (Entity SEnP.AdaPots)
insertAdaPotsStmt =
  insert
    SEnP.adaPotsEncoder
    (WithResult $ HsqlD.singleRow SEnP.entityAdaPotsDecoder)

insertAdaPots :: MonadIO m => SEnP.AdaPots -> DbAction m Id.AdaPotsId
insertAdaPots adaPots = do
  entity <- runDbSession (mkCallInfo "insertAdaPots") $ HsqlS.statement adaPots insertAdaPotsStmt
  pure $ entityKey entity

-- | QUERY

-- AdaPots query statement
queryAdaPotsIdStmt :: HsqlS.Statement Id.BlockId (Maybe (Entity SEnP.AdaPots))
queryAdaPotsIdStmt = selectByField "block_id" (Id.idEncoder Id.getBlockId) SEnP.entityAdaPotsDecoder

-- AdaPots query function
queryAdaPotsId :: MonadIO m => Id.BlockId -> DbAction m (Maybe (Entity SEnP.AdaPots))
queryAdaPotsId blockId =
  runDbSession (mkCallInfo "queryAdaPotsId") $
    HsqlS.statement blockId queryAdaPotsIdStmt

replaceAdaPotsStmt :: HsqlS.Statement (Id.AdaPotsId, SEnP.AdaPots) ()
replaceAdaPotsStmt =
  replace
    (Id.idEncoder Id.getAdaPotsId)
    SEnP.adaPotsEncoder

replaceAdaPots :: MonadIO m => Id.BlockId -> SEnP.AdaPots -> DbAction m Bool
replaceAdaPots blockId adapots = do
  -- Do the query first
  mAdaPotsEntity <-
    runDbSession (mkCallInfo "queryAdaPots") $
      HsqlS.statement blockId queryAdaPotsIdStmt

  -- Then conditionally do the update
  case mAdaPotsEntity of
    Nothing -> pure False
    Just adaPotsEntity
      | entityVal adaPotsEntity == adapots -> pure False
      | otherwise -> do
          runDbSession (mkCallInfo "updateAdaPots") $
            HsqlS.statement (entityKey adaPotsEntity, adapots) replaceAdaPotsStmt
          pure True

--------------------------------------------------------------------------------

-- | Epoch

--------------------------------------------------------------------------------
insertEpochStmt :: HsqlS.Statement SEnP.Epoch (Entity SEnP.Epoch)
insertEpochStmt =
  insert
    SEnP.epochEncoder
    (WithResult $ HsqlD.singleRow SEnP.entityEpochDecoder)

insertEpoch :: MonadIO m => SEnP.Epoch -> DbAction m Id.EpochId
insertEpoch epoch = do
  entity <- runDbSession (mkCallInfo "insertEpoch") $ HsqlS.statement epoch insertEpochStmt
  pure $ entityKey entity

insertEpochParamStmt :: HsqlS.Statement SEnP.EpochParam (Entity SEnP.EpochParam)
insertEpochParamStmt =
  insert
    SEnP.epochParamEncoder
    (WithResult $ HsqlD.singleRow SEnP.entityEpochParamDecoder)

insertEpochParam :: MonadIO m => SEnP.EpochParam -> DbAction m Id.EpochParamId
insertEpochParam epochParam = do
  entity <- runDbSession (mkCallInfo "insertEpochParam") $ HsqlS.statement epochParam insertEpochParamStmt
  pure $ entityKey entity

insertEpochSyncTimeStmt :: HsqlS.Statement SEnP.EpochSyncTime (Entity SEnP.EpochSyncTime)
insertEpochSyncTimeStmt =
  insert
    SEnP.epochSyncTimeEncoder
    (WithResult $ HsqlD.singleRow SEnP.entityEpochSyncTimeDecoder)

insertEpochSyncTime :: MonadIO m => SEnP.EpochSyncTime -> DbAction m Id.EpochSyncTimeId
insertEpochSyncTime epochSyncTime = do
  entity <- runDbSession (mkCallInfo "insertEpochSyncTime") $ HsqlS.statement epochSyncTime insertEpochSyncTimeStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------

-- | EpochStake

--------------------------------------------------------------------------------
bulkInsertEpochStakeStmt :: HsqlS.Statement [SSD.EpochStake] ()
bulkInsertEpochStakeStmt =
  bulkInsert
    extractEpochStake
    SSD.epochStakeBulkEncoder
    NoResultBulk
  where
    extractEpochStake :: [SSD.EpochStake] -> ([Id.StakeAddressId], [Id.PoolHashId], [DbLovelace], [Word64])
    extractEpochStake xs =
      ( map SSD.epochStakeAddrId xs
      , map SSD.epochStakePoolId xs
      , map SSD.epochStakeAmount xs
      , map SSD.epochStakeEpochNo xs
      )

bulkInsertEpochStake :: MonadIO m => [SSD.EpochStake] -> DbAction m ()
bulkInsertEpochStake epochStakes =
  void $
    runDbSession (mkCallInfo "bulkInsertEpochStake") $
      HsqlS.statement epochStakes bulkInsertEpochStakeStmt

--------------------------------------------------------------------------------

-- | EpochState

--------------------------------------------------------------------------------
insertEpochStateStmt :: HsqlS.Statement SEnP.EpochState (Entity SEnP.EpochState)
insertEpochStateStmt =
  insert
    SEnP.epochStateEncoder
    (WithResult $ HsqlD.singleRow SEnP.entityEpochStateDecoder)

insertEpochState :: MonadIO m => SEnP.EpochState -> DbAction m Id.EpochStateId
insertEpochState epochState = do
  entity <- runDbSession (mkCallInfo "insertEpochState") $ HsqlS.statement epochState insertEpochStateStmt
  pure $ entityKey entity

bulkInsertEpochStateStmt :: HsqlS.Statement [SEnP.EpochState] ()
bulkInsertEpochStateStmt =
  bulkInsert
    extractEpochState
    SEnP.epochStateBulkEncoder
    NoResultBulk
  where
    extractEpochState :: [SEnP.EpochState] -> ([Maybe Id.CommitteeId], [Maybe Id.GovActionProposalId], [Maybe Id.ConstitutionId], [Word64])
    extractEpochState xs =
      ( map SEnP.epochStateCommitteeId xs
      , map SEnP.epochStateNoConfidenceId xs
      , map SEnP.epochStateConstitutionId xs
      , map SEnP.epochStateEpochNo xs
      )

bulkInsertEpochState :: MonadIO m => [SEnP.EpochState] -> DbAction m ()
bulkInsertEpochState epochStates =
  void $
    runDbSession (mkCallInfo "bulkInsertEpochState") $
      HsqlS.statement epochStates bulkInsertEpochStateStmt

--------------------------------------------------------------------------------

-- | PotTransfer

--------------------------------------------------------------------------------
insertPotTransferStmt :: HsqlS.Statement SEnP.PotTransfer (Entity SEnP.PotTransfer)
insertPotTransferStmt =
  insert
    SEnP.potTransferEncoder
    (WithResult $ HsqlD.singleRow SEnP.entityPotTransferDecoder)

insertPotTransfer :: MonadIO m => SEnP.PotTransfer -> DbAction m Id.PotTransferId
insertPotTransfer potTransfer = do
  entity <- runDbSession (mkCallInfo "insertPotTransfer") $ HsqlS.statement potTransfer insertPotTransferStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------

-- | Reserve

--------------------------------------------------------------------------------
insertRervedStmt :: HsqlS.Statement SEnP.Reserve (Entity SEnP.Reserve)
insertRervedStmt =
  insert
    SEnP.reserveEncoder
    (WithResult $ HsqlD.singleRow SEnP.entityReserveDecoder)

insertRerved :: MonadIO m => SEnP.Reserve -> DbAction m Id.ReserveId
insertRerved reserve = do
  entity <- runDbSession (mkCallInfo "insertRerved") $ HsqlS.statement reserve insertRervedStmt
  pure $ entityKey entity

-- Epoch And Protocol Parameters
-- These tables store epoch-specific data and protocol parameters.

-- ada_pots
-- cost_model
-- epoch
-- epoch_param
-- epoch_stake
-- epoch_stake_progress
-- epoch_state
-- epoch_sync_time
-- pot_transfer
-- reserve
-- treasury
