{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.EpochAndProtocol where

import qualified Hasql.Decoders as HsqlD

import qualified Cardano.Db.Schema.Core.EpochAndProtocol as SEnP
import qualified Cardano.Db.Schema.Core.StakeDeligation as SSD
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Types (DbAction (..), DbTransMode (..))
import Cardano.Prelude (MonadIO (..), Word64, void, Proxy (..), MonadError (..))
import Cardano.Db.Statement.Function.Core (runDbT, mkDbTransaction, ResultType (..), ResultTypeBulk (..))
import Cardano.Db.Statement.Function.Insert (insert, bulkInsert)
import Cardano.Db.Statement.Types (Entity(..), DbInfo (..))
import qualified Hasql.Transaction as HsqlT
import qualified Hasql.Statement as HsqlS
import qualified Data.Text.Encoding as TextEnc
import qualified Data.Text as Text
import Cardano.Db.Statement.Function.Query (replace)

--------------------------------------------------------------------------------
-- | CostModel
--------------------------------------------------------------------------------
insertCostModel :: MonadIO m => SEnP.CostModel -> DbAction m Id.CostModelId
insertCostModel costModel =
  runDbT TransWrite $ mkDbTransaction "insertCostModel" $ do
    entity <- insert
      SEnP.costModelEncoder
      (WithResult $ HsqlD.singleRow SEnP.entityCostModelDecoder)
      costModel
    pure $ entityKey entity

--------------------------------------------------------------------------------
-- | AdaPots
--------------------------------------------------------------------------------

-- | INSERT
insertAdaPots :: MonadIO m => SEnP.AdaPots -> DbAction m Id.AdaPotsId
insertAdaPots adaPots =
  runDbT TransWrite $ mkDbTransaction "insertAdaPots" $ do
    entity <- insert
      SEnP.adaPotsEncoder
      (WithResult (HsqlD.singleRow SEnP.entityAdaPotsDecoder))
      adaPots
    pure $ entityKey entity

-- | QUERY
replaceAdaPots :: MonadIO m => Id.BlockId -> SEnP.AdaPots -> DbAction m Bool
replaceAdaPots blockId adaPots = runDbT TransWrite $ mkDbTransaction "replaceAdaPots" $ do
  mExistingEntity <- queryAdaPotsWithIdTx blockId
  case mExistingEntity of
    Nothing -> pure False
    Just existingEntity
      | entityVal existingEntity == adaPots -> pure False
      | otherwise -> do
          replace
            (entityKey existingEntity)
            (Id.idEncoder Id.getAdaPotsId)
            SEnP.adaPotsEncoder
            adaPots
          pure True

queryAdaPotsWithIdTx :: Id.BlockId -> HsqlT.Transaction (Maybe (Entity SEnP.AdaPots))
queryAdaPotsWithIdTx blockId =
  HsqlT.statement blockId $ HsqlS.Statement sql blockIdEncoder entityDecoder True
  where
    table = tableName (Proxy @SEnP.AdaPots)
    blockIdEncoder = Id.idEncoder Id.getBlockId
    entityDecoder = HsqlD.rowMaybe SEnP.entityAdaPotsDecoder

    sql = TextEnc.encodeUtf8 $ Text.concat
      [ "SELECT * FROM " <> table
      , " WHERE block_id = $1"
      ]

catchDbError :: String -> HsqlT.Transaction a -> HsqlT.Transaction a
catchDbError context action =
  action `catch` \e ->
    throwError $ DbError $ context ++ ": " ++ show e

--------------------------------------------------------------------------------
-- | Epoch
--------------------------------------------------------------------------------
insertEpoch:: MonadIO m => SEnP.Epoch -> DbAction m Id.EpochId
insertEpoch epoch =
  runDbT TransWrite $ mkDbTransaction "insertEpoch" $ do
    entity <- insert
      SEnP.epochEncoder
      (WithResult $ HsqlD.singleRow SEnP.entityEpochDecoder)
      epoch
    pure $ entityKey entity

insertEpochParam :: MonadIO m => SEnP.EpochParam -> DbAction m Id.EpochParamId
insertEpochParam epochParam =
  runDbT TransWrite $ mkDbTransaction "insertEpochParam" $ do
    entity <- insert
      SEnP.epochParamEncoder
      (WithResult $ HsqlD.singleRow SEnP.entityEpochParamDecoder)
      epochParam
    pure $ entityKey entity

insertEpochSyncTime:: MonadIO m => SEnP.EpochSyncTime -> DbAction m Id.EpochSyncTimeId
insertEpochSyncTime epochSyncTime =
  runDbT TransWrite $ mkDbTransaction "insertEpochSyncTime" $ do
    entity <- insert
      SEnP.epochSyncTimeEncoder
      (WithResult $ HsqlD.singleRow SEnP.entityEpochSyncTimeDecoder)
      epochSyncTime
    pure $ entityKey entity

--------------------------------------------------------------------------------
-- | EpochStake
--------------------------------------------------------------------------------

bulkInsertEpochStake:: MonadIO m => [SSD.EpochStake] -> DbAction m ()
bulkInsertEpochStake epochStakes = runDbT TransWrite $ mkDbTransaction "bulkInsertEpochStake" $ do
  void $ bulkInsert
    extractEpochStake
    SSD.epochStakeBulkEncoder
    NoResultBulk
    epochStakes
  where
    extractEpochStake :: [SSD.EpochStake] -> ([Maybe Id.StakeAddressId], [Maybe Id.EpochId], [Word64], [Word64])
    extractEpochStake xs =
        ( map Id.epochStakeAddrId xs
        , map SSD.epochStakeEpochId xs
        , map SSD.epochStakeAmount xs
        , map SSD.epochStakeEpochNo xs
        )

--------------------------------------------------------------------------------
-- | EpochState
--------------------------------------------------------------------------------
insertEpochState:: MonadIO m => SEnP.EpochState -> DbAction m Id.EpochStateId
insertEpochState epochState =
  runDbT TransWrite $ mkDbTransaction "insertEpochState" $ do
    entity <- insert
      SEnP.epochStateEncoder
      (WithResult $ HsqlD.singleRow SEnP.entityEpochStateDecoder)
      epochState
    pure $ entityKey entity

bulkInsertEpochState:: MonadIO m => [SEnP.EpochState] -> DbAction m ()
bulkInsertEpochState epochStates = runDbT TransWrite $ mkDbTransaction "bulkInsertEpochState" $ do
  void $ bulkInsert
    extractEpochState
    SEnP.epochStateBulkEncoder
    NoResultBulk
    epochStates
  where
    extractEpochState :: [SEnP.EpochState] -> ([Maybe Id.CommitteeId], [Maybe Id.GovActionProposalId], [Maybe Id.ConstitutionId], [Word64])
    extractEpochState xs =
        ( map SEnP.epochStateCommitteeId xs
        , map SEnP.epochStateNoConfidenceId xs
        , map SEnP.epochStateConstitutionId xs
        , map SEnP.epochStateEpochNo xs
        )

--------------------------------------------------------------------------------
-- | PotTransfer
--------------------------------------------------------------------------------
insertPotTransfer:: MonadIO m => SEnP.PotTransfer -> DbAction m Id.PotTransferId
insertPotTransfer potTransfer =
  runDbT TransWrite $ mkDbTransaction "insertPotTransfer" $ do
    entity <- insert
      SEnP.potTransferEncoder
      (WithResult $ HsqlD.singleRow SEnP.entityPotTransferDecoder)
      potTransfer
    pure $ entityKey entity

--------------------------------------------------------------------------------
-- | Reserve
--------------------------------------------------------------------------------
insertRerved:: MonadIO m => SEnP.Reserve -> DbAction m Id.ReserveId
insertRerved reserve =
  runDbT TransWrite $ mkDbTransaction "insertRerved" $ do
    entity <- insert
      SEnP.reserveEncoder
      (WithResult $ HsqlD.singleRow SEnP.entityReserveDecoder)
      reserve
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
