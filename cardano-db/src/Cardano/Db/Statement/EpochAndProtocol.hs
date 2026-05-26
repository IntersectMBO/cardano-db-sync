{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.EpochAndProtocol where

import Cardano.Prelude (Int64, Proxy (..), Word64)
import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

import Cardano.Db.Error (DbLookupError (..), mkDbCallStack)
import qualified Cardano.Db.Schema.Core.EpochAndProtocol as SEnP
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Statement.Function.Core (ResultType (..), runSession, runSessionEntity)
import Cardano.Db.Statement.Function.Insert (insert, insertCheckUnique, insertReplace)
import Cardano.Db.Statement.Function.Query (countAll, replace, selectByFieldFirst)
import Cardano.Db.Statement.Types (Entity (..), tableName)
import Cardano.Db.Types (DbM)

--------------------------------------------------------------------------------
-- CostModel
--------------------------------------------------------------------------------
costModelStmt :: HsqlStmt.Statement SEnP.CostModel Id.CostModelId
costModelStmt =
  insertCheckUnique
    SEnP.costModelEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.CostModelId)

insertCostModel :: SEnP.CostModel -> DbM Id.CostModelId
insertCostModel costModel =
  runSession mkDbCallStack $ HsqlSes.statement costModel costModelStmt

--------------------------------------------------------------------------------
-- AdaPots
--------------------------------------------------------------------------------

-- | INSERT
insertAdaPotsStmt :: HsqlStmt.Statement SEnP.AdaPots Id.AdaPotsId
insertAdaPotsStmt =
  insert
    SEnP.adaPotsEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.AdaPotsId)

insertAdaPots :: SEnP.AdaPots -> DbM Id.AdaPotsId
insertAdaPots adaPots =
  runSession mkDbCallStack $ HsqlSes.statement adaPots insertAdaPotsStmt

-- | QUERY

-- AdaPots query statement
queryAdaPotsIdStmt :: HsqlStmt.Statement Id.BlockId (Maybe (Entity SEnP.AdaPots))
queryAdaPotsIdStmt = selectByFieldFirst "block_id" (Id.idEncoder Id.getBlockId) SEnP.entityAdaPotsDecoder

-- AdaPots query function used in tests
queryAdaPotsIdTest :: Id.BlockId -> DbM (Maybe SEnP.AdaPots)
queryAdaPotsIdTest blockId =
  runSessionEntity mkDbCallStack $
    HsqlSes.statement blockId queryAdaPotsIdStmt

--------------------------------------------------------------------------------
replaceAdaPotsStmt :: HsqlStmt.Statement (Id.AdaPotsId, SEnP.AdaPots) ()
replaceAdaPotsStmt =
  replace
    (Id.idEncoder Id.getAdaPotsId)
    SEnP.adaPotsEncoder

replaceAdaPots :: Id.BlockId -> SEnP.AdaPots -> DbM Bool
replaceAdaPots blockId adapots = do
  -- Do the query first
  mAdaPotsEntity <-
    runSession mkDbCallStack $ HsqlSes.statement blockId queryAdaPotsIdStmt

  -- Then conditionally do the update
  case mAdaPotsEntity of
    Nothing -> pure False
    Just adaPotsEntity
      | entityVal adaPotsEntity == adapots -> pure False
      | otherwise -> do
          runSession mkDbCallStack $
            HsqlSes.statement (entityKey adaPotsEntity, adapots) replaceAdaPotsStmt
          pure True

--------------------------------------------------------------------------------
-- Epoch (public view backed by epoch_finalized table + epoch_current view)
--------------------------------------------------------------------------------

setEpochSyncEnabledStmt :: HsqlStmt.Statement Bool ()
setEpochSyncEnabledStmt =
  HsqlStmt.Statement sql encoder HsqlD.noResult True
  where
    encoder = HsqlE.param (HsqlE.nonNullable HsqlE.bool)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "INSERT INTO epoch_sync_enabled (singleton, enabled)"
          , " VALUES (TRUE, $1)"
          , " ON CONFLICT (singleton) DO UPDATE SET enabled = EXCLUDED.enabled"
          ]

setEpochSyncEnabled :: Bool -> DbM ()
setEpochSyncEnabled enabled =
  runSession mkDbCallStack $ HsqlSes.statement enabled setEpochSyncEnabledStmt

epochFinalizedSelectSql :: Text.Text
epochFinalizedSelectSql =
  Text.concat
    [ " SELECT"
    , "   (b.epoch_no::bigint + 1)              AS id,"
    , "   COALESCE(SUM(tx.out_sum), 0)::numeric AS out_sum,"
    , "   COALESCE(SUM(tx.fee), 0)              AS fees,"
    , "   COUNT(tx.id)::bigint                  AS tx_count,"
    , "   COUNT(DISTINCT b.id)::bigint          AS blk_count,"
    , "   b.epoch_no::bigint                    AS no,"
    , "   MIN(b.time)                           AS start_time,"
    , "   MAX(b.time)                           AS end_time"
    , " FROM block b"
    , " LEFT JOIN tx ON tx.block_id = b.id"
    ]

epochFinalizedUpsertTail :: Text.Text
epochFinalizedUpsertTail =
  Text.concat
    [ " GROUP BY b.epoch_no"
    , " ON CONFLICT (no) DO UPDATE SET"
    , "   id         = EXCLUDED.id,"
    , "   out_sum    = EXCLUDED.out_sum,"
    , "   fees       = EXCLUDED.fees,"
    , "   tx_count   = EXCLUDED.tx_count,"
    , "   blk_count  = EXCLUDED.blk_count,"
    , "   start_time = EXCLUDED.start_time,"
    , "   end_time   = EXCLUDED.end_time"
    ]

appendEpochFinalizedStmt :: HsqlStmt.Statement Word64 ()
appendEpochFinalizedStmt =
  HsqlStmt.Statement sql encoder HsqlD.noResult True
  where
    encoder = HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "INSERT INTO epoch_finalized"
          , "   (id, out_sum, fees, tx_count, blk_count, no, start_time, end_time)"
          , epochFinalizedSelectSql
          , " WHERE b.epoch_no = $1"
          , epochFinalizedUpsertTail
          ]

appendEpochFinalized :: Word64 -> DbM ()
appendEpochFinalized epochNum =
  runSession mkDbCallStack $ HsqlSes.statement epochNum appendEpochFinalizedStmt

backfillEpochFinalizedStmt :: HsqlStmt.Statement () ()
backfillEpochFinalizedStmt =
  HsqlStmt.Statement sql HsqlE.noParams HsqlD.noResult True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "INSERT INTO epoch_finalized"
          , "   (id, out_sum, fees, tx_count, blk_count, no, start_time, end_time)"
          , epochFinalizedSelectSql
          , " WHERE b.epoch_no IS NOT NULL"
          , "   AND b.epoch_no > COALESCE((SELECT MAX(no) FROM epoch_finalized), -1)"
          , "   AND b.epoch_no < COALESCE("
          , "         (SELECT MAX(epoch_no) FROM block WHERE epoch_no IS NOT NULL),"
          , "         -1)"
          , epochFinalizedUpsertTail
          ]

backfillEpochFinalized :: DbM ()
backfillEpochFinalized =
  runSession mkDbCallStack $ HsqlSes.statement () backfillEpochFinalizedStmt

deleteEpochFinalizedAfterStmt :: HsqlStmt.Statement Word64 Int64
deleteEpochFinalizedAfterStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder = HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
    decoder = HsqlD.rowsAffected
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "DELETE FROM epoch_finalized"
          , " WHERE no > $1"
          ]

deleteEpochFinalizedAfter :: Word64 -> DbM Int64
deleteEpochFinalizedAfter epochNum =
  runSession mkDbCallStack $ HsqlSes.statement epochNum deleteEpochFinalizedAfterStmt

--------------------------------------------------------------------------------
insertEpochParamStmt :: HsqlStmt.Statement SEnP.EpochParam Id.EpochParamId
insertEpochParamStmt =
  insert
    SEnP.epochParamEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.EpochParamId)

insertEpochParam :: SEnP.EpochParam -> DbM Id.EpochParamId
insertEpochParam epochParam =
  runSession mkDbCallStack $ HsqlSes.statement epochParam insertEpochParamStmt

--------------------------------------------------------------------------------
insertEpochSyncTimeStmt :: HsqlStmt.Statement SEnP.EpochSyncTime Id.EpochSyncTimeId
insertEpochSyncTimeStmt =
  insertReplace
    SEnP.epochSyncTimeEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.EpochSyncTimeId)

insertEpochSyncTime :: SEnP.EpochSyncTime -> DbM Id.EpochSyncTimeId
insertEpochSyncTime epochSyncTime =
  runSession mkDbCallStack $ HsqlSes.statement epochSyncTime insertEpochSyncTimeStmt

-- | QUERY ----------------------------------------------------------------------------------
queryEpochEntryStmt :: HsqlStmt.Statement Word64 (Maybe SEnP.Epoch)
queryEpochEntryStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder = HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
    decoder = HsqlD.rowMaybe (entityVal <$> SEnP.entityEpochDecoder)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT *"
          , " FROM epoch"
          , " WHERE no = $1"
          ]

queryEpochEntry :: Word64 -> DbM (Either DbLookupError SEnP.Epoch)
queryEpochEntry epochNum = do
  result <- runSession mkDbCallStack $ HsqlSes.statement epochNum queryEpochEntryStmt
  case result of
    Just res -> pure $ Right res
    Nothing -> pure $ Left $ DbLookupError mkDbCallStack errorMsg
  where
    errorMsg = "Epoch not found with number: " <> Text.pack (show epochNum)

--------------------------------------------------------------------------------
queryLatestEpochStmt :: HsqlStmt.Statement () (Maybe SEnP.Epoch)
queryLatestEpochStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT *"
          , " FROM epoch"
          , " WHERE no = (SELECT MAX(no) FROM epoch)"
          ]

    decoder = HsqlD.rowMaybe (entityVal <$> SEnP.entityEpochDecoder)

-- | Get the most recent epoch in the Epoch DB table.
queryLatestEpoch :: DbM (Maybe SEnP.Epoch)
queryLatestEpoch =
  runSession mkDbCallStack $ HsqlSes.statement () queryLatestEpochStmt

--------------------------------------------------------------------------------
queryEpochCount :: DbM Word64
queryEpochCount =
  runSession mkDbCallStack $
    HsqlSes.statement () (countAll @SEnP.Epoch)

--------------------------------------------------------------------------------
-- EpochState
--------------------------------------------------------------------------------
insertEpochStateStmt :: HsqlStmt.Statement SEnP.EpochState Id.EpochStateId
insertEpochStateStmt =
  insert
    SEnP.epochStateEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.EpochStateId)

insertEpochState :: SEnP.EpochState -> DbM Id.EpochStateId
insertEpochState epochState =
  runSession mkDbCallStack $ HsqlSes.statement epochState insertEpochStateStmt

--------------------------------------------------------------------------------
-- PotTransfer
--------------------------------------------------------------------------------
insertPotTransferStmt :: HsqlStmt.Statement SEnP.PotTransfer Id.PotTransferId
insertPotTransferStmt =
  insert
    SEnP.potTransferEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.PotTransferId)

insertPotTransfer :: SEnP.PotTransfer -> DbM Id.PotTransferId
insertPotTransfer potTransfer =
  runSession mkDbCallStack $ HsqlSes.statement potTransfer insertPotTransferStmt

--------------------------------------------------------------------------------
-- AdaPots
--------------------------------------------------------------------------------
queryAdaPotsAllStmt :: HsqlStmt.Statement () [(Int64, Int64)]
queryAdaPotsAllStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    adaPotsTable = tableName (Proxy @SEnP.AdaPots)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT "
          , "  epoch_no, "
          , "  ( treasury "
          , "  + reserves "
          , "  + rewards "
          , "  + utxo "
          , "  + deposits_stake "
          , "  + deposits_drep "
          , "  + deposits_proposal "
          , "  + fees "
          , "  )::bigint "
          , "FROM "
          , adaPotsTable
          , " ORDER BY epoch_no"
          ]

    encoder = mempty

    decoder =
      HsqlD.rowList $
        (,)
          <$> HsqlD.column (HsqlD.nonNullable HsqlD.int8) -- epoch_no
          <*> HsqlD.column (HsqlD.nonNullable HsqlD.int8) -- computed total

queryAdaPotsAll :: DbM [(Int64, Int64)]
queryAdaPotsAll =
  runSession mkDbCallStack $
    HsqlSes.statement () queryAdaPotsAllStmt

--------------------------------------------------------------------------------
-- Reserve
--------------------------------------------------------------------------------
insertReserveStmt :: HsqlStmt.Statement SEnP.Reserve Id.ReserveId
insertReserveStmt =
  insert
    SEnP.reserveEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.ReserveId)

insertReserve :: SEnP.Reserve -> DbM Id.ReserveId
insertReserve reserve =
  runSession mkDbCallStack $ HsqlSes.statement reserve insertReserveStmt
