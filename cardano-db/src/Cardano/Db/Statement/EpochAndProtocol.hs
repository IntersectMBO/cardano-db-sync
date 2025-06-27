{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.EpochAndProtocol where

import Cardano.Prelude (MonadError (..), MonadIO (..), Proxy (..), Word64, void)
import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import Data.Time (UTCTime)
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

import Cardano.Db.Error (DbError (..))
import qualified Cardano.Db.Schema.Core.EpochAndProtocol as SEnP
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Schema.Types (utcTimeAsTimestampDecoder)
import Cardano.Db.Statement.Function.Core (ResultType (..), ResultTypeBulk (..), mkDbCallStack, runDbSession)
import Cardano.Db.Statement.Function.Insert (insert, insertCheckUnique, insertReplace)
import Cardano.Db.Statement.Function.InsertBulk (insertBulk)
import Cardano.Db.Statement.Function.Query (countAll, replace, selectByFieldFirst)
import Cardano.Db.Statement.Types (DbInfo (..), Entity (..))
import Cardano.Db.Types (DbAction (..), DbLovelace (..))
import Data.WideWord (Word128 (..))

--------------------------------------------------------------------------------
-- CostModel
--------------------------------------------------------------------------------
costModelStmt :: HsqlStmt.Statement SEnP.CostModel Id.CostModelId
costModelStmt =
  insertCheckUnique
    SEnP.costModelEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.CostModelId)

insertCostModel :: MonadIO m => SEnP.CostModel -> DbAction m Id.CostModelId
insertCostModel costModel =
  runDbSession (mkDbCallStack "insertCostModel") $ HsqlSes.statement costModel costModelStmt

queryCostModelStmt :: HsqlStmt.Statement () [Id.CostModelId]
queryCostModelStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    tableN = tableName (Proxy @SEnP.CostModel)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id"
          , " FROM " <> tableN
          , " ORDER BY id ASC"
          ]
    decoder =
      HsqlD.rowList $
        Id.idDecoder Id.CostModelId

queryCostModel :: MonadIO m => DbAction m [Id.CostModelId]
queryCostModel =
  runDbSession (mkDbCallStack "queryCostModel") $
    HsqlSes.statement () queryCostModelStmt

--------------------------------------------------------------------------------
-- AdaPots
--------------------------------------------------------------------------------

-- | INSERT
insertAdaPotsStmt :: HsqlStmt.Statement SEnP.AdaPots Id.AdaPotsId
insertAdaPotsStmt =
  insert
    SEnP.adaPotsEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.AdaPotsId)

insertAdaPots :: MonadIO m => SEnP.AdaPots -> DbAction m Id.AdaPotsId
insertAdaPots adaPots =
  runDbSession (mkDbCallStack "insertAdaPots") $ HsqlSes.statement adaPots insertAdaPotsStmt

-- | QUERY

-- AdaPots query statement
queryAdaPotsIdStmt :: HsqlStmt.Statement Id.BlockId (Maybe (Entity SEnP.AdaPots))
queryAdaPotsIdStmt = selectByFieldFirst "block_id" (Id.idEncoder Id.getBlockId) SEnP.entityAdaPotsDecoder

-- AdaPots query function
queryAdaPotsId :: MonadIO m => Id.BlockId -> DbAction m (Maybe (Entity SEnP.AdaPots))
queryAdaPotsId blockId =
  runDbSession (mkDbCallStack "queryAdaPotsId") $
    HsqlSes.statement blockId queryAdaPotsIdStmt

-- AdaPots query function used in tests
queryAdaPotsIdTest :: MonadIO m => Id.BlockId -> DbAction m (Maybe SEnP.AdaPots)
queryAdaPotsIdTest blockId = do
  mEntityAdaPots <-
    runDbSession (mkDbCallStack "queryAdaPotsId") $
      HsqlSes.statement blockId queryAdaPotsIdStmt
  pure $ entityVal <$> mEntityAdaPots

--------------------------------------------------------------------------------
replaceAdaPotsStmt :: HsqlStmt.Statement (Id.AdaPotsId, SEnP.AdaPots) ()
replaceAdaPotsStmt =
  replace
    (Id.idEncoder Id.getAdaPotsId)
    SEnP.adaPotsEncoder

replaceAdaPots :: MonadIO m => Id.BlockId -> SEnP.AdaPots -> DbAction m Bool
replaceAdaPots blockId adapots = do
  -- Do the query first
  mAdaPotsEntity <-
    runDbSession (mkDbCallStack "queryAdaPots") $
      HsqlSes.statement blockId queryAdaPotsIdStmt

  -- Then conditionally do the update
  case mAdaPotsEntity of
    Nothing -> pure False
    Just adaPotsEntity
      | entityVal adaPotsEntity == adapots -> pure False
      | otherwise -> do
          runDbSession (mkDbCallStack "updateAdaPots") $
            HsqlSes.statement (entityKey adaPotsEntity, adapots) replaceAdaPotsStmt
          pure True

--------------------------------------------------------------------------------
-- Epoch
--------------------------------------------------------------------------------
insertEpochStmt :: HsqlStmt.Statement SEnP.Epoch Id.EpochId
insertEpochStmt =
  insertCheckUnique
    SEnP.epochEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.EpochId)

insertEpoch :: MonadIO m => SEnP.Epoch -> DbAction m Id.EpochId
insertEpoch epoch =
  runDbSession (mkDbCallStack "insertEpoch") $ HsqlSes.statement epoch insertEpochStmt

--------------------------------------------------------------------------------
insertEpochParamStmt :: HsqlStmt.Statement SEnP.EpochParam Id.EpochParamId
insertEpochParamStmt =
  insert
    SEnP.epochParamEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.EpochParamId)

insertEpochParam :: MonadIO m => SEnP.EpochParam -> DbAction m Id.EpochParamId
insertEpochParam epochParam =
  runDbSession (mkDbCallStack "insertEpochParam") $ HsqlSes.statement epochParam insertEpochParamStmt

--------------------------------------------------------------------------------
insertEpochSyncTimeStmt :: HsqlStmt.Statement SEnP.EpochSyncTime Id.EpochSyncTimeId
insertEpochSyncTimeStmt =
  insertReplace
    SEnP.epochSyncTimeEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.EpochSyncTimeId)

insertEpochSyncTime :: MonadIO m => SEnP.EpochSyncTime -> DbAction m Id.EpochSyncTimeId
insertEpochSyncTime epochSyncTime =
  runDbSession (mkDbCallStack "insertEpochSyncTime") $ HsqlSes.statement epochSyncTime insertEpochSyncTimeStmt

-- | QUERY ----------------------------------------------------------------------------------
queryEpochEntryStmt :: HsqlStmt.Statement Word64 (Maybe SEnP.Epoch)
queryEpochEntryStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder = HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
    decoder = HsqlD.rowMaybe SEnP.epochDecoder
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT *"
          , " FROM epoch"
          , " WHERE no = $1"
          ]

queryEpochEntry :: MonadIO m => Word64 -> DbAction m SEnP.Epoch
queryEpochEntry epochNum = do
  result <- runDbSession dbCallStack $ HsqlSes.statement epochNum queryEpochEntryStmt
  case result of
    Just res -> pure res
    Nothing -> throwError $ DbError dbCallStack errorMsg Nothing
  where
    dbCallStack = mkDbCallStack "queryEpochEntry"
    errorMsg = "Epoch not found with number: " <> Text.pack (show epochNum)

--------------------------------------------------------------------------------
queryCalcEpochEntryStmt :: HsqlStmt.Statement Word64 SEnP.Epoch
queryCalcEpochEntryStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "WITH block_stats AS ("
          , "  SELECT COUNT(*) as block_count, MIN(time) as min_time, MAX(time) as max_time"
          , "  FROM block"
          , "  WHERE epoch_no = $1"
          , "),"
          , "tx_stats AS ("
          , "  SELECT COALESCE(SUM(tx.out_sum), 0) as out_sum, "
          , "         COALESCE(SUM(tx.fee), 0) as fee_sum, "
          , "         COUNT(tx.out_sum) as tx_count"
          , "  FROM tx"
          , "  INNER JOIN block ON tx.block_id = block.id"
          , "  WHERE block.epoch_no = $1"
          , ") "
          , "SELECT $1 as epoch_no, "
          , "       bs.block_count, "
          , "       bs.min_time, "
          , "       bs.max_time, "
          , "       ts.out_sum, "
          , "       ts.fee_sum, "
          , "       ts.tx_count "
          , "FROM block_stats bs, tx_stats ts"
          ]

    encoder = HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)

    decoder = HsqlD.singleRow $ do
      epochNo <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
      blockCount <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
      minTime <- HsqlD.column (HsqlD.nullable utcTimeAsTimestampDecoder)
      maxTime <- HsqlD.column (HsqlD.nullable utcTimeAsTimestampDecoder)
      outSum <- HsqlD.column (HsqlD.nonNullable HsqlD.int8) -- Decode as single int8
      feeSum <- HsqlD.column (HsqlD.nonNullable HsqlD.int8)
      txCount <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)

      pure $ case (blockCount, minTime, maxTime) of
        (0, _, _) -> emptyEpoch epochNo
        (_, Just start, Just end) ->
          if txCount == 0
            then convertBlk epochNo (blockCount, Just start, Just end)
            else
              SEnP.Epoch
                { SEnP.epochOutSum = Word128 0 (fromIntegral outSum) -- Construct Word128 from single value
                , SEnP.epochFees = DbLovelace $ fromIntegral feeSum
                , SEnP.epochTxCount = txCount
                , SEnP.epochBlkCount = blockCount
                , SEnP.epochNo = epochNo
                , SEnP.epochStartTime = start
                , SEnP.epochEndTime = end
                }
        _otherwise -> emptyEpoch epochNo

convertBlk :: Word64 -> (Word64, Maybe UTCTime, Maybe UTCTime) -> SEnP.Epoch
convertBlk epochNum (blkCount, b, c) =
  case (b, c) of
    (Just start, Just end) -> SEnP.Epoch 0 (DbLovelace 0) 0 blkCount epochNum start end
    _otherwise -> emptyEpoch epochNum

-- We only return this when something has screwed up.
emptyEpoch :: Word64 -> SEnP.Epoch
emptyEpoch epochNum =
  SEnP.Epoch
    { SEnP.epochOutSum = 0
    , SEnP.epochFees = DbLovelace 0
    , SEnP.epochTxCount = 0
    , SEnP.epochBlkCount = 0
    , SEnP.epochNo = epochNum
    , SEnP.epochStartTime = defaultUTCTime
    , SEnP.epochEndTime = defaultUTCTime
    }

defaultUTCTime :: UTCTime
defaultUTCTime = read "2000-01-01 00:00:00.000000 UTC"

-- | Calculate the Epoch table entry for the specified epoch.
-- When syncing the chain or filling an empty table, this is called at each epoch boundary to
-- calculate the Epoch entry for the last epoch.
queryCalcEpochEntry :: MonadIO m => Word64 -> DbAction m SEnP.Epoch
queryCalcEpochEntry epochNum =
  runDbSession (mkDbCallStack "queryCalcEpochEntry") $
    HsqlSes.statement epochNum queryCalcEpochEntryStmt

--------------------------------------------------------------------------------
queryForEpochIdStmt :: HsqlStmt.Statement Word64 (Maybe Id.EpochId)
queryForEpochIdStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder = HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
    decoder = HsqlD.rowMaybe (Id.idDecoder Id.EpochId)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id"
          , " FROM epoch"
          , " WHERE no = $1"
          ]

-- | Get the PostgreSQL row index (EpochId) that matches the given epoch number.
queryForEpochId :: MonadIO m => Word64 -> DbAction m (Maybe Id.EpochId)
queryForEpochId epochNum =
  runDbSession (mkDbCallStack "queryForEpochId") $
    HsqlSes.statement epochNum queryForEpochIdStmt

--------------------------------------------------------------------------------
queryEpochFromNumStmt :: HsqlStmt.Statement Word64 (Maybe SEnP.Epoch)
queryEpochFromNumStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT *"
          , " FROM epoch"
          , " WHERE no = $1"
          ]

    encoder = HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
    decoder = HsqlD.rowMaybe SEnP.epochDecoder

-- | Get an epoch given it's number.
queryEpochFromNum :: MonadIO m => Word64 -> DbAction m (Maybe SEnP.Epoch)
queryEpochFromNum epochNum =
  runDbSession (mkDbCallStack "queryEpochFromNum") $
    HsqlSes.statement epochNum queryEpochFromNumStmt

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
          , " ORDER BY no DESC"
          , " LIMIT 1"
          ]

    decoder = HsqlD.rowMaybe SEnP.epochDecoder

-- | Get the most recent epoch in the Epoch DB table.
queryLatestEpoch :: MonadIO m => DbAction m (Maybe SEnP.Epoch)
queryLatestEpoch =
  runDbSession (mkDbCallStack "queryLatestEpoch") $
    HsqlSes.statement () queryLatestEpochStmt

--------------------------------------------------------------------------------
queryEpochCount :: MonadIO m => DbAction m Word64
queryEpochCount =
  runDbSession (mkDbCallStack "queryEpochCount") $
    HsqlSes.statement () (countAll @SEnP.Epoch)

--------------------------------------------------------------------------------
queryLatestCachedEpochNoStmt :: HsqlStmt.Statement () (Maybe Word64)
queryLatestCachedEpochNoStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT no"
          , " FROM epoch"
          , " ORDER BY no DESC"
          , " LIMIT 1"
          ]

    decoder = HsqlD.rowMaybe $ do
      epochNo <- HsqlD.column (HsqlD.nonNullable HsqlD.int8)
      pure $ fromIntegral epochNo

queryLatestCachedEpochNo :: MonadIO m => DbAction m (Maybe Word64)
queryLatestCachedEpochNo =
  runDbSession (mkDbCallStack "queryLatestCachedEpochNo") $
    HsqlSes.statement () queryLatestCachedEpochNoStmt

--------------------------------------------------------------------------------
replaceEpochStmt :: HsqlStmt.Statement (Id.EpochId, SEnP.Epoch) ()
replaceEpochStmt =
  replace
    (Id.idEncoder Id.getEpochId)
    SEnP.epochEncoder

replaceEpoch :: MonadIO m => Id.EpochId -> SEnP.Epoch -> DbAction m ()
replaceEpoch epochId epoch =
  runDbSession (mkDbCallStack "replaceEpoch") $
    HsqlSes.statement (epochId, epoch) replaceEpochStmt

--------------------------------------------------------------------------------
-- EpochState
--------------------------------------------------------------------------------
insertEpochStateStmt :: HsqlStmt.Statement SEnP.EpochState Id.EpochStateId
insertEpochStateStmt =
  insert
    SEnP.epochStateEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.EpochStateId)

insertEpochState :: MonadIO m => SEnP.EpochState -> DbAction m Id.EpochStateId
insertEpochState epochState =
  runDbSession (mkDbCallStack "insertEpochState") $ HsqlSes.statement epochState insertEpochStateStmt

insertBulkEpochStateStmt :: HsqlStmt.Statement [SEnP.EpochState] ()
insertBulkEpochStateStmt =
  insertBulk
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

insertBulkEpochState :: MonadIO m => [SEnP.EpochState] -> DbAction m ()
insertBulkEpochState epochStates =
  void $
    runDbSession (mkDbCallStack "insertBulkEpochState") $
      HsqlSes.statement epochStates insertBulkEpochStateStmt

--------------------------------------------------------------------------------
-- PotTransfer
--------------------------------------------------------------------------------
insertPotTransferStmt :: HsqlStmt.Statement SEnP.PotTransfer Id.PotTransferId
insertPotTransferStmt =
  insert
    SEnP.potTransferEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.PotTransferId)

insertPotTransfer :: MonadIO m => SEnP.PotTransfer -> DbAction m Id.PotTransferId
insertPotTransfer potTransfer =
  runDbSession (mkDbCallStack "insertPotTransfer") $ HsqlSes.statement potTransfer insertPotTransferStmt

--------------------------------------------------------------------------------
-- Reserve
--------------------------------------------------------------------------------
insertReserveStmt :: HsqlStmt.Statement SEnP.Reserve Id.ReserveId
insertReserveStmt =
  insert
    SEnP.reserveEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.ReserveId)

insertReserve :: MonadIO m => SEnP.Reserve -> DbAction m Id.ReserveId
insertReserve reserve =
  runDbSession (mkDbCallStack "insertReserve") $ HsqlSes.statement reserve insertReserveStmt

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
