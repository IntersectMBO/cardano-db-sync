{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.EpochAndProtocol where

import Cardano.Prelude (Word64)
import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import Data.Time (UTCTime)
import Data.WideWord (Word128 (..))
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

import Cardano.Db.Error (DbError (..))
import qualified Cardano.Db.Schema.Core.EpochAndProtocol as SEnP
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Schema.Types (utcTimeAsTimestampDecoder)
import Cardano.Db.Statement.Function.Core (ResultType (..), runSession, runSessionEntity)
import Cardano.Db.Statement.Function.Insert (insert, insertCheckUnique, insertReplace)
import Cardano.Db.Statement.Function.Query (countAll, replace, selectByFieldFirst)
import Cardano.Db.Statement.Types (Entity (..))
import Cardano.Db.Types (DbLovelace (..), DbM)

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
  runSession $ HsqlSes.statement costModel costModelStmt

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
  runSession $ HsqlSes.statement adaPots insertAdaPotsStmt

-- | QUERY

-- AdaPots query statement
queryAdaPotsIdStmt :: HsqlStmt.Statement Id.BlockId (Maybe (Entity SEnP.AdaPots))
queryAdaPotsIdStmt = selectByFieldFirst "block_id" (Id.idEncoder Id.getBlockId) SEnP.entityAdaPotsDecoder

-- AdaPots query function used in tests
queryAdaPotsIdTest :: Id.BlockId -> DbM (Maybe SEnP.AdaPots)
queryAdaPotsIdTest blockId =
  runSessionEntity $
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
    runSession $ HsqlSes.statement blockId queryAdaPotsIdStmt

  -- Then conditionally do the update
  case mAdaPotsEntity of
    Nothing -> pure False
    Just adaPotsEntity
      | entityVal adaPotsEntity == adapots -> pure False
      | otherwise -> do
          runSession $
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

insertEpoch :: SEnP.Epoch -> DbM Id.EpochId
insertEpoch epoch =
  runSession $ HsqlSes.statement epoch insertEpochStmt

--------------------------------------------------------------------------------
insertEpochParamStmt :: HsqlStmt.Statement SEnP.EpochParam Id.EpochParamId
insertEpochParamStmt =
  insert
    SEnP.epochParamEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.EpochParamId)

insertEpochParam :: SEnP.EpochParam -> DbM Id.EpochParamId
insertEpochParam epochParam =
  runSession $ HsqlSes.statement epochParam insertEpochParamStmt

--------------------------------------------------------------------------------
insertEpochSyncTimeStmt :: HsqlStmt.Statement SEnP.EpochSyncTime Id.EpochSyncTimeId
insertEpochSyncTimeStmt =
  insertReplace
    SEnP.epochSyncTimeEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.EpochSyncTimeId)

insertEpochSyncTime :: SEnP.EpochSyncTime -> DbM Id.EpochSyncTimeId
insertEpochSyncTime epochSyncTime =
  runSession $ HsqlSes.statement epochSyncTime insertEpochSyncTimeStmt

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

queryEpochEntry :: Word64 -> DbM (Either DbError SEnP.Epoch)
queryEpochEntry epochNum = do
  result <- runSession $ HsqlSes.statement epochNum queryEpochEntryStmt
  case result of
    Just res -> pure $ Right res
    Nothing -> pure $ Left $ DbError errorMsg
  where
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
queryCalcEpochEntry :: Word64 -> DbM SEnP.Epoch
queryCalcEpochEntry epochNum =
  runSession $
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
queryForEpochId :: Word64 -> DbM (Maybe Id.EpochId)
queryForEpochId epochNum =
  runSession $ HsqlSes.statement epochNum queryForEpochIdStmt

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

    decoder = HsqlD.rowMaybe SEnP.epochDecoder

-- | Get the most recent epoch in the Epoch DB table.
queryLatestEpoch :: DbM (Maybe SEnP.Epoch)
queryLatestEpoch =
  runSession $ HsqlSes.statement () queryLatestEpochStmt

--------------------------------------------------------------------------------
queryEpochCount :: DbM Word64
queryEpochCount =
  runSession $
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

queryLatestCachedEpochNo :: DbM (Maybe Word64)
queryLatestCachedEpochNo =
  runSession $ HsqlSes.statement () queryLatestCachedEpochNoStmt

--------------------------------------------------------------------------------
replaceEpochStmt :: HsqlStmt.Statement (Id.EpochId, SEnP.Epoch) ()
replaceEpochStmt =
  replace
    (Id.idEncoder Id.getEpochId)
    SEnP.epochEncoder

replaceEpoch :: Id.EpochId -> SEnP.Epoch -> DbM ()
replaceEpoch epochId epoch =
  runSession $ HsqlSes.statement (epochId, epoch) replaceEpochStmt

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
  runSession $ HsqlSes.statement epochState insertEpochStateStmt

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
  runSession $ HsqlSes.statement potTransfer insertPotTransferStmt

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
  runSession $ HsqlSes.statement reserve insertReserveStmt
