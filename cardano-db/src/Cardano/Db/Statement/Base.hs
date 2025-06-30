{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Cardano.Db.Statement.Base where

import Cardano.BM.Data.Trace (Trace)
import Cardano.BM.Trace (logInfo, logWarning, nullTracer)
import Cardano.Ledger.BaseTypes (SlotNo (..))
import Cardano.Prelude (ByteString, Int64, MonadError (..), MonadIO (..), Proxy (..), Word64, textShow, void)
import Data.Functor.Contravariant (Contravariant (..), (>$<))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (partition)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import System.IO (hFlush, stdout)

import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Pipeline as HsqlPipeL
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

import Cardano.Db.Error (DbError (..))
import qualified Cardano.Db.Schema.Core as SC
import qualified Cardano.Db.Schema.Core.Base as SCB
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Schema.MinIds (MinIds (..), MinIdsWrapper (..), textToMinIds)
import Cardano.Db.Schema.Types (utcTimeAsTimestampDecoder)
import Cardano.Db.Schema.Variants (TxOutVariantType)
import Cardano.Db.Statement.Function.Core (ResultType (..), ResultTypeBulk (..), mkDbCallStack, runDbSession)
import Cardano.Db.Statement.Function.Delete (deleteWhereCount)
import Cardano.Db.Statement.Function.Insert (insert, insertCheckUnique)
import Cardano.Db.Statement.Function.InsertBulk (insertBulk, insertBulkJsonb)
import Cardano.Db.Statement.Function.Query (adaSumDecoder, countAll, parameterisedCountWhere)
import Cardano.Db.Statement.GovernanceAndVoting (setNullDroppedStmt, setNullEnactedStmt, setNullExpiredStmt, setNullRatifiedStmt)
import Cardano.Db.Statement.MinIds (completeMinId, queryMinRefId)
import Cardano.Db.Statement.Rollback (deleteTablesAfterBlockId)
import Cardano.Db.Statement.Types (DbInfo, Entity (..), tableName, validateColumn)
import Cardano.Db.Statement.Variants.TxOut (querySetNullTxOut)
import Cardano.Db.Types (Ada (..), DbAction, DbWord64, ExtraMigration, extraDescription)
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------

-- | INSERT --------------------------------------------------------------------
insertBlockStmt :: HsqlStmt.Statement SCB.Block Id.BlockId
insertBlockStmt =
  insert
    SCB.blockEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.BlockId)

insertBlock :: MonadIO m => SCB.Block -> DbAction m Id.BlockId
insertBlock block =
  runDbSession (mkDbCallStack "insertBlock") $ HsqlSes.statement block insertBlockStmt

insertCheckUniqueBlockStmt :: HsqlStmt.Statement SCB.Block Id.BlockId
insertCheckUniqueBlockStmt =
  insertCheckUnique
    SCB.blockEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.BlockId)

insertCheckUniqueBlock :: MonadIO m => SCB.Block -> DbAction m Id.BlockId
insertCheckUniqueBlock block =
  runDbSession (mkDbCallStack "insertCheckUniqueBlock") $
    HsqlSes.statement block insertCheckUniqueBlockStmt

-- | QUERIES -------------------------------------------------------------------
queryBlockHashBlockNoStmt :: HsqlStmt.Statement ByteString [Word64]
queryBlockHashBlockNoStmt =
  HsqlStmt.Statement sql hashEncoder blockNoDecoder True
  where
    table = tableName (Proxy @SCB.Block)
    hashEncoder = HsqlE.param (HsqlE.nonNullable HsqlE.bytea)
    blockNoDecoder = HsqlD.rowList (HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8))
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          ["SELECT block_no FROM " <> table <> " WHERE hash = $1"]

queryBlockHashBlockNo :: MonadIO m => ByteString -> DbAction m (Maybe Word64)
queryBlockHashBlockNo hash = do
  let dbCallStack = mkDbCallStack "queryBlockHashBlockNo"
  result <-
    runDbSession dbCallStack $
      HsqlSes.statement hash queryBlockHashBlockNoStmt
  case result of
    [] -> pure Nothing
    [blockNo] -> pure (Just blockNo)
    results -> throwError $ DbError dbCallStack errorMsg Nothing
      where
        errorMsg =
          "Multiple blocks found with same hash: "
            <> Text.pack (show hash)
            <> " (found "
            <> Text.pack (show $ length results)
            <> ")"

--------------------------------------------------------------------------------
queryBlockCountStmt :: HsqlStmt.Statement () Word64
queryBlockCountStmt =
  HsqlStmt.Statement sql mempty blockCountDecoder True
  where
    table = tableName (Proxy @SCB.Block)
    blockCountDecoder = HsqlD.singleRow (HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8))
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          ["SELECT COUNT(*) FROM " <> table]

queryBlockCount :: MonadIO m => DbAction m Word64
queryBlockCount = runDbSession (mkDbCallStack "queryBlockCount") $ HsqlSes.statement () queryBlockCountStmt

--------------------------------------------------------------------------------
querySlotUtcTimeStmt :: HsqlStmt.Statement Word64 (Maybe UTCTime)
querySlotUtcTimeStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder = HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
    decoder = HsqlD.rowMaybe (HsqlD.column (HsqlD.nonNullable utcTimeAsTimestampDecoder))
    blockTable = tableName (Proxy @SC.Block)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT time"
          , " FROM " <> blockTable
          , " WHERE slot_no = $1"
          ]

-- | Calculate the slot time (as UTCTime) for a given slot number.
-- This will fail if the slot is empty.
querySlotUtcTime :: MonadIO m => Word64 -> DbAction m UTCTime
querySlotUtcTime slotNo = do
  result <- runDbSession dbCallStack $ HsqlSes.statement slotNo querySlotUtcTimeStmt
  case result of
    Just time -> pure time
    Nothing -> throwError $ DbError dbCallStack errorMsg Nothing
  where
    dbCallStack = mkDbCallStack "querySlotUtcTime"
    errorMsg = "slot_no not found with number: " <> Text.pack (show slotNo)

querySlotUtcTimeEither :: MonadIO m => Word64 -> DbAction m (Either DbError UTCTime)
querySlotUtcTimeEither slotNo = do
  result <- runDbSession dbCallStack $ HsqlSes.statement slotNo querySlotUtcTimeStmt
  case result of
    Just time -> pure $ Right time
    Nothing -> pure $ Left $ DbError dbCallStack ("Slot not found for slot_no: " <> Text.pack (show slotNo)) Nothing
  where
    dbCallStack = mkDbCallStack "querySlotUtcTimeEither"

--------------------------------------------------------------------------------
queryBlockByIdStmt :: HsqlStmt.Statement Id.BlockId (Maybe (Entity SCB.Block))
queryBlockByIdStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT *"
          , " FROM " <> tableName (Proxy @SCB.Block)
          , " WHERE id = $1"
          ]
    encoder = Id.idEncoder Id.getBlockId
    decoder = HsqlD.rowMaybe SCB.entityBlockDecoder

queryBlockById :: MonadIO m => Id.BlockId -> DbAction m (Maybe SCB.Block)
queryBlockById blockId = do
  res <-
    runDbSession (mkDbCallStack "queryBlockSlotAndHash") $
      HsqlSes.statement blockId queryBlockByIdStmt
  pure $ entityVal <$> res

--------------------------------------------------------------------------------

-- counting blocks after a specific BlockNo with >= operator
queryBlockCountAfterEqBlockNoStmt :: HsqlStmt.Statement Word64 Word64
queryBlockCountAfterEqBlockNoStmt =
  parameterisedCountWhere @SCB.Block
    "block_no"
    ">= $1"
    (HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8))

-- counting blocks after a specific BlockNo with > operator
queryBlockCountAfterBlockNoStmt :: HsqlStmt.Statement Word64 Word64
queryBlockCountAfterBlockNoStmt =
  parameterisedCountWhere @SCB.Block
    "block_no"
    "> $1"
    (HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8))

-- | Count the number of blocks in the Block table after a 'BlockNo'.
queryBlockCountAfterBlockNo :: MonadIO m => Word64 -> Bool -> DbAction m Word64
queryBlockCountAfterBlockNo blockNo queryEq = do
  let dbCallStack = mkDbCallStack "queryBlockCountAfterBlockNo"
      stmt =
        if queryEq
          then queryBlockCountAfterEqBlockNoStmt
          else queryBlockCountAfterBlockNoStmt
  runDbSession dbCallStack $ HsqlSes.statement blockNo stmt

--------------------------------------------------------------------------------
queryBlockNoStmt ::
  forall a.
  DbInfo a =>
  HsqlStmt.Statement Word64 (Maybe Id.BlockId)
queryBlockNoStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder = HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
    decoder = HsqlD.rowMaybe (Id.idDecoder Id.BlockId)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id"
          , " FROM " <> tableName (Proxy @a)
          , " WHERE block_no = $1"
          ]

queryBlockNo :: MonadIO m => Word64 -> DbAction m (Maybe Id.BlockId)
queryBlockNo blkNo =
  runDbSession (mkDbCallStack "queryBlockNo") $
    HsqlSes.statement blkNo $
      queryBlockNoStmt @SCB.Block

--------------------------------------------------------------------------------
queryBlockNoAndEpochStmt ::
  forall a.
  DbInfo a =>
  HsqlStmt.Statement Word64 (Maybe (Id.BlockId, Word64))
queryBlockNoAndEpochStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder = HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
    decoder = HsqlD.rowMaybe $ do
      blockId <- Id.idDecoder Id.BlockId
      epochNo <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
      pure (blockId, epochNo)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id, epoch_no"
          , " FROM " <> tableName (Proxy @a)
          , " WHERE block_no = $1"
          ]

queryBlockNoAndEpoch :: MonadIO m => Word64 -> DbAction m (Maybe (Id.BlockId, Word64))
queryBlockNoAndEpoch blkNo =
  runDbSession (mkDbCallStack "queryBlockNoAndEpoch") $
    HsqlSes.statement blkNo $
      queryBlockNoAndEpochStmt @SCB.Block

--------------------------------------------------------------------------------
queryBlockSlotAndHashStmt :: HsqlStmt.Statement Id.BlockId (Maybe (SlotNo, ByteString))
queryBlockSlotAndHashStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT slot_no, hash"
          , " FROM " <> tableName (Proxy @SCB.Block)
          , " WHERE id = $1"
          ]
    encoder = Id.idEncoder Id.getBlockId
    decoder = HsqlD.rowMaybe $ do
      slotNo <- SlotNo . fromIntegral <$> HsqlD.column (HsqlD.nonNullable HsqlD.int8)
      hash <- HsqlD.column (HsqlD.nonNullable HsqlD.bytea)
      pure (slotNo, hash)

queryBlockSlotAndHash :: MonadIO m => Id.BlockId -> DbAction m (Maybe (SlotNo, ByteString))
queryBlockSlotAndHash blockId =
  runDbSession (mkDbCallStack "queryBlockSlotAndHash") $
    HsqlSes.statement blockId queryBlockSlotAndHashStmt

--------------------------------------------------------------------------------
queryNearestBlockSlotNoStmt ::
  forall a.
  DbInfo a =>
  HsqlStmt.Statement Word64 (Maybe (Id.BlockId, Word64))
queryNearestBlockSlotNoStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id, block_no"
          , " FROM " <> tableName (Proxy @a)
          , " WHERE slot_no IS NULL OR slot_no >= $1"
          , " ORDER BY slot_no ASC"
          , " LIMIT 1"
          ]
    encoder = HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
    decoder = HsqlD.rowMaybe $ do
      blockId <- Id.idDecoder Id.BlockId
      blockNo <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
      pure (blockId, blockNo)

queryNearestBlockSlotNo :: MonadIO m => Word64 -> DbAction m (Maybe (Id.BlockId, Word64))
queryNearestBlockSlotNo slotNo =
  runDbSession (mkDbCallStack "queryNearestBlockSlotNo") $
    HsqlSes.statement slotNo $
      queryNearestBlockSlotNoStmt @SCB.Block

--------------------------------------------------------------------------------
queryBlockHashStmt ::
  forall a.
  DbInfo a =>
  HsqlStmt.Statement ByteString (Maybe (Id.BlockId, Word64))
queryBlockHashStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id, epoch_no"
          , " FROM " <> tableName (Proxy @a)
          , " WHERE hash = $1"
          ]
    encoder = HsqlE.param (HsqlE.nonNullable HsqlE.bytea)
    decoder = HsqlD.rowMaybe $ do
      blockId <- Id.idDecoder Id.BlockId
      epochNo <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
      pure (blockId, epochNo)

queryBlockHash :: MonadIO m => SCB.Block -> DbAction m (Maybe (Id.BlockId, Word64))
queryBlockHash block =
  runDbSession (mkDbCallStack "queryBlockHash") $
    HsqlSes.statement (SCB.blockHash block) $
      queryBlockHashStmt @SCB.Block

--------------------------------------------------------------------------------
queryMinBlockStmt ::
  forall a.
  DbInfo a =>
  HsqlStmt.Statement () (Maybe (Id.BlockId, Word64))
queryMinBlockStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id, block_no"
          , " FROM " <> tableName (Proxy @a)
          , " ORDER BY id ASC"
          , " LIMIT 1"
          ]

    decoder = HsqlD.rowMaybe $ do
      blockId <- Id.idDecoder Id.BlockId
      blockNo <- HsqlD.column (HsqlD.nullable $ fromIntegral <$> HsqlD.int8)
      pure (blockId, fromMaybe 0 blockNo)

queryMinBlock :: MonadIO m => DbAction m (Maybe (Id.BlockId, Word64))
queryMinBlock =
  runDbSession (mkDbCallStack "queryMinBlock") $
    HsqlSes.statement () $
      queryMinBlockStmt @SCB.Block

--------------------------------------------------------------------------------
queryReverseIndexBlockIdStmt ::
  forall a.
  DbInfo a =>
  HsqlStmt.Statement Id.BlockId [Maybe Text.Text]
queryReverseIndexBlockIdStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder = Id.idEncoder Id.getBlockId
    decoder = HsqlD.rowList $ HsqlD.column (HsqlD.nullable HsqlD.text)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT ridx.min_ids"
          , " FROM " <> tableName (Proxy @a) <> " blk"
          , " LEFT JOIN reverse_index ridx ON blk.id = ridx.block_id"
          , " WHERE blk.id >= $1"
          , " ORDER BY blk.id ASC"
          ]

queryReverseIndexBlockId :: MonadIO m => Id.BlockId -> DbAction m [Maybe Text.Text]
queryReverseIndexBlockId blockId =
  runDbSession (mkDbCallStack "queryReverseIndexBlockId") $
    HsqlSes.statement blockId $
      queryReverseIndexBlockIdStmt @SCB.Block

--------------------------------------------------------------------------------
queryMinIdsAfterReverseIndexStmt :: HsqlStmt.Statement Id.ReverseIndexId [Text.Text]
queryMinIdsAfterReverseIndexStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder = Id.idEncoder Id.getReverseIndexId
    decoder = HsqlD.rowList $ HsqlD.column (HsqlD.nonNullable HsqlD.text)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT min_ids"
          , " FROM reverse_index"
          , " WHERE id >= $1"
          , " ORDER BY id DESC"
          ]

queryMinIdsAfterReverseIndex :: MonadIO m => Id.ReverseIndexId -> DbAction m [Text.Text]
queryMinIdsAfterReverseIndex rollbackId =
  runDbSession (mkDbCallStack "queryMinIdsAfterReverseIndex") $
    HsqlSes.statement rollbackId queryMinIdsAfterReverseIndexStmt

--------------------------------------------------------------------------------

-- | Get the number of transactions in the specified block.
queryBlockTxCountStmt :: HsqlStmt.Statement Id.BlockId Word64
queryBlockTxCountStmt =
  parameterisedCountWhere @SCB.Tx "block_id" "= $1" (Id.idEncoder Id.getBlockId)

queryBlockTxCount :: MonadIO m => Id.BlockId -> DbAction m Word64
queryBlockTxCount blkId =
  runDbSession (mkDbCallStack "queryBlockTxCount") $
    HsqlSes.statement blkId queryBlockTxCountStmt

--------------------------------------------------------------------------------
queryBlockIdStmt :: HsqlStmt.Statement ByteString (Maybe Id.BlockId)
queryBlockIdStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder = HsqlE.param (HsqlE.nonNullable HsqlE.bytea)
    decoder = HsqlD.rowMaybe (Id.idDecoder Id.BlockId)
    blockTable = tableName (Proxy @SC.Block)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id"
          , " FROM " <> blockTable
          , " WHERE hash = $1"
          ]

queryBlockId :: MonadIO m => ByteString -> Text.Text -> DbAction m Id.BlockId
queryBlockId hash errMsg = do
  result <- runDbSession callStack $ HsqlSes.statement hash queryBlockIdStmt
  case result of
    Just blockId -> pure blockId
    Nothing -> throwError $ DbError callStack ("Block not found for hash: " <> errMsg) Nothing
  where
    callStack = mkDbCallStack "queryBlockId"

queryBlockIdEither :: MonadIO m => ByteString -> Text.Text -> DbAction m (Either DbError Id.BlockId)
queryBlockIdEither hash errMsg = do
  result <- runDbSession callStack $ HsqlSes.statement hash queryBlockIdStmt
  case result of
    Just blockId -> pure $ Right blockId
    Nothing -> pure $ Left $ DbError callStack ("Block not found for hash: " <> errMsg) Nothing
  where
    callStack = mkDbCallStack "queryBlockIdEither"

--------------------------------------------------------------------------------
queryBlocksForCurrentEpochNoStmt :: HsqlStmt.Statement () (Maybe Word64)
queryBlocksForCurrentEpochNoStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    blockTable = tableName (Proxy @SC.Block)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT MAX(epoch_no)"
          , " FROM " <> blockTable
          ]

    decoder =
      HsqlD.singleRow $
        HsqlD.column (HsqlD.nullable $ fromIntegral <$> HsqlD.int8)

queryBlocksForCurrentEpochNo :: MonadIO m => DbAction m (Maybe Word64)
queryBlocksForCurrentEpochNo =
  runDbSession (mkDbCallStack "queryBlocksForCurrentEpochNo") $
    HsqlSes.statement () queryBlocksForCurrentEpochNoStmt

--------------------------------------------------------------------------------
queryLatestBlockStmt :: HsqlStmt.Statement () (Maybe (Entity SCB.Block))
queryLatestBlockStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    blockTable = tableName (Proxy @SC.Block)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT *"
          , " FROM " <> blockTable
          , " WHERE slot_no IS NOT NULL"
          , " ORDER BY slot_no DESC"
          , " LIMIT 1"
          ]
    decoder = HsqlD.rowMaybe SCB.entityBlockDecoder

queryLatestBlock :: MonadIO m => DbAction m (Maybe SCB.Block)
queryLatestBlock = do
  result <-
    runDbSession (mkDbCallStack "queryLatestBlock") $
      HsqlSes.statement () queryLatestBlockStmt
  pure $ entityVal <$> result

--------------------------------------------------------------------------------
queryLatestEpochNoFromBlockStmt :: HsqlStmt.Statement () Word64
queryLatestEpochNoFromBlockStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    blockTable = tableName (Proxy @SC.Block)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COALESCE(epoch_no, 0)::bigint"
          , " FROM " <> blockTable
          , " WHERE slot_no IS NOT NULL"
          , " ORDER BY epoch_no DESC"
          , " LIMIT 1"
          ]

    decoder =
      HsqlD.singleRow $
        fromIntegral <$> HsqlD.column (HsqlD.nonNullable HsqlD.int8)

queryLatestEpochNoFromBlock :: MonadIO m => DbAction m Word64
queryLatestEpochNoFromBlock =
  runDbSession (mkDbCallStack "queryLatestEpochNoFromBlock") $
    HsqlSes.statement () queryLatestEpochNoFromBlockStmt

--------------------------------------------------------------------------------
queryLatestBlockIdStmt :: HsqlStmt.Statement () (Maybe Id.BlockId)
queryLatestBlockIdStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    decoder = HsqlD.rowMaybe (Id.idDecoder Id.BlockId)
    blockTable = tableName (Proxy @SC.Block)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id"
          , " FROM " <> blockTable
          , " ORDER BY slot_no DESC"
          , " LIMIT 1"
          ]

-- | Get 'BlockId' of the latest block.
queryLatestBlockId :: MonadIO m => DbAction m (Maybe Id.BlockId)
queryLatestBlockId =
  runDbSession (mkDbCallStack "queryLatestBlockId") $
    HsqlSes.statement () queryLatestBlockIdStmt

--------------------------------------------------------------------------------
queryDepositUpToBlockNoStmt :: HsqlStmt.Statement Word64 Ada
queryDepositUpToBlockNoStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    txTable = tableName (Proxy @SC.Tx)
    blockTable = tableName (Proxy @SC.Block)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COALESCE(SUM(tx.deposit), 0) "
          , "FROM "
          , txTable
          , " tx "
          , "INNER JOIN "
          , blockTable
          , " blk ON tx.block_id = blk.id "
          , "WHERE blk.block_no <= $1"
          ]
    encoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)
    decoder = HsqlD.singleRow adaSumDecoder

queryDepositUpToBlockNo :: MonadIO m => Word64 -> DbAction m Ada
queryDepositUpToBlockNo blkNo =
  runDbSession (mkDbCallStack "queryDepositUpToBlockNo") $
    HsqlSes.statement blkNo queryDepositUpToBlockNoStmt

--------------------------------------------------------------------------------
queryLatestSlotNoStmt :: HsqlStmt.Statement () Word64
queryLatestSlotNoStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    blockTable = tableName (Proxy @SC.Block)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COALESCE(slot_no, 0)::bigint"
          , " FROM " <> blockTable
          , " WHERE slot_no IS NOT NULL"
          , " ORDER BY slot_no DESC"
          , " LIMIT 1"
          ]

    decoder =
      HsqlD.singleRow $
        fromIntegral <$> HsqlD.column (HsqlD.nonNullable HsqlD.int8)

queryLatestSlotNo :: MonadIO m => DbAction m Word64
queryLatestSlotNo =
  runDbSession (mkDbCallStack "queryLatestSlotNo") $
    HsqlSes.statement () queryLatestSlotNoStmt

--------------------------------------------------------------------------------
queryLatestPointsStmt :: HsqlStmt.Statement () [(Maybe Word64, ByteString)]
queryLatestPointsStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    blockTable = tableName (Proxy @SC.Block)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT slot_no, hash"
          , " FROM " <> blockTable
          , " WHERE slot_no IS NOT NULL"
          , " ORDER BY slot_no DESC"
          , " LIMIT 5"
          ]

    decoder = HsqlD.rowList $ do
      slotNo <- HsqlD.column (HsqlD.nullable $ fromIntegral <$> HsqlD.int8)
      hash <- HsqlD.column (HsqlD.nonNullable HsqlD.bytea)
      pure (slotNo, hash)

queryLatestPoints :: MonadIO m => DbAction m [(Maybe Word64, ByteString)]
queryLatestPoints =
  runDbSession (mkDbCallStack "queryLatestPoints") $
    HsqlSes.statement () queryLatestPointsStmt

-----------------------------------------------------------------------------------
querySlotHashStmt :: HsqlStmt.Statement Word64 [ByteString]
querySlotHashStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    blockTable = tableName (Proxy @SC.Block)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT hash"
          , " FROM " <> blockTable
          , " WHERE slot_no = $1"
          ]
    encoder = HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
    decoder = HsqlD.rowList (HsqlD.column (HsqlD.nonNullable HsqlD.bytea))

querySlotHash :: MonadIO m => SlotNo -> DbAction m [(SlotNo, ByteString)]
querySlotHash slotNo = do
  hashes <-
    runDbSession (mkDbCallStack "querySlotHash") $
      HsqlSes.statement (unSlotNo slotNo) querySlotHashStmt
  pure $ map (\hash -> (slotNo, hash)) hashes

-----------------------------------------------------------------------------------
queryCountSlotNosGreaterThanStmt :: HsqlStmt.Statement Word64 Word64
queryCountSlotNosGreaterThanStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    blockTable = tableName (Proxy @SC.Block)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COUNT(*)::bigint"
          , " FROM " <> blockTable
          , " WHERE slot_no > $1"
          ]

    encoder = HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
    decoder =
      HsqlD.singleRow $
        fromIntegral <$> HsqlD.column (HsqlD.nonNullable HsqlD.int8)

queryCountSlotNosGreaterThan :: MonadIO m => Word64 -> DbAction m Word64
queryCountSlotNosGreaterThan slotNo =
  runDbSession (mkDbCallStack "queryCountSlotNosGreaterThan") $
    HsqlSes.statement slotNo queryCountSlotNosGreaterThanStmt

-----------------------------------------------------------------------------------
queryCountSlotNoStmt :: HsqlStmt.Statement () Word64
queryCountSlotNoStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    blockTable = tableName (Proxy @SC.Block)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COUNT(*)::bigint"
          , " FROM " <> blockTable
          , " WHERE slot_no IS NOT NULL"
          ]

    decoder =
      HsqlD.singleRow $
        fromIntegral <$> HsqlD.column (HsqlD.nonNullable HsqlD.int8)

-- | Like 'queryCountSlotNosGreaterThan', but returns all slots in the same order.
queryCountSlotNo :: MonadIO m => DbAction m Word64
queryCountSlotNo =
  runDbSession (mkDbCallStack "queryCountSlotNo") $
    HsqlSes.statement () queryCountSlotNoStmt

-----------------------------------------------------------------------------------
queryBlockHeightStmt :: forall a. DbInfo a => Text.Text -> HsqlStmt.Statement () (Maybe Word64)
queryBlockHeightStmt colName =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    table = tableName (Proxy @a)
    validCol = validateColumn @a colName

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT "
          , validCol
          , " FROM "
          , table
          , " WHERE "
          , validCol
          , " IS NOT NULL"
          , " ORDER BY "
          , validCol
          , " DESC"
          , " LIMIT 1"
          ]

    decoder = HsqlD.rowMaybe $ do
      blockNo <- HsqlD.column (HsqlD.nonNullable HsqlD.int8)
      pure $ fromIntegral blockNo

queryBlockHeight :: MonadIO m => DbAction m (Maybe Word64)
queryBlockHeight =
  runDbSession (mkDbCallStack "queryBlockHeight") $
    HsqlSes.statement () $
      queryBlockHeightStmt @SC.Block "block_no"

-----------------------------------------------------------------------------------
queryGenesisStmt :: HsqlStmt.Statement () [Id.BlockId]
queryGenesisStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    decoder = HsqlD.rowList (Id.idDecoder Id.BlockId)
    blockTable = tableName (Proxy @SC.Block)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id"
          , " FROM " <> blockTable
          , " WHERE previous_id IS NULL"
          ]

queryGenesis :: MonadIO m => Text.Text -> DbAction m Id.BlockId
queryGenesis errMsg = do
  let dbCallStack = mkDbCallStack "queryGenesis"
      errorMsg = "Multiple Genesis blocks found: " <> errMsg

  result <- runDbSession dbCallStack $ HsqlSes.statement () queryGenesisStmt
  case result of
    [blk] -> pure blk
    _otherwise -> throwError $ DbError dbCallStack errorMsg Nothing

-----------------------------------------------------------------------------------
queryLatestBlockNoStmt :: HsqlStmt.Statement () (Maybe Word64)
queryLatestBlockNoStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    blockTable = tableName (Proxy @SC.Block)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT block_no"
          , " FROM " <> blockTable
          , " WHERE block_no IS NOT NULL"
          , " ORDER BY block_no DESC"
          , " LIMIT 1"
          ]

    decoder = HsqlD.rowMaybe $ do
      blockNo <- HsqlD.column (HsqlD.nonNullable HsqlD.int8)
      pure $ fromIntegral blockNo

queryLatestBlockNo :: MonadIO m => DbAction m (Maybe Word64)
queryLatestBlockNo =
  runDbSession (mkDbCallStack "queryLatestBlockNo") $
    HsqlSes.statement () queryLatestBlockNoStmt

-----------------------------------------------------------------------------------
querySlotNosGreaterThanStmt :: HsqlStmt.Statement Word64 [SlotNo]
querySlotNosGreaterThanStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    blockTable = tableName (Proxy @SC.Block)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT slot_no"
          , " FROM " <> blockTable
          , " WHERE slot_no > $1"
          , " ORDER BY slot_no DESC"
          ]
    encoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)
    decoder = HsqlD.rowList $ do
      slotValue <- HsqlD.column (HsqlD.nonNullable HsqlD.int8)
      pure $ SlotNo (fromIntegral slotValue)

querySlotNosGreaterThan :: MonadIO m => Word64 -> DbAction m [SlotNo]
querySlotNosGreaterThan slotNo =
  runDbSession (mkDbCallStack "querySlotNosGreaterThan") $
    HsqlSes.statement slotNo querySlotNosGreaterThanStmt

-----------------------------------------------------------------------------------

-- | Like 'querySlotNosGreaterThan', but returns all slots in the same order.
querySlotNosStmt :: HsqlStmt.Statement () [SlotNo]
querySlotNosStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    blockTable = tableName (Proxy @SC.Block)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT slot_no"
          , " FROM " <> blockTable
          , " WHERE slot_no IS NOT NULL"
          , " ORDER BY slot_no DESC"
          ]
    decoder = HsqlD.rowList $ do
      slotValue <- HsqlD.column (HsqlD.nonNullable HsqlD.int8)
      pure $ SlotNo (fromIntegral slotValue)

querySlotNos :: MonadIO m => DbAction m [SlotNo]
querySlotNos =
  runDbSession (mkDbCallStack "querySlotNos") $
    HsqlSes.statement () querySlotNosStmt

-----------------------------------------------------------------------------------
queryPreviousSlotNoStmt :: HsqlStmt.Statement Word64 (Maybe Word64)
queryPreviousSlotNoStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    blockTableN = tableName (Proxy @SCB.Block)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT prev_block.slot_no"
          , " FROM " <> blockTableN <> " block"
          , " INNER JOIN " <> blockTableN <> " prev_block"
          , " ON block.previous_id = prev_block.id"
          , " WHERE block.slot_no = $1"
          ]
    encoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)
    decoder = HsqlD.rowMaybe $ do
      slotNo <- HsqlD.column (HsqlD.nonNullable HsqlD.int8)
      pure $ fromIntegral slotNo

queryPreviousSlotNo :: MonadIO m => Word64 -> DbAction m (Maybe Word64)
queryPreviousSlotNo slotNo =
  runDbSession (mkDbCallStack "queryPreviousSlotNo") $
    HsqlSes.statement slotNo queryPreviousSlotNoStmt

-----------------------------------------------------------------------------------
-- DELETE
-----------------------------------------------------------------------------------

deleteBlocksBlockIdStmt :: HsqlStmt.Statement (Id.BlockId, Word64, Bool) Int64
deleteBlocksBlockIdStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder =
      contramap (\(a, _, _) -> a) (Id.idEncoder Id.getBlockId)
        <> contramap (\(_, b, _) -> b) (HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8))
        <> contramap (\(_, _, c) -> c) (HsqlE.param (HsqlE.nonNullable HsqlE.bool))
    decoder = HsqlD.singleRow (HsqlD.column (HsqlD.nonNullable HsqlD.int8))
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "WITH deleted AS ("
          , "  DELETE FROM block"
          , "  WHERE id >= $1"
          , "  RETURNING *"
          , ")"
          , "SELECT COUNT(*)::bigint FROM deleted"
          ]

-- Progress tracking data type
data RollbackProgress = RollbackProgress
  { rpCurrentStep :: !Int
  , rpTotalSteps :: !Int
  , rpCurrentPhase :: !Text.Text
  , rpStartTime :: !UTCTime
  }
  deriving (Show)

-- Progress bar rendering
renderProgressBar :: RollbackProgress -> IO ()
renderProgressBar progress = do
  let percentage :: Double
      percentage = fromIntegral (rpCurrentStep progress) / fromIntegral (rpTotalSteps progress) * 100
      barWidth = 50
      filled = round (fromIntegral barWidth * percentage / 100)
      bar = replicate filled '█' ++ replicate (barWidth - filled) '░'

  putStr $
    "\r\ESC[K" -- Clear entire line
      ++ show (rpCurrentStep progress)
      ++ "/"
      ++ show (rpTotalSteps progress)
      ++ " ["
      ++ bar
      ++ "] "
      ++ printf "%.1f%% - " percentage
      ++ Text.unpack (rpCurrentPhase progress)
  hFlush stdout

deleteBlocksBlockId ::
  MonadIO m =>
  Trace IO Text.Text ->
  TxOutVariantType ->
  Id.BlockId ->
  Word64 ->
  Bool ->
  DbAction m Int64
deleteBlocksBlockId trce txOutVariantType blockId epochN isConsumedTxOut = do
  startTime <- liftIO getCurrentTime
  progressRef <- liftIO $ newIORef $ RollbackProgress 0 6 "Initializing..." startTime

  liftIO $ do
    putStrLn ""
    renderProgressBar =<< readIORef progressRef

  -- Step 1: Find minimum IDs
  liftIO $ do
    writeIORef progressRef . (\p -> p {rpCurrentStep = 1, rpCurrentPhase = "Finding reverse indexes..."}) =<< readIORef progressRef
    putStrLn "" -- Clear the line for better visibility
    renderProgressBar =<< readIORef progressRef

  mMinIds <- fmap (textToMinIds txOutVariantType =<<) <$> queryReverseIndexBlockId blockId
  (cminIds, completed) <- findMinIdsRec progressRef mMinIds mempty
  mRawTxId <- queryMinRefId @SCB.Tx "block_id" blockId (Id.idEncoder Id.getBlockId)
  let mTxId = Id.TxId <$> mRawTxId
  minIds <- if completed then pure cminIds else completeMinId mTxId cminIds

  -- Step 2: Delete epoch-related data
  liftIO $ do
    writeIORef progressRef . (\p -> p {rpCurrentStep = 2, rpCurrentPhase = "Deleting epoch data..."}) =<< readIORef progressRef
    renderProgressBar =<< readIORef progressRef

  deleteEpochLogs <- deleteUsingEpochNo epochN

  -- Step 3: Delete block-related data
  liftIO $ do
    writeIORef progressRef . (\p -> p {rpCurrentStep = 3, rpCurrentPhase = "Deleting block data..."}) =<< readIORef progressRef
    renderProgressBar =<< readIORef progressRef

  (deleteBlockCount, blockDeleteLogs) <- deleteTablesAfterBlockId txOutVariantType blockId mTxId minIds

  -- Step 4: Handle consumed transactions
  liftIO $ do
    writeIORef progressRef . (\p -> p {rpCurrentStep = 4, rpCurrentPhase = "Updating consumed transactions..."}) =<< readIORef progressRef
    renderProgressBar =<< readIORef progressRef

  setNullLogs <-
    if isConsumedTxOut
      then querySetNullTxOut txOutVariantType mTxId
      else pure ("ConsumedTxOut is not active so no Nulls set", 0)

  -- Step 5: Generate summary
  liftIO $ do
    writeIORef progressRef . (\p -> p {rpCurrentStep = 5, rpCurrentPhase = "Generating summary..."}) =<< readIORef progressRef
    renderProgressBar =<< readIORef progressRef

  let summary = mkRollbackSummary (deleteEpochLogs <> blockDeleteLogs) setNullLogs

  -- Step 6: Complete
  endTime <- liftIO getCurrentTime
  let duration = diffUTCTime endTime startTime

  liftIO $ do
    writeIORef progressRef . (\p -> p {rpCurrentStep = 6, rpCurrentPhase = "Complete!"}) =<< readIORef progressRef
    finalProgress <- readIORef progressRef
    renderProgressBar finalProgress
    putStrLn $ "\nRollback completed in " ++ show duration
    logInfo trce summary

  pure deleteBlockCount
  where
    findMinIdsRec :: MonadIO m => IORef RollbackProgress -> [Maybe MinIdsWrapper] -> MinIdsWrapper -> DbAction m (MinIdsWrapper, Bool)
    findMinIdsRec _ [] minIds = pure (minIds, True)
    findMinIdsRec progressRef (mMinIds : rest) minIds =
      case mMinIds of
        Nothing -> do
          liftIO $ putStr "\ESC[A\r\ESC[K" -- Move up one line and clear it
          liftIO $ putStr "Failed to find ReverseIndex. Deletion may take longer."
          liftIO $ putStr "\n"
          liftIO $ renderProgressBar =<< readIORef progressRef
          pure (minIds, False)
        Just minIdDB -> do
          let minIds' = minIds <> minIdDB
          if isComplete minIds'
            then pure (minIds', True)
            else findMinIdsRec progressRef rest minIds'

    isComplete minIdsW = case minIdsW of
      CMinIdsWrapper (MinIds m1 m2 m3) -> isJust m1 && isJust m2 && isJust m3
      VMinIdsWrapper (MinIds m1 m2 m3) -> isJust m1 && isJust m2 && isJust m3

---------------------------------------------------------------------------------

mkRollbackSummary :: [(Text.Text, Int64)] -> (Text.Text, Int64) -> Text.Text
mkRollbackSummary logs setNullLogs =
  "\n----------------------- Rollback Summary: ----------------------- \n"
    <> formattedLog
    <> zeroDeletedEntry
    <> formatSetNullLog setNullLogs
    <> "\n"
  where
    (zeroDeletes, nonZeroDeletes) = partition ((== 0) . snd) logs
    formattedLog = Text.intercalate "\n" (map formatEntry nonZeroDeletes)
    zeroDeletedEntry
      | null zeroDeletes = ""
      | otherwise = "\n\nNo Deletes in tables: " <> Text.intercalate ", " (map fst zeroDeletes)
    formatEntry (tName, rowCount) =
      "Table: " <> tName <> " - Count: " <> Text.pack (show rowCount)
    formatSetNullLog (nullMessage, nullCount) =
      if nullCount == 0
        then "\n\nSet Null: " <> nullMessage
        else "\n\nSet Null: " <> nullMessage <> " - Count: " <> Text.pack (show nullCount)

---------------------------------------------------------------------------------
-- Custom type for holding all the results
data DeleteResults = DeleteResults
  { epochCount :: !Int64
  , drepDistrCount :: !Int64
  , rewardRestCount :: !Int64
  , poolStatCount :: !Int64
  , rewardCount :: !Int64
  , enactedNullCount :: !Int64
  , ratifiedNullCount :: !Int64
  , droppedNullCount :: !Int64
  , expiredNullCount :: !Int64
  }

deleteUsingEpochNo :: MonadIO m => Word64 -> DbAction m [(Text.Text, Int64)]
deleteUsingEpochNo epochN = do
  let dbCallStack = mkDbCallStack "deleteUsingEpochNo"
      epochEncoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)
      epochInt64 = fromIntegral epochN

  -- Execute batch deletes in a pipeline
  results <- runDbSession dbCallStack $
    HsqlSes.pipeline $ do
      c1 <- HsqlPipeL.statement epochN (deleteWhereCount @SC.Epoch "no" "=" epochEncoder)
      c2 <- HsqlPipeL.statement epochN (deleteWhereCount @SC.DrepDistr "epoch_no" ">" epochEncoder)
      c3 <- HsqlPipeL.statement epochN (deleteWhereCount @SC.RewardRest "spendable_epoch" ">" epochEncoder)
      c4 <- HsqlPipeL.statement epochN (deleteWhereCount @SC.PoolStat "epoch_no" ">" epochEncoder)
      c5 <- HsqlPipeL.statement epochN (deleteWhereCount @SC.Reward "spendable_epoch" ">" epochEncoder)

      -- Null operations
      n1 <- HsqlPipeL.statement epochInt64 setNullEnactedStmt
      n2 <- HsqlPipeL.statement epochInt64 setNullRatifiedStmt
      n3 <- HsqlPipeL.statement epochInt64 setNullDroppedStmt
      n4 <- HsqlPipeL.statement epochInt64 setNullExpiredStmt

      pure $ DeleteResults c1 c2 c3 c4 c5 n1 n2 n3 n4

  -- Collect results
  let
    countLogs =
      [ ("Epoch", epochCount results)
      , ("DrepDistr", drepDistrCount results)
      , ("RewardRest", rewardRestCount results)
      , ("PoolStat", poolStatCount results)
      , ("Reward", rewardCount results)
      ]

    nullTotal =
      sum
        [ enactedNullCount results
        , ratifiedNullCount results
        , droppedNullCount results
        , expiredNullCount results
        ]

    nullLogs = [("GovActionProposal Nulled", nullTotal)]
  pure $ countLogs <> nullLogs

--------------------------------------------------------------------------------
deleteBlocksSlotNo ::
  MonadIO m =>
  Trace IO Text.Text ->
  TxOutVariantType ->
  SlotNo ->
  Bool ->
  DbAction m Bool
deleteBlocksSlotNo trce txOutVariantType (SlotNo slotNo) isConsumedTxOut = do
  mBlockEpoch <- queryNearestBlockSlotNo slotNo
  case mBlockEpoch of
    Nothing -> do
      liftIO $ logWarning trce $ "deleteBlocksSlotNo: No block contains the the slot: " <> Text.pack (show slotNo)
      pure False
    Just (blockId, epochN) -> do
      void $ deleteBlocksBlockId trce txOutVariantType blockId epochN isConsumedTxOut
      pure True

--------------------------------------------------------------------------------
deleteBlocksSlotNoNoTrace :: MonadIO m => TxOutVariantType -> SlotNo -> DbAction m Bool
deleteBlocksSlotNoNoTrace txOutVariantType slotNo = deleteBlocksSlotNo nullTracer txOutVariantType slotNo True

--------------------------------------------------------------------------------
deleteBlocksForTests :: MonadIO m => TxOutVariantType -> Id.BlockId -> Word64 -> DbAction m ()
deleteBlocksForTests txOutVariantType blockId epochN = void $ deleteBlocksBlockId nullTracer txOutVariantType blockId epochN False

--------------------------------------------------------------------------------

-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteBlock :: MonadIO m => TxOutVariantType -> SC.Block -> DbAction m Bool
deleteBlock txOutVariantType block = do
  mBlockId <- queryBlockHash block
  case mBlockId of
    Nothing -> pure False
    Just (blockId, epochN) -> do
      void $ deleteBlocksBlockId nullTracer txOutVariantType blockId epochN False
      pure True

--------------------------------------------------------------------------------
-- Datum
--------------------------------------------------------------------------------

-- | INSERT --------------------------------------------------------------------
insertDatumStmt :: HsqlStmt.Statement SCB.Datum Id.DatumId
insertDatumStmt =
  insertCheckUnique
    SCB.datumEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.DatumId)

insertDatum :: MonadIO m => SCB.Datum -> DbAction m Id.DatumId
insertDatum datum =
  runDbSession (mkDbCallStack "insertDatum") $ HsqlSes.statement datum insertDatumStmt

-- | QUERY ---------------------------------------------------------------------
queryDatumStmt :: HsqlStmt.Statement ByteString (Maybe Id.DatumId)
queryDatumStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id"
          , " FROM datum"
          , " WHERE hash = $1"
          ]
    encoder = id >$< HsqlE.param (HsqlE.nonNullable HsqlE.bytea)
    decoder = HsqlD.rowMaybe $ Id.idDecoder Id.DatumId

queryDatum :: MonadIO m => ByteString -> DbAction m (Maybe Id.DatumId)
queryDatum hash =
  runDbSession (mkDbCallStack "queryDatum") $
    HsqlSes.statement hash queryDatumStmt

--------------------------------------------------------------------------------
-- ExtraMigration
--------------------------------------------------------------------------------
queryAllExtraMigrationsStmt :: forall a. DbInfo a => Text.Text -> HsqlStmt.Statement () [ExtraMigration]
queryAllExtraMigrationsStmt colName =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    table = tableName (Proxy @a)
    validCol = validateColumn @a colName

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          ["SELECT ", validCol, " FROM ", table]

    decoder =
      HsqlD.rowList $
        HsqlD.column $
          HsqlD.nonNullable $
            read . Text.unpack <$> HsqlD.text

queryAllExtraMigrations :: MonadIO m => DbAction m [ExtraMigration]
queryAllExtraMigrations =
  runDbSession (mkDbCallStack "queryAllExtraMigrations") $
    HsqlSes.statement () $
      queryAllExtraMigrationsStmt @SC.ExtraMigrations "token"

--------------------------------------------------------------------------------
-- TxMetadata
--------------------------------------------------------------------------------

-- TxMetadata can have a jsonb field which needs to be handled differently
insertBulkTxMetadataStmt :: Bool -> HsqlStmt.Statement [SCB.TxMetadata] [Id.TxMetadataId]
insertBulkTxMetadataStmt removeJsonb =
  insertBulkJsonb
    removeJsonb
    extractTxMetadata
    SCB.txMetadataBulkEncoder
    (WithResultBulk (HsqlD.rowList $ Id.idDecoder Id.TxMetadataId))
  where
    extractTxMetadata :: [SCB.TxMetadata] -> ([DbWord64], [Maybe Text.Text], [ByteString], [Id.TxId])
    extractTxMetadata xs =
      ( map SCB.txMetadataKey xs
      , map SCB.txMetadataJson xs
      , map SCB.txMetadataBytes xs
      , map SCB.txMetadataTxId xs
      )

insertBulkTxMetadata :: MonadIO m => Bool -> [SCB.TxMetadata] -> DbAction m [Id.TxMetadataId]
insertBulkTxMetadata removeJsonb txMetas =
  runDbSession (mkDbCallStack "insertBulkTxMetadata") $
    HsqlSes.statement txMetas (insertBulkTxMetadataStmt removeJsonb)

--------------------------------------------------------------------------------
-- CollateralTxIn
--------------------------------------------------------------------------------
insertCollateralTxInStmt :: HsqlStmt.Statement SCB.CollateralTxIn Id.CollateralTxInId
insertCollateralTxInStmt =
  insert
    SCB.collateralTxInEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.CollateralTxInId)

insertCollateralTxIn :: MonadIO m => SCB.CollateralTxIn -> DbAction m Id.CollateralTxInId
insertCollateralTxIn cTxIn = runDbSession (mkDbCallStack "insertCollateralTxIn") $ HsqlSes.statement cTxIn insertCollateralTxInStmt

--------------------------------------------------------------------------------
-- Meta
--------------------------------------------------------------------------------
queryMetaStmt :: HsqlStmt.Statement () [Entity SCB.Meta]
queryMetaStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    decoder = HsqlD.rowList SCB.entityMetaDecoder
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT *"
          , " FROM meta"
          ]

{-# INLINEABLE queryMeta #-}
queryMeta :: MonadIO m => DbAction m (Maybe SCB.Meta)
queryMeta = do
  let dbCallStack = mkDbCallStack "queryMeta"
  result <- runDbSession dbCallStack $ HsqlSes.statement () queryMetaStmt
  case result of
    [] -> pure Nothing -- Empty table is valid
    [m] -> pure $ Just $ entityVal m
    _otherwise -> throwError $ DbError dbCallStack "Multiple rows in meta table" Nothing

--------------------------------------------------------------------------------
-- ReferenceTxIn
--------------------------------------------------------------------------------
insertReferenceTxInStmt :: HsqlStmt.Statement SCB.ReferenceTxIn Id.ReferenceTxInId
insertReferenceTxInStmt =
  insert
    SCB.referenceTxInEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.ReferenceTxInId)

insertReferenceTxIn :: MonadIO m => SCB.ReferenceTxIn -> DbAction m Id.ReferenceTxInId
insertReferenceTxIn rTxIn = runDbSession (mkDbCallStack "insertReferenceTxIn") $ HsqlSes.statement rTxIn insertReferenceTxInStmt

--------------------------------------------------------------------------------
insertExtraMigrationStmt :: HsqlStmt.Statement SCB.ExtraMigrations ()
insertExtraMigrationStmt =
  insert
    SCB.extraMigrationsEncoder
    NoResult

insertExtraMigration :: MonadIO m => ExtraMigration -> DbAction m ()
insertExtraMigration extraMigration =
  void $ runDbSession (mkDbCallStack "insertExtraMigration") $ HsqlSes.statement input insertExtraMigrationStmt
  where
    input = SCB.ExtraMigrations (textShow extraMigration) (Just $ extraDescription extraMigration)

--------------------------------------------------------------------------------
-- ExtraKeyWitness
--------------------------------------------------------------------------------
insertExtraKeyWitnessStmt :: HsqlStmt.Statement SCB.ExtraKeyWitness Id.ExtraKeyWitnessId
insertExtraKeyWitnessStmt =
  insert
    SCB.extraKeyWitnessEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.ExtraKeyWitnessId)

insertExtraKeyWitness :: MonadIO m => SCB.ExtraKeyWitness -> DbAction m Id.ExtraKeyWitnessId
insertExtraKeyWitness eKeyWitness = runDbSession (mkDbCallStack "insertExtraKeyWitness") $ HsqlSes.statement eKeyWitness insertExtraKeyWitnessStmt

--------------------------------------------------------------------------------
-- Meta
--------------------------------------------------------------------------------
insertMetaStmt :: HsqlStmt.Statement SCB.Meta Id.MetaId
insertMetaStmt =
  insertCheckUnique
    SCB.metaEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.MetaId)

insertMeta :: MonadIO m => SCB.Meta -> DbAction m Id.MetaId
insertMeta meta = runDbSession (mkDbCallStack "insertMeta") $ HsqlSes.statement meta insertMetaStmt

--------------------------------------------------------------------------------
-- Redeemer
--------------------------------------------------------------------------------
insertRedeemerStmt :: HsqlStmt.Statement SCB.Redeemer Id.RedeemerId
insertRedeemerStmt =
  insert
    SCB.redeemerEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.RedeemerId)

insertRedeemer :: MonadIO m => SCB.Redeemer -> DbAction m Id.RedeemerId
insertRedeemer redeemer = runDbSession (mkDbCallStack "insertRedeemer") $ HsqlSes.statement redeemer insertRedeemerStmt

--------------------------------------------------------------------------------
-- RedeemerData
--------------------------------------------------------------------------------
insertRedeemerDataStmt :: HsqlStmt.Statement SCB.RedeemerData Id.RedeemerDataId
insertRedeemerDataStmt =
  insertCheckUnique
    SCB.redeemerDataEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.RedeemerDataId)

insertRedeemerData :: MonadIO m => SCB.RedeemerData -> DbAction m Id.RedeemerDataId
insertRedeemerData redeemerData = runDbSession (mkDbCallStack "insertRedeemerData") $ HsqlSes.statement redeemerData insertRedeemerDataStmt

--------------------------------------------------------------------------------
queryRedeemerDataStmt :: HsqlStmt.Statement ByteString (Maybe Id.RedeemerDataId)
queryRedeemerDataStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id"
          , " FROM redeemer_data"
          , " WHERE hash = $1"
          ]

    encoder = HsqlE.param (HsqlE.nonNullable HsqlE.bytea)
    decoder = HsqlD.rowMaybe (Id.idDecoder Id.RedeemerDataId)

queryRedeemerData :: MonadIO m => ByteString -> DbAction m (Maybe Id.RedeemerDataId)
queryRedeemerData hash =
  runDbSession (mkDbCallStack "queryRedeemerData") $
    HsqlSes.statement hash queryRedeemerDataStmt

--------------------------------------------------------------------------------
-- ReverseIndex
--------------------------------------------------------------------------------
insertReverseIndexStmt :: HsqlStmt.Statement SCB.ReverseIndex Id.ReverseIndexId
insertReverseIndexStmt =
  insert
    SCB.reverseIndexEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.ReverseIndexId)

insertReverseIndex :: MonadIO m => SCB.ReverseIndex -> DbAction m Id.ReverseIndexId
insertReverseIndex reverseIndex = runDbSession (mkDbCallStack "insertReverseIndex") $ HsqlSes.statement reverseIndex insertReverseIndexStmt

--------------------------------------------------------------------------------

-- | SchemaVersion

--------------------------------------------------------------------------------
querySchemaVersionStmt :: HsqlStmt.Statement () (Maybe SCB.SchemaVersion)
querySchemaVersionStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    tableN = tableName (Proxy @SCB.SchemaVersion)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT stage_one, stage_two, stage_three"
          , " FROM " <> tableN
          , " ORDER BY stage_one DESC"
          , " LIMIT 1"
          ]
    decoder = HsqlD.rowMaybe SCB.schemaVersionDecoder

querySchemaVersion :: MonadIO m => DbAction m (Maybe SCB.SchemaVersion)
querySchemaVersion =
  runDbSession (mkDbCallStack "querySchemaVersion") $
    HsqlSes.statement () querySchemaVersionStmt

--------------------------------------------------------------------------------
-- Script
--------------------------------------------------------------------------------

-- | INSERTS
insertScriptStmt :: HsqlStmt.Statement SCB.Script Id.ScriptId
insertScriptStmt =
  insertCheckUnique
    SCB.scriptEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.ScriptId)

insertScript :: MonadIO m => SCB.Script -> DbAction m Id.ScriptId
insertScript script = runDbSession (mkDbCallStack "insertScript") $ HsqlSes.statement script insertScriptStmt

-- | QUERIES

--------------------------------------------------------------------------------
queryScriptWithIdStmt :: HsqlStmt.Statement ByteString (Maybe Id.ScriptId)
queryScriptWithIdStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id"
          , " FROM script"
          , " WHERE hash = $1"
          ]

    encoder = HsqlE.param (HsqlE.nonNullable HsqlE.bytea)
    decoder = HsqlD.rowMaybe (Id.idDecoder Id.ScriptId)

queryScriptWithId :: MonadIO m => ByteString -> DbAction m (Maybe Id.ScriptId)
queryScriptWithId hash =
  runDbSession (mkDbCallStack "queryScriptWithId") $
    HsqlSes.statement hash queryScriptWithIdStmt

--------------------------------------------------------------------------------
-- SlotLeader
--------------------------------------------------------------------------------
insertCheckUniqueSlotLeaderStmt :: HsqlStmt.Statement SCB.SlotLeader Id.SlotLeaderId
insertCheckUniqueSlotLeaderStmt =
  insertCheckUnique
    SCB.slotLeaderEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.SlotLeaderId)

insertSlotLeader :: MonadIO m => SCB.SlotLeader -> DbAction m Id.SlotLeaderId
insertSlotLeader slotLeader = runDbSession (mkDbCallStack "insertSlotLeader") $ HsqlSes.statement slotLeader insertCheckUniqueSlotLeaderStmt

--------------------------------------------------------------------------------
-- TxCbor
--------------------------------------------------------------------------------
insertTxCborStmt :: HsqlStmt.Statement SCB.TxCbor Id.TxCborId
insertTxCborStmt =
  insert
    SCB.txCborEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.TxCborId)

insertTxCbor :: MonadIO m => SCB.TxCbor -> DbAction m Id.TxCborId
insertTxCbor txCBOR =
  runDbSession (mkDbCallStack "insertTxCBOR") $ HsqlSes.statement txCBOR insertTxCborStmt

--------------------------------------------------------------------------------
-- Tx
--------------------------------------------------------------------------------

-- | INSERTS -------------------------------------------------------------------
insertTxStmt :: HsqlStmt.Statement SCB.Tx Id.TxId
insertTxStmt =
  insert
    SCB.txEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.TxId)

insertTx :: MonadIO m => SCB.Tx -> DbAction m Id.TxId
insertTx tx = runDbSession (mkDbCallStack "insertTx") $ HsqlSes.statement tx insertTxStmt

-- | QUERIES ------------------------------------------------------------------

-- | Count the number of transactions in the Tx table.
queryTxCount :: MonadIO m => DbAction m Word64
queryTxCount =
  runDbSession (mkDbCallStack "queryTxCount") $
    HsqlSes.statement () $
      countAll @SCB.Tx

--------------------------------------------------------------------------------
queryWithdrawalsUpToBlockNoStmt :: HsqlStmt.Statement Word64 Ada
queryWithdrawalsUpToBlockNoStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    txTableN = tableName (Proxy @SCB.Tx)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT SUM(withdrawal.amount)"
          , " FROM " <> txTableN
          , " INNER JOIN withdrawal ON tx.id = withdrawal.tx_id"
          , " INNER JOIN block ON tx.block_id = block.id"
          , " WHERE block.block_no <= $1"
          ]
    encoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)
    decoder = HsqlD.singleRow adaSumDecoder

queryWithdrawalsUpToBlockNo :: MonadIO m => Word64 -> DbAction m Ada
queryWithdrawalsUpToBlockNo blkNo =
  runDbSession (mkDbCallStack "queryWithdrawalsUpToBlockNo") $
    HsqlSes.statement blkNo queryWithdrawalsUpToBlockNoStmt

--------------------------------------------------------------------------------
queryTxIdStmt :: HsqlStmt.Statement ByteString (Maybe Id.TxId)
queryTxIdStmt = HsqlStmt.Statement sql encoder decoder True
  where
    table = tableName (Proxy @SCB.Tx)
    encoder = HsqlE.param (HsqlE.nonNullable HsqlE.bytea)
    decoder = HsqlD.rowMaybe (Id.idDecoder Id.TxId)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id"
          , " FROM " <> table
          , " WHERE hash = $1"
          ]

-- | Get the 'TxId' associated with the given hash.
queryTxId :: MonadIO m => ByteString -> DbAction m (Maybe Id.TxId)
queryTxId txHash =
  runDbSession (mkDbCallStack "queryTxId") $
    HsqlSes.statement txHash queryTxIdStmt

--------------------------------------------------------------------------------
queryFeesUpToBlockNoStmt :: HsqlStmt.Statement Word64 Ada
queryFeesUpToBlockNoStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    txTableN = tableName (Proxy @SCB.Tx)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT SUM(tx.fee)"
          , " FROM " <> txTableN
          , " INNER JOIN block ON tx.block_id = block.id"
          , " WHERE block.block_no <= $1"
          ]
    encoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)
    decoder = HsqlD.singleRow adaSumDecoder

queryFeesUpToBlockNo :: MonadIO m => Word64 -> DbAction m Ada
queryFeesUpToBlockNo blkNo =
  runDbSession (mkDbCallStack "queryFeesUpToBlockNo") $
    HsqlSes.statement blkNo queryFeesUpToBlockNoStmt

--------------------------------------------------------------------------------
queryFeesUpToSlotNoStmt :: HsqlStmt.Statement Word64 Ada
queryFeesUpToSlotNoStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    txTableN = tableName (Proxy @SCB.Tx)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT SUM(tx.fee)"
          , " FROM " <> txTableN
          , " INNER JOIN block ON tx.block_id = block.id"
          , " WHERE block.slot_no IS NOT NULL"
          , " AND block.slot_no <= $1"
          ]
    encoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)
    decoder = HsqlD.singleRow adaSumDecoder

queryFeesUpToSlotNo :: MonadIO m => Word64 -> DbAction m Ada
queryFeesUpToSlotNo slotNo =
  runDbSession (mkDbCallStack "queryFeesUpToSlotNo") $
    HsqlSes.statement slotNo queryFeesUpToSlotNoStmt

--------------------------------------------------------------------------------
queryInvalidTxStmt :: HsqlStmt.Statement () [Entity SCB.Tx]
queryInvalidTxStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    txTableN = tableName (Proxy @SCB.Tx)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT *"
          , " FROM " <> txTableN
          , " WHERE valid_contract = FALSE"
          ]
    decoder = HsqlD.rowList SCB.entityTxDecoder

queryInvalidTx :: MonadIO m => DbAction m [SCB.Tx]
queryInvalidTx = do
  result <-
    runDbSession (mkDbCallStack "queryInvalidTx") $
      HsqlSes.statement () queryInvalidTxStmt
  pure $ entityVal <$> result

--------------------------------------------------------------------------------
-- TxIn
--------------------------------------------------------------------------------
insertTxInStmt :: HsqlStmt.Statement SCB.TxIn Id.TxInId
insertTxInStmt =
  insert
    SCB.txInEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.TxInId)

insertTxIn :: MonadIO m => SCB.TxIn -> DbAction m Id.TxInId
insertTxIn txIn = runDbSession (mkDbCallStack "insertTxIn") $ HsqlSes.statement txIn insertTxInStmt

--------------------------------------------------------------------------------
insertBulkTxInStmt :: HsqlStmt.Statement [SCB.TxIn] [Id.TxInId]
insertBulkTxInStmt =
  insertBulk
    extractTxIn
    SCB.encodeTxInBulk
    (WithResultBulk $ HsqlD.rowList $ Id.idDecoder Id.TxInId)
  where
    extractTxIn :: [SCB.TxIn] -> ([Id.TxId], [Id.TxId], [Word64], [Maybe Id.RedeemerId])
    extractTxIn xs =
      ( map SCB.txInTxInId xs
      , map SCB.txInTxOutId xs
      , map SCB.txInTxOutIndex xs
      , map SCB.txInRedeemerId xs
      )

insertBulkTxIn :: MonadIO m => [SCB.TxIn] -> DbAction m [Id.TxInId]
insertBulkTxIn txIns =
  runDbSession (mkDbCallStack "insertBulkTxIn") $
    HsqlSes.statement txIns insertBulkTxInStmt

--------------------------------------------------------------------------------
queryTxInCount :: MonadIO m => DbAction m Word64
queryTxInCount =
  runDbSession (mkDbCallStack "queryTxInCount") $
    HsqlSes.statement () $
      countAll @SCB.TxIn

--------------------------------------------------------------------------------
queryTxInRedeemerStmt :: HsqlStmt.Statement () [SCB.TxIn]
queryTxInRedeemerStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    tableN = tableName (Proxy @SCB.TxIn)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT *"
          , " FROM " <> tableN
          , " WHERE redeemer_id IS NOT NULL"
          ]
    decoder = HsqlD.rowList SCB.txInDecoder

queryTxInRedeemer :: MonadIO m => DbAction m [SCB.TxIn]
queryTxInRedeemer =
  runDbSession (mkDbCallStack "queryTxInRedeemer") $
    HsqlSes.statement () queryTxInRedeemerStmt

--------------------------------------------------------------------------------

-- | Gets all the 'TxIn' of invalid txs
queryTxInFailedTxStmt :: HsqlStmt.Statement () [SCB.TxIn]
queryTxInFailedTxStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    txInTableN = tableName (Proxy @SCB.TxIn)
    txTableN = tableName (Proxy @SCB.Tx)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT tx_in.*"
          , " FROM " <> txInTableN <> " tx_in"
          , " INNER JOIN " <> txTableN <> " tx"
          , " ON tx_in.tx_in_id = tx.id"
          , " WHERE tx.valid_contract = FALSE"
          ]
    decoder = HsqlD.rowList SCB.txInDecoder

queryTxInFailedTx :: MonadIO m => DbAction m [SCB.TxIn]
queryTxInFailedTx =
  runDbSession (mkDbCallStack "queryTxInFailedTx") $
    HsqlSes.statement () queryTxInFailedTxStmt

--------------------------------------------------------------------------------
-- Withdrawal
--------------------------------------------------------------------------------
insertWithdrawalStmt :: HsqlStmt.Statement SCB.Withdrawal Id.WithdrawalId
insertWithdrawalStmt =
  insert
    SCB.withdrawalEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.WithdrawalId)

insertWithdrawal :: MonadIO m => SCB.Withdrawal -> DbAction m Id.WithdrawalId
insertWithdrawal withdrawal = runDbSession (mkDbCallStack "insertWithdrawal") $ HsqlSes.statement withdrawal insertWithdrawalStmt

--------------------------------------------------------------------------------
-- Statement for querying withdrawals with non-null redeemer_id
queryWithdrawalScriptStmt :: HsqlStmt.Statement () [SCB.Withdrawal]
queryWithdrawalScriptStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    tableN = tableName (Proxy @SCB.Withdrawal)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT *"
          , " FROM " <> tableN
          , " WHERE redeemer_id IS NOT NULL"
          ]
    decoder = HsqlD.rowList SCB.withdrawalDecoder

queryWithdrawalScript :: MonadIO m => DbAction m [SCB.Withdrawal]
queryWithdrawalScript =
  runDbSession (mkDbCallStack "queryWithdrawalScript") $
    HsqlSes.statement () queryWithdrawalScriptStmt

--------------------------------------------------------------------------------

-- Get all stake addresses with have seen a withdrawal, and return them in shuffled order.
queryWithdrawalAddressesStmt :: HsqlStmt.Statement () [Id.StakeAddressId]
queryWithdrawalAddressesStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    withdrawalTableN = tableName (Proxy @SCB.Withdrawal)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT DISTINCT addr_id"
          , " FROM " <> withdrawalTableN
          , " ORDER BY addr_id ASC"
          ]

    decoder =
      HsqlD.rowList $
        HsqlD.column (HsqlD.nonNullable (Id.StakeAddressId <$> HsqlD.int8))

queryWithdrawalAddresses :: MonadIO m => DbAction m [Id.StakeAddressId]
queryWithdrawalAddresses =
  runDbSession (mkDbCallStack "queryWithdrawalAddresses") $
    HsqlSes.statement () queryWithdrawalAddressesStmt
