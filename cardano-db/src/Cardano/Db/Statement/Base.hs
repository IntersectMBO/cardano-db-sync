{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.Base where

import Cardano.BM.Data.Trace (Trace)
import Cardano.BM.Trace (logInfo, logWarning, nullTracer)
import Cardano.Ledger.BaseTypes (SlotNo (..))
import Cardano.Prelude (ByteString, Int64, MonadError (..), MonadIO (..), Proxy (..), Word64, textShow, void)
import Data.Functor.Contravariant (Contravariant (..), (>$<))
import Data.List (partition)
import Data.Maybe (isJust)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import Data.Time (UTCTime)
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Pipeline as HsqlPipeL
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

import Cardano.Db.Error (DbError (..))
import qualified Cardano.Db.Schema.Core as SC
import qualified Cardano.Db.Schema.Core.Base as SCB
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Schema.MinIds (MinIds (..), MinIdsWrapper (..), completeMinId, textToMinIds)
import Cardano.Db.Schema.Variants (TxOutVariantType)
import Cardano.Db.Statement.Function.Core (ResultType (..), ResultTypeBulk (..), mkCallInfo, mkCallSite, runDbSession)
import Cardano.Db.Statement.Function.Delete (deleteWhereCount)
import Cardano.Db.Statement.Function.Insert (insert, insertBulk, insertCheckUnique)
import Cardano.Db.Statement.Function.Query (adaSumDecoder, countAll, parameterisedCountWhere, queryMinRefId)
import Cardano.Db.Statement.GovernanceAndVoting (setNullDroppedStmt, setNullEnactedStmt, setNullExpiredStmt, setNullRatifiedStmt)
import Cardano.Db.Statement.Rollback (deleteTablesAfterBlockId)
import Cardano.Db.Statement.Types (DbInfo, Entity (..), tableName, validateColumn)
import Cardano.Db.Statement.Variants.TxOut (querySetNullTxOut)
import Cardano.Db.Types (Ada (..), DbAction, DbCallInfo (..), DbWord64, ExtraMigration, extraDescription)

--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------

-- | INSERT --------------------------------------------------------------------
insertBlockStmt :: HsqlStmt.Statement SCB.Block (Entity SCB.Block)
insertBlockStmt =
  insert
    SCB.blockEncoder
    (WithResult $ HsqlD.singleRow SCB.entityBlockDecoder)

insertBlock :: MonadIO m => SCB.Block -> DbAction m Id.BlockId
insertBlock block = do
  entity <- runDbSession (mkCallInfo "insertBlock") $ HsqlSes.statement block insertBlockStmt
  pure $ entityKey entity

insertCheckUniqueBlockStmt :: HsqlStmt.Statement SCB.Block (Entity SCB.Block)
insertCheckUniqueBlockStmt =
  insertCheckUnique
    SCB.blockEncoder
    (WithResult $ HsqlD.singleRow SCB.entityBlockDecoder)

insertCheckUniqueBlock :: MonadIO m => SCB.Block -> DbAction m Id.BlockId
insertCheckUniqueBlock stakeAddress =
  runDbSession (mkCallInfo "insertCheckUniqueBlock") $ do
    entity <-
      HsqlSes.statement stakeAddress insertCheckUniqueBlockStmt
    pure $ entityKey entity


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
  result <-
    runDbSession (mkCallInfo "queryBlockHashBlockNo") $
      HsqlSes.statement hash queryBlockHashBlockNoStmt
  case result of
    [] -> pure Nothing
    [blockNo] -> pure (Just blockNo)
    results ->
      let callInfo = mkCallSite
          errorMsg =
            "Multiple blocks found with same hash: "
              <> Text.pack (show hash)
              <> " (found "
              <> Text.pack (show $ length results)
              <> ")"
       in throwError $
            DbError
              callInfo
              errorMsg
              Nothing

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
queryBlockCount = runDbSession (mkCallInfo "queryBlockCount") $ HsqlSes.statement () queryBlockCountStmt

--------------------------------------------------------------------------------
querySlotUtcTimeStmt :: HsqlStmt.Statement Word64 (Maybe UTCTime)
querySlotUtcTimeStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder = HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
    decoder = HsqlD.rowMaybe (HsqlD.column (HsqlD.nonNullable HsqlD.timestamptz))
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT time"
          , " FROM block"
          , " WHERE slot_no = $1"
          ]

-- | Calculate the slot time (as UTCTime) for a given slot number.
-- This will fail if the slot is empty.
querySlotUtcTime :: MonadIO m => Word64 -> DbAction m UTCTime
querySlotUtcTime slotNo = do
  result <- runDbSession callInfo $ HsqlSes.statement slotNo querySlotUtcTimeStmt
  case result of
    Just time -> pure time
    Nothing -> throwError $ DbError (dciCallSite callInfo) errorMsg Nothing
  where
    callInfo = mkCallInfo "querySlotUtcTime"
    errorMsg = "slot_no not found with number: " <> Text.pack (show slotNo)

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
  let callInfo = mkCallInfo "queryBlockCountAfterBlockNo"
      stmt =
        if queryEq
          then queryBlockCountAfterEqBlockNoStmt
          else queryBlockCountAfterBlockNoStmt
  runDbSession callInfo $ HsqlSes.statement blockNo stmt

--------------------------------------------------------------------------------
queryBlockNoStmt ::
  forall a.
  (DbInfo a) =>
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
  runDbSession (mkCallInfo "queryBlockNo") $
    HsqlSes.statement blkNo $
      queryBlockNoStmt @SCB.Block

--------------------------------------------------------------------------------
queryBlockNoAndEpochStmt ::
  forall a.
  (DbInfo a) =>
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
  runDbSession (mkCallInfo "queryBlockNoAndEpoch") $
    HsqlSes.statement blkNo $
      queryBlockNoAndEpochStmt @SCB.Block

--------------------------------------------------------------------------------
queryNearestBlockSlotNoStmt ::
  forall a.
  (DbInfo a) =>
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
  runDbSession (mkCallInfo "queryNearestBlockSlotNo") $
    HsqlSes.statement slotNo $
      queryNearestBlockSlotNoStmt @SCB.Block

--------------------------------------------------------------------------------
queryBlockHashStmt ::
  forall a.
  (DbInfo a) =>
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
  runDbSession (mkCallInfo "queryBlockHash") $
    HsqlSes.statement (SCB.blockHash block) $
      queryBlockHashStmt @SCB.Block

--------------------------------------------------------------------------------
queryMinBlockStmt ::
  forall a.
  (DbInfo a) =>
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
      blockNo <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
      pure (blockId, blockNo)

queryMinBlock :: MonadIO m => DbAction m (Maybe (Id.BlockId, Word64))
queryMinBlock =
  runDbSession (mkCallInfo "queryMinBlock") $
    HsqlSes.statement () $
      queryMinBlockStmt @SCB.Block

--------------------------------------------------------------------------------
queryReverseIndexBlockIdStmt ::
  forall a.
  (DbInfo a) =>
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
  runDbSession (mkCallInfo "queryReverseIndexBlockId") $
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
  runDbSession (mkCallInfo "queryMinIdsAfterReverseIndex") $
    HsqlSes.statement rollbackId queryMinIdsAfterReverseIndexStmt

--------------------------------------------------------------------------------

-- | Get the number of transactions in the specified block.
queryBlockTxCountStmt :: HsqlStmt.Statement Id.BlockId Word64
queryBlockTxCountStmt =
  parameterisedCountWhere @SCB.Tx "block_id" "= $1" (Id.idEncoder Id.getBlockId)

queryBlockTxCount :: MonadIO m => Id.BlockId -> DbAction m Word64
queryBlockTxCount blkId =
  runDbSession (mkCallInfo "queryBlockTxCount") $
    HsqlSes.statement blkId queryBlockTxCountStmt

--------------------------------------------------------------------------------
queryBlockIdStmt :: HsqlStmt.Statement ByteString (Maybe Id.BlockId)
queryBlockIdStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder = HsqlE.param (HsqlE.nonNullable HsqlE.bytea)
    decoder = HsqlD.rowMaybe (Id.idDecoder Id.BlockId)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id"
          , " FROM block"
          , " WHERE hash = $1"
          ]

queryBlockId :: MonadIO m => ByteString -> DbAction m (Maybe Id.BlockId)
queryBlockId hash = do
  runDbSession callInfo $ HsqlSes.statement hash queryBlockIdStmt
  where
    callInfo = mkCallInfo "queryBlockId"

--------------------------------------------------------------------------------
queryBlocksForCurrentEpochNoStmt :: HsqlStmt.Statement () (Maybe Word64)
queryBlocksForCurrentEpochNoStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT MAX(epoch_no)"
          , " FROM block"
          ]

    decoder =
      HsqlD.singleRow $
        HsqlD.column (HsqlD.nullable $ fromIntegral <$> HsqlD.int8)

queryBlocksForCurrentEpochNo :: MonadIO m => DbAction m (Maybe Word64)
queryBlocksForCurrentEpochNo =
  runDbSession (mkCallInfo "queryBlocksForCurrentEpochNo") $
    HsqlSes.statement () queryBlocksForCurrentEpochNoStmt

--------------------------------------------------------------------------------
queryLatestBlockStmt :: HsqlStmt.Statement () (Maybe SCB.Block)
queryLatestBlockStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT *"
          , " FROM block"
          , " WHERE slot_no IS NOT NULL"
          , " ORDER BY slot_no DESC"
          , " LIMIT 1"
          ]
    decoder = HsqlD.rowMaybe SCB.blockDecoder

queryLatestBlock :: MonadIO m => DbAction m (Maybe SCB.Block)
queryLatestBlock =
  runDbSession (mkCallInfo "queryLatestBlock") $
    HsqlSes.statement () queryLatestBlockStmt

--------------------------------------------------------------------------------
queryLatestEpochNoFromBlockStmt :: HsqlStmt.Statement () Word64
queryLatestEpochNoFromBlockStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COALESCE(epoch_no, 0)::bigint"
          , " FROM block"
          , " WHERE slot_no IS NOT NULL"
          , " ORDER BY epoch_no DESC"
          , " LIMIT 1"
          ]

    decoder =
      HsqlD.singleRow $
        fromIntegral <$> HsqlD.column (HsqlD.nonNullable HsqlD.int8)

queryLatestEpochNoFromBlock :: MonadIO m => DbAction m Word64
queryLatestEpochNoFromBlock =
  runDbSession (mkCallInfo "queryLatestEpochNoFromBlock") $
    HsqlSes.statement () queryLatestEpochNoFromBlockStmt

--------------------------------------------------------------------------------
queryLatestBlockIdStmt :: HsqlStmt.Statement () (Maybe Id.BlockId)
queryLatestBlockIdStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    decoder = HsqlD.rowMaybe (Id.idDecoder Id.BlockId)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id"
          , " FROM block"
          , " ORDER BY slot_no DESC"
          , " LIMIT 1"
          ]

-- | Get 'BlockId' of the latest block.
queryLatestBlockId :: MonadIO m => DbAction m (Maybe Id.BlockId)
queryLatestBlockId =
  runDbSession (mkCallInfo "queryLatestBlockId") $
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
  runDbSession (mkCallInfo "queryDepositUpToBlockNo") $
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
  runDbSession (mkCallInfo "queryLatestSlotNo") $
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
  runDbSession (mkCallInfo "queryLatestPoints") $
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
    runDbSession (mkCallInfo "querySlotHash") $
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
  runDbSession (mkCallInfo "queryCountSlotNosGreaterThan") $
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
  runDbSession (mkCallInfo "queryCountSlotNo") $
    HsqlSes.statement () queryCountSlotNoStmt

-----------------------------------------------------------------------------------
queryBlockHeightStmt :: forall a. (DbInfo a) => Text.Text -> HsqlStmt.Statement () (Maybe Word64)
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
  runDbSession (mkCallInfo "queryBlockHeight") $
    HsqlSes.statement () $
      queryBlockHeightStmt @SC.Block "block_no"

-----------------------------------------------------------------------------------
queryGenesisStmt :: HsqlStmt.Statement () [Id.BlockId]
queryGenesisStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    decoder = HsqlD.rowList (Id.idDecoder Id.BlockId)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id"
          , " FROM block"
          , " WHERE previous_id IS NULL"
          ]

queryGenesis :: MonadIO m => DbAction m Id.BlockId
queryGenesis = do
  let callInfo = mkCallInfo "queryGenesis"
      errorMsg = "Multiple Genesis blocks found"

  result <- runDbSession callInfo $ HsqlSes.statement () queryGenesisStmt
  case result of
    [blk] -> pure blk
    _otherwise -> throwError $ DbError (dciCallSite callInfo) errorMsg Nothing

-----------------------------------------------------------------------------------
queryLatestBlockNoStmt :: HsqlStmt.Statement () (Maybe Word64)
queryLatestBlockNoStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT block_no"
          , " FROM block"
          , " WHERE block_no IS NOT NULL"
          , " ORDER BY block_no DESC"
          , " LIMIT 1"
          ]

    decoder = HsqlD.rowMaybe $ do
      blockNo <- HsqlD.column (HsqlD.nonNullable HsqlD.int8)
      pure $ fromIntegral blockNo

queryLatestBlockNo :: MonadIO m => DbAction m (Maybe Word64)
queryLatestBlockNo =
  runDbSession (mkCallInfo "queryLatestBlockNo") $
    HsqlSes.statement () queryLatestBlockNoStmt

-----------------------------------------------------------------------------------
querySlotNosGreaterThanStmt :: HsqlStmt.Statement Word64 [SlotNo]
querySlotNosGreaterThanStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT slot_no"
          , " FROM block"
          , " WHERE slot_no > $1"
          , " ORDER BY slot_no DESC"
          ]
    encoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)
    decoder = HsqlD.rowList $ do
      slotValue <- HsqlD.column (HsqlD.nonNullable HsqlD.int8)
      pure $ SlotNo (fromIntegral slotValue)

querySlotNosGreaterThan :: MonadIO m => Word64 -> DbAction m [SlotNo]
querySlotNosGreaterThan slotNo =
  runDbSession (mkCallInfo "querySlotNosGreaterThan") $
    HsqlSes.statement slotNo querySlotNosGreaterThanStmt

-----------------------------------------------------------------------------------

-- | Like 'querySlotNosGreaterThan', but returns all slots in the same order.
querySlotNosStmt :: HsqlStmt.Statement () [SlotNo]
querySlotNosStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT slot_no"
          , " FROM block"
          , " WHERE slot_no IS NOT NULL"
          , " ORDER BY slot_no DESC"
          ]
    decoder = HsqlD.rowList $ do
      slotValue <- HsqlD.column (HsqlD.nonNullable HsqlD.int8)
      pure $ SlotNo (fromIntegral slotValue)

querySlotNos :: MonadIO m => DbAction m [SlotNo]
querySlotNos =
  runDbSession (mkCallInfo "querySlotNos") $
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
  runDbSession (mkCallInfo "queryPreviousSlotNo") $
    HsqlSes.statement slotNo queryPreviousSlotNoStmt

-- | DELETE --------------------------------------------------------------------
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

deleteBlocksBlockId ::
  MonadIO m =>
  Trace IO Text.Text ->
  TxOutVariantType ->
  Id.BlockId ->
  -- | The 'EpochNo' of the block to delete.
  Word64 ->
  -- | Is ConsumeTxout
  Bool ->
  DbAction m Int64
deleteBlocksBlockId trce txOutVariantType blockId epochN isConsumedTxOut = do
  mMinIds <- fmap (textToMinIds txOutVariantType =<<) <$> queryReverseIndexBlockId blockId
  (cminIds, completed) <- findMinIdsRec mMinIds mempty
  mTxId <-
    queryMinRefId @SCB.Tx
      "block_id"
      blockId
      (Id.idEncoder Id.getBlockId)
      (Id.idDecoder Id.TxId)
  minIds <- if completed then pure cminIds else completeMinId mTxId cminIds

  deleteEpochLogs <- deleteUsingEpochNo epochN
  (deleteBlockCount, blockDeleteLogs) <- deleteTablesAfterBlockId txOutVariantType blockId mTxId minIds

  setNullLogs <-
    if isConsumedTxOut
      then querySetNullTxOut txOutVariantType mTxId
      else pure ("ConsumedTxOut is not active so no Nulls set", 0)

  -- log all the deleted rows in the rollback
  liftIO $ logInfo trce $ mkRollbackSummary (deleteEpochLogs <> blockDeleteLogs) setNullLogs
  pure deleteBlockCount
  where
    findMinIdsRec :: MonadIO m => [Maybe MinIdsWrapper] -> MinIdsWrapper -> DbAction m (MinIdsWrapper, Bool)
    findMinIdsRec [] minIds = pure (minIds, True)
    findMinIdsRec (mMinIds : rest) minIds =
      case mMinIds of
        Nothing -> do
          liftIO $
            logWarning
              trce
              "Failed to find ReverseIndex. Deletion may take longer."
          pure (minIds, False)
        Just minIdDB -> do
          let minIds' = minIds <> minIdDB
          if isComplete minIds'
            then pure (minIds', True)
            else findMinIdsRec rest minIds'

    isComplete minIdsW = case minIdsW of
      CMinIdsWrapper (MinIds m1 m2 m3) -> isJust m1 && isJust m2 && isJust m3
      VMinIdsWrapper (MinIds m1 m2 m3) -> isJust m1 && isJust m2 && isJust m3

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
  , enactedNullCount :: !Int64
  , ratifiedNullCount :: !Int64
  , droppedNullCount :: !Int64
  , expiredNullCount :: !Int64
  }

deleteUsingEpochNo :: (MonadIO m) => Word64 -> DbAction m [(Text.Text, Int64)]
deleteUsingEpochNo epochN = do
  let callInfo = mkCallInfo "deleteUsingEpochNo"
      epochEncoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)
      epochInt64 = fromIntegral epochN

  -- Execute batch deletes in a pipeline
  results <- runDbSession callInfo $
    HsqlSes.pipeline $ do
      c1 <- HsqlPipeL.statement epochN (deleteWhereCount @SC.Epoch "no" "=" epochEncoder)
      c2 <- HsqlPipeL.statement epochN (deleteWhereCount @SC.DrepDistr "epoch_no" ">" epochEncoder)
      c3 <- HsqlPipeL.statement epochN (deleteWhereCount @SC.RewardRest "spendable_epoch" ">" epochEncoder)
      c4 <- HsqlPipeL.statement epochN (deleteWhereCount @SC.PoolStat "epoch_no" ">" epochEncoder)

      -- Null operations
      n1 <- HsqlPipeL.statement epochInt64 setNullEnactedStmt
      n2 <- HsqlPipeL.statement epochInt64 setNullRatifiedStmt
      n3 <- HsqlPipeL.statement epochInt64 setNullDroppedStmt
      n4 <- HsqlPipeL.statement epochInt64 setNullExpiredStmt

      pure $ DeleteResults c1 c2 c3 c4 n1 n2 n3 n4

  -- Collect results
  let
    countLogs =
      [ ("Epoch", epochCount results)
      , ("DrepDistr", drepDistrCount results)
      , ("RewardRest", rewardRestCount results)
      , ("PoolStat", poolStatCount results)
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
deleteBlocksForTests txOutVariantType blockId epochN = do
  void $ deleteBlocksBlockId nullTracer txOutVariantType blockId epochN False

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
insertDatumStmt :: HsqlStmt.Statement SCB.Datum (Entity SCB.Datum)
insertDatumStmt =
  insert
    SCB.datumEncoder
    (WithResult $ HsqlD.singleRow SCB.entityDatumDecoder)

insertDatum :: MonadIO m => SCB.Datum -> DbAction m Id.DatumId
insertDatum datum = do
  entity <- runDbSession (mkCallInfo "insertDatum") $ HsqlSes.statement datum insertDatumStmt
  pure $ entityKey entity

-- | QUERY ---------------------------------------------------------------------

queryDatumStmt :: HsqlStmt.Statement ByteString (Maybe Id.DatumId)
queryDatumStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql = TextEnc.encodeUtf8 $ Text.concat
      [ "SELECT id"
      , " FROM datum"
      , " WHERE hash = $1"
      ]
    encoder = id >$< HsqlE.param (HsqlE.nonNullable HsqlE.bytea)
    decoder = HsqlD.rowMaybe $ Id.idDecoder Id.DatumId

queryDatum :: MonadIO m => ByteString -> DbAction m (Maybe Id.DatumId)
queryDatum hash =
  runDbSession (mkCallInfo "queryDatum") $
    HsqlSes.statement hash queryDatumStmt

--------------------------------------------------------------------------------
-- ExtraMigration
--------------------------------------------------------------------------------
queryAllExtraMigrationsStmt :: forall a. (DbInfo a) => Text.Text -> HsqlStmt.Statement () [ExtraMigration]
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
  runDbSession (mkCallInfo "queryAllExtraMigrations") $
    HsqlSes.statement () $
      queryAllExtraMigrationsStmt @SC.ExtraMigrations "token"

--------------------------------------------------------------------------------
-- TxMetadata
--------------------------------------------------------------------------------
insertBulkTxMetadataStmt :: HsqlStmt.Statement [SCB.TxMetadata] [Entity SCB.TxMetadata]
insertBulkTxMetadataStmt =
  insertBulk
    extractTxMetadata
    SCB.txMetadataBulkEncoder
    (WithResultBulk (HsqlD.rowList SCB.entityTxMetadataDecoder))
  where
    extractTxMetadata :: [SCB.TxMetadata] -> ([DbWord64], [Maybe Text.Text], [ByteString], [Id.TxId])
    extractTxMetadata xs =
      ( map SCB.txMetadataKey xs
      , map SCB.txMetadataJson xs
      , map SCB.txMetadataBytes xs
      , map SCB.txMetadataTxId xs
      )

insertBulkTxMetadata :: MonadIO m => [SCB.TxMetadata] -> DbAction m [Id.TxMetadataId]
insertBulkTxMetadata txMetas = do
  entities <-
    runDbSession (mkCallInfo "insertBulkTxMetadata") $
      HsqlSes.statement txMetas insertBulkTxMetadataStmt
  pure $ map entityKey entities

--------------------------------------------------------------------------------
-- CollateralTxIn
--------------------------------------------------------------------------------
insertCollateralTxInStmt :: HsqlStmt.Statement SCB.CollateralTxIn (Entity SCB.CollateralTxIn)
insertCollateralTxInStmt =
  insert
    SCB.collateralTxInEncoder
    (WithResult $ HsqlD.singleRow SCB.entityCollateralTxInDecoder)

insertCollateralTxIn :: MonadIO m => SCB.CollateralTxIn -> DbAction m Id.CollateralTxInId
insertCollateralTxIn cTxIn = do
  entity <- runDbSession (mkCallInfo "insertCollateralTxIn") $ HsqlSes.statement cTxIn insertCollateralTxInStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- Meta
--------------------------------------------------------------------------------
queryMetaStmt :: HsqlStmt.Statement () [SCB.Meta]
queryMetaStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    decoder = HsqlD.rowList SCB.metaDecoder
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT *"
          , " FROM meta"
          ]

{-# INLINEABLE queryMeta #-}
queryMeta :: MonadIO m => DbAction m SCB.Meta
queryMeta = do
  let callInfo = mkCallInfo "queryMeta"
  result <- runDbSession callInfo $ HsqlSes.statement () queryMetaStmt
  case result of
    [] -> throwError $ DbError (dciCallSite callInfo) "Meta table is empty" Nothing
    [m] -> pure m
    _otherwise -> throwError $ DbError (dciCallSite callInfo) "Multiple rows in meta table" Nothing

--------------------------------------------------------------------------------
-- ReferenceTxIn
--------------------------------------------------------------------------------
insertReferenceTxInStmt :: HsqlStmt.Statement SCB.ReferenceTxIn (Entity SCB.ReferenceTxIn)
insertReferenceTxInStmt =
  insert
    SCB.referenceTxInEncoder
    (WithResult $ HsqlD.singleRow SCB.entityReferenceTxInDecoder)

insertReferenceTxIn :: MonadIO m => SCB.ReferenceTxIn -> DbAction m Id.ReferenceTxInId
insertReferenceTxIn rTxIn = do
  entity <- runDbSession (mkCallInfo "insertReferenceTxIn") $ HsqlSes.statement rTxIn insertReferenceTxInStmt
  pure (entityKey entity)

--------------------------------------------------------------------------------
insertExtraMigrationStmt :: HsqlStmt.Statement SCB.ExtraMigrations ()
insertExtraMigrationStmt =
  insert
    SCB.extraMigrationsEncoder
    NoResult

insertExtraMigration :: MonadIO m => ExtraMigration -> DbAction m ()
insertExtraMigration extraMigration =
  void $ runDbSession (mkCallInfo "insertExtraMigration") $ HsqlSes.statement input insertExtraMigrationStmt
  where
    input = SCB.ExtraMigrations (textShow extraMigration) (Just $ extraDescription extraMigration)

--------------------------------------------------------------------------------
-- ExtraKeyWitness
--------------------------------------------------------------------------------
insertExtraKeyWitnessStmt :: HsqlStmt.Statement SCB.ExtraKeyWitness (Entity SCB.ExtraKeyWitness)
insertExtraKeyWitnessStmt =
  insert
    SCB.extraKeyWitnessEncoder
    (WithResult $ HsqlD.singleRow SCB.entityExtraKeyWitnessDecoder)

insertExtraKeyWitness :: MonadIO m => SCB.ExtraKeyWitness -> DbAction m Id.ExtraKeyWitnessId
insertExtraKeyWitness eKeyWitness = do
  entity <- runDbSession (mkCallInfo "insertExtraKeyWitness") $ HsqlSes.statement eKeyWitness insertExtraKeyWitnessStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- Meta
--------------------------------------------------------------------------------
insertMetaStmt :: HsqlStmt.Statement SCB.Meta (Entity SCB.Meta)
insertMetaStmt =
  insert
    SCB.metaEncoder
    (WithResult $ HsqlD.singleRow SCB.entityMetaDecoder)

insertMeta :: MonadIO m => SCB.Meta -> DbAction m Id.MetaId
insertMeta meta = do
  entity <- runDbSession (mkCallInfo "insertMeta") $ HsqlSes.statement meta insertMetaStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- Redeemer
--------------------------------------------------------------------------------
insertRedeemerStmt :: HsqlStmt.Statement SCB.Redeemer (Entity SCB.Redeemer)
insertRedeemerStmt =
  insert
    SCB.redeemerEncoder
    (WithResult $ HsqlD.singleRow SCB.entityRedeemerDecoder)

insertRedeemer :: MonadIO m => SCB.Redeemer -> DbAction m Id.RedeemerId
insertRedeemer redeemer = do
  entity <- runDbSession (mkCallInfo "insertRedeemer") $ HsqlSes.statement redeemer insertRedeemerStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- RedeemerData
--------------------------------------------------------------------------------
insertRedeemerDataStmt :: HsqlStmt.Statement SCB.RedeemerData (Entity SCB.RedeemerData)
insertRedeemerDataStmt =
  insert
    SCB.redeemerDataEncoder
    (WithResult $ HsqlD.singleRow SCB.entityRedeemerDataDecoder)

insertRedeemerData :: MonadIO m => SCB.RedeemerData -> DbAction m Id.RedeemerDataId
insertRedeemerData redeemerData = do
  entity <- runDbSession (mkCallInfo "insertRedeemerData") $ HsqlSes.statement redeemerData insertRedeemerDataStmt
  pure $ entityKey entity

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
  runDbSession (mkCallInfo "queryRedeemerData") $
    HsqlSes.statement hash queryRedeemerDataStmt

--------------------------------------------------------------------------------
-- ReverseIndex
--------------------------------------------------------------------------------
insertReverseIndexStmt :: HsqlStmt.Statement SCB.ReverseIndex (Entity SCB.ReverseIndex)
insertReverseIndexStmt =
  insert
    SCB.reverseIndexEncoder
    (WithResult $ HsqlD.singleRow SCB.entityReverseIndexDecoder)

insertReverseIndex :: MonadIO m => SCB.ReverseIndex -> DbAction m Id.ReverseIndexId
insertReverseIndex reverseIndex = do
  entity <- runDbSession (mkCallInfo "insertReverseIndex") $ HsqlSes.statement reverseIndex insertReverseIndexStmt
  pure $ entityKey entity

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
  runDbSession (mkCallInfo "querySchemaVersion") $
    HsqlSes.statement () querySchemaVersionStmt

--------------------------------------------------------------------------------
-- Script
--------------------------------------------------------------------------------

-- | INSERTS
insertScriptStmt :: HsqlStmt.Statement SCB.Script (Entity SCB.Script)
insertScriptStmt =
  insert
    SCB.scriptEncoder
    (WithResult $ HsqlD.singleRow SCB.entityScriptDecoder)

insertScript :: MonadIO m => SCB.Script -> DbAction m Id.ScriptId
insertScript script = do
  entity <- runDbSession (mkCallInfo "insertScript") $ HsqlSes.statement script insertScriptStmt
  pure $ entityKey entity

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
  runDbSession (mkCallInfo "queryScriptWithId") $
    HsqlSes.statement hash queryScriptWithIdStmt

--------------------------------------------------------------------------------
-- SlotLeader
--------------------------------------------------------------------------------
insertSlotLeaderStmt :: HsqlStmt.Statement SCB.SlotLeader (Entity SCB.SlotLeader)
insertSlotLeaderStmt =
  insert
    SCB.slotLeaderEncoder
    (WithResult $ HsqlD.singleRow SCB.entitySlotLeaderDecoder)

insertSlotLeader :: MonadIO m => SCB.SlotLeader -> DbAction m Id.SlotLeaderId
insertSlotLeader slotLeader = do
  entity <- runDbSession (mkCallInfo "insertSlotLeader") $ HsqlSes.statement slotLeader insertSlotLeaderStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
insertTxCborStmt :: HsqlStmt.Statement SCB.TxCbor (Entity SCB.TxCbor)
insertTxCborStmt =
  insert
    SCB.txCborEncoder
    (WithResult $ HsqlD.singleRow SCB.entityTxCborDecoder)

insertTxCbor :: MonadIO m => SCB.TxCbor -> DbAction m Id.TxCborId
insertTxCbor txCBOR = do
  entity <- runDbSession (mkCallInfo "insertTxCBOR") $ HsqlSes.statement txCBOR insertTxCborStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- Tx
--------------------------------------------------------------------------------

-- | INSERTS -------------------------------------------------------------------
insertTxStmt :: HsqlStmt.Statement SCB.Tx (Entity SCB.Tx)
insertTxStmt =
  insert
    SCB.txEncoder
    (WithResult $ HsqlD.singleRow SCB.entityTxDecoder)

insertTx :: MonadIO m => SCB.Tx -> DbAction m Id.TxId
insertTx tx = do
  entity <- runDbSession (mkCallInfo "insertTx") $ HsqlSes.statement tx insertTxStmt
  pure $ entityKey entity

-- | QUERIES ------------------------------------------------------------------

-- | Count the number of transactions in the Tx table.
queryTxCount :: MonadIO m => DbAction m Word64
queryTxCount =
  runDbSession (mkCallInfo "queryTxCount") $
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
  runDbSession (mkCallInfo "queryWithdrawalsUpToBlockNo") $
    HsqlSes.statement blkNo queryWithdrawalsUpToBlockNoStmt

--------------------------------------------------------------------------------
queryTxIdStmt :: HsqlStmt.Statement ByteString (Maybe Id.TxId)
queryTxIdStmt = do
  HsqlStmt.Statement sql encoder decoder True
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
queryTxId :: MonadIO m => ByteString -> DbAction m Id.TxId
queryTxId hash = do
  result <- runDbSession callInfo $ HsqlSes.statement hash queryTxIdStmt
  case result of
    Just res -> pure res
    Nothing -> throwError $ DbError (dciCallSite callInfo) errorMsg Nothing
  where
    callInfo = mkCallInfo "queryTxId"
    errorMsg = "Transaction not found with hash: " <> Text.pack (show hash)

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
  runDbSession (mkCallInfo "queryFeesUpToBlockNo") $
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
  runDbSession (mkCallInfo "queryFeesUpToSlotNo") $
    HsqlSes.statement slotNo queryFeesUpToSlotNoStmt

--------------------------------------------------------------------------------
queryInvalidTxStmt :: HsqlStmt.Statement () [SCB.Tx]
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
    decoder = HsqlD.rowList SCB.txDecoder

queryInvalidTx :: MonadIO m => DbAction m [SCB.Tx]
queryInvalidTx =
  runDbSession (mkCallInfo "queryInvalidTx") $
    HsqlSes.statement () queryInvalidTxStmt

--------------------------------------------------------------------------------
-- TxIn
--------------------------------------------------------------------------------
insertTxInStmt :: HsqlStmt.Statement SCB.TxIn (Entity SCB.TxIn)
insertTxInStmt =
  insert
    SCB.txInEncoder
    (WithResult $ HsqlD.singleRow SCB.entityTxInDecoder)

insertTxIn :: MonadIO m => SCB.TxIn -> DbAction m Id.TxInId
insertTxIn txIn = do
  entity <- runDbSession (mkCallInfo "insertTxIn") $ HsqlSes.statement txIn insertTxInStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
insertBulkTxInStmt :: HsqlStmt.Statement [SCB.TxIn] [Entity SCB.TxIn]
insertBulkTxInStmt =
  insertBulk
    extractTxIn
    SCB.encodeTxInBulk
    (WithResultBulk $ HsqlD.rowList SCB.entityTxInDecoder)
  where
    extractTxIn :: [SCB.TxIn] -> ([Id.TxId], [Id.TxId], [Word64], [Maybe Id.RedeemerId])
    extractTxIn xs =
      ( map SCB.txInTxInId xs
      , map SCB.txInTxOutId xs
      , map SCB.txInTxOutIndex xs
      , map SCB.txInRedeemerId xs
      )

insertBulkTxIn :: MonadIO m => [SCB.TxIn] -> DbAction m [Id.TxInId]
insertBulkTxIn txIns = do
  entities <-
    runDbSession (mkCallInfo "insertBulkTxIn") $
      HsqlSes.statement txIns insertBulkTxInStmt
  pure $ map entityKey entities

--------------------------------------------------------------------------------
queryTxInCount :: MonadIO m => DbAction m Word64
queryTxInCount =
  runDbSession (mkCallInfo "queryTxInCount") $
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
  runDbSession (mkCallInfo "queryTxInRedeemer") $
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
  runDbSession (mkCallInfo "queryTxInFailedTx") $
    HsqlSes.statement () queryTxInFailedTxStmt

--------------------------------------------------------------------------------
-- Withdrawal
--------------------------------------------------------------------------------
insertWithdrawalStmt :: HsqlStmt.Statement SCB.Withdrawal (Entity SCB.Withdrawal)
insertWithdrawalStmt =
  insert
    SCB.withdrawalEncoder
    (WithResult $ HsqlD.singleRow SCB.entityWithdrawalDecoder)

insertWithdrawal :: MonadIO m => SCB.Withdrawal -> DbAction m Id.WithdrawalId
insertWithdrawal withdrawal = do
  entity <- runDbSession (mkCallInfo "insertWithdrawal") $ HsqlSes.statement withdrawal insertWithdrawalStmt
  pure $ entityKey entity

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
  runDbSession (mkCallInfo "queryWithdrawalScript") $
    HsqlSes.statement () queryWithdrawalScriptStmt

--------------------------------------------------------------------------------

-- Get all stake addresses with have seen a withdrawal, and return them in shuffled order.
queryWithdrawalAddressesStmt :: HsqlStmt.Statement () [Id.StakeAddressId]
queryWithdrawalAddressesStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    withdrawalTableN = tableName (Proxy @SCB.Withdrawal)
    sql = TextEnc.encodeUtf8 $ Text.concat
      [ "SELECT DISTINCT addr_id"
      , " FROM " <> withdrawalTableN
      , " ORDER BY addr_id ASC"
      ]

    decoder = HsqlD.rowList $
      HsqlD.column (HsqlD.nonNullable (Id.StakeAddressId <$> HsqlD.int8))

queryWithdrawalAddresses :: MonadIO m => DbAction m [Id.StakeAddressId]
queryWithdrawalAddresses =
  runDbSession (mkCallInfo "queryWithdrawalAddresses") $
    HsqlSes.statement () queryWithdrawalAddressesStmt



-- These tables store fundamental blockchain data, such as blocks, transactions, and UTXOs.

-- block
-- collateral_tx_in
-- collateral_tx_out
-- datum
-- extra_key_witness
-- metaa
-- redeemer
-- redeemer_data
-- reference_tx_in
-- reverse_index
-- script
-- slot_leader
-- tx
-- tx_cbor
-- tx_in
-- tx_out
-- utxo_byron_view
-- utxo_view
