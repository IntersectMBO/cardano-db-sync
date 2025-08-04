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
import Cardano.Prelude (ByteString, Int64, MonadIO (..), Proxy (..), Word64, for, textShow, void)
import Data.Functor.Contravariant ((>$<))
import Data.List (partition)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import Data.Time (UTCTime)
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Pipeline as HsqlP
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

import Cardano.Db.Error (DbError (..))
import Cardano.Db.Progress (ProgressRef, updateProgress, withProgress)
import qualified Cardano.Db.Schema.Core as SC
import qualified Cardano.Db.Schema.Core.Base as SCB
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Schema.MinIds (MinIds (..), MinIdsWrapper (..), textToMinIds)
import Cardano.Db.Schema.Types (utcTimeAsTimestampDecoder)
import Cardano.Db.Schema.Variants (TxOutVariantType)
import Cardano.Db.Statement.Function.Core (ResultType (..), ResultTypeBulk (..), runSession, runSessionEntity)
import Cardano.Db.Statement.Function.Delete (deleteWhereCount)
import Cardano.Db.Statement.Function.Insert (insert, insertCheckUnique)
import Cardano.Db.Statement.Function.InsertBulk (insertBulk, insertBulkJsonb)
import Cardano.Db.Statement.Function.Query (adaSumDecoder, countAll, parameterisedCountWhere)
import Cardano.Db.Statement.GovernanceAndVoting (setNullDroppedStmt, setNullEnactedStmt, setNullExpiredStmt, setNullRatifiedStmt)
import Cardano.Db.Statement.MinIds (completeMinId, queryMinRefId)
import Cardano.Db.Statement.Rollback (deleteTablesAfterBlockId)
import Cardano.Db.Statement.Types (DbInfo, Entity (..), tableName, validateColumn)
import Cardano.Db.Statement.Variants.TxOut (querySetNullTxOut)
import Cardano.Db.Types (Ada (..), DbM, DbWord64, ExtraMigration, extraDescription)

--------------------------------------------------------------------------------
-- Block
--------------------------------------------------------------------------------

-- | INSERT --------------------------------------------------------------------
insertBlockStmt :: HsqlStmt.Statement SCB.Block Id.BlockId
insertBlockStmt =
  insert
    SCB.blockEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.BlockId)

insertBlock :: SCB.Block -> DbM Id.BlockId
insertBlock block =
  runSession $ HsqlSes.statement block insertBlockStmt

insertCheckUniqueBlockStmt :: HsqlStmt.Statement SCB.Block Id.BlockId
insertCheckUniqueBlockStmt =
  insertCheckUnique
    SCB.blockEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.BlockId)

insertCheckUniqueBlock :: SCB.Block -> DbM Id.BlockId
insertCheckUniqueBlock block =
  runSession $ HsqlSes.statement block insertCheckUniqueBlockStmt

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

queryBlockHashBlockNo ::
  ByteString ->
  DbM (Either DbError (Maybe Word64))
queryBlockHashBlockNo hash = do
  result <- runSession $ HsqlSes.statement hash queryBlockHashBlockNoStmt
  case result of
    [] -> pure $ Right Nothing
    [blockNo] -> pure $ Right (Just blockNo)
    results ->
      pure $
        Left $
          DbError
            ( "Multiple blocks found with same hash: "
                <> textShow hash
                <> " (found "
                <> textShow (length results)
                <> ")"
            )

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

queryBlockCount :: DbM Word64
queryBlockCount = runSession $ HsqlSes.statement () queryBlockCountStmt

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
querySlotUtcTime :: Word64 -> DbM (Either DbError UTCTime)
querySlotUtcTime slotNo = do
  result <- runSession $ HsqlSes.statement slotNo querySlotUtcTimeStmt
  case result of
    Just time -> pure $ Right time
    Nothing -> pure $ Left $ DbError ("Slot not found for slot_no: " <> textShow slotNo)

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
queryBlockCountAfterBlockNo :: Word64 -> Bool -> DbM Word64
queryBlockCountAfterBlockNo blockNo queryEq =
  runSession $ HsqlSes.statement blockNo stmt
  where
    stmt =
      if queryEq
        then queryBlockCountAfterEqBlockNoStmt
        else queryBlockCountAfterBlockNoStmt

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

queryBlockNoAndEpoch :: Word64 -> DbM (Maybe (Id.BlockId, Word64))
queryBlockNoAndEpoch blkNo =
  runSession $ HsqlSes.statement blkNo $ queryBlockNoAndEpochStmt @SCB.Block

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

queryNearestBlockSlotNo :: Word64 -> DbM (Maybe (Id.BlockId, Word64))
queryNearestBlockSlotNo slotNo =
  runSession $ HsqlSes.statement slotNo $ queryNearestBlockSlotNoStmt @SCB.Block

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

queryBlockHash :: SCB.Block -> DbM (Maybe (Id.BlockId, Word64))
queryBlockHash block =
  runSession $ HsqlSes.statement (SCB.blockHash block) $ queryBlockHashStmt @SCB.Block

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
          , " WHERE id = (SELECT MIN(id) FROM " <> tableName (Proxy @a) <> ")"
          ]

    decoder = HsqlD.rowMaybe $ do
      blockId <- Id.idDecoder Id.BlockId
      blockNo <- HsqlD.column (HsqlD.nullable $ fromIntegral <$> HsqlD.int8)
      pure (blockId, fromMaybe 0 blockNo)

queryMinBlock :: DbM (Maybe (Id.BlockId, Word64))
queryMinBlock = runSession $ HsqlSes.statement () $ queryMinBlockStmt @SCB.Block

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

queryReverseIndexBlockId :: Id.BlockId -> DbM [Maybe Text.Text]
queryReverseIndexBlockId blockId =
  runSession $ HsqlSes.statement blockId $ queryReverseIndexBlockIdStmt @SCB.Block

--------------------------------------------------------------------------------

-- | Get the number of transactions in the specified block.
queryBlockTxCountStmt :: HsqlStmt.Statement Id.BlockId Word64
queryBlockTxCountStmt =
  parameterisedCountWhere @SCB.Tx "block_id" "= $1" (Id.idEncoder Id.getBlockId)

queryBlockTxCount :: Id.BlockId -> DbM Word64
queryBlockTxCount blkId =
  runSession $ HsqlSes.statement blkId queryBlockTxCountStmt

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

queryBlockId :: ByteString -> Text.Text -> DbM (Either DbError Id.BlockId)
queryBlockId hash errMsg = do
  mBlockId <- runSession $ HsqlSes.statement hash queryBlockIdStmt
  case mBlockId of
    Just blockId -> pure $ Right blockId
    Nothing -> pure $ Left $ DbError ("Block not found for hash: " <> errMsg)

queryBlockIdEither ::
  ByteString ->
  DbM (Either DbError Id.BlockId)
queryBlockIdEither hash = do
  mBlockId <- runSession $ HsqlSes.statement hash queryBlockIdStmt
  case mBlockId of
    Just blockId -> pure $ Right blockId
    Nothing -> pure $ Left $ DbError ("Block not found for hash: " <> textShow hash)

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

queryBlocksForCurrentEpochNo :: DbM (Maybe Word64)
queryBlocksForCurrentEpochNo =
  runSession $ HsqlSes.statement () queryBlocksForCurrentEpochNoStmt

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

queryLatestBlock :: DbM (Maybe SCB.Block)
queryLatestBlock =
  runSessionEntity $ HsqlSes.statement () queryLatestBlockStmt

--------------------------------------------------------------------------------
queryLatestEpochNoFromBlockStmt :: HsqlStmt.Statement () Word64
queryLatestEpochNoFromBlockStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    blockTable = tableName (Proxy @SC.Block)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COALESCE(MAX(epoch_no), 0)::bigint"
          , " FROM " <> blockTable
          , " WHERE slot_no IS NOT NULL"
          ]

    decoder =
      HsqlD.singleRow $
        fromIntegral <$> HsqlD.column (HsqlD.nonNullable HsqlD.int8)

queryLatestEpochNoFromBlock :: DbM Word64
queryLatestEpochNoFromBlock =
  runSession $ HsqlSes.statement () queryLatestEpochNoFromBlockStmt

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
queryLatestBlockId :: DbM (Maybe Id.BlockId)
queryLatestBlockId =
  runSession $ HsqlSes.statement () queryLatestBlockIdStmt

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

queryDepositUpToBlockNo :: Word64 -> DbM Ada
queryDepositUpToBlockNo blkNo =
  runSession $ HsqlSes.statement blkNo queryDepositUpToBlockNoStmt

--------------------------------------------------------------------------------
queryLatestSlotNoStmt :: HsqlStmt.Statement () Word64
queryLatestSlotNoStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    blockTable = tableName (Proxy @SC.Block)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COALESCE(MAX(slot_no), 0)::bigint"
          , " FROM " <> blockTable
          , " WHERE slot_no IS NOT NULL"
          ]

    decoder =
      HsqlD.singleRow $
        fromIntegral <$> HsqlD.column (HsqlD.nonNullable HsqlD.int8)

queryLatestSlotNo :: DbM Word64
queryLatestSlotNo =
  runSession $ HsqlSes.statement () queryLatestSlotNoStmt

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

queryLatestPoints :: DbM [(Maybe Word64, ByteString)]
queryLatestPoints = runSession $ HsqlSes.statement () queryLatestPointsStmt

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

querySlotHash :: SlotNo -> DbM [(SlotNo, ByteString)]
querySlotHash slotNo = do
  hashes <-
    runSession $
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

queryCountSlotNosGreaterThan :: Word64 -> DbM Word64
queryCountSlotNosGreaterThan slotNo =
  runSession $ HsqlSes.statement slotNo queryCountSlotNosGreaterThanStmt

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
queryCountSlotNo :: DbM Word64
queryCountSlotNo =
  runSession $ HsqlSes.statement () queryCountSlotNoStmt

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

queryBlockHeight :: DbM (Maybe Word64)
queryBlockHeight =
  runSession $
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

queryGenesis :: Text.Text -> DbM (Either DbError Id.BlockId)
queryGenesis errMsg = do
  result <- runSession $ HsqlSes.statement () queryGenesisStmt
  case result of
    [blk] -> pure $ Right blk
    _otherwise -> pure $ Left $ DbError ("Multiple Genesis blocks found: " <> errMsg)

-----------------------------------------------------------------------------------
queryLatestBlockNoStmt :: HsqlStmt.Statement () (Maybe Word64)
queryLatestBlockNoStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    blockTable = tableName (Proxy @SC.Block)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT MAX(block_no)"
          , " FROM " <> blockTable
          , " WHERE block_no IS NOT NULL"
          ]

    decoder = HsqlD.rowMaybe $ do
      blockNo <- HsqlD.column (HsqlD.nonNullable HsqlD.int8)
      pure $ fromIntegral blockNo

queryLatestBlockNo :: DbM (Maybe Word64)
queryLatestBlockNo =
  runSession $ HsqlSes.statement () queryLatestBlockNoStmt

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

queryPreviousSlotNo :: Word64 -> DbM (Maybe Word64)
queryPreviousSlotNo slotNo =
  runSession $ HsqlSes.statement slotNo queryPreviousSlotNoStmt

-----------------------------------------------------------------------------------
-- DELETE
-----------------------------------------------------------------------------------

deleteBlocksBlockId ::
  Trace IO Text.Text ->
  TxOutVariantType ->
  Id.BlockId ->
  Word64 ->
  Bool ->
  DbM Int64
deleteBlocksBlockId trce txOutVariantType blockId epochN isConsumedTxOut = do
  let rb = "Rollback - "

  withProgress (Just trce) 6 rb $ \progressRef -> do
    -- Step 0: Initialize
    liftIO $ updateProgress (Just trce) progressRef 0 (rb <> "Initializing rollback...")

    -- Step 1: Find minimum IDs
    liftIO $ updateProgress (Just trce) progressRef 1 (rb <> "Finding reverse indexes...")

    reverseIndexData <- queryReverseIndexBlockId blockId
    let mMinIds = fmap (textToMinIds txOutVariantType =<<) reverseIndexData
    (cminIds, completed) <- findMinIdsRec progressRef mMinIds mempty
    liftIO $ logInfo trce (rb <> "Querying minimum transaction ID...")
    mRawTxId <- queryMinRefId @SCB.Tx "block_id" blockId (Id.idEncoder Id.getBlockId)
    let mTxId = Id.TxId <$> mRawTxId
    minIds <-
      if completed
        then do
          liftIO $ logInfo trce (rb <> "Using reverse index data for minimum IDs")
          pure cminIds
        else do
          liftIO $ logInfo trce (rb <> "Reverse index incomplete - querying missing minimum IDs (this may take several minutes)...")
          liftIO $ logInfo trce (rb <> "Scanning TxIn, TxOut, and MaTxOut tables for minimum IDs...")
          result <- completeMinId mTxId cminIds
          liftIO $ logInfo trce (rb <> "Completed minimum ID lookup")
          pure result

    -- Step 2: Delete epoch-related data
    liftIO $ updateProgress (Just trce) progressRef 2 (rb <> "Deleting epoch data...")
    deleteEpochLogsE <- deleteUsingEpochNo trce epochN

    -- Step 3: Delete block-related data
    liftIO $ updateProgress (Just trce) progressRef 3 (rb <> "Deleting block data...")
    (deleteBlockCount, blockDeleteLogs) <- deleteTablesAfterBlockId txOutVariantType blockId mTxId minIds

    -- Step 4: Handle consumed transactions
    liftIO $ updateProgress (Just trce) progressRef 4 (rb <> "Updating consumed transactions...")
    setNullLogs <-
      if isConsumedTxOut
        then querySetNullTxOut txOutVariantType mTxId
        else pure ("ConsumedTxOut is not active so no Nulls set", 0)

    -- Step 5: Generate summary
    liftIO $ updateProgress (Just trce) progressRef 5 (rb <> "Generating summary...")
    let summary = mkRollbackSummary (deleteEpochLogsE <> blockDeleteLogs) setNullLogs

    -- Step 6: Complete
    liftIO $ updateProgress (Just trce) progressRef 6 (rb <> "Complete!")
    liftIO $ logInfo trce summary

    pure deleteBlockCount
  where
    findMinIdsRec :: ProgressRef -> [Maybe MinIdsWrapper] -> MinIdsWrapper -> DbM (MinIdsWrapper, Bool)
    findMinIdsRec _ [] minIds = pure (minIds, True)
    findMinIdsRec progressRef (mMinIds : rest) minIds =
      case mMinIds of
        Nothing -> do
          -- Show error message
          liftIO $ logInfo trce "Rollback - Failed to find ReverseIndex. Deletion may take longer..."
          pure (minIds, False)
        Just minIdDB -> do
          let minIds' = minIds <> minIdDB
          if isComplete minIds'
            then pure (minIds', True)
            else findMinIdsRec progressRef rest minIds'

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

deleteUsingEpochNo :: Trace IO Text.Text -> Word64 -> DbM [(Text.Text, Int64)]
deleteUsingEpochNo trce epochN = do
  let epochEncoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)
      epochInt64 = fromIntegral epochN

  -- First, count what we're about to delete for progress tracking
  totalCounts <- withProgress (Just trce) 5 "Counting epoch records..." $ \progressRef -> do
    liftIO $ updateProgress (Just trce) progressRef 0 "Counting Epoch records..."
    ec <- runSession $ HsqlSes.statement epochN (parameterisedCountWhere @SC.Epoch "no" ">= $1" epochEncoder)

    liftIO $ updateProgress (Just trce) progressRef 1 "Counting DrepDistr records..."
    dc <- runSession $ HsqlSes.statement epochN (parameterisedCountWhere @SC.DrepDistr "epoch_no" "> $1" epochEncoder)

    liftIO $ updateProgress (Just trce) progressRef 2 "Counting RewardRest records..."
    rrc <- runSession $ HsqlSes.statement epochN (parameterisedCountWhere @SC.RewardRest "spendable_epoch" "> $1" epochEncoder)

    liftIO $ updateProgress (Just trce) progressRef 3 "Counting PoolStat records..."
    psc <- runSession $ HsqlSes.statement epochN (parameterisedCountWhere @SC.PoolStat "epoch_no" "> $1" epochEncoder)

    liftIO $ updateProgress (Just trce) progressRef 4 "Counting Reward records..."
    rc <- runSession $ HsqlSes.statement epochN (parameterisedCountWhere @SC.Reward "spendable_epoch" "> $1" epochEncoder)

    liftIO $ updateProgress (Just trce) progressRef 5 "Count completed"
    pure (ec, dc, rrc, psc, rc)

  let (epochCount, drepCount, rewardRestCount, poolStatCount, rewardCount) = totalCounts
      totalRecords = epochCount + drepCount + rewardRestCount + poolStatCount + rewardCount
  liftIO $ logInfo trce $ "Deleting " <> textShow totalRecords <> " records across 5 tables..."

  -- Execute deletes with progress logging
  (epochDeletedCount, drepDeletedCount, rewardRestDeletedCount, poolStatDeletedCount, rewardDeletedCount) <-
    withProgress (Just trce) 5 "Deleting epoch records..." $ \progressRef -> do
      liftIO $ updateProgress (Just trce) progressRef 1 $ "Deleting " <> textShow epochCount <> " Epochs..."
      epochDeletedCount <- runSession $ HsqlSes.statement epochN (deleteWhereCount @SC.Epoch "no" "=" epochEncoder)

      liftIO $ updateProgress (Just trce) progressRef 2 $ "Deleting " <> textShow drepCount <> " DrepDistr records..."
      drepDeletedCount <- runSession $ HsqlSes.statement epochN (deleteWhereCount @SC.DrepDistr "epoch_no" ">" epochEncoder)

      liftIO $ updateProgress (Just trce) progressRef 3 $ "Deleting " <> textShow rewardRestCount <> " RewardRest records..."
      rewardRestDeletedCount <- runSession $ HsqlSes.statement epochN (deleteWhereCount @SC.RewardRest "spendable_epoch" ">" epochEncoder)

      liftIO $ updateProgress (Just trce) progressRef 4 $ "Deleting " <> textShow poolStatCount <> " PoolStat records..."
      poolStatDeletedCount <- runSession $ HsqlSes.statement epochN (deleteWhereCount @SC.PoolStat "epoch_no" ">" epochEncoder)

      liftIO $ updateProgress (Just trce) progressRef 5 $ "Deleting " <> textShow rewardCount <> " Rewards..."
      rewardDeletedCount <- runSession $ HsqlSes.statement epochN (deleteWhereCount @SC.Reward "spendable_epoch" ">" epochEncoder)

      pure (epochDeletedCount, drepDeletedCount, rewardRestDeletedCount, poolStatDeletedCount, rewardDeletedCount)

  liftIO $ logInfo trce "Setting null values for governance actions..."
  -- Null operations
  n1 <- runSession $ HsqlSes.statement epochInt64 setNullEnactedStmt
  n2 <- runSession $ HsqlSes.statement epochInt64 setNullRatifiedStmt
  n3 <- runSession $ HsqlSes.statement epochInt64 setNullDroppedStmt
  n4 <- runSession $ HsqlSes.statement epochInt64 setNullExpiredStmt

  let nullTotal = n1 + n2 + n3 + n4
      countLogs =
        [ ("Epoch", epochDeletedCount)
        , ("DrepDistr", drepDeletedCount)
        , ("RewardRest", rewardRestDeletedCount)
        , ("PoolStat", poolStatDeletedCount)
        , ("Reward", rewardDeletedCount)
        ]
      nullLogs = [("GovActionProposal Nulled", nullTotal)]

  liftIO $ logInfo trce $ "Rollback epoch deletion completed - actual deleted: " <> textShow (epochDeletedCount + drepDeletedCount + rewardRestDeletedCount + poolStatDeletedCount + rewardDeletedCount)
  pure $ countLogs <> nullLogs

--------------------------------------------------------------------------------
deleteBlocksSlotNo ::
  Trace IO Text.Text ->
  TxOutVariantType ->
  SlotNo ->
  Bool ->
  DbM Bool
deleteBlocksSlotNo trce txOutVariantType (SlotNo slotNo) isConsumedTxOut = do
  blockEpochE <- queryNearestBlockSlotNo slotNo
  case blockEpochE of
    Nothing -> pure False
    (Just (blockId, epochN)) -> do
      -- Delete the block and return whether it was successful
      deleteCount <- deleteBlocksBlockId trce txOutVariantType blockId epochN isConsumedTxOut
      if deleteCount > 0
        then pure True
        else do
          liftIO $ logWarning trce $ "deleteBlocksSlotNo: No blocks found for slot: " <> Text.pack (show slotNo)
          pure False

--------------------------------------------------------------------------------
deleteBlocksSlotNoNoTrace :: TxOutVariantType -> SlotNo -> DbM Bool
deleteBlocksSlotNoNoTrace txOutVariantType slotNo = deleteBlocksSlotNo nullTracer txOutVariantType slotNo True

--------------------------------------------------------------------------------
deleteBlocksForTests :: TxOutVariantType -> Id.BlockId -> Word64 -> DbM (Either DbError ())
deleteBlocksForTests txOutVariantType blockId epochN = do
  resCount <- deleteBlocksBlockId nullTracer txOutVariantType blockId epochN False
  if resCount > 0
    then pure $ Right ()
    else pure $ Left $ DbError "No blocks deleted"

--------------------------------------------------------------------------------

-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteBlock :: TxOutVariantType -> SC.Block -> DbM Bool
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

insertDatum :: SCB.Datum -> DbM Id.DatumId
insertDatum datum =
  runSession $ HsqlSes.statement datum insertDatumStmt

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

queryDatum :: ByteString -> DbM (Maybe Id.DatumId)
queryDatum hash =
  runSession $ HsqlSes.statement hash queryDatumStmt

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

queryAllExtraMigrations :: DbM [ExtraMigration]
queryAllExtraMigrations =
  runSession $
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

insertBulkTxMetadataPiped :: Bool -> [[SCB.TxMetadata]] -> DbM [Id.TxMetadataId]
insertBulkTxMetadataPiped removeJsonb txMetaChunks =
  runSession $
    HsqlSes.pipeline $
      concat <$> traverse (\chunk -> HsqlP.statement chunk (insertBulkTxMetadataStmt removeJsonb)) txMetaChunks

--------------------------------------------------------------------------------
-- CollateralTxIn
--------------------------------------------------------------------------------
insertCollateralTxInStmt :: HsqlStmt.Statement SCB.CollateralTxIn Id.CollateralTxInId
insertCollateralTxInStmt =
  insert
    SCB.collateralTxInEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.CollateralTxInId)

insertCollateralTxIn :: SCB.CollateralTxIn -> DbM Id.CollateralTxInId
insertCollateralTxIn cTxIn = runSession $ HsqlSes.statement cTxIn insertCollateralTxInStmt

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
queryMeta :: DbM (Either DbError (Maybe SCB.Meta))
queryMeta = do
  result <- runSession $ HsqlSes.statement () queryMetaStmt
  case result of
    [] -> pure $ Right Nothing -- Empty table is valid
    [m] -> pure $ Right $ Just $ entityVal m
    _otherwise -> pure $ Left $ DbError "Multiple rows in meta table"

--------------------------------------------------------------------------------
-- ReferenceTxIn
--------------------------------------------------------------------------------
insertReferenceTxInStmt :: HsqlStmt.Statement SCB.ReferenceTxIn Id.ReferenceTxInId
insertReferenceTxInStmt =
  insert
    SCB.referenceTxInEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.ReferenceTxInId)

insertReferenceTxIn :: SCB.ReferenceTxIn -> DbM Id.ReferenceTxInId
insertReferenceTxIn rTxIn = runSession $ HsqlSes.statement rTxIn insertReferenceTxInStmt

--------------------------------------------------------------------------------
insertExtraMigrationStmt :: HsqlStmt.Statement SCB.ExtraMigrations ()
insertExtraMigrationStmt =
  insert
    SCB.extraMigrationsEncoder
    NoResult

insertExtraMigration :: ExtraMigration -> DbM ()
insertExtraMigration extraMigration =
  runSession $ HsqlSes.statement input insertExtraMigrationStmt
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

insertExtraKeyWitness :: SCB.ExtraKeyWitness -> DbM Id.ExtraKeyWitnessId
insertExtraKeyWitness eKeyWitness = runSession $ HsqlSes.statement eKeyWitness insertExtraKeyWitnessStmt

--------------------------------------------------------------------------------
-- Meta
--------------------------------------------------------------------------------
insertMetaStmt :: HsqlStmt.Statement SCB.Meta Id.MetaId
insertMetaStmt =
  insertCheckUnique
    SCB.metaEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.MetaId)

insertMeta :: SCB.Meta -> DbM Id.MetaId
insertMeta meta = runSession $ HsqlSes.statement meta insertMetaStmt

--------------------------------------------------------------------------------
-- Redeemer
--------------------------------------------------------------------------------
insertRedeemerStmt :: HsqlStmt.Statement SCB.Redeemer Id.RedeemerId
insertRedeemerStmt =
  insert
    SCB.redeemerEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.RedeemerId)

insertRedeemer :: SCB.Redeemer -> DbM Id.RedeemerId
insertRedeemer redeemer = runSession $ HsqlSes.statement redeemer insertRedeemerStmt

--------------------------------------------------------------------------------
-- RedeemerData
--------------------------------------------------------------------------------
insertRedeemerDataStmt :: HsqlStmt.Statement SCB.RedeemerData Id.RedeemerDataId
insertRedeemerDataStmt =
  insertCheckUnique
    SCB.redeemerDataEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.RedeemerDataId)

insertRedeemerData :: SCB.RedeemerData -> DbM Id.RedeemerDataId
insertRedeemerData redeemerData = runSession $ HsqlSes.statement redeemerData insertRedeemerDataStmt

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

queryRedeemerData :: ByteString -> DbM (Maybe Id.RedeemerDataId)
queryRedeemerData hash =
  runSession $
    HsqlSes.statement hash queryRedeemerDataStmt

--------------------------------------------------------------------------------
-- ReverseIndex
--------------------------------------------------------------------------------
insertReverseIndexStmt :: HsqlStmt.Statement SCB.ReverseIndex Id.ReverseIndexId
insertReverseIndexStmt =
  insert
    SCB.reverseIndexEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.ReverseIndexId)

insertReverseIndex :: SCB.ReverseIndex -> DbM Id.ReverseIndexId
insertReverseIndex reverseIndex = runSession $ HsqlSes.statement reverseIndex insertReverseIndexStmt

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

querySchemaVersion :: DbM (Maybe SCB.SchemaVersion)
querySchemaVersion =
  runSession $ HsqlSes.statement () querySchemaVersionStmt

--------------------------------------------------------------------------------
-- Script
--------------------------------------------------------------------------------

-- | INSERTS
insertScriptStmt :: HsqlStmt.Statement SCB.Script Id.ScriptId
insertScriptStmt =
  insertCheckUnique
    SCB.scriptEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.ScriptId)

insertScript :: SCB.Script -> DbM Id.ScriptId
insertScript script = runSession $ HsqlSes.statement script insertScriptStmt

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

queryScriptWithId :: ByteString -> DbM (Maybe Id.ScriptId)
queryScriptWithId hash =
  runSession $ HsqlSes.statement hash queryScriptWithIdStmt

--------------------------------------------------------------------------------
-- SlotLeader
--------------------------------------------------------------------------------
insertCheckUniqueSlotLeaderStmt :: HsqlStmt.Statement SCB.SlotLeader Id.SlotLeaderId
insertCheckUniqueSlotLeaderStmt =
  insertCheckUnique
    SCB.slotLeaderEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.SlotLeaderId)

insertSlotLeader :: SCB.SlotLeader -> DbM Id.SlotLeaderId
insertSlotLeader slotLeader =
  runSession $ HsqlSes.statement slotLeader insertCheckUniqueSlotLeaderStmt

--------------------------------------------------------------------------------
-- TxCbor
--------------------------------------------------------------------------------
insertTxCborStmt :: HsqlStmt.Statement SCB.TxCbor Id.TxCborId
insertTxCborStmt =
  insert
    SCB.txCborEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.TxCborId)

insertTxCbor :: SCB.TxCbor -> DbM Id.TxCborId
insertTxCbor txCBOR =
  runSession $ HsqlSes.statement txCBOR insertTxCborStmt

--------------------------------------------------------------------------------
-- Tx
--------------------------------------------------------------------------------

-- | INSERTS -------------------------------------------------------------------
insertTxStmt :: HsqlStmt.Statement SCB.Tx Id.TxId
insertTxStmt =
  insert
    SCB.txEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.TxId)

insertTx :: SCB.Tx -> DbM Id.TxId
insertTx tx = runSession $ HsqlSes.statement tx insertTxStmt

-- | QUERIES ------------------------------------------------------------------

-- | Count the number of transactions in the Tx table.
queryTxCount :: DbM Word64
queryTxCount =
  runSession $ HsqlSes.statement () $ countAll @SCB.Tx

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

queryWithdrawalsUpToBlockNo :: Word64 -> DbM Ada
queryWithdrawalsUpToBlockNo blkNo =
  runSession $ HsqlSes.statement blkNo queryWithdrawalsUpToBlockNoStmt

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
queryTxId :: ByteString -> DbM (Maybe Id.TxId)
queryTxId txHash =
  runSession $ HsqlSes.statement txHash queryTxIdStmt

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

queryFeesUpToBlockNo :: Word64 -> DbM Ada
queryFeesUpToBlockNo blkNo =
  runSession $ HsqlSes.statement blkNo queryFeesUpToBlockNoStmt

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

queryFeesUpToSlotNo :: Word64 -> DbM Ada
queryFeesUpToSlotNo slotNo =
  runSession $ HsqlSes.statement slotNo queryFeesUpToSlotNoStmt

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

queryInvalidTx :: DbM [SCB.Tx]
queryInvalidTx = do
  result <- runSession $ HsqlSes.statement () queryInvalidTxStmt
  pure $ map entityVal result

--------------------------------------------------------------------------------
-- TxIn
--------------------------------------------------------------------------------
insertTxInStmt :: HsqlStmt.Statement SCB.TxIn Id.TxInId
insertTxInStmt =
  insert
    SCB.txInEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.TxInId)

insertTxIn :: SCB.TxIn -> DbM Id.TxInId
insertTxIn txIn = runSession $ HsqlSes.statement txIn insertTxInStmt

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

insertBulkTxInPiped :: [[SCB.TxIn]] -> DbM [Id.TxInId]
insertBulkTxInPiped txInChunks =
  concat
    <$> runSession
      ( HsqlSes.pipeline $
          for txInChunks $ \chunk ->
            HsqlP.statement chunk insertBulkTxInStmt
      )

--------------------------------------------------------------------------------
queryTxInCount :: DbM Word64
queryTxInCount =
  runSession $ HsqlSes.statement () $ countAll @SCB.TxIn

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

queryTxInRedeemer :: DbM [SCB.TxIn]
queryTxInRedeemer =
  runSession $ HsqlSes.statement () queryTxInRedeemerStmt

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

queryTxInFailedTx :: DbM [SCB.TxIn]
queryTxInFailedTx = runSession $ HsqlSes.statement () queryTxInFailedTxStmt

--------------------------------------------------------------------------------
-- Withdrawal
--------------------------------------------------------------------------------
insertWithdrawalStmt :: HsqlStmt.Statement SCB.Withdrawal Id.WithdrawalId
insertWithdrawalStmt =
  insert
    SCB.withdrawalEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.WithdrawalId)

insertWithdrawal :: SCB.Withdrawal -> DbM Id.WithdrawalId
insertWithdrawal withdrawal = runSession $ HsqlSes.statement withdrawal insertWithdrawalStmt

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

queryWithdrawalScript :: DbM [SCB.Withdrawal]
queryWithdrawalScript = runSession $ HsqlSes.statement () queryWithdrawalScriptStmt

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

queryWithdrawalAddresses :: DbM [Id.StakeAddressId]
queryWithdrawalAddresses =
  runSession $ HsqlSes.statement () queryWithdrawalAddressesStmt
