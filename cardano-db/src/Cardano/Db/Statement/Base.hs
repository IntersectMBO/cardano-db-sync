{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.Base where

import Cardano.Prelude (ByteString, MonadError (..), MonadIO, Proxy (..), Word64, textShow, void)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

import Cardano.Db.Error (DbError (..))
import qualified Cardano.Db.Schema.Core.Base as SCB
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Statement.Function.Core (ResultType (..), ResultTypeBulk (..), mkCallInfo, mkCallSite, runDbSession)
import Cardano.Db.Statement.Function.Insert (insert, insertBulk)
import Cardano.Db.Statement.Function.Query (parameterisedCountWhere)
import Cardano.Db.Statement.Types (DbInfo, Entity (..), tableName)
import Cardano.Db.Types (DbAction, DbCallInfo (..), DbWord64, ExtraMigration, extraDescription)
import Data.Functor.Contravariant ((>$<))
import Data.Time (UTCTime)

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

queryBlockId :: MonadIO m => ByteString -> DbAction m Id.BlockId
queryBlockId hash = do
  result <- runDbSession callInfo $ HsqlSes.statement hash queryBlockIdStmt
  case result of
    Just res -> pure res
    Nothing ->
      throwError $ DbError (dciCallSite callInfo) errorMsg Nothing
  where
    callInfo = mkCallInfo "queryBlockId"
    errorMsg = "Block not found with hash: " <> Text.pack (show hash)

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

-- | DELETE

--------------------------------------------------------------------------------
-- Datum
--------------------------------------------------------------------------------
insertDatumStmt :: HsqlStmt.Statement SCB.Datum (Entity SCB.Datum)
insertDatumStmt =
  insert
    SCB.datumEncoder
    (WithResult $ HsqlD.singleRow SCB.entityDatumDecoder)

insertDatum :: MonadIO m => SCB.Datum -> DbAction m Id.DatumId
insertDatum datum = do
  entity <- runDbSession (mkCallInfo "insertDatum") $ HsqlSes.statement datum insertDatumStmt
  pure $ entityKey entity

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
-- Script
--------------------------------------------------------------------------------
insertScriptStmt :: HsqlStmt.Statement SCB.Script (Entity SCB.Script)
insertScriptStmt =
  insert
    SCB.scriptEncoder
    (WithResult $ HsqlD.singleRow SCB.entityScriptDecoder)

insertScript :: MonadIO m => SCB.Script -> DbAction m Id.ScriptId
insertScript script = do
  entity <- runDbSession (mkCallInfo "insertScript") $ HsqlSes.statement script insertScriptStmt
  pure $ entityKey entity

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

-- | INSERTTS
insertTxStmt :: HsqlStmt.Statement SCB.Tx (Entity SCB.Tx)
insertTxStmt =
  insert
    SCB.txEncoder
    (WithResult $ HsqlD.singleRow SCB.entityTxDecoder)

insertTx :: MonadIO m => SCB.Tx -> DbAction m Id.TxId
insertTx tx = do
  entity <- runDbSession (mkCallInfo "insertTx") $ HsqlSes.statement tx insertTxStmt
  pure $ entityKey entity

-- | QUERIES

--------------------------------------------------------------------------------
queryTxIdStmt :: HsqlStmt.Statement ByteString (Maybe Id.TxId)
queryTxIdStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder = HsqlE.param (HsqlE.nonNullable HsqlE.bytea)
    decoder = HsqlD.rowMaybe (Id.idDecoder Id.TxId)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id"
          , " FROM tx"
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
