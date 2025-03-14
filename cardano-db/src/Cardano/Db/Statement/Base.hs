{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Cardano.Db.Statement.Base where

import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Statement as HsqlS
import qualified Hasql.Transaction as HsqlT

import qualified Cardano.Db.Schema.Core.Base as SCB
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Statement.Function.Core (runDbT, mkDbTransaction, ResultType (..), ResultTypeBulk (..))
import Cardano.Db.Statement.Function.Insert (insert, insertCheckUnique, bulkInsert)
import Cardano.Db.Statement.Types (Entity (..))
import Cardano.Db.Types (DbAction, DbTransMode (..), DbWord64, ExtraMigration, extraDescription)
import Cardano.Prelude (MonadIO, Word64, ByteString, textShow, MonadError (..))
import Cardano.Db.Error (DbError(..))

--------------------------------------------------------------------------------
-- | Block
--------------------------------------------------------------------------------

-- | INSERT
insertBlock :: MonadIO m => SCB.Block -> DbAction m Id.BlockId
insertBlock block =
  runDbT TransWrite $ mkDbTransaction "insertBlock" $ do
    entity <- insert
      SCB.blockEncoder
      (WithResult (HsqlD.singleRow SCB.entityBlockDecoder))
      block
    pure $ entityKey entity

-- | QUERY
queryBlockHashBlockNo :: MonadIO m => ByteString -> DbAction m (Maybe Word64)
queryBlockHashBlockNo hash = runDbT TransReadOnly $ mkDbTransaction "queryBlockHashBlockNo" $ do
  result <- HsqlT.statement hash $ HsqlS.Statement sql hashEncoder blockNoDecoder True
  case result of
    [] -> pure Nothing
    [blockNo] -> pure (Just blockNo)
    _otherwise -> throwError $ DbLookupError mkCallSite "Multiple blocks found with same hash: " (BlockHashContext hash)
  where
    table = tableName (Proxy @SCB.Block)

    sql = TextEnc.encodeUtf8 $ Text.concat
      [ "SELECT block_no FROM " <> table <> " WHERE hash = $1" ]

    hashEncoder = E.param (E.nonNullable E.bytea)
    blockNoDecoder = HsqlD.rowList (D.column (D.nullable $ fromIntegral <$> D.int8))

--------------------------------------------------------------------------------
-- | Datum
--------------------------------------------------------------------------------
insertDatum :: MonadIO m => SCB.Datum -> DbAction m Id.DatumId
insertDatum datum =
  runDbT TransWrite $ mkDbTransaction "insertDatum" $ do
    entity <- insert
      SCB.datumEncoder
      (WithResult $ HsqlD.singleRow SCB.entityDatumDecoder)
      datum
    pure $ entityKey entity

--------------------------------------------------------------------------------
-- | TxMetadata
--------------------------------------------------------------------------------
bulkInsertTxMetadata :: MonadIO m => [SCB.TxMetadata] -> DbAction m [Id.TxMetadataId]
bulkInsertTxMetadata txMetas = runDbT TransWrite $ mkDbTransaction "bulkInsertTxInMetadata" $ do
  entity <- bulkInsert
    extractTxMetadata
    SCB.txMetadataBulkEncoder
    (WithResultBulk (HsqlD.rowList SCB.entityTxMetadataDecoder))
    txMetas
  pure $ map entityKey entity
  where
    extractTxMetadata :: [SCB.TxMetadata] -> ([DbWord64], [Maybe Text.Text], [ByteString], [Id.TxId])
    extractTxMetadata xs =
      ( map SCB.txMetadataKey xs
      , map SCB.txMetadataJson xs
      , map SCB.txMetadataBytes xs
      , map SCB.txMetadataTxId xs
      )

--------------------------------------------------------------------------------
-- | CollateralTxIn
--------------------------------------------------------------------------------
insertCollateralTxIn :: MonadIO m => SCB.CollateralTxIn -> DbAction m Id.CollateralTxInId
insertCollateralTxIn cTxIn =
  runDbT TransWrite $ mkDbTransaction "insertCollateralTxIn" $ do
    entity <- insert
      SCB.collateralTxInEncoder
      (WithResult $ HsqlD.singleRow SCB.entityCollateralTxInDecoder)
      cTxIn
    pure $ entityKey entity

--------------------------------------------------------------------------------
-- | ReferenceTxIn
--------------------------------------------------------------------------------
insertReferenceTxIn :: MonadIO m => SCB.ReferenceTxIn -> DbAction m Id.ReferenceTxInId
insertReferenceTxIn rTxIn =
  runDbT TransWrite $ mkDbTransaction "insertReferenceTxIn" $ do
    entity <- insert
      SCB.referenceTxInEncoder
      (WithResult $ HsqlD.singleRow SCB.entityReferenceTxInDecoder)
      rTxIn
    pure $ entityKey entity

insertExtraMigration :: MonadIO m => ExtraMigration -> DbAction m ()
insertExtraMigration extraMigration = runDbT TransWrite $ mkDbTransaction "insertExtraMigration" $
  insert
    SCB.extraMigrationsEncoder
    NoResult
    (SCB.ExtraMigrations (textShow extraMigration) (Just $ extraDescription extraMigration))

--------------------------------------------------------------------------------
-- | ExtraKeyWitness
--------------------------------------------------------------------------------
insertExtraKeyWitness :: MonadIO m => SCB.ExtraKeyWitness -> DbAction m Id.ExtraKeyWitnessId
insertExtraKeyWitness eKeyWitness =
  runDbT TransWrite $ mkDbTransaction "insertExtraKeyWitness" $ do
    entity <- insert
      SCB.extraKeyWitnessEncoder
      (WithResult $ HsqlD.singleRow SCB.entityExtraKeyWitnessDecoder)
      eKeyWitness
    pure $ entityKey entity

--------------------------------------------------------------------------------
-- | Meta
--------------------------------------------------------------------------------
insertMeta :: MonadIO m => SCB.Meta -> DbAction m Id.MetaId
insertMeta meta = runDbT TransWrite $ mkDbTransaction "insertMeta" $ do
  entity <- insertCheckUnique
    SCB.metaEncoder
    (WithResult (HsqlD.singleRow SCB.entityMetaDecoder))
    meta
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- | Redeemer
--------------------------------------------------------------------------------
insertRedeemer :: MonadIO m => SCB.Redeemer -> DbAction m Id.RedeemerId
insertRedeemer redeemer =
  runDbT TransWrite $ mkDbTransaction "insertRedeemer" $ do
    entity <- insert
      SCB.redeemerEncoder
      (WithResult $ HsqlD.singleRow SCB.entityRedeemerDecoder)
      redeemer
    pure $ entityKey entity

insertRedeemerData :: MonadIO m => SCB.RedeemerData -> DbAction m Id.RedeemerDataId
insertRedeemerData redeemerData =
  runDbT TransWrite $ mkDbTransaction "insertRedeemerData" $ do
    entity <- insert
      SCB.redeemerDataEncoder
      (WithResult $ HsqlD.singleRow SCB.entityRedeemerDataDecoder)
      redeemerData
    pure $ entityKey entity

--------------------------------------------------------------------------------
-- | ReverseIndex
--------------------------------------------------------------------------------
insertReverseIndex :: MonadIO m => SCB.ReverseIndex -> DbAction m Id.ReverseIndexId
insertReverseIndex reverseIndex =
  runDbT TransWrite $ mkDbTransaction "insertReverseIndex" $ do
  entity <- insert
    SCB.reverseIndexEncoder
    (WithResult $ HsqlD.singleRow SCB.entityReverseIndexDecoder)
    reverseIndex
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- | Script
--------------------------------------------------------------------------------
insertScript :: MonadIO m => SCB.Script -> DbAction m Id.ScriptId
insertScript script =
  runDbT TransWrite $ mkDbTransaction "insertScript" $ do
    entity <- insertCheckUnique
      SCB.scriptEncoder
      (WithResult $ HsqlD.singleRow SCB.entityScriptDecoder)
      script
    pure $ entityKey entity

--------------------------------------------------------------------------------
-- | SlotLeader
--------------------------------------------------------------------------------
insertSlotLeader :: MonadIO m => SCB.SlotLeader -> DbAction m Id.SlotLeaderId
insertSlotLeader slotLeader = runDbT TransWrite $ mkDbTransaction "insertSlotLeader" $ do
  entity <- insertCheckUnique
    SCB.slotLeaderEncoder
    (WithResult $ HsqlD.singleRow SCB.entitySlotLeaderDecoder)
    slotLeader
  pure $ entityKey entity


insertTxCbor :: MonadIO m => SCB.TxCbor -> DbAction m Id.TxCborId
insertTxCbor txCBOR =
  runDbT TransWrite $ mkDbTransaction "insertTxCBOR" $ do
    entity <- insert
      SCB.txCborEncoder
      (WithResult $ HsqlD.singleRow SCB.entityTxCborDecoder)
      txCBOR
    pure $ entityKey entity

--------------------------------------------------------------------------------
-- | Tx
--------------------------------------------------------------------------------
insertTx :: MonadIO m => SCB.Tx -> DbAction m Id.TxId
insertTx tx =
  runDbT TransWrite $ mkDbTransaction ("insertTx: " <> Text.pack (show $ BS.length $ SCB.txHash tx)) $ do
    entity <- insert
      SCB.txEncoder
      (WithResult $ HsqlD.singleRow SCB.entityTxDecoder)
      tx
    pure $ entityKey entity

--------------------------------------------------------------------------------
-- | TxIn
--------------------------------------------------------------------------------
insertTxIn :: MonadIO m => SCB.TxIn -> DbAction m Id.TxInId
insertTxIn txIn =
  runDbT TransWrite $ mkDbTransaction "insertTxIn" $ do
    entity <- insert
      SCB.txInEncoder
      (WithResult $ HsqlD.singleRow SCB.entityTxInDecoder)
      txIn
    pure $ entityKey entity

bulkInsertTxIn :: MonadIO m => [SCB.TxIn] -> DbAction m [Id.TxInId]
bulkInsertTxIn txIns = runDbT TransWrite $ mkDbTransaction "bulkInsertTxIn" $ do
  entity <- bulkInsert
    extractTxIn
    SCB.encodeTxInBulk
    (WithResultBulk $ HsqlD.rowList SCB.entityTxInDecoder)
    txIns
  pure $ map entityKey entity
  where
    extractTxIn :: [SCB.TxIn] -> ([Id.TxId], [Id.TxId], [Word64], [Maybe Id.RedeemerId])
    extractTxIn xs =
      ( map SCB.txInTxInId xs
      , map SCB.txInTxOutId xs
      , map SCB.txInTxOutIndex xs
      , map SCB.txInRedeemerId xs
      )

--------------------------------------------------------------------------------
-- | Withdrawal
--------------------------------------------------------------------------------
insertWithdrawal :: MonadIO m => SCB.Withdrawal -> DbAction m Id.WithdrawalId
insertWithdrawal withdrawal =
  runDbT TransWrite $ mkDbTransaction "insertWithdrawal" $ do
    entity <- insert
      SCB.withdrawalEncoder
      (WithResult $ HsqlD.singleRow SCB.entityWithdrawalDecoder)
      withdrawal
    pure $ entityKey entity

-- These tables store fundamental blockchain data, such as blocks, transactions, and UTXOs.

-- block
-- collateral_tx_in
-- collateral_tx_out
-- datum
-- extra_key_witness
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
