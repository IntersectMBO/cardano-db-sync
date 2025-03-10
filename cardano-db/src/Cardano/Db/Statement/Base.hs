{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Cardano.Db.Statement.Base where

import Cardano.Prelude (ByteString, MonadError (..), MonadIO, Proxy (..), Word64, textShow, void)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStm

import Cardano.Db.Error (DbError (..))
import qualified Cardano.Db.Schema.Core.Base as SCB
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Statement.Function.Core (ResultType (..), ResultTypeBulk (..), mkCallInfo, mkCallSite, runDbSession)
import Cardano.Db.Statement.Function.Insert (bulkInsert, insert)
import Cardano.Db.Statement.Types (Entity (..), tableName)
import Cardano.Db.Types (DbAction, DbWord64, ExtraMigration, extraDescription)

--------------------------------------------------------------------------------

-- | Block

--------------------------------------------------------------------------------

-- | INSERT
insertBlockStmt :: HsqlStm.Statement SCB.Block (Entity SCB.Block)
insertBlockStmt =
  insert
    SCB.blockEncoder
    (WithResult $ HsqlD.singleRow SCB.entityBlockDecoder)

insertBlock :: MonadIO m => SCB.Block -> DbAction m Id.BlockId
insertBlock block = do
  entity <- runDbSession (mkCallInfo "insertBlock") $ HsqlSes.statement block insertBlockStmt
  pure $ entityKey entity

-- | QUERIES
queryBlockHashBlockNoStmt :: HsqlStm.Statement ByteString [Word64]
queryBlockHashBlockNoStmt =
  HsqlStm.Statement sql hashEncoder blockNoDecoder True
  where
    table = tableName (Proxy @SCB.Block)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          ["SELECT block_no FROM " <> table <> " WHERE hash = $1"]

    hashEncoder = HsqlE.param (HsqlE.nonNullable HsqlE.bytea)
    blockNoDecoder = HsqlD.rowList (HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8))

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

queryBlockCountStmt :: HsqlStm.Statement () Word64
queryBlockCountStmt =
  HsqlStm.Statement sql mempty blockCountDecoder True
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

-- | Datum

--------------------------------------------------------------------------------
insertDatumStmt :: HsqlStm.Statement SCB.Datum (Entity SCB.Datum)
insertDatumStmt =
  insert
    SCB.datumEncoder
    (WithResult $ HsqlD.singleRow SCB.entityDatumDecoder)

insertDatum :: MonadIO m => SCB.Datum -> DbAction m Id.DatumId
insertDatum datum = do
  entity <- runDbSession (mkCallInfo "insertDatum") $ HsqlSes.statement datum insertDatumStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------

-- | TxMetadata

--------------------------------------------------------------------------------
bulkInsertTxMetadataStmt :: HsqlStm.Statement [SCB.TxMetadata] [Entity SCB.TxMetadata]
bulkInsertTxMetadataStmt =
  bulkInsert
    extractTxMetadata -- 1. Extractor function
    SCB.txMetadataBulkEncoder -- 2. Encoder for the tuple
    (WithResultBulk (HsqlD.rowList SCB.entityTxMetadataDecoder)) -- 3. Result type
  where
    extractTxMetadata :: [SCB.TxMetadata] -> ([DbWord64], [Maybe Text.Text], [ByteString], [Id.TxId])
    extractTxMetadata xs =
      ( map SCB.txMetadataKey xs
      , map SCB.txMetadataJson xs
      , map SCB.txMetadataBytes xs
      , map SCB.txMetadataTxId xs
      )

bulkInsertTxMetadata :: MonadIO m => [SCB.TxMetadata] -> DbAction m [Id.TxMetadataId]
bulkInsertTxMetadata txMetas = do
  entities <-
    runDbSession (mkCallInfo "bulkInsertTxMetadata") $
      HsqlSes.statement txMetas bulkInsertTxMetadataStmt
  pure $ map entityKey entities

--------------------------------------------------------------------------------

-- | CollateralTxIn

--------------------------------------------------------------------------------
insertCollateralTxInStmt :: HsqlStm.Statement SCB.CollateralTxIn (Entity SCB.CollateralTxIn)
insertCollateralTxInStmt =
  insert
    SCB.collateralTxInEncoder
    (WithResult $ HsqlD.singleRow SCB.entityCollateralTxInDecoder)

insertCollateralTxIn :: MonadIO m => SCB.CollateralTxIn -> DbAction m Id.CollateralTxInId
insertCollateralTxIn cTxIn = do
  entity <- runDbSession (mkCallInfo "insertCollateralTxIn") $ HsqlSes.statement cTxIn insertCollateralTxInStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------

-- | ReferenceTxIn

--------------------------------------------------------------------------------
insertReferenceTxInStmt :: HsqlStm.Statement SCB.ReferenceTxIn (Entity SCB.ReferenceTxIn)
insertReferenceTxInStmt =
  insert
    SCB.referenceTxInEncoder
    (WithResult $ HsqlD.singleRow SCB.entityReferenceTxInDecoder)

insertReferenceTxIn :: MonadIO m => SCB.ReferenceTxIn -> DbAction m Id.ReferenceTxInId
insertReferenceTxIn rTxIn = do
  entity <- runDbSession (mkCallInfo "insertReferenceTxIn") $ HsqlSes.statement rTxIn insertReferenceTxInStmt
  pure (entityKey entity)

insertExtraMigrationStmt :: HsqlStm.Statement SCB.ExtraMigrations ()
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

-- | ExtraKeyWitness

--------------------------------------------------------------------------------
insertExtraKeyWitnessStmt :: HsqlStm.Statement SCB.ExtraKeyWitness (Entity SCB.ExtraKeyWitness)
insertExtraKeyWitnessStmt =
  insert
    SCB.extraKeyWitnessEncoder
    (WithResult $ HsqlD.singleRow SCB.entityExtraKeyWitnessDecoder)

insertExtraKeyWitness :: MonadIO m => SCB.ExtraKeyWitness -> DbAction m Id.ExtraKeyWitnessId
insertExtraKeyWitness eKeyWitness = do
  entity <- runDbSession (mkCallInfo "insertExtraKeyWitness") $ HsqlSes.statement eKeyWitness insertExtraKeyWitnessStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------

-- | Meta

--------------------------------------------------------------------------------
insertMetaStmt :: HsqlStm.Statement SCB.Meta (Entity SCB.Meta)
insertMetaStmt =
  insert
    SCB.metaEncoder
    (WithResult $ HsqlD.singleRow SCB.entityMetaDecoder)

insertMeta :: MonadIO m => SCB.Meta -> DbAction m Id.MetaId
insertMeta meta = do
  entity <- runDbSession (mkCallInfo "insertMeta") $ HsqlSes.statement meta insertMetaStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------

-- | Redeemer

--------------------------------------------------------------------------------
insertRedeemerStmt :: HsqlStm.Statement SCB.Redeemer (Entity SCB.Redeemer)
insertRedeemerStmt =
  insert
    SCB.redeemerEncoder
    (WithResult $ HsqlD.singleRow SCB.entityRedeemerDecoder)

insertRedeemer :: MonadIO m => SCB.Redeemer -> DbAction m Id.RedeemerId
insertRedeemer redeemer = do
  entity <- runDbSession (mkCallInfo "insertRedeemer") $ HsqlSes.statement redeemer insertRedeemerStmt
  pure $ entityKey entity

insertRedeemerDataStmt :: HsqlStm.Statement SCB.RedeemerData (Entity SCB.RedeemerData)
insertRedeemerDataStmt =
  insert
    SCB.redeemerDataEncoder
    (WithResult $ HsqlD.singleRow SCB.entityRedeemerDataDecoder)

insertRedeemerData :: MonadIO m => SCB.RedeemerData -> DbAction m Id.RedeemerDataId
insertRedeemerData redeemerData = do
  entity <- runDbSession (mkCallInfo "insertRedeemerData") $ HsqlSes.statement redeemerData insertRedeemerDataStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------

-- | ReverseIndex

--------------------------------------------------------------------------------
insertReverseIndexStmt :: HsqlStm.Statement SCB.ReverseIndex (Entity SCB.ReverseIndex)
insertReverseIndexStmt =
  insert
    SCB.reverseIndexEncoder
    (WithResult $ HsqlD.singleRow SCB.entityReverseIndexDecoder)

insertReverseIndex :: MonadIO m => SCB.ReverseIndex -> DbAction m Id.ReverseIndexId
insertReverseIndex reverseIndex = do
  entity <- runDbSession (mkCallInfo "insertReverseIndex") $ HsqlSes.statement reverseIndex insertReverseIndexStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------

-- | Script

--------------------------------------------------------------------------------
insertScriptStmt :: HsqlStm.Statement SCB.Script (Entity SCB.Script)
insertScriptStmt =
  insert
    SCB.scriptEncoder
    (WithResult $ HsqlD.singleRow SCB.entityScriptDecoder)

insertScript :: MonadIO m => SCB.Script -> DbAction m Id.ScriptId
insertScript script = do
  entity <- runDbSession (mkCallInfo "insertScript") $ HsqlSes.statement script insertScriptStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------

-- | SlotLeader

--------------------------------------------------------------------------------
insertSlotLeaderStmt :: HsqlStm.Statement SCB.SlotLeader (Entity SCB.SlotLeader)
insertSlotLeaderStmt =
  insert
    SCB.slotLeaderEncoder
    (WithResult $ HsqlD.singleRow SCB.entitySlotLeaderDecoder)

insertSlotLeader :: MonadIO m => SCB.SlotLeader -> DbAction m Id.SlotLeaderId
insertSlotLeader slotLeader = do
  entity <- runDbSession (mkCallInfo "insertSlotLeader") $ HsqlSes.statement slotLeader insertSlotLeaderStmt
  pure $ entityKey entity

insertTxCborStmt :: HsqlStm.Statement SCB.TxCbor (Entity SCB.TxCbor)
insertTxCborStmt =
  insert
    SCB.txCborEncoder
    (WithResult $ HsqlD.singleRow SCB.entityTxCborDecoder)

insertTxCbor :: MonadIO m => SCB.TxCbor -> DbAction m Id.TxCborId
insertTxCbor txCBOR = do
  entity <- runDbSession (mkCallInfo "insertTxCBOR") $ HsqlSes.statement txCBOR insertTxCborStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------

-- | Tx

--------------------------------------------------------------------------------
insertTxStmt :: HsqlStm.Statement SCB.Tx (Entity SCB.Tx)
insertTxStmt =
  insert
    SCB.txEncoder
    (WithResult $ HsqlD.singleRow SCB.entityTxDecoder)

insertTx :: MonadIO m => SCB.Tx -> DbAction m Id.TxId
insertTx tx = do
  entity <- runDbSession (mkCallInfo "insertTx") $ HsqlSes.statement tx insertTxStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------

-- | TxIn

--------------------------------------------------------------------------------
insertTxInStmt :: HsqlStm.Statement SCB.TxIn (Entity SCB.TxIn)
insertTxInStmt =
  insert
    SCB.txInEncoder
    (WithResult $ HsqlD.singleRow SCB.entityTxInDecoder)

insertTxIn :: MonadIO m => SCB.TxIn -> DbAction m Id.TxInId
insertTxIn txIn = do
  entity <- runDbSession (mkCallInfo "insertTxIn") $ HsqlSes.statement txIn insertTxInStmt
  pure $ entityKey entity

bulkInsertTxInStmt :: HsqlStm.Statement [SCB.TxIn] [Entity SCB.TxIn]
bulkInsertTxInStmt =
  bulkInsert
    extractTxIn -- 1. Extractor function first
    SCB.encodeTxInBulk -- 2. Encoder
    (WithResultBulk $ HsqlD.rowList SCB.entityTxInDecoder) -- 3. Result type
  where
    extractTxIn :: [SCB.TxIn] -> ([Id.TxId], [Id.TxId], [Word64], [Maybe Id.RedeemerId])
    extractTxIn xs =
      ( map SCB.txInTxInId xs
      , map SCB.txInTxOutId xs
      , map SCB.txInTxOutIndex xs
      , map SCB.txInRedeemerId xs
      )

bulkInsertTxIn :: MonadIO m => [SCB.TxIn] -> DbAction m [Id.TxInId]
bulkInsertTxIn txIns = do
  entities <-
    runDbSession (mkCallInfo "bulkInsertTxIn") $
      HsqlSes.statement txIns bulkInsertTxInStmt -- Pass txIns directly
  pure $ map entityKey entities

--------------------------------------------------------------------------------

-- | Withdrawal

--------------------------------------------------------------------------------
insertWithdrawalStmt :: HsqlStm.Statement SCB.Withdrawal (Entity SCB.Withdrawal)
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
