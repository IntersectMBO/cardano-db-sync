{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Statement.Base where

import Data.Text (Text)
import qualified Hasql.Decoders as HsqlD

import Cardano.Db.Schema.Core (Block(..), TxMetadata(..))
import qualified Cardano.Db.Schema.Core.Base as SCB
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Statement.Function.Core (runDbT, mkDbTransaction, ResultType (..))
import Cardano.Db.Statement.Function.Insert (insert, bulkInsertReturnIds, insertCheckUnique)
import Cardano.Db.Types (DbAction, DbTransMode (..), DbWord64)
import Cardano.Prelude (MonadIO, Word64, ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- | Block
--------------------------------------------------------------------------------
insertBlock :: MonadIO m => Block -> DbAction m Id.BlockId
insertBlock block =
  runDbT TransWrite $ mkDbTransaction "insertBlock" $
    insert
      SCB.blockEncoder
      (WithResult (HsqlD.singleRow $ Id.idDecoder Id.BlockId))
      block

--------------------------------------------------------------------------------
-- | Datum
--------------------------------------------------------------------------------
insertDatum :: MonadIO m => SCB.Datum -> DbAction m Id.DatumId
insertDatum datum = runDbT TransWrite $ mkDbTransaction "insertDatum" $
  insert
    SCB.datumEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.DatumId))
    datum

--------------------------------------------------------------------------------
-- | TxMetadata
--------------------------------------------------------------------------------
insertManyTxMetadata :: MonadIO m => [TxMetadata] -> DbAction m [Id.TxMetadataId]
insertManyTxMetadata txMetas = runDbT TransWrite $ mkDbTransaction "insertManyTxInMetadata" $
  bulkInsertReturnIds
    extractTxMetadata
    SCB.txMetadataEncoderMany
    (HsqlD.rowList $ Id.idDecoder Id.TxMetadataId)
    txMetas
  where
    extractTxMetadata :: [TxMetadata] -> ([DbWord64], [Maybe Text], [ByteString], [Id.TxId])
    extractTxMetadata xs =
      ( map txMetadataKey xs
      , map txMetadataJson xs
      , map txMetadataBytes xs
      , map txMetadataTxId xs
      )

--------------------------------------------------------------------------------
-- | CollateralTxIn
--------------------------------------------------------------------------------
insertCollateralTxIn :: MonadIO m => SCB.CollateralTxIn -> DbAction m Id.CollateralTxInId
insertCollateralTxIn cTxIn = runDbT TransWrite $ mkDbTransaction "insertCollateralTxIn" $
  insert
    SCB.collateralTxInEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.CollateralTxInId))
    cTxIn

--------------------------------------------------------------------------------
-- | ReferenceTxIn
--------------------------------------------------------------------------------
insertReferenceTxIn :: MonadIO m => SCB.ReferenceTxIn -> DbAction m Id.ReferenceTxInId
insertReferenceTxIn rTxIn = runDbT TransWrite $ mkDbTransaction "insertReferenceTxIn" $
  insert
    SCB.referenceTxInEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.ReferenceTxInId))
    rTxIn

--------------------------------------------------------------------------------
-- | ExtraKeyWitness
--------------------------------------------------------------------------------
insertExtraKeyWitness :: MonadIO m => SCB.ExtraKeyWitness -> DbAction m Id.ExtraKeyWitnessId
insertExtraKeyWitness eKeyWitness = runDbT TransWrite $ mkDbTransaction "insertExtraKeyWitness" $
  insert
    SCB.extraKeyWitnessEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.ExtraKeyWitnessId))
    eKeyWitness

--------------------------------------------------------------------------------
-- | Meta
--------------------------------------------------------------------------------
insertMeta :: MonadIO m => SCB.Meta -> DbAction m Id.MetaId
insertMeta meta = runDbT TransWrite $ mkDbTransaction "insertMeta" $
  insertCheckUnique
    SCB.metaEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.MetaId))
    meta

--------------------------------------------------------------------------------
-- | Redeemer
--------------------------------------------------------------------------------
insertRedeemer :: MonadIO m => SCB.Redeemer -> DbAction m Id.RedeemerId
insertRedeemer redeemer = runDbT TransWrite $ mkDbTransaction "insertRedeemer" $
  insert
    SCB.redeemerEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.RedeemerId))
    redeemer

insertRedeemerData :: MonadIO m => SCB.RedeemerData -> DbAction m Id.RedeemerDataId
insertRedeemerData redeemerData = runDbT TransWrite $ mkDbTransaction "insertRedeemerData" $
  insert
    SCB.redeemerDataEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.RedeemerDataId))
    redeemerData

--------------------------------------------------------------------------------
-- | ReverseIndex
--------------------------------------------------------------------------------
insertReverseIndex :: MonadIO m => SCB.ReverseIndex -> DbAction m Id.ReverseIndexId
insertReverseIndex reverseIndex = runDbT TransWrite $ mkDbTransaction "insertReverseIndex" $
  insert
    SCB.reverseIndexEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.ReverseIndexId))
    reverseIndex

--------------------------------------------------------------------------------
-- | Script
--------------------------------------------------------------------------------
insertScript :: MonadIO m => SCB.Script -> DbAction m Id.ScriptId
insertScript script = runDbT TransWrite $ mkDbTransaction "insertScript" $
  insertCheckUnique
    SCB.scriptEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.ScriptId))
    script

--------------------------------------------------------------------------------
-- | SlotLeader
--------------------------------------------------------------------------------
insertSlotLeader :: MonadIO m => SCB.SlotLeader -> DbAction m Id.SlotLeaderId
insertSlotLeader slotLeader = runDbT TransWrite $ mkDbTransaction "insertSlotLeader" $
  insertCheckUnique
    SCB.slotLeaderEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.SlotLeaderId))
    slotLeader


insertTxCbor :: MonadIO m => SCB.TxCbor -> DbAction m Id.TxCborId
insertTxCbor txCBOR = runDbT TransWrite $ mkDbTransaction "insertTxCBOR" $
  insert
    SCB.txCborEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.TxCborId))
    txCBOR

--------------------------------------------------------------------------------
-- | Tx
--------------------------------------------------------------------------------
insertTx :: MonadIO m => SCB.Tx -> DbAction m Id.TxId
insertTx tx = runDbT TransWrite $ mkDbTransaction ("insertTx: " <> Text.pack (show $ BS.length $ SCB.txHash tx)) $
  insert
    SCB.txEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.TxId))
    tx

--------------------------------------------------------------------------------
-- | TxIn
--------------------------------------------------------------------------------
insertTxIn :: MonadIO m => SCB.TxIn -> DbAction m Id.TxInId
insertTxIn txIn = runDbT TransWrite $ mkDbTransaction "insertTxIn" $
  insert
    SCB.txInEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.TxInId))
    txIn

insertManyTxIn :: MonadIO m => [SCB.TxIn] -> DbAction m [Id.TxInId]
insertManyTxIn txIns = runDbT TransWrite $ mkDbTransaction "insertManyTxIn" $
  bulkInsertReturnIds
    extractTxIn
    SCB.encodeTxInMany
    (HsqlD.rowList $ Id.idDecoder Id.TxInId)
    txIns
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
insertWithdrawal withdrawal = runDbT TransWrite $ mkDbTransaction "insertWithdrawal" $
  insert
    SCB.withdrawalEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.WithdrawalId))
    withdrawal

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
