{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Statement.Base where

import Cardano.Db.Schema.Core (Block(..), TxMetadata(..))
import Cardano.Db.Schema.Core.Base ( TxIn (..), blockEncoder, txMetadataEncoderMany, encodeTxInMany )
import Cardano.Db.Schema.Ids (BlockId (..), idDecoder, TxInId (..), TxId (..), RedeemerId (..), TxMetadataId (..))
import Cardano.Db.Types (DbAction, DbTxMode (..), DbWord64)
import Cardano.Prelude (MonadIO, Word64, ByteString)
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Statement as HsqlS
import qualified Hasql.Transaction as HsqlT
import Cardano.Db.Statement.Helpers (runDbT, mkDbTransaction, bulkInsertReturnIds)
import Data.Text (Text)

-- The wrapped version that provides the DbAction context
insertBlock :: MonadIO m => Block -> DbAction m BlockId
insertBlock block =
  runDbT Write $ mkDbTransaction "insertBlock" $
    HsqlT.statement block $ HsqlS.Statement sql blockEncoder (HsqlD.singleRow $ idDecoder BlockId) True

  where
    sql =
      "INSERT INTO block \
      \(hash, epoch_no, slot_no, epoch_slot_no, block_no, previous_id, \
      \slot_leader_id, size, time, tx_count, proto_major, proto_minor, \
      \vrf_key, op_cert, op_cert_counter) \
      \VALUES \
      \($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15) \
      \RETURNING id"
 
insertManyTxIn :: MonadIO m => [TxIn] -> DbAction m [TxInId]
insertManyTxIn txIns = runDbT Write $ mkDbTransaction "insertManyTxIn" $
  bulkInsertReturnIds
    extractTxIn
    encodeTxInMany
    (HsqlD.rowList $ idDecoder TxInId)
    txIns

  where
    extractTxIn :: [TxIn] -> ([TxId], [TxId], [Word64], [Maybe RedeemerId])
    extractTxIn xs =
      ( map txInTxInId xs
      , map txInTxOutId xs
      , map txInTxOutIndex xs
      , map txInRedeemerId xs
      )

insertManyTxMetadata :: MonadIO m => [TxMetadata] -> DbAction m [TxMetadataId]
insertManyTxMetadata txMetas = runDbT Write $ mkDbTransaction "insertManyTxInMetadata" $
  bulkInsertReturnIds
    extractTxMetadata
    txMetadataEncoderMany
    (HsqlD.rowList $ idDecoder TxMetadataId)
    txMetas
  where
    extractTxMetadata :: [TxMetadata] -> ([DbWord64], [Maybe Text], [ByteString], [TxId])
    extractTxMetadata xs =
      ( map txMetadataKey xs
      , map txMetadataJson xs
      , map txMetadataBytes xs
      , map txMetadataTxId xs
      )



-- These tables store fundamental blockchain data, such as blocks, transactions, and UTXOs.

-- block
-- tx
-- tx_in
-- tx_out
-- utxo_view
-- utxo_byron_view
-- collateral_tx_in
-- collateral_tx_out
-- reference_tx_in
-- reverse_index
-- tx_cbor
-- datum
-- script
-- redeemer
-- redeemer_data
-- extra_key_witness
-- slot_leader
