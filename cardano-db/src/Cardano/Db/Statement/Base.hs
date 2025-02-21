{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Statement.Base where

import Cardano.Db.Schema.Core (Block)
import Cardano.Db.Schema.Core.Base ( TxIn (..), blockEncoder )
import Cardano.Db.Schema.Ids (BlockId (..), idDecoder, TxInId (..), TxId (..), RedeemerId (..))
import Cardano.Db.Types (DbAction, DbTxMode (..))
import Cardano.Prelude (MonadIO, Word64)
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Statement as HsqlS
import qualified Hasql.Transaction as HsqlT
import qualified Hasql.Encoders as HsqlE
import Data.Functor.Contravariant ((>$<))
import Contravariant.Extras (contrazip4)
import Cardano.Db.Statement.Helpers (runDbT, mkDbTransaction, bulkInsert)

-- The wrapped version that provides the DbAction context
insertBlock :: MonadIO m => Block -> DbAction m BlockId
insertBlock block =
  runDbT Write $ mkDbTransaction "" $ insertBlockStm block

insertBlockStm :: Block -> HsqlT.Transaction BlockId
insertBlockStm block =
  HsqlT.statement block $ HsqlS.Statement sql blockEncoder (HsqlD.singleRow $ idDecoder BlockId) True
  where
    sql =
      "INSERT INTO block \
      \(id, hash, epoch_no, slot_no, epoch_slot_no, block_no, previous_id, \
      \slot_leader_id, size, time, tx_count, proto_major, proto_minor, \
      \vrf_key, op_cert, op_cert_counter) \
      \VALUES \
      \($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16) \
      \RETURNING id"


insertManyTxIn :: MonadIO m => [TxIn] -> DbAction m [TxInId]
insertManyTxIn txIns = runDbT Write $ mkDbTransaction "insertManyTxIn" (insertManyTxInStm txIns)

insertManyTxInStm :: [TxIn] -> HsqlT.Transaction [TxInId]
insertManyTxInStm txIns =
  bulkInsert
    "tx_in"
    ["tx_in_id", "tx_out_id", "tx_out_index", "redeemer_id"]
    ["bigint[]", "bigint[]", "int8[]", "int8[]"]
    extractTxIn
    encodeTxIn
    (HsqlD.rowList $ idDecoder TxInId)
    txIns

  where
    extractTxIn :: [TxIn] -> ([TxId], [TxId], [Word64], [Maybe RedeemerId])
    extractTxIn xs = ( map txInTxInId xs
      , map txInTxOutId xs
      , map txInTxOutIndex xs
      , map txInRedeemerId xs
      )

    encodeTxIn :: HsqlE.Params ([TxId], [TxId], [Word64], [Maybe RedeemerId])
    encodeTxIn = contrazip4
      (HsqlE.param $ HsqlE.nonNullable $ HsqlE.foldableArray $ HsqlE.nonNullable $ getTxId >$< HsqlE.int8)
      (HsqlE.param $ HsqlE.nonNullable $ HsqlE.foldableArray $ HsqlE.nonNullable $ getTxId >$< HsqlE.int8)
      (HsqlE.param $ HsqlE.nonNullable $ HsqlE.foldableArray $ HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
      (HsqlE.param $ HsqlE.nonNullable $ HsqlE.foldableArray $ HsqlE.nullable $ getRedeemerId >$< HsqlE.int8)

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
