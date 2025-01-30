{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Statement.Base where

import Cardano.Db.Schema.Core (Block)
import Cardano.Db.Schema.Ids (BlockId (..), idDecoder)
import qualified Hasql.Transaction as SqlTx
import Cardano.Prelude (MonadIO)
import Cardano.Db.Error (AsDbError)
import Cardano.Db.Types (DbAction, runDbTx, DbTxMode (..))
import Cardano.Db.Schema.Core.Base (blockEncoder)
import qualified Hasql.Statement as SqlStmt
import qualified Hasql.Decoders as SqlDecode

-- The wrapped version that provides the DbAction context
insertBlockTx :: (MonadIO m, AsDbError e) => Block -> DbAction e m BlockId
insertBlockTx block = runDbTx Write $ insertBlockStm block

insertBlockStm :: Block -> SqlTx.Transaction BlockId
insertBlockStm block =
  SqlTx.statement block $ SqlStmt.Statement sql blockEncoder (SqlDecode.singleRow $ idDecoder BlockId) True
  where
    sql =
      "INSERT INTO block \
      \(id, hash, epoch_no, slot_no, epoch_slot_no, block_no, previous_id, \
      \slot_leader_id, size, time, tx_count, proto_major, proto_minor, \
      \vrf_key, op_cert, op_cert_counter) \
      \VALUES \
      \($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16) \
      \RETURNING id"



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
