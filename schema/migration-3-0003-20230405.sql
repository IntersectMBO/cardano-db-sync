
-- Hand crafted indices for performance
-- These indexes are used by queries db-sync does.
CREATE INDEX IF NOT EXISTS idx_tx_out_consumed_by_tx_in_id ON tx_out (consumed_by_tx_in_id);
