
CREATE INDEX IF NOT EXISTS idx_block_time ON block(time);
CREATE INDEX IF NOT EXISTS idx_tx_out_payment_cred ON tx_out(payment_cred);
CREATE INDEX IF NOT EXISTS idx_pool_update_hash_id ON pool_update(hash_id);
