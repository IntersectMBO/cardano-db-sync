
CREATE INDEX IF NOT EXISTS idx_block_time ON block(time);
CREATE INDEX IF NOT EXISTS idx_pool_update_hash_id ON pool_update(hash_id);
