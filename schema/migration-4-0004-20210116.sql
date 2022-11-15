
-- Indexes which optimize inserting new rewards
CREATE INDEX IF NOT EXISTS idx_stake_address_hash_raw ON stake_address(hash_raw) ;
CREATE INDEX IF NOT EXISTS idx_delegation_active_epoch_no ON delegation(active_epoch_no) ;
CREATE INDEX IF NOT EXISTS idx_pool_update_reward_addr ON pool_update(reward_addr_id) ;
CREATE INDEX IF NOT EXISTS idx_pool_update_active_epoch_no ON pool_update(active_epoch_no) ;

UPDATE schema_version SET stage_three = 6 ;
