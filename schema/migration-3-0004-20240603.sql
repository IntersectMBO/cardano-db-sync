
CREATE INDEX IF NOT EXISTS idx_committee_member_committee_id ON committee_member (committee_id);
CREATE INDEX IF NOT EXISTS idx_pool_update_registered_tx_id ON pool_update (registered_tx_id);
