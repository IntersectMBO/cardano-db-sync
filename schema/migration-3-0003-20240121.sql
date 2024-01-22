
-- as of 13.2.0.0 offline was renamed to off_chain so we rename previous indexes if they exists
ALTER INDEX IF EXISTS idx_pool_offline_data_pmr_id RENAME TO idx_off_chain_pool_data_pmr_id;
ALTER INDEX IF EXISTS idx_pool_offline_fetch_error_pmr_id RENAME TO idx_off_chain_pool_fetch_error_pmr_id;

CREATE INDEX IF NOT EXISTS idx_off_chain_pool_fetch_error_pmr_id ON off_chain_pool_fetch_error (pmr_id);
CREATE INDEX IF NOT EXISTS idx_off_chain_pool_data_pmr_id ON off_chain_pool_data (pmr_id);
