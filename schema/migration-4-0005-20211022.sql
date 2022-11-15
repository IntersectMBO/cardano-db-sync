
-- Indexes which optimize inserting new rewards
CREATE INDEX IF NOT EXISTS idx_stake_address_view ON stake_address USING hash (view);
CREATE INDEX IF NOT EXISTS idx_epoch_no ON epoch(no);
CREATE INDEX IF NOT EXISTS idx_tx_in_redeemer_id ON tx_in (redeemer_id);
CREATE INDEX IF NOT EXISTS idx_delegation_redeemer_id ON delegation (redeemer_id);
CREATE INDEX IF NOT EXISTS idx_withdrawal_redeemer_id ON withdrawal (redeemer_id);
CREATE INDEX IF NOT EXISTS idx_stake_deregistration_redeemer_id ON stake_deregistration (redeemer_id);
CREATE INDEX IF NOT EXISTS idx_reserved_pool_ticker_pool_hash ON reserved_pool_ticker (pool_hash);
CREATE INDEX IF NOT EXISTS idx_collateral_tx_in_tx_out_id ON collateral_tx_in (tx_out_id);
CREATE INDEX IF NOT EXISTS idx_script_tx_id ON script (tx_id);
