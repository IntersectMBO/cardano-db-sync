
CREATE INDEX IF NOT EXISTS tx_out_inline_datum_id_idx ON tx_out (inline_datum_id);
CREATE INDEX IF NOT EXISTS tx_out_reference_script_id_idx ON tx_out (reference_script_id);
CREATE INDEX IF NOT EXISTS redeemer_redeemer_data_id_idx ON redeemer (redeemer_data_id);
CREATE INDEX IF NOT EXISTS pool_owner_pool_update_id_idx ON pool_owner (pool_update_id);
CREATE INDEX IF NOT EXISTS redeemer_data_tx_id_idx ON redeemer_data (tx_id);
CREATE INDEX IF NOT EXISTS reference_tx_in_tx_out_id_idx ON reference_tx_in (tx_out_id);
CREATE INDEX IF NOT EXISTS collateral_tx_out_stake_address_id_idx ON collateral_tx_out (stake_address_id);
CREATE INDEX IF NOT EXISTS collateral_tx_out_inline_datum_id_idx ON collateral_tx_out (inline_datum_id);
CREATE INDEX IF NOT EXISTS collateral_tx_out_reference_script_id_idx ON collateral_tx_out (reference_script_id);
