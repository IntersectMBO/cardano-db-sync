
CREATE INDEX IF NOT EXISTS idx_redeemer_tx_id ON redeemer(tx_id);
CREATE INDEX IF NOT EXISTS idx_collateral_tx_out_tx_id ON collateral_tx_out(tx_id);
CREATE INDEX IF NOT EXISTS idx_reference_tx_in_tx_in_id ON reference_tx_in(tx_in_id);
CREATE INDEX IF NOT EXISTS idx_tx_metadata_tx_id_key ON tx_metadata(tx_id, key);
DROP INDEX IF EXISTS idx_tx_metadata_tx_id;

-- faster autoanalyze on heavy tables:
ALTER TABLE public.block SET (autovacuum_analyze_scale_factor = 0.02, autovacuum_analyze_threshold = 5000);
ALTER TABLE public.tx SET (autovacuum_analyze_scale_factor = 0.02, autovacuum_analyze_threshold = 5000);
