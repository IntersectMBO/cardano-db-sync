
CREATE INDEX IF NOT EXISTS idx_datum_tx_id ON datum (tx_id) ;
CREATE INDEX IF NOT EXISTS idx_extra_key_witness_tx_id ON extra_key_witness (tx_id) ;
CREATE INDEX IF NOT EXISTS idx_param_proposal_cost_model_id ON param_proposal (cost_model_id) ;
CREATE INDEX IF NOT EXISTS idx_epoch_param_cost_model_id ON epoch_param (cost_model_id) ;
