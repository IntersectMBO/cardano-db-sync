CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_three + 1 INTO next_version FROM schema_version ;
  IF next_version <= 8 THEN

    CREATE INDEX IF NOT EXISTS idx_datum_tx_id ON datum (tx_id) ;
    CREATE INDEX IF NOT EXISTS idx_extra_key_witness_tx_id ON extra_key_witness (tx_id) ;
    CREATE INDEX IF NOT EXISTS idx_param_proposal_cost_model_id ON param_proposal (cost_model_id) ;
    CREATE INDEX IF NOT EXISTS idx_epoch_param_cost_model_id ON epoch_param (cost_model_id) ;

    UPDATE schema_version SET stage_three = 8 ;
    RAISE NOTICE 'DB has been migrated to stage_three version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
