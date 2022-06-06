CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_three + 1 INTO next_version FROM schema_version ;
  IF next_version <= 9 THEN
    CREATE INDEX tx_out_inline_datum_id_idx ON tx_out (inline_datum_id);
    CREATE INDEX tx_out_reference_script_id_idx ON tx_out (reference_script_id);
    CREATE INDEX redeemer_redeemer_data_id_idx ON redeemer (redeemer_data_id);
    CREATE INDEX pool_owner_pool_update_id_idx ON pool_owner (pool_update_id);
    CREATE INDEX redeemer_data_tx_id_idx ON redeemer_data (tx_id);
    CREATE INDEX reference_tx_in_tx_out_id_idx ON reference_tx_in (tx_out_id);
    CREATE INDEX collateral_tx_out_stake_address_id_idx ON collateral_tx_out (stake_address_id);
    CREATE INDEX collateral_tx_out_inline_datum_id_idx ON collateral_tx_out (inline_datum_id);
    CREATE INDEX collateral_tx_out_reference_script_id_idx ON collateral_tx_out (reference_script_id);
    UPDATE schema_version SET stage_three = 9 ;
    RAISE NOTICE 'DB has been migrated to stage_three version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
