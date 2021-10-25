CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_three + 1 INTO next_version FROM schema_version ;
  IF next_version <= 7 THEN

    -- Indexes which optimize inserting new rewards
    CREATE INDEX idx_stake_address_view ON stake_address USING hash (view);

    CREATE INDEX idx_epoch_no ON epoch(no);

    CREATE INDEX idx_pool_offline_fetch_error_pmr_id ON pool_offline_fetch_error (pmr_id);
    CREATE INDEX idx_tx_in_redeemer_id ON tx_in (redeemer_id);
    CREATE INDEX idx_delegation_redeemer_id ON delegation (redeemer_id);
    CREATE INDEX idx_pool_offline_data_pmr_id ON pool_offline_data (pmr_id);
    CREATE INDEX idx_withdrawal_redeemer_id ON withdrawal (redeemer_id);
    CREATE INDEX idx_stake_deregistration_redeemer_id ON stake_deregistration (redeemer_id);
    CREATE INDEX idx_reserved_pool_ticker_pool_id ON reserved_pool_ticker (pool_id);
    CREATE INDEX idx_collateral_tx_in_tx_out_id ON collateral_tx_in (tx_out_id);
    CREATE INDEX idx_script_tx_id ON script (tx_id);

    UPDATE schema_version SET stage_three = 7 ;
    RAISE NOTICE 'DB has been migrated to stage_three version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
