CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_three + 1 INTO next_version FROM schema_version ;
  IF next_version <= 4 THEN
    -- -------------------------------------------------------------------------
    -- More hand crafted indices for performance

    -- helpful in case anyone is looking for a utxo by a payment credential.

    CREATE INDEX idx_tx_out_payment_cred
    ON tx_out(payment_cred);

    CREATE INDEX idx_pool_update_hash_id
    ON pool_update(hash_id);

    -- -------------------------------------------------------------------------

    UPDATE schema_version SET stage_three = 4 ;
    RAISE NOTICE 'DB has been migrated to stage_three version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
