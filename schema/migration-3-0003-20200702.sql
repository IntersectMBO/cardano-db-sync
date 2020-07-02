CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_three + 1 INTO next_version FROM schema_version ;
  IF next_version <= 3 THEN
    -- -------------------------------------------------------------------------
    -- More hand crafted indices for performance

    -- These speed up some common/simple graphql queries for lightweight wallets.

    CREATE INDEX idx_block_time
    ON block(time);

    CREATE INDEX idx_tx_out_address
    ON tx_out USING hash (address);

    -- -------------------------------------------------------------------------

    UPDATE schema_version SET stage_three = 3 ;
    RAISE NOTICE 'DB has been migrated to stage_three version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
