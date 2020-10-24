CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_three + 1 INTO next_version FROM schema_version ;
  IF next_version <= 2 THEN
    -- -------------------------------------------------------------------------
    -- Hand crafted indices for performance

    -- Without these indices 'cardano-db-tool validate' will put a heavy load
    -- on Postgres.

    CREATE INDEX idx_block_slot_no
    ON block(slot_no);

    CREATE INDEX idx_block_block_no
    ON block(block_no);

    CREATE INDEX idx_block_epoch_no
    ON block(epoch_no);

    CREATE INDEX idx_block_previous_id
    ON block(previous_id);

    CREATE INDEX idx_tx_block_id
    ON tx(block_id);

    CREATE INDEX idx_tx_in_source_tx
    ON tx_in(tx_in_id);

    -- -------------------------------------------------------------------------

    UPDATE schema_version SET stage_three = 2 ;
    RAISE NOTICE 'DB has been migrated to stage_three version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
