CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 10 THEN
    -- -------------------------------------------------------------------------
    -- Hand crafted indices for performance
    CREATE INDEX idx_block_slot_no 
    ON block(slot_no);

    CREATE INDEX idx_block_block_no 
    ON block(block_no);

    CREATE INDEX idx_block_epoch_no 
    ON block(epoch_no);

    CREATE INDEX idx_block_previous 
    ON block(previous);

    CREATE INDEX idx_block_hash
    ON block(hash);

    CREATE INDEX idx_tx_block 
    ON tx(block);

    CREATE INDEX idx_tx_hash
    ON tx(hash);

    CREATE INDEX idx_tx_in_source_tx 
    ON tx_in(tx_in_id);

    CREATE INDEX idx_tx_in_consuming_tx 
    ON tx_in(tx_out_id);

    CREATE INDEX idx_tx_out_tx 
    ON tx_out(tx_id);
    -- -------------------------------------------------------------------------

    UPDATE schema_version SET stage_two = 10 ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
