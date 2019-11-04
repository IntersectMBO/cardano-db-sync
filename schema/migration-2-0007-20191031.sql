-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 7 THEN
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "tx_count" uinteger NOT NULL' ;
    -- -------------------------------------------------------------------------
    -- It would be possible to write a schema migration here that calculates
    -- and inserts values in this column, but the run time of that migration
    -- is slower the syncing the chain from scratch.
    -- -------------------------------------------------------------------------
    UPDATE schema_version SET stage_two = 7 ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
