-- Add synced_at timestamp to epoch_sync_time table
-- This tracks WHEN db-sync processed the epoch (not blockchain time)

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 46 THEN
    EXECUTE 'ALTER TABLE "epoch_sync_time" ADD COLUMN "synced_at" TIMESTAMP WITH TIME ZONE NULL' ;

    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
