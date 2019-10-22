-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 4 THEN
    EXECUTE 'ALTER TABLE "meta" ALTER COLUMN "start_time" TYPE timestamp' ;

    -- -------------------------------------------------------------------------
    -- Hand written SQL to update the start_time
    update meta set start_time = (select time from block where id = 1) where id =1 ;
    -- -------------------------------------------------------------------------

    UPDATE schema_version SET stage_two = 4 ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
