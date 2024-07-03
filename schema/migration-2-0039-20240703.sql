-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 39 THEN
    EXECUTE 'ALTER TABLE "ada_pots" ALTER COLUMN "deposits_drep" DROP DEFAULT' ;
    EXECUTE 'ALTER TABLE "ada_pots" ALTER COLUMN "deposits_proposal" DROP DEFAULT' ;
    EXECUTE 'ALTER TABLE "pool_metadata_ref" DROP CONSTRAINT "unique_pool_metadata_ref"' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
