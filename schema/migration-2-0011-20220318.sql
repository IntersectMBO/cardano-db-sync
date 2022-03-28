-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 11 THEN
    EXECUTE 'ALTER TABLE "block" ALTER COLUMN "slot_no" TYPE word63type' ;
    EXECUTE 'ALTER TABLE "ada_pots" ALTER COLUMN "slot_no" TYPE word63type' ;
    EXECUTE 'ALTER TABLE "delegation" ALTER COLUMN "slot_no" TYPE word63type' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
