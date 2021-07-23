-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 12 THEN
    EXECUTE 'ALTER TABLE "tx" ADD COLUMN "ex_unit_number" uinteger NOT NULL' ;
    EXECUTE 'ALTER TABLE "tx" ADD COLUMN "ex_unit_fee" lovelace NOT NULL' ;
    EXECUTE 'ALTER TABLE "tx" ADD COLUMN "script_size" uinteger NOT NULL' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
