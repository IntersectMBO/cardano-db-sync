-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 29 THEN
    ALTER TABLE committee_registration ALTER COLUMN cold_key SET DATA TYPE bytea;
    ALTER TABLE committee_registration ALTER COLUMN hot_key SET DATA TYPE bytea;
    ALTER TABLE committee_de_registration ALTER COLUMN hot_key SET DATA TYPE bytea;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
