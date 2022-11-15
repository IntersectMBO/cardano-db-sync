-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 4 THEN
    EXECUTE 'CREATe TABLE "delisted_pool"("id" SERIAL8  PRIMARY KEY UNIQUE,"hash_raw" hash28type NOT NULL)' ;
    EXECUTE 'ALTER TABLE "delisted_pool" ADD CONSTRAINT "unique_delisted_pool" UNIQUE("hash_raw")' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
