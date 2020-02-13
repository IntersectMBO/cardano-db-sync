-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 11 THEN
    EXECUTE 'CREATe TABLE "epoch"("id" SERIAL8  PRIMARY KEY UNIQUE,"out_sum" outsum NOT NULL,"tx_count" uinteger NOT NULL,"no" uinteger NOT NULL,"start_time" timestamp NOT NULL,"end_time" timestamp NOT NULL)' ;
    EXECUTE 'ALTER TABLE "epoch" ADD CONSTRAINT "unique_epoch" UNIQUE("no")' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = 11 ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
