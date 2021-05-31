-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 3 THEN
    EXECUTE 'CREATe TABLE "epoch_sync_time"("id" SERIAL8  PRIMARY KEY UNIQUE,"no" INT8 NOT NULL,"seconds" DOUBLE PRECISION NULL,"state" syncstatetype NOT NULL)' ;
    EXECUTE 'ALTER TABLE "epoch_sync_time" ADD CONSTRAINT "unique_epoch_sync_time" UNIQUE("no")' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
