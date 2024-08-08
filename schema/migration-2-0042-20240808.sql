-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 42 THEN
    EXECUTE 'ALTER TABLE "voting_procedure" ADD COLUMN "invalid" INT8 NULL' ;
    EXECUTE 'CREATe TABLE "event_info"("id" SERIAL8  PRIMARY KEY UNIQUE,"tx_id" INT8 NULL,"epoch" word31type NOT NULL,"type" VARCHAR NOT NULL,"explanation" VARCHAR NULL)' ;
    EXECUTE 'ALTER TABLE "off_chain_pool_data" DROP CONSTRAINT "unique_off_chain_pool_data"' ;
    EXECUTE 'ALTER TABLE "off_chain_pool_data" ADD CONSTRAINT "unique_off_chain_pool_data" UNIQUE("pool_id","pmr_id")' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
