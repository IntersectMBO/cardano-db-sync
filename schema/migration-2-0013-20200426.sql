-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 13 THEN
    EXECUTE 'CREATE TABLE "pool_on_data"("id" SERIAL8 PRIMARY KEY UNIQUE, "pool_url" VARCHAR NOT NULL, "pool_hash" hash32type NOT NULL, "tx_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "pool_on_data" ADD CONSTRAINT "unique_pool_on_data" UNIQUE("pool_hash")' ;
    EXECUTE 'ALTER TABLE "pool_on_data" ADD CONSTRAINT "pool_on_data_tx_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id")' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = 13 ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
