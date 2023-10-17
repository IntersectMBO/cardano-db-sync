-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 29 THEN
    -- if the table "pool_offline_data" all ready exists just rename everything
    IF EXISTS (SELECT 1 FROM information_schema.tables WHERE table_name = 'pool_offline_data') THEN
      EXECUTE 'ALTER TABLE "pool_offline_data" RENAME TO "off_chain_pool_data"' ;
      EXECUTE 'ALTER TABLE "off_chain_pool_data" RENAME CONSTRAINT "unique_pool_offline_data" TO "unique_off_chain_pool_data"' ;
      EXECUTE 'ALTER TABLE "off_chain_pool_data" RENAME CONSTRAINT "pool_offline_data_pkey" TO "off_chain_pool_data_pkey"' ;
    -- otherwise we create the newly named table and it's constraint "off_chain_pool_data"
    ELSE
      EXECUTE 'CREATE TABLE "off_chain_pool_data"("id" SERIAL8 PRIMARY KEY UNIQUE,"pool_id" INT8 NOT NULL,"ticker_name" VARCHAR NOT NULL,"hash" hash32type NOT NULL,"json" jsonb NOT NULL,"bytes" bytea NOT NULL,"pmr_id" INT8 NOT NULL)' ;
      EXECUTE 'ALTER TABLE "off_chain_pool_data" ADD CONSTRAINT "unique_off_chain_pool_data" UNIQUE("pool_id","hash")' ;
    END IF ;

    -- if the table "pool_offline_fetch_error" all ready exists just rename everything.
    IF EXISTS (SELECT 1 FROM information_schema.tables WHERE table_name = 'pool_offline_fetch_error') THEN
      EXECUTE 'ALTER TABLE "pool_offline_fetch_error" RENAME TO "off_chain_pool_fetch_error"' ;
      EXECUTE 'ALTER TABLE "off_chain_pool_fetch_error" RENAME CONSTRAINT "unique_pool_offline_fetch_error" TO "unique_off_chain_pool_fetch_error"' ;
      EXECUTE 'ALTER TABLE "off_chain_pool_fetch_error" RENAME CONSTRAINT "pool_offline_fetch_error_pkey" TO "off_chain_pool_fetch_error_pkey"' ;
    ELSE
      EXECUTE 'CREATE TABLE "off_chain_pool_fetch_error"("id" SERIAL8 PRIMARY KEY UNIQUE,"pool_id" INT8 NOT NULL,"fetch_time" timestamp NOT NULL,"pmr_id" INT8 NOT NULL,"fetch_error" VARCHAR NOT NULL,"retry_count" word31type NOT NULL)' ;
      EXECUTE 'ALTER TABLE "off_chain_pool_fetch_error" ADD CONSTRAINT "unique_off_chain_pool_fetch_error" UNIQUE("pool_id","fetch_time","retry_count")' ;
    END IF ;


    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
