-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 30 THEN
    EXECUTE 'ALTER TABLE "pool_offline_data" RENAME TO "off_chain_pool_data"' ;
    EXECUTE 'ALTER SEQUENCE "pool_offline_data_id_seq" RENAME TO "off_chain_pool_data_id_seq"' ;
    EXECUTE 'ALTER TABLE "off_chain_pool_data" RENAME CONSTRAINT "unique_pool_offline_data" TO "unique_off_chain_pool_data"' ;
    EXECUTE 'ALTER TABLE "off_chain_pool_data" RENAME CONSTRAINT "pool_offline_data_pkey" TO "off_chain_pool_data_pkey"' ;

    EXECUTE 'ALTER TABLE "pool_offline_fetch_error" RENAME TO "off_chain_pool_fetch_error"' ;
    EXECUTE 'ALTER SEQUENCE "pool_offline_fetch_error_id_seq" RENAME TO "off_chain_pool_fetch_error_id_seq"' ;
    EXECUTE 'ALTER TABLE "off_chain_pool_fetch_error" RENAME CONSTRAINT "unique_pool_offline_fetch_error" TO "unique_off_chain_pool_fetch_error"' ;
    EXECUTE 'ALTER TABLE "off_chain_pool_fetch_error" RENAME CONSTRAINT "pool_offline_fetch_error_pkey" TO "off_chain_pool_fetch_error_pkey"' ;

    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
