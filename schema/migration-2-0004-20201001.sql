-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 4 THEN
    EXECUTE 'ALTER TABLE "stake_address" DROP CONSTRAINT "unique_stake_address"' ;
    EXECUTE 'ALTER TABLE "stake_address" ADD CONSTRAINT "unique_stake_address" UNIQUE("hash_raw","registered_tx_id")' ;
    EXECUTE 'ALTER TABLE "pool_owner" DROP CONSTRAINT "unique_pool_owner"' ;
    EXECUTE 'ALTER TABLE "pool_owner" ADD CONSTRAINT "unique_pool_owner" UNIQUE("hash","pool_hash_id","registered_tx_id")' ;
    EXECUTE 'ALTER TABLE "pool_retire" DROP CONSTRAINT "unique_pool_retiring"' ;
    EXECUTE 'ALTER TABLE "pool_retire" ADD CONSTRAINT "unique_pool_retiring" UNIQUE("hash_id","announced_tx_id")' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = 4 ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
