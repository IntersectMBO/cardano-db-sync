-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 8 THEN
    EXECUTE 'ALTER TABLE "stake_address" ADD COLUMN "tx_id" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "stake_address" DROP COLUMN "registered_tx_id"' ;
    EXECUTE 'ALTER TABLE "pool_update" ADD COLUMN "reward_addr_id" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "pool_update" DROP COLUMN "reward_addr"' ;
    EXECUTE 'ALTER TABLE "pool_owner" ADD COLUMN "pool_update_id" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "pool_owner" DROP COLUMN "pool_hash_id"' ;
    EXECUTE 'ALTER TABLE "pool_owner" DROP COLUMN "registered_tx_id"' ;
    EXECUTE 'ALTER TABLE "pool_owner" ADD CONSTRAINT "unique_pool_owner" UNIQUE("addr_id","pool_update_id")' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
