-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 9 THEN
    EXECUTE 'ALTER TABLE "stake_address" ADD CONSTRAINT "stake_address_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "pool_update" ADD CONSTRAINT "pool_update_reward_addr_id_fkey" FOREIGN KEY("reward_addr_id") REFERENCES "stake_address"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "pool_owner" ADD CONSTRAINT "pool_owner_pool_update_id_fkey" FOREIGN KEY("pool_update_id") REFERENCES "pool_update"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "treasury" DROP CONSTRAINT "unique_treasury"' ;
    EXECUTE 'ALTER TABLE "treasury" ADD CONSTRAINT "unique_treasury" UNIQUE("addr_id","tx_id","cert_index")' ;
    EXECUTE 'ALTER TABLE "reserve" DROP CONSTRAINT "unique_reserves"' ;
    EXECUTE 'ALTER TABLE "reserve" ADD CONSTRAINT "unique_reserves" UNIQUE("addr_id","tx_id","cert_index")' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
