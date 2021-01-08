-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 6 THEN
    EXECUTE 'CREATe TABLE "orphaned_reward"("id" SERIAL8  PRIMARY KEY UNIQUE,"addr_id" INT8 NOT NULL,"amount" lovelace NOT NULL,"epoch_no" INT8 NOT NULL,"pool_id" INT8 NOT NULL,"block_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "orphaned_reward" ADD CONSTRAINT "unique_orphaned" UNIQUE("addr_id","block_id")' ;
    EXECUTE 'ALTER TABLE "orphaned_reward" ADD CONSTRAINT "orphaned_reward_addr_id_fkey" FOREIGN KEY("addr_id") REFERENCES "stake_address"("id") ON DELETE RESTRICT  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "orphaned_reward" ADD CONSTRAINT "orphaned_reward_pool_id_fkey" FOREIGN KEY("pool_id") REFERENCES "pool_hash"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "orphaned_reward" ADD CONSTRAINT "orphaned_reward_block_id_fkey" FOREIGN KEY("block_id") REFERENCES "block"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
