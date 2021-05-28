-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 2 THEN
    EXECUTE 'ALTER TABLE "reward" DROP CONSTRAINT "unique_reward"' ;
    EXECUTE 'ALTER TABLE "reward" ADD CONSTRAINT "unique_reward" UNIQUE("epoch_no","addr_id","pool_id")' ;
    EXECUTE 'ALTER TABLE "orphaned_reward" DROP CONSTRAINT "unique_orphaned"' ;
    EXECUTE 'ALTER TABLE "orphaned_reward" ADD CONSTRAINT "unique_orphaned" UNIQUE("epoch_no","addr_id","pool_id")' ;
    EXECUTE 'ALTER TABLE "epoch_stake" DROP CONSTRAINT "unique_stake"' ;
    EXECUTE 'ALTER TABLE "epoch_stake" ADD CONSTRAINT "unique_stake" UNIQUE("epoch_no","addr_id","pool_id")' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
