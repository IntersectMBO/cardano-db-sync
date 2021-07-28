-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 14 THEN
    EXECUTE 'ALTER TABLE "reward" DROP CONSTRAINT "unique_reward"' ;
    EXECUTE 'ALTER TABLE "orphaned_reward" DROP CONSTRAINT "unique_orphaned"' ;
    EXECUTE 'ALTER TABLE "reward" ADD COLUMN "earned_epoch" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "reward" ADD COLUMN "spendable_epoch" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "reward" ALTER COLUMN "pool_id" DROP NOT NULL' ;
    EXECUTE 'ALTER TABLE "reward" DROP COLUMN "epoch_no"' ;
    EXECUTE 'ALTER TABLE "reward" ADD CONSTRAINT "unique_reward" UNIQUE("addr_id","type","amount","earned_epoch")' ;
    EXECUTE 'ALTER TABLE "orphaned_reward" ALTER COLUMN "pool_id" DROP NOT NULL' ;
    EXECUTE 'ALTER TABLE "orphaned_reward" ADD CONSTRAINT "unique_orphaned" UNIQUE("addr_id","type","amount","epoch_no")' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
