-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 29 THEN
    EXECUTE 'ALTER TABLE "instant_reward" ALTER COLUMN "id" SET NOT NULL' ;
    EXECUTE 'ALTER TABLE "instant_reward" DROP COLUMN id';
    EXECUTE 'ALTER TABLE "instant_reward" ALTER COLUMN "addr_id" SET NOT NULL' ;
    EXECUTE 'ALTER TABLE "instant_reward" ALTER COLUMN "type" SET NOT NULL' ;
    EXECUTE 'ALTER TABLE "instant_reward" ALTER COLUMN "amount" SET NOT NULL' ;
    EXECUTE 'ALTER TABLE "instant_reward" ALTER COLUMN "spendable_epoch" SET NOT NULL' ;
    EXECUTE 'ALTER TABLE "instant_reward" DROP COLUMN "earned_epoch"' ;
    EXECUTE 'ALTER TABLE "instant_reward" ADD COLUMN "earned_epoch" bigint NOT NULL GENERATED ALWAYS AS ((CASE WHEN spendable_epoch >= 1 then spendable_epoch-1 else 0 end)) STORED' ;
    EXECUTE 'ALTER TABLE "instant_reward" DROP COLUMN "pool_id"' ;
    EXECUTE 'ALTER TABLE "instant_reward" ADD CONSTRAINT "unique_instant_reward" UNIQUE("addr_id","earned_epoch","type")' ;

    EXECUTE 'DELETE FROM "reward" WHERE "pool_id" IS NULL' ;
    EXECUTE 'ALTER TABLE "reward" ALTER COLUMN "pool_id" SET NOT NULL' ;
    EXECUTE 'ALTER TABLE "reward" DROP COLUMN id';
    EXECUTE 'ALTER TABLE "reward" DROP COLUMN "earned_epoch"' ;
    ALTER TABLE "reward" ADD COLUMN "earned_epoch" bigint NOT NULL GENERATED ALWAYS AS
      (CASE WHEN (type='refund') then spendable_epoch else (CASE WHEN spendable_epoch >= 2 then spendable_epoch-2 else 0 end) end) STORED;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
