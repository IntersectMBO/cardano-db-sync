-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 37 THEN
    EXECUTE 'CREATe TABLE instant_reward AS TABLE reward WITH NO DATA';
    EXECUTE 'INSERT INTO instant_reward (SELECT * FROM reward WHERE pool_id IS NULL)';
--    EXECUTE 'ALTER TABLE "instant_reward"("id" SERIAL8  PRIMARY KEY UNIQUE,"addr_id" INT8 NOT NULL,"type" rewardtype NOT NULL,"amount" lovelace NOT NULL,"earned_epoch" INT8 NOT NULL GENERATED ALWAYS AS ((CASE WHEN spendable_epoch >= 1 then spendable_epoch-1 else 0 end)) STORED,"spendable_epoch" INT8 NOT NULL)' ;
--    EXECUTE 'ALTER TABLE "instant_reward" ADD CONSTRAINT "unique_instant_reward" UNIQUE("addr_id","earned_epoch","type")' ;
    -- EXECUTE 'ALTER TABLE "reward" ALTER COLUMN "pool_id" SET NOT NULL' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
