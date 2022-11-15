-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 16 THEN
    EXECUTE 'ALTER TABLE "cost_model" ADD COLUMN "hash" hash32type NOT NULL' ;
    EXECUTE 'ALTER TABLE "cost_model" DROP CONSTRAINT "unique_cost_model"' ;
    EXECUTE 'ALTER TABLE "cost_model" ADD CONSTRAINT "unique_cost_model" UNIQUE("hash")' ;
    EXECUTE 'DROP TABLE epoch_reward_total_received';
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
