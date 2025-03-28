-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 44 THEN
    -- EXECUTE 'ALTER TABLE "reward" ADD COLUMN "id" INT8 NOT NULL' ;
    -- EXECUTE 'ALTER TABLE "reward_rest" ADD COLUMN "id" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "tx_out" ADD COLUMN IF NOT EXISTS "consumed_by_tx_id" INT8 NULL' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
