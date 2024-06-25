-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 41 THEN
    EXECUTE 'ALTER TABLE "tx_metadata" ALTER COLUMN "json" TYPE VARCHAR' ;
    EXECUTE 'ALTER TABLE "script" ALTER COLUMN "json" TYPE VARCHAR' ;
    EXECUTE 'ALTER TABLE "datum" ALTER COLUMN "value" TYPE VARCHAR' ;
    EXECUTE 'ALTER TABLE "redeemer_data" ALTER COLUMN "value" TYPE VARCHAR' ;
    EXECUTE 'ALTER TABLE "cost_model" ALTER COLUMN "costs" TYPE VARCHAR' ;
    EXECUTE 'ALTER TABLE "gov_action_proposal" ALTER COLUMN "description" TYPE VARCHAR' ;
    EXECUTE 'ALTER TABLE "off_chain_pool_data" ALTER COLUMN "json" TYPE VARCHAR' ;
    EXECUTE 'ALTER TABLE "off_chain_vote_data" ALTER COLUMN "json" TYPE VARCHAR' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
