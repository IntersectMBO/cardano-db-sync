-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 38 THEN
    EXECUTE 'ALTER TABLE "ada_pots" RENAME COLUMN "deposits" TO "deposits_stake"';
    EXECUTE 'ALTER TABLE "ada_pots" ADD COLUMN "deposits_drep" lovelace NOT NULL' ;
    EXECUTE 'ALTER TABLE "ada_pots" ADD COLUMN "deposits_proposal" lovelace NOT NULL';

    EXECUTE 'ALTER TABLE "instant_reward" RENAME TO "reward_rest"';
    EXECUTE 'ALTER TABLE "reward_rest" RENAME CONSTRAINT "unique_instant_reward" TO "unique_reward_rest"';
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
