-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 20 THEN
    EXECUTE 'CREATe TABLE "epoch_reward_total_received"("id" SERIAL8  PRIMARY KEY UNIQUE,"earned_epoch" uinteger NOT NULL,"amount" lovelace NOT NULL)' ;
    EXECUTE 'ALTER TABLE "epoch_reward_total_received" ADD CONSTRAINT "unique_epoch_reward_total_received" UNIQUE("earned_epoch")' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
