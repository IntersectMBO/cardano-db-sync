-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 41 THEN
    EXECUTE 'CREATe TABLE "pool_stat"("id" SERIAL8  PRIMARY KEY UNIQUE,"pool_hash_id" INT8 NOT NULL,"epoch_no" word31type NOT NULL,"number_of_blocks" word64type NOT NULL,"number_of_delegators" word64type NOT NULL,"stake" word64type NOT NULL,"voting_power" word64type NULL)' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
