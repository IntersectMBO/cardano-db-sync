-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 34 THEN
    EXECUTE 'CREATe TABLE "constitution"("id" SERIAL8  PRIMARY KEY UNIQUE,"gov_action_proposal_id" INT8 NOT NULL,"voting_anchor_id" INT8 NOT NULL,"script_hash" hash28type NULL)' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
