-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 41 THEN
    EXECUTE 'ALTER TABLE "voting_anchor" ADD COLUMN "block_id" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "voting_anchor" DROP COLUMN "tx_id"' ;
    EXECUTE 'ALTER TABLE "constitution" ALTER COLUMN "gov_action_proposal_id" DROP NOT NULL' ;
    EXECUTE 'CREATe TABLE "epoch_state"("id" SERIAL8  PRIMARY KEY UNIQUE,"committee_id" INT8 NULL,"no_confidence_id" INT8 NULL,"constitution_id" INT8 NULL,"epoch_no" word31type NOT NULL)' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
