-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 43 THEN
    EXECUTE 'CREATe TABLE "committee"("id" SERIAL8  PRIMARY KEY UNIQUE,"gov_action_proposal_id" INT8 NULL,"quorum_numerator" INT8 NOT NULL,"quorum_denominator" INT8 NOT NULL)' ;
    EXECUTE 'CREATe TABLE "committee_member"("id" SERIAL8  PRIMARY KEY UNIQUE,"committee_id" INT8 NOT NULL,"committee_hash_id" INT8 NOT NULL,"expiration_epoch" word31type NOT NULL)' ;
    EXECUTE 'ALTER TABLE "committee_member" ADD CONSTRAINT "committee_member_committee_id_fkey" FOREIGN KEY("committee_id") REFERENCES "committee"("id") ON DELETE RESTRICT  ON UPDATE RESTRICT' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
