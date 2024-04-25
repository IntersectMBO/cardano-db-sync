-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 35 THEN
    EXECUTE 'ALTER TABLE "drep_hash" DROP CONSTRAINT "unique_drep_hash"' ;
    EXECUTE 'ALTER TABLE "drep_hash" ADD CONSTRAINT "unique_drep_hash" UNIQUE("raw","has_script")' ;
    EXECUTE 'CREATe TABLE "committee_hash"("id" SERIAL8  PRIMARY KEY UNIQUE,"raw" hash28type NOT NULL,"has_script" BOOLEAN NOT NULL)' ;
    EXECUTE 'ALTER TABLE "committee_hash" ADD CONSTRAINT "unique_committee_hash" UNIQUE("raw","has_script")' ;
    EXECUTE 'ALTER TABLE "committee_registration" ADD COLUMN "cold_key_id" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "committee_registration" ADD COLUMN "hot_key_id" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "committee_registration" DROP COLUMN "cold_key"' ;
    EXECUTE 'ALTER TABLE "committee_registration" DROP COLUMN "hot_key"' ;
    EXECUTE 'ALTER TABLE "committee_de_registration" ADD COLUMN "cold_key_id" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "committee_de_registration" DROP COLUMN "cold_key"' ;
    EXECUTE 'CREATe TABLE "new_committee_info"("id" SERIAL8  PRIMARY KEY UNIQUE,"gov_action_proposal_id" INT8 NOT NULL,"quorum_numerator" INT8 NOT NULL,"quorum_denominator" INT8 NOT NULL)' ;
    EXECUTE 'CREATe TABLE "new_committee_member"("id" SERIAL8  PRIMARY KEY UNIQUE,"gov_action_proposal_id" INT8 NOT NULL,"committee_hash_id" INT8 NOT NULL,"expiration_epoch" word31type NOT NULL)' ;
    EXECUTE 'ALTER TABLE "voting_procedure" DROP COLUMN "committee_voter"';
    EXECUTE 'ALTER TABLE "voting_procedure" ADD COLUMN "committee_voter" INT8 NULL' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
