-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 33 THEN
    EXECUTE 'ALTER TABLE "drep_hash" ALTER COLUMN "raw" DROP NOT NULL' ;
    EXECUTE 'ALTER TABLE "drep_hash" DROP CONSTRAINT "unique_drep_hash"' ;
    EXECUTE 'ALTER TABLE "drep_hash" ADD CONSTRAINT "unique_drep_hash" UNIQUE("raw")' ;
    EXECUTE 'CREATe TABLE "gov_action_proposal"("id" SERIAL8  PRIMARY KEY UNIQUE,"tx_id" INT8 NOT NULL,"index" INT8 NOT NULL,"prev_gov_action_proposal" INT8 NULL,"deposit" lovelace NOT NULL,"return_address" INT8 NOT NULL,"expiration" word31type NULL,"voting_anchor_id" INT8 NULL,"type" govactiontype NOT NULL,"description" VARCHAR NOT NULL,"param_proposal" INT8 NULL,"ratified_epoch" word31type NULL,"enacted_epoch" word31type NULL,"dropped_epoch" word31type NULL,"expired_epoch" word31type NULL)' ;
    EXECUTE 'ALTER TABLE "treasury_withdrawal" ADD COLUMN "gov_action_proposal_id" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "treasury_withdrawal" DROP COLUMN "governance_action_id"' ;
    EXECUTE 'ALTER TABLE "new_committee" ADD COLUMN "gov_action_proposal_id" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "new_committee" ADD COLUMN "quorum_nominator" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "new_committee" ADD COLUMN "quorum_denominator" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "new_committee" DROP COLUMN "governance_action_id"' ;
    EXECUTE 'ALTER TABLE "new_committee" DROP COLUMN "quorum"' ;
    EXECUTE 'ALTER TABLE "voting_procedure" ADD COLUMN "gov_action_proposal_id" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "voting_procedure" DROP COLUMN "governance_action_id"' ;
    EXECUTE 'DROP TABLE "governance_action"';
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
