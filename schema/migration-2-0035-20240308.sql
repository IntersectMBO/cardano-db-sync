-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 35 THEN
    EXECUTE 'ALTER TABLE "gov_action_proposal" ALTER COLUMN "description" TYPE jsonb USING description::jsonb' ;
    EXECUTE 'ALTER TABLE "drep_hash" DROP CONSTRAINT "unique_drep_hash"' ;
    EXECUTE 'ALTER TABLE "drep_hash" ADD CONSTRAINT "unique_drep_hash" UNIQUE("raw","has_script")' ;
    EXECUTE 'ALTER TABLE "committee_registration" ADD COLUMN "cold_key_id" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "committee_registration" ADD COLUMN "hot_key_id" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "committee_registration" DROP COLUMN "cold_key"' ;
    EXECUTE 'ALTER TABLE "committee_registration" DROP COLUMN "hot_key"' ;
    EXECUTE 'ALTER TABLE "committee_de_registration" ADD COLUMN "cold_key_id" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "committee_de_registration" DROP COLUMN "cold_key"' ;
    EXECUTE 'ALTER TABLE "voting_procedure" DROP COLUMN "committee_voter"';
    EXECUTE 'ALTER TABLE "voting_procedure" ADD COLUMN "committee_voter" INT8 NULL' ;
    EXECUTE 'ALTER TABLE "voting_anchor" ADD COLUMN "type" anchorType NOT NULL' ;
    EXECUTE 'ALTER TABLE "voting_anchor" DROP CONSTRAINT "unique_voting_anchor"' ;
    EXECUTE 'ALTER TABLE "voting_anchor" ADD CONSTRAINT "unique_voting_anchor" UNIQUE("data_hash","url","type")' ;
    EXECUTE 'ALTER TABLE "off_chain_vote_data" ADD COLUMN "language" VARCHAR NOT NULL' ;
    EXECUTE 'ALTER TABLE "off_chain_vote_data" ADD COLUMN "comment" VARCHAR NULL' ;
    EXECUTE 'ALTER TABLE "off_chain_vote_data" ADD COLUMN "title" VARCHAR NULL' ;
    EXECUTE 'ALTER TABLE "off_chain_vote_data" ADD COLUMN "abstract" VARCHAR NULL' ;
    EXECUTE 'ALTER TABLE "off_chain_vote_data" ADD COLUMN "motivation" VARCHAR NULL' ;
    EXECUTE 'ALTER TABLE "off_chain_vote_data" ADD COLUMN "rationale" VARCHAR NULL' ;
    EXECUTE 'ALTER TABLE "off_chain_vote_data" ADD COLUMN "is_valid" BOOLEAN NULL' ;
    EXECUTE 'ALTER TABLE "voting_anchor" ADD COLUMN "block_id" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "voting_anchor" DROP COLUMN "tx_id"' ;
    EXECUTE 'ALTER TABLE "constitution" ALTER COLUMN "gov_action_proposal_id" DROP NOT NULL' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
