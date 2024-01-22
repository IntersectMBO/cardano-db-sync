-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 32 THEN
    EXECUTE 'ALTER TABLE "reverse_index" ALTER COLUMN "min_ids" SET NOT NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ALTER COLUMN "epoch_no" DROP NOT NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ALTER COLUMN "key" DROP NOT NULL' ;
    EXECUTE 'CREATe TABLE "drep_hash"("id" SERIAL8  PRIMARY KEY UNIQUE,"raw" hash28type NULL,"view" VARCHAR NOT NULL,"has_script" BOOLEAN NOT NULL)' ;
    EXECUTE 'ALTER TABLE "drep_hash" ADD CONSTRAINT "unique_drep_hash" UNIQUE("raw")' ;
    EXECUTE 'CREATe TABLE "delegation_vote"("id" SERIAL8  PRIMARY KEY UNIQUE,"addr_id" INT8 NOT NULL,"cert_index" INT4 NOT NULL,"drep_hash_id" INT8 NOT NULL,"tx_id" INT8 NOT NULL,"redeemer_id" INT8 NULL)' ;
    EXECUTE 'CREATe TABLE "committee_registration"("id" SERIAL8  PRIMARY KEY UNIQUE,"tx_id" INT8 NOT NULL,"cert_index" INT4 NOT NULL,"cold_key" hash28type NOT NULL,"hot_key" hash28type NOT NULL)' ;
    EXECUTE 'CREATe TABLE "committee_de_registration"("id" SERIAL8  PRIMARY KEY UNIQUE,"tx_id" INT8 NOT NULL,"cert_index" INT4 NOT NULL,"cold_key" hash28type NOT NULL)' ;
    EXECUTE 'ALTER TABLE "committee_de_registration" ADD COLUMN "voting_anchor_id" INT8 NULL' ;
    EXECUTE 'CREATe TABLE "drep_registration"("id" SERIAL8  PRIMARY KEY UNIQUE,"tx_id" INT8 NOT NULL,"cert_index" INT4 NOT NULL,"deposit" INT8 NULL,"drep_hash_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "drep_registration" ADD COLUMN "voting_anchor_id" INT8 NULL' ;
    EXECUTE 'CREATe TABLE "voting_anchor"("id" SERIAL8  PRIMARY KEY UNIQUE,"tx_id" INT8 NOT NULL,"url" varchar NOT NULL,"data_hash" BYTEA NOT NULL)' ;
    EXECUTE 'ALTER TABLE "voting_anchor" ADD CONSTRAINT "unique_voting_anchor" UNIQUE("data_hash","url")' ;
    EXECUTE 'CREATe TABLE "gov_action_proposal"("id" SERIAL8  PRIMARY KEY UNIQUE,"tx_id" INT8 NOT NULL,"index" INT8 NOT NULL,"prev_gov_action_proposal" INT8 NULL,"deposit" lovelace NOT NULL,"return_address" INT8 NOT NULL,"expiration" word31type NULL,"voting_anchor_id" INT8 NULL,"type" govactiontype NOT NULL,"description" VARCHAR NOT NULL,"param_proposal" INT8 NULL,"ratified_epoch" word31type NULL,"enacted_epoch" word31type NULL,"dropped_epoch" word31type NULL,"expired_epoch" word31type NULL)' ;
    EXECUTE 'CREATe TABLE "treasury_withdrawal"("id" SERIAL8  PRIMARY KEY UNIQUE,"gov_action_proposal_id" INT8 NOT NULL,"stake_address_id" INT8 NOT NULL,"amount" lovelace NOT NULL)' ;
    EXECUTE 'CREATe TABLE "new_committee"("id" SERIAL8  PRIMARY KEY UNIQUE,"gov_action_proposal_id" INT8 NOT NULL,"quorum" DOUBLE PRECISION NOT NULL,"members" VARCHAR NOT NULL)' ;
    EXECUTE 'ALTER TABLE "new_committee" ADD COLUMN "deleted_members" VARCHAR NOT NULL' ;
    EXECUTE 'ALTER TABLE "new_committee" ADD COLUMN "added_members" VARCHAR NOT NULL' ;
    EXECUTE 'ALTER TABLE "new_committee" DROP COLUMN "members"' ;
    EXECUTE 'ALTER TABLE "new_committee" ADD COLUMN "quorum_numerator" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "new_committee" ADD COLUMN "quorum_denominator" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "new_committee" DROP COLUMN "quorum"' ;
    EXECUTE 'CREATe TABLE "voting_procedure"("id" SERIAL8 PRIMARY KEY UNIQUE,"tx_id" INT8 NOT NULL,"index" INT4 NOT NULL,"gov_action_proposal_id" INT8 NOT NULL,"voter_role" voterrole NOT NULL,"committee_voter" BYTEA NULL,"drep_voter" INT8 NULL,"pool_voter" INT8 NULL,"vote" vote NOT NULL,"voting_anchor_id" INT8 NULL)' ;
    EXECUTE 'CREATe TABLE "drep_distr"("id" SERIAL8  PRIMARY KEY UNIQUE,"hash_id" INT8 NOT NULL,"amount" INT8 NOT NULL,"epoch_no" word31type NOT NULL)' ;
    EXECUTE 'ALTER TABLE "drep_distr" ADD COLUMN "active_until" word31type NULL' ;
    EXECUTE 'ALTER TABLE "drep_distr" ADD CONSTRAINT "unique_drep_distr" UNIQUE("hash_id","epoch_no")' ;
    EXECUTE 'CREATe TABLE "off_chain_vote_data"("id" SERIAL8  PRIMARY KEY UNIQUE,"voting_anchor_id" INT8 NOT NULL,"hash" BYTEA NOT NULL,"json" jsonb NOT NULL,"bytes" bytea NOT NULL)' ;
    EXECUTE 'ALTER TABLE "off_chain_vote_data" ADD CONSTRAINT "unique_off_chain_vote_data" UNIQUE("voting_anchor_id","hash")' ;
    EXECUTE 'ALTER TABLE "off_chain_vote_data" ADD COLUMN "warning" VARCHAR NULL' ;
    EXECUTE 'CREATe TABLE "off_chain_vote_fetch_error"("id" SERIAL8  PRIMARY KEY UNIQUE,"voting_anchor_id" INT8 NOT NULL,"fetch_error" VARCHAR NOT NULL,"fetch_time" timestamp NOT NULL,"retry_count" word31type NOT NULL)' ;
    EXECUTE 'ALTER TABLE "off_chain_vote_fetch_error" ADD CONSTRAINT "unique_off_chain_vote_fetch_error" UNIQUE("voting_anchor_id","retry_count")' ;
    EXECUTE 'CREATe TABLE "constitution"("id" SERIAL8  PRIMARY KEY UNIQUE,"gov_action_proposal_id" INT8 NOT NULL,"voting_anchor_id" INT8 NOT NULL,"script_hash" hash28type NULL)' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
