-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 27 THEN
    EXECUTE 'ALTER TABLE "reverse_index" ALTER COLUMN "min_ids" SET NOT NULL' ;
    EXECUTE 'ALTER TABLE "delegation" ADD COLUMN "deposit" lovelace NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ALTER COLUMN "epoch_no" DROP NOT NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ALTER COLUMN "key" DROP NOT NULL' ;
    EXECUTE 'CREATe TABLE "drep_hash"("id" SERIAL8  PRIMARY KEY UNIQUE,"raw" hash28type NULL,"view" VARCHAR NULL,"has_script" BOOLEAN NOT NULL)' ;
    EXECUTE 'ALTER TABLE "drep_hash" ADD CONSTRAINT "unique_drep_hash" UNIQUE("raw","view")' ;
    EXECUTE 'CREATe TABLE "delegation_vote"("id" SERIAL8  PRIMARY KEY UNIQUE,"addr_id" INT8 NOT NULL,"cert_index" INT4 NOT NULL,"drep_hash_id" INT8 NOT NULL,"active_epoch_no" INT8 NOT NULL,"tx_id" INT8 NOT NULL,"redeemer_id" INT8 NULL)' ;
    EXECUTE 'CREATe TABLE "committee_registration"("id" SERIAL8  PRIMARY KEY UNIQUE,"tx_id" INT8 NOT NULL,"cert_index" INT4 NOT NULL,"cold_key" addr29type NOT NULL,"hot_key" addr29type NOT NULL)' ;
    EXECUTE 'CREATe TABLE "committee_de_registration"("id" SERIAL8  PRIMARY KEY UNIQUE,"tx_id" INT8 NOT NULL,"cert_index" INT4 NOT NULL,"hot_key" addr29type NOT NULL)' ;
    EXECUTE 'CREATe TABLE "drep_registration"("id" SERIAL8  PRIMARY KEY UNIQUE,"tx_id" INT8 NOT NULL,"cert_index" INT4 NOT NULL,"deposit" lovelace NOT NULL,"drep_hash_id" INT8 NOT NULL)' ;
    EXECUTE 'CREATe TABLE "drep_de_registration"("id" SERIAL8  PRIMARY KEY UNIQUE,"tx_id" INT8 NOT NULL,"cert_index" INT4 NOT NULL,"deposit" lovelace NOT NULL,"drep_hash_id" INT8 NOT NULL)' ;
    EXECUTE 'CREATe TABLE "voting_anchor"("id" SERIAL8  PRIMARY KEY UNIQUE,"tx_id" INT8 NOT NULL,"url" varchar NOT NULL,"data_hash" BYTEA NOT NULL)' ;
    EXECUTE 'CREATe TABLE "governance_action"("id" SERIAL8  PRIMARY KEY UNIQUE,"tx_id" INT8 NOT NULL,"index" INT8 NOT NULL,"deposit" lovelace NOT NULL,"return_address" INT8 NOT NULL,"voting_anchor_id" INT8 NULL,"type" govactiontype NOT NULL,"description" VARCHAR NOT NULL,"param_proposal" INT8 NULL,"ratified_epoch" word31type NULL,"enacted_epoch" word31type NULL,"dropped_epoch" word31type NULL,"expired_epoch" word31type NULL)' ;
    EXECUTE 'CREATe TABLE "treasury_withdrawal"("id" SERIAL8  PRIMARY KEY UNIQUE,"governance_action_id" INT8 NOT NULL,"stake_address_id" INT8 NOT NULL,"amount" lovelace NOT NULL)' ;
    EXECUTE 'CREATe TABLE "new_committee"("id" SERIAL8  PRIMARY KEY UNIQUE,"governance_action_id" INT8 NOT NULL,"quorum" DOUBLE PRECISION NOT NULL,"members" VARCHAR NOT NULL)' ;
    EXECUTE 'CREATe TABLE "voting_procedure"("id" SERIAL8  PRIMARY KEY UNIQUE,"tx_id" INT8 NOT NULL,"index" INT4 NOT NULL,"governance_action_id" INT8 NOT NULL,"voter_role" voterrole NOT NULL,"comittee_voter" BYTEA NULL,"drep_voter" INT8 NULL,"pool_voter" INT8 NULL,"vote" vote NOT NULL,"voting_anchor_id" INT8 NULL)' ;
    EXECUTE 'CREATe TABLE "anchor_offline_data"("id" SERIAL8  PRIMARY KEY UNIQUE,"voting_anchor_id" INT8 NOT NULL,"hash" BYTEA NOT NULL,"json" jsonb NOT NULL,"bytes" bytea NOT NULL)' ;
    EXECUTE 'CREATe TABLE "anchor_offline_fetch_error"("id" SERIAL8  PRIMARY KEY UNIQUE,"voting_anchor_id" INT8 NOT NULL,"fetch_error" VARCHAR NOT NULL,"retry_count" word31type NOT NULL)' ;
    EXECUTE 'CREATe TABLE "drep_distr"("id" SERIAL8  PRIMARY KEY UNIQUE,"hash_id" INT8 NOT NULL,"amount" INT8 NOT NULL,"epoch_no" word31type NOT NULL)' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
