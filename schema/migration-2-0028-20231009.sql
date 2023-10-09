-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 28 THEN
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "pvt_motion_no_confidence" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "pvt_committee_normal" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "pvt_committee_no_confidence" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "pvt_hard_fork_initiation" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "dvt_motion_no_confidence" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "dvt_committee_normal" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "dvt_committee_no_confidence" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "dvt_update_to_constitution" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "dvt_hard_fork_initiation" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "dvt_p_p_network_group" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "dvt_p_p_economic_group" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "dvt_p_p_technical_group" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "dvt_p_p_gov_group" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "dvt_treasury_withdrawal" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "committee_min_size" word64type NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "committee_max_term_length" word64type NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "gov_action_lifetime" word64type NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "gov_action_deposit" word64type NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "drep_deposit" word64type NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "drep_activity" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "pvt_motion_no_confidence" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "pvt_committee_normal" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "pvt_committee_no_confidence" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "pvt_hard_fork_initiation" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "dvt_motion_no_confidence" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "dvt_committee_normal" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "dvt_committee_no_confidence" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "dvt_update_to_constitution" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "dvt_hard_fork_initiation" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "dvt_p_p_network_group" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "dvt_p_p_economic_group" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "dvt_p_p_technical_group" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "dvt_p_p_gov_group" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "dvt_treasury_withdrawal" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "committee_min_size" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "committee_max_term_length" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "gov_action_lifetime" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "gov_action_deposit" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "drep_deposit" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "drep_activity" word64type NULL' ;
    EXECUTE 'ALTER TABLE "drep_hash" ALTER COLUMN "raw" SET NOT NULL' ;
    EXECUTE 'ALTER TABLE "drep_hash" ALTER COLUMN "view" SET NOT NULL' ;
    EXECUTE 'ALTER TABLE "delegation_vote" DROP COLUMN "active_epoch_no"' ;
    EXECUTE 'ALTER TABLE "drep_registration" ALTER COLUMN "deposit" DROP NOT NULL' ;
    EXECUTE 'ALTER TABLE "drep_registration" ALTER COLUMN "deposit" TYPE INT8' ;
    EXECUTE 'ALTER TABLE "drep_registration" ADD COLUMN "voting_anchor_id" INT8 NULL' ;
    EXECUTE 'ALTER TABLE "voting_anchor" ADD CONSTRAINT "unique_voting_anchor" UNIQUE("data_hash","url")' ;
    EXECUTE 'ALTER TABLE "governance_action" ADD COLUMN "expiration" word31type NULL' ;
    EXECUTE 'ALTER TABLE "new_committee" ADD COLUMN "deleted_members" VARCHAR NOT NULL' ;
    EXECUTE 'ALTER TABLE "new_committee" ADD COLUMN "added_members" VARCHAR NOT NULL' ;
    EXECUTE 'ALTER TABLE "new_committee" DROP COLUMN "members"' ;
    EXECUTE 'ALTER TABLE "drep_distr" ADD COLUMN "active_until" word31type NULL' ;
    EXECUTE 'ALTER TABLE "drep_distr" ADD CONSTRAINT "unique_drep_distr" UNIQUE("hash_id","epoch_no")' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
