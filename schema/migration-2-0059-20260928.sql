-- The Leios (Dijkstra) protocol parameters as they appear in governance parameter-change
-- proposals: committee size, quorum stake threshold, the announcement/vote/diffusion periods,
-- and the endorser-block limits. Recorded on param_proposal; NULL for pre-Dijkstra proposals
-- and for proposals that do not touch these fields.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 59 THEN
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "leios_committee_size" word64type NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "leios_quorum_stake_threshold" double precision NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "leios_announcement_period_length" word64type NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "leios_vote_period_length" word64type NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "leios_diffusion_period_length" word64type NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "leios_max_eb_references_size" word64type NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "leios_max_eb_txs_size" word64type NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "leios_max_eb_ex_mem" word64type NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "leios_max_eb_ex_steps" word64type NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "leios_max_ref_script_size_per_eb" word64type NULL' ;
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
