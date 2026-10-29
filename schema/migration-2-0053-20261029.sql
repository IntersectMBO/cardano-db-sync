-- EXPERIMENTAL (Dijkstra): provisional schema for an unreleased era; see the note at the
-- top of doc/schema.md. These columns may be amended or dropped by a later migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 53 THEN
    -- A Dijkstra sub-transaction is stored as an ordinary tx row; parent_tx_id points at
    -- the enclosing top-level tx, and is NULL for a top-level tx.
    EXECUTE 'ALTER TABLE "tx" ADD COLUMN "parent_tx_id" INT8 NULL' ;

    -- The Dijkstra protocol parameters, on epoch_param and param_proposal alike. The
    -- ten Leios ones (committee size and quorum threshold, the three period lengths in
    -- milliseconds, and the endorser-block limits, whose ExUnits split into mem and
    -- steps), then the reference-script limits and cost curve promoted from Conway
    -- constants, the two reference-input costs, the CIP-50 pool bounds and the six
    -- Peras ones. NULL for every pre-Dijkstra epoch, and on param_proposal also for a
    -- proposal that does not touch the field. max_pledge_leverage and
    -- peras_bootstrap_round are optional in the ledger too, so NULL cannot tell an
    -- untouched parameter from one deliberately set to no value.
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "leios_committee_size" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "leios_quorum_stake_threshold" double precision NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "leios_announcement_period_length" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "leios_vote_period_length" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "leios_diffusion_period_length" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "leios_max_eb_references_size" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "leios_max_eb_txs_size" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "leios_max_eb_ex_mem" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "leios_max_eb_ex_steps" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "leios_max_ref_script_size_per_eb" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "max_ref_script_size_per_block" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "max_ref_script_size_per_tx" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "ref_script_cost_stride" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "ref_script_cost_multiplier" double precision NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "max_pledge_leverage" double precision NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "min_pool_margin" double precision NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "ref_inputs_cost_per_multi_asset_policy" lovelace NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "ref_inputs_cost_per_datum_byte" lovelace NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "peras_min_candidate_block_age" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "peras_healing_factor" double precision NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "peras_cert_boost" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "peras_target_committee_size" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "peras_bootstrap_round" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "peras_quorum_threshold_safety_margin" double precision NULL' ;
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
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "max_ref_script_size_per_block" word64type NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "max_ref_script_size_per_tx" word64type NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "ref_script_cost_stride" word64type NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "ref_script_cost_multiplier" double precision NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "max_pledge_leverage" double precision NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "min_pool_margin" double precision NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "ref_inputs_cost_per_multi_asset_policy" lovelace NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "ref_inputs_cost_per_datum_byte" lovelace NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "peras_min_candidate_block_age" word64type NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "peras_healing_factor" double precision NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "peras_cert_boost" word64type NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "peras_target_committee_size" word64type NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "peras_bootstrap_round" word64type NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "peras_quorum_threshold_safety_margin" double precision NULL' ;

    -- The Leios certificate a Dijkstra block body may carry. Its presence is what makes
    -- the block a certifying block; block.has_leios_cert is the same fact as a flag, for
    -- cheap filtering. signers is the committee-seat bitfield and signer_count its
    -- population count. FALSE on every block before the Dijkstra era.
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "has_leios_cert" BOOLEAN NOT NULL DEFAULT FALSE' ;
    -- The BLS key a Dijkstra pool registration may declare, with its proof of possession.
    -- The key rides the registration certificate and there is no separate cert to add or
    -- update it, so both stay NULL until a pool re-registers declaring one.
    EXECUTE 'ALTER TABLE "pool_update" ADD COLUMN "bls_key" BYTEA NULL' ;
    EXECUTE 'ALTER TABLE "pool_update" ADD COLUMN "bls_key_pop" BYTEA NULL' ;

    -- A Dijkstra direct deposit: lovelace paid straight into an account with no output.
    -- The mirror of a withdrawal, minus the redeemer a deposit cannot have.
    EXECUTE 'CREATE TABLE "direct_deposit"("id" SERIAL8 PRIMARY KEY UNIQUE,"addr_id" INT8 NOT NULL,"amount" lovelace NOT NULL,"tx_id" INT8 NOT NULL)' ;
    EXECUTE 'CREATE TABLE "leios_cert"("id" SERIAL8 PRIMARY KEY UNIQUE,"block_id" INT8 NOT NULL,"signer_count" word31type NOT NULL,"signers" BYTEA NOT NULL,"signature" BYTEA NOT NULL)' ;
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
