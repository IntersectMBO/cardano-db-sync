-- Test referential integrity by temporarily adding foreign key constraints.
-- If ADD CONSTRAINT succeeds, the relationship has no dangling references.
-- The constraint is immediately dropped after a successful check.
-- If it fails, a WARNING is raised with the violation details.
--
-- Usage: psql -d <dbname> -f schema/test-referential-integrity.sql
-- Or run all tests: ./scripts/run-schema-checks.sh <dbname>
--
-- The entire script runs inside a transaction that is always rolled back,
-- so no foreign key constraints can ever remain in the database.

BEGIN;

CREATE FUNCTION check_fk(_table text, _col text, _ref_table text, _ref_col text DEFAULT 'id') RETURNS int AS $$
DECLARE
  _cname text := 'test_fk_' || _table || '_' || _col;
  _label text := _table || '.' || _col || ' -> ' || _ref_table || '.' || _ref_col;
BEGIN
  EXECUTE format('ALTER TABLE %I ADD CONSTRAINT %I FOREIGN KEY(%I) REFERENCES %I(%I)',
    _table, _cname, _col, _ref_table, _ref_col);
  EXECUTE format('ALTER TABLE %I DROP CONSTRAINT %I', _table, _cname);
  RAISE NOTICE 'PASS: %', _label;
  RETURN 1;
EXCEPTION WHEN OTHERS THEN
  RAISE WARNING 'FAIL: %: %', _label, SQLERRM;
  RETURN 0;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION test_referential_integrity() RETURNS void AS $$
DECLARE
  _total INT := 0;
  _passed INT := 0;
  _failed INT;
BEGIN

  -- =====================================================
  -- BLOCK
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('block', 'previous_id', 'block');
  _total := _total + 1; _passed := _passed + check_fk('block', 'slot_leader_id', 'slot_leader');

  -- =====================================================
  -- SLOT_LEADER
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('slot_leader', 'pool_hash_id', 'pool_hash');

  -- =====================================================
  -- TX
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('tx', 'block_id', 'block');

  -- =====================================================
  -- TX_OUT
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('tx_out', 'tx_id', 'tx');
  _total := _total + 1; _passed := _passed + check_fk('tx_out', 'stake_address_id', 'stake_address');
  _total := _total + 1; _passed := _passed + check_fk('tx_out', 'inline_datum_id', 'datum');
  _total := _total + 1; _passed := _passed + check_fk('tx_out', 'reference_script_id', 'script');

  -- =====================================================
  -- COLLATERAL_TX_OUT
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('collateral_tx_out', 'tx_id', 'tx');
  _total := _total + 1; _passed := _passed + check_fk('collateral_tx_out', 'stake_address_id', 'stake_address');
  _total := _total + 1; _passed := _passed + check_fk('collateral_tx_out', 'inline_datum_id', 'datum');
  _total := _total + 1; _passed := _passed + check_fk('collateral_tx_out', 'reference_script_id', 'script');

  -- =====================================================
  -- TX_IN
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('tx_in', 'tx_in_id', 'tx');
  _total := _total + 1; _passed := _passed + check_fk('tx_in', 'tx_out_id', 'tx');
  _total := _total + 1; _passed := _passed + check_fk('tx_in', 'redeemer_id', 'redeemer');

  -- =====================================================
  -- COLLATERAL_TX_IN
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('collateral_tx_in', 'tx_in_id', 'tx');
  _total := _total + 1; _passed := _passed + check_fk('collateral_tx_in', 'tx_out_id', 'tx');

  -- =====================================================
  -- REFERENCE_TX_IN
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('reference_tx_in', 'tx_in_id', 'tx');
  _total := _total + 1; _passed := _passed + check_fk('reference_tx_in', 'tx_out_id', 'tx');

  -- =====================================================
  -- REVERSE_INDEX
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('reverse_index', 'block_id', 'block');

  -- =====================================================
  -- TX_CBOR
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('tx_cbor', 'tx_id', 'tx');

  -- =====================================================
  -- DATUM
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('datum', 'tx_id', 'tx');

  -- =====================================================
  -- SCRIPT
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('script', 'tx_id', 'tx');

  -- =====================================================
  -- REDEEMER
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('redeemer', 'tx_id', 'tx');
  _total := _total + 1; _passed := _passed + check_fk('redeemer', 'redeemer_data_id', 'redeemer_data');

  -- =====================================================
  -- REDEEMER_DATA
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('redeemer_data', 'tx_id', 'tx');

  -- =====================================================
  -- EXTRA_KEY_WITNESS
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('extra_key_witness', 'tx_id', 'tx');

  -- =====================================================
  -- TX_METADATA
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('tx_metadata', 'tx_id', 'tx');

  -- =====================================================
  -- MA_TX_MINT
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('ma_tx_mint', 'tx_id', 'tx');
  _total := _total + 1; _passed := _passed + check_fk('ma_tx_mint', 'ident', 'multi_asset');

  -- =====================================================
  -- MA_TX_OUT
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('ma_tx_out', 'tx_out_id', 'tx_out');
  _total := _total + 1; _passed := _passed + check_fk('ma_tx_out', 'ident', 'multi_asset');

  -- =====================================================
  -- STAKE DELEGATION
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('stake_registration', 'addr_id', 'stake_address');
  _total := _total + 1; _passed := _passed + check_fk('stake_registration', 'tx_id', 'tx');
  _total := _total + 1; _passed := _passed + check_fk('stake_deregistration', 'addr_id', 'stake_address');
  _total := _total + 1; _passed := _passed + check_fk('stake_deregistration', 'tx_id', 'tx');
  _total := _total + 1; _passed := _passed + check_fk('stake_deregistration', 'redeemer_id', 'redeemer');
  _total := _total + 1; _passed := _passed + check_fk('delegation', 'addr_id', 'stake_address');
  _total := _total + 1; _passed := _passed + check_fk('delegation', 'pool_hash_id', 'pool_hash');
  _total := _total + 1; _passed := _passed + check_fk('delegation', 'tx_id', 'tx');
  _total := _total + 1; _passed := _passed + check_fk('delegation', 'redeemer_id', 'redeemer');

  -- =====================================================
  -- REWARD
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('reward', 'addr_id', 'stake_address');
  _total := _total + 1; _passed := _passed + check_fk('reward', 'pool_id', 'pool_hash');
  _total := _total + 1; _passed := _passed + check_fk('reward_rest', 'addr_id', 'stake_address');

  -- =====================================================
  -- EPOCH_STAKE
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('epoch_stake', 'addr_id', 'stake_address');
  _total := _total + 1; _passed := _passed + check_fk('epoch_stake', 'pool_id', 'pool_hash');

  -- =====================================================
  -- WITHDRAWAL
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('withdrawal', 'addr_id', 'stake_address');
  _total := _total + 1; _passed := _passed + check_fk('withdrawal', 'redeemer_id', 'redeemer');
  _total := _total + 1; _passed := _passed + check_fk('withdrawal', 'tx_id', 'tx');

  -- =====================================================
  -- TREASURY / RESERVE / POT_TRANSFER
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('treasury', 'addr_id', 'stake_address');
  _total := _total + 1; _passed := _passed + check_fk('treasury', 'tx_id', 'tx');
  _total := _total + 1; _passed := _passed + check_fk('reserve', 'addr_id', 'stake_address');
  _total := _total + 1; _passed := _passed + check_fk('reserve', 'tx_id', 'tx');
  _total := _total + 1; _passed := _passed + check_fk('pot_transfer', 'tx_id', 'tx');

  -- =====================================================
  -- POOLS
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('pool_stat', 'pool_hash_id', 'pool_hash');
  _total := _total + 1; _passed := _passed + check_fk('pool_update', 'hash_id', 'pool_hash');
  _total := _total + 1; _passed := _passed + check_fk('pool_update', 'meta_id', 'pool_metadata_ref');
  _total := _total + 1; _passed := _passed + check_fk('pool_update', 'registered_tx_id', 'tx');
  _total := _total + 1; _passed := _passed + check_fk('pool_update', 'reward_addr_id', 'stake_address');
  _total := _total + 1; _passed := _passed + check_fk('pool_metadata_ref', 'pool_id', 'pool_hash');
  _total := _total + 1; _passed := _passed + check_fk('pool_metadata_ref', 'registered_tx_id', 'tx');
  _total := _total + 1; _passed := _passed + check_fk('pool_owner', 'addr_id', 'stake_address');
  _total := _total + 1; _passed := _passed + check_fk('pool_owner', 'pool_update_id', 'pool_update');
  _total := _total + 1; _passed := _passed + check_fk('pool_retire', 'hash_id', 'pool_hash');
  _total := _total + 1; _passed := _passed + check_fk('pool_retire', 'announced_tx_id', 'tx');
  _total := _total + 1; _passed := _passed + check_fk('pool_relay', 'update_id', 'pool_update');

  -- =====================================================
  -- EPOCH / PROTOCOL PARAMS
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('ada_pots', 'block_id', 'block');
  _total := _total + 1; _passed := _passed + check_fk('epoch_param', 'cost_model_id', 'cost_model');
  _total := _total + 1; _passed := _passed + check_fk('epoch_param', 'block_id', 'block');
  _total := _total + 1; _passed := _passed + check_fk('param_proposal', 'cost_model_id', 'cost_model');
  _total := _total + 1; _passed := _passed + check_fk('param_proposal', 'registered_tx_id', 'tx');
  _total := _total + 1; _passed := _passed + check_fk('epoch_state', 'committee_id', 'committee');
  _total := _total + 1; _passed := _passed + check_fk('epoch_state', 'no_confidence_id', 'gov_action_proposal');
  _total := _total + 1; _passed := _passed + check_fk('epoch_state', 'constitution_id', 'constitution');

  -- =====================================================
  -- OFF-CHAIN DATA
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('off_chain_pool_data', 'pool_id', 'pool_hash');
  _total := _total + 1; _passed := _passed + check_fk('off_chain_pool_data', 'pmr_id', 'pool_metadata_ref');
  _total := _total + 1; _passed := _passed + check_fk('off_chain_pool_fetch_error', 'pool_id', 'pool_hash');
  _total := _total + 1; _passed := _passed + check_fk('off_chain_pool_fetch_error', 'pmr_id', 'pool_metadata_ref');
  _total := _total + 1; _passed := _passed + check_fk('off_chain_vote_data', 'voting_anchor_id', 'voting_anchor');
  _total := _total + 1; _passed := _passed + check_fk('off_chain_vote_gov_action_data', 'off_chain_vote_data_id', 'off_chain_vote_data');
  _total := _total + 1; _passed := _passed + check_fk('off_chain_vote_drep_data', 'off_chain_vote_data_id', 'off_chain_vote_data');
  _total := _total + 1; _passed := _passed + check_fk('off_chain_vote_author', 'off_chain_vote_data_id', 'off_chain_vote_data');
  _total := _total + 1; _passed := _passed + check_fk('off_chain_vote_reference', 'off_chain_vote_data_id', 'off_chain_vote_data');
  _total := _total + 1; _passed := _passed + check_fk('off_chain_vote_external_update', 'off_chain_vote_data_id', 'off_chain_vote_data');
  _total := _total + 1; _passed := _passed + check_fk('off_chain_vote_fetch_error', 'voting_anchor_id', 'voting_anchor');

  -- =====================================================
  -- GOVERNANCE (CONWAY)
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('drep_registration', 'tx_id', 'tx');
  _total := _total + 1; _passed := _passed + check_fk('drep_registration', 'drep_hash_id', 'drep_hash');
  _total := _total + 1; _passed := _passed + check_fk('drep_registration', 'voting_anchor_id', 'voting_anchor');
  _total := _total + 1; _passed := _passed + check_fk('drep_distr', 'hash_id', 'drep_hash');
  _total := _total + 1; _passed := _passed + check_fk('delegation_vote', 'addr_id', 'stake_address');
  _total := _total + 1; _passed := _passed + check_fk('delegation_vote', 'drep_hash_id', 'drep_hash');
  _total := _total + 1; _passed := _passed + check_fk('delegation_vote', 'tx_id', 'tx');
  _total := _total + 1; _passed := _passed + check_fk('delegation_vote', 'redeemer_id', 'redeemer');
  _total := _total + 1; _passed := _passed + check_fk('gov_action_proposal', 'tx_id', 'tx');
  _total := _total + 1; _passed := _passed + check_fk('gov_action_proposal', 'prev_gov_action_proposal', 'gov_action_proposal');
  _total := _total + 1; _passed := _passed + check_fk('gov_action_proposal', 'return_address', 'stake_address');
  _total := _total + 1; _passed := _passed + check_fk('gov_action_proposal', 'voting_anchor_id', 'voting_anchor');
  _total := _total + 1; _passed := _passed + check_fk('gov_action_proposal', 'param_proposal', 'param_proposal');
  _total := _total + 1; _passed := _passed + check_fk('voting_procedure', 'tx_id', 'tx');
  _total := _total + 1; _passed := _passed + check_fk('voting_procedure', 'gov_action_proposal_id', 'gov_action_proposal');
  _total := _total + 1; _passed := _passed + check_fk('voting_procedure', 'drep_voter', 'drep_hash');
  _total := _total + 1; _passed := _passed + check_fk('voting_procedure', 'pool_voter', 'pool_hash');
  _total := _total + 1; _passed := _passed + check_fk('voting_procedure', 'voting_anchor_id', 'voting_anchor');
  _total := _total + 1; _passed := _passed + check_fk('voting_procedure', 'committee_voter', 'committee_hash');
  _total := _total + 1; _passed := _passed + check_fk('voting_anchor', 'block_id', 'block');
  _total := _total + 1; _passed := _passed + check_fk('constitution', 'gov_action_proposal_id', 'gov_action_proposal');
  _total := _total + 1; _passed := _passed + check_fk('constitution', 'voting_anchor_id', 'voting_anchor');
  _total := _total + 1; _passed := _passed + check_fk('committee', 'gov_action_proposal_id', 'gov_action_proposal');
  _total := _total + 1; _passed := _passed + check_fk('committee_member', 'committee_id', 'committee');
  _total := _total + 1; _passed := _passed + check_fk('committee_member', 'committee_hash_id', 'committee_hash');
  _total := _total + 1; _passed := _passed + check_fk('committee_registration', 'tx_id', 'tx');
  _total := _total + 1; _passed := _passed + check_fk('committee_registration', 'cold_key_id', 'committee_hash');
  _total := _total + 1; _passed := _passed + check_fk('committee_registration', 'hot_key_id', 'committee_hash');
  _total := _total + 1; _passed := _passed + check_fk('committee_de_registration', 'tx_id', 'tx');
  _total := _total + 1; _passed := _passed + check_fk('committee_de_registration', 'voting_anchor_id', 'voting_anchor');
  _total := _total + 1; _passed := _passed + check_fk('committee_de_registration', 'cold_key_id', 'committee_hash');
  _total := _total + 1; _passed := _passed + check_fk('treasury_withdrawal', 'gov_action_proposal_id', 'gov_action_proposal');
  _total := _total + 1; _passed := _passed + check_fk('treasury_withdrawal', 'stake_address_id', 'stake_address');
  _total := _total + 1; _passed := _passed + check_fk('event_info', 'tx_id', 'tx');
  _total := _total + 1; _passed := _passed + check_fk('voting_procedure', 'invalid', 'event_info');

  -- =====================================================
  -- CONSUMED TX_OUT
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_fk('tx_out', 'consumed_by_tx_id', 'tx');

  -- =====================================================
  -- OPTIONAL: ADDRESS TABLE VARIANT (use_address_table)
  -- =====================================================

  IF EXISTS (SELECT 1 FROM information_schema.tables WHERE table_name = 'address') THEN
    RAISE NOTICE '';
    RAISE NOTICE 'Address table variant detected, running additional checks...';

    _total := _total + 1; _passed := _passed + check_fk('address', 'stake_address_id', 'stake_address');
    _total := _total + 1; _passed := _passed + check_fk('tx_out', 'address_id', 'address');
    _total := _total + 1; _passed := _passed + check_fk('collateral_tx_out', 'address_id', 'address');

  ELSE
    RAISE NOTICE '';
    RAISE NOTICE 'Address table variant not detected, skipping address checks.';
  END IF;

  -- =====================================================
  -- SUMMARY
  -- =====================================================

  _failed := _total - _passed;

  RAISE NOTICE '';
  RAISE NOTICE '=============================================';
  RAISE NOTICE 'Referential Integrity Test Results';
  RAISE NOTICE '=============================================';
  RAISE NOTICE 'Total:  %', _total;
  RAISE NOTICE 'Passed: %', _passed;
  RAISE NOTICE 'Failed: %', _failed;
  RAISE NOTICE '=============================================';
END;
$$ LANGUAGE plpgsql;

SELECT test_referential_integrity();

ROLLBACK;

-- Functions were rolled back with the transaction, so nothing to clean up.
