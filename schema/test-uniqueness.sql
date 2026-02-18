-- Test uniqueness by temporarily adding UNIQUE constraints.
-- If ADD CONSTRAINT succeeds, there are no duplicates for that combination.
-- The constraint is immediately dropped after a successful check.
-- If it fails, a WARNING is raised with the violation details.
--
-- Usage: psql -d <dbname> -f schema/test-uniqueness.sql
-- Or run all tests: ./scripts/run-schema-checks.sh <dbname>
--
-- The entire script runs inside a transaction that is always rolled back,
-- so no unique constraints can ever remain in the database.

BEGIN;

CREATE FUNCTION check_unique(_table text, VARIADIC _cols text[]) RETURNS int AS $$
DECLARE
  _cname text := 'test_unique_' || _table;
  _col_list text;
  _label text;
BEGIN
  SELECT string_agg(format('%I', c), ', ') INTO _col_list FROM unnest(_cols) AS c;
  _label := _table || '(' || array_to_string(_cols, ', ') || ')';
  EXECUTE format('ALTER TABLE %I ADD CONSTRAINT %I UNIQUE(', _table, _cname) || _col_list || ')';
  EXECUTE format('ALTER TABLE %I DROP CONSTRAINT %I', _table, _cname);
  RAISE NOTICE 'PASS: %', _label;
  RETURN 1;
EXCEPTION WHEN OTHERS THEN
  RAISE WARNING 'FAIL: %: %', _label, SQLERRM;
  RETURN 0;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION test_uniqueness() RETURNS void AS $$
DECLARE
  _total INT := 0;
  _passed INT := 0;
  _failed INT;
BEGIN

  -- =====================================================
  -- BLOCK / TX (dropped in migration-2-0021)
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_unique('tx_in', 'tx_out_id', 'tx_out_index');
  _total := _total + 1; _passed := _passed + check_unique('ada_pots', 'block_id');
  _total := _total + 1; _passed := _passed + check_unique('collateral_tx_out', 'tx_id', 'index');
  _total := _total + 1; _passed := _passed + check_unique('collateral_tx_in', 'tx_in_id', 'tx_out_id', 'tx_out_index');
  _total := _total + 1; _passed := _passed + check_unique('reference_tx_in', 'tx_in_id', 'tx_out_id', 'tx_out_index');
  _total := _total + 1; _passed := _passed + check_unique('tx_metadata', 'key', 'tx_id');
  _total := _total + 1; _passed := _passed + check_unique('redeemer', 'tx_id', 'purpose', 'index');

  -- =====================================================
  -- MULTI ASSETS (dropped in migration-2-0021)
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_unique('ma_tx_mint', 'ident', 'tx_id');
  _total := _total + 1; _passed := _passed + check_unique('ma_tx_out', 'ident', 'tx_out_id');

  -- =====================================================
  -- STAKE DELEGATION (dropped in migration-2-0021)
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_unique('stake_registration', 'tx_id', 'cert_index');
  _total := _total + 1; _passed := _passed + check_unique('stake_deregistration', 'tx_id', 'cert_index');
  _total := _total + 1; _passed := _passed + check_unique('delegation', 'tx_id', 'cert_index');
  _total := _total + 1; _passed := _passed + check_unique('withdrawal', 'addr_id', 'tx_id');

  -- =====================================================
  -- TREASURY / RESERVE / POT_TRANSFER (dropped in migration-2-0021)
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_unique('treasury', 'tx_id', 'cert_index');
  _total := _total + 1; _passed := _passed + check_unique('reserve', 'tx_id', 'cert_index');
  _total := _total + 1; _passed := _passed + check_unique('pot_transfer', 'tx_id', 'cert_index');

  -- =====================================================
  -- POOLS (dropped in migration-2-0021)
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_unique('pool_update', 'registered_tx_id', 'cert_index');
  _total := _total + 1; _passed := _passed + check_unique('pool_retire', 'announced_tx_id', 'cert_index');
  _total := _total + 1; _passed := _passed + check_unique('pool_relay', 'update_id', 'ipv4', 'ipv6', 'dns_name');
  _total := _total + 1; _passed := _passed + check_unique('pool_owner', 'addr_id', 'pool_update_id');

  -- =====================================================
  -- EPOCH / PROTOCOL PARAMS (dropped in migration-2-0021)
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_unique('param_proposal', 'key', 'registered_tx_id');
  _total := _total + 1; _passed := _passed + check_unique('epoch_param', 'epoch_no', 'block_id');

  -- =====================================================
  -- REWARDS (dropped in migration-2-0034)
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_unique('reward', 'addr_id', 'type', 'earned_epoch', 'pool_id');
  _total := _total + 1; _passed := _passed + check_unique('epoch_stake', 'addr_id', 'pool_id', 'epoch_no');

  -- =====================================================
  -- GOVERNANCE / CONWAY ERA (created without unique constraints)
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_unique('drep_hash', 'raw', 'has_script');
  _total := _total + 1; _passed := _passed + check_unique('committee_hash', 'raw', 'has_script');
  _total := _total + 1; _passed := _passed + check_unique('voting_anchor', 'data_hash', 'url', 'type');
  _total := _total + 1; _passed := _passed + check_unique('drep_distr', 'hash_id', 'epoch_no');

  -- =====================================================
  -- EPOCH STAKE PROGRESS (created in migration-2-0027)
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_unique('epoch_stake_progress', 'epoch_no');

  -- =====================================================
  -- OFF-CHAIN DATA (created without unique constraints)
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_unique('off_chain_pool_data', 'pool_id', 'pmr_id');
  _total := _total + 1; _passed := _passed + check_unique('off_chain_pool_fetch_error', 'pool_id', 'fetch_time', 'retry_count');
  _total := _total + 1; _passed := _passed + check_unique('off_chain_vote_data', 'hash', 'voting_anchor_id');
  _total := _total + 1; _passed := _passed + check_unique('off_chain_vote_fetch_error', 'voting_anchor_id', 'retry_count');

  -- =====================================================
  -- OPTIONAL: ADDRESS TABLE VARIANT (use_address_table)
  -- =====================================================

  IF EXISTS (SELECT 1 FROM information_schema.tables WHERE table_name = 'address') THEN
    RAISE NOTICE '';
    RAISE NOTICE 'Address table variant detected, running additional checks...';

    _total := _total + 1; _passed := _passed + check_unique('address', 'address');

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
  RAISE NOTICE 'Uniqueness Test Results';
  RAISE NOTICE '=============================================';
  RAISE NOTICE 'Total:  %', _total;
  RAISE NOTICE 'Passed: %', _passed;
  RAISE NOTICE 'Failed: %', _failed;
  RAISE NOTICE '=============================================';
END;
$$ LANGUAGE plpgsql;

SELECT test_uniqueness();

ROLLBACK;

-- Functions were rolled back with the transaction, so nothing to clean up.
