-- Test uniqueness: for each listed column combination, count the key values that occur
-- in more than one row. Rows with a NULL in any key column are skipped, as a UNIQUE
-- constraint would allow them. A FAIL is raised as a WARNING with the number of such keys.
--
-- Usage: psql -d <dbname> -f scripts/test-uniqueness.sql
-- Or run all tests: ./scripts/run-schema-checks.sh <dbname>
-- Exits non-zero (psql exit code 3) if any check fails, so it can gate a release or CI.
--
-- The checks are plain SELECTs, so they do not block db-sync's writes or other readers.
-- Like any query they hold ACCESS SHARE locks until the script ends, so DDL that needs
-- ACCESS EXCLUSIVE (such as ALTER TABLE) waits until then, and while it waits, later
-- queries on that table, db-sync's included, queue behind it. db-sync runs such DDL when
-- it starts (migrations) and once near the tip, so avoid running the checks then. The
-- script runs in a transaction that is always rolled back, which also drops its helpers.

\set ON_ERROR_STOP on

BEGIN;

CREATE FUNCTION check_unique(_table text, VARIADIC _cols text[]) RETURNS int AS $$
DECLARE
  _col_list text;
  _not_null text;
  _label text;
  _dups bigint;
BEGIN
  SELECT string_agg(format('%I', c), ', '), string_agg(format('%I IS NOT NULL', c), ' AND ')
    INTO _col_list, _not_null FROM unnest(_cols) AS c;
  _label := _table || '(' || array_to_string(_cols, ', ') || ')';
  EXECUTE format('SELECT count(*) FROM (SELECT 1 FROM %I WHERE %s GROUP BY %s '
                 'HAVING count(*) > 1) d', _table, _not_null, _col_list) INTO _dups;
  IF _dups = 0 THEN
    RAISE NOTICE 'PASS: %', _label;
    RETURN 1;
  END IF;
  RAISE WARNING 'FAIL: %: % key(s) in more than one row', _label, _dups;
  RETURN 0;
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
  -- POT_TRANSFER
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_unique('pot_transfer', 'tx_id', 'cert_index');

  -- =====================================================
  -- POOLS (dropped in migration-2-0021)
  -- =====================================================

  _total := _total + 1; _passed := _passed + check_unique('pool_update', 'registered_tx_id', 'cert_index');
  _total := _total + 1; _passed := _passed + check_unique('pool_retire', 'announced_tx_id', 'cert_index');
  _total := _total + 1; _passed := _passed + check_unique('pool_relay', 'update_id', 'ipv4', 'ipv6', 'dns_name');
  _total := _total + 1; _passed := _passed + check_unique('pool_owner', 'addr_id', 'pool_update_id');
  _total := _total + 1; _passed := _passed + check_unique('pool_stat', 'pool_hash_id', 'epoch_no');

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

  IF _failed > 0 THEN
    RAISE EXCEPTION 'uniqueness checks did not pass: % of % failed', _failed, _total;
  END IF;
END;
$$ LANGUAGE plpgsql;

SELECT test_uniqueness();

ROLLBACK;

-- Functions were rolled back with the transaction, so nothing to clean up.
