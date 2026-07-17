-- Value-domain / data-sanity checks for a populated db-sync database.
--
-- test-uniqueness.sql checks structure and test-referential-integrity.sql checks
-- relations; this script checks that stored VALUES make sense: ranges,
-- non-negativity, agreement with recomputed totals, and no duplicate/missing rows.
-- It catches corruption that is structurally valid but wrong.
--
-- Why it is needed: the numeric domains (lovelace, word*type, ...) were defined
-- with range CHECK constraints, but migration-2-0030 DROPS them all. So a real DB
-- has no value-domain enforcement; the Haskell layer is the only guard, and a
-- decoder or insert bug can store a bad value (e.g. #2135 negative ports,
-- #2118 inflated epoch sums, #2155 duplicate epoch_state rows).
--
-- Usage:  psql -d <dbname> -f scripts/test-value-domains.sql
--   or:   ./scripts/run-schema-checks.sh <dbname>
--
-- Each check counts bad rows: 0 = PASS, >0 = FAIL. A check whose table/column is
-- absent is SKIP (works across 13.6 and 13.7 schemas). A check returning NULL is
-- SKIP too (precondition unmet, e.g. epoch sync disabled). An unexpected SQL error
-- is ERROR (not silently hidden). A SKIP is not coverage: watch the SKIP count
-- between versions, a check that starts SKIPping has silently stopped working.
--
-- The script runs read-only in a transaction that is always rolled back, and
-- exits non-zero if any check FAILs or ERRORs (so it can gate a release).
--
-- Runtime: on a full mainnet DB the non-negativity sweep scans multi-GB tables
-- (tx_out, ma_tx_out, ...) and the whole run takes ~10-15 minutes; on preview it
-- is seconds. It prints a "running:"/"sweeping:" heartbeat before each check and
-- table scan so a long run does not look stuck. Run it off-peak, not on a hot path.
--
-- Note: since 13.7.2.1 "epoch" is a view (epoch_finalized UNION ALL epoch_current)
-- with the same columns, so these queries work on both table and view forms. The
-- epoch checks were validated on the table form (13.6 / 13.7.1) and on the view
-- form on live 13.7.2.1 DBs (preprod Core, preprod Address-table, and preview).

BEGIN;

-- Collects one (label, status) row per check; the summary counts them. Rolled back.
CREATE TEMP TABLE _vd_results (label text, status text);

-- Runs _query (returns a single bad-row count) and records the outcome.
CREATE FUNCTION check_domain(_label text, _query text) RETURNS void AS $$
DECLARE
  _bad bigint;
BEGIN
  RAISE NOTICE 'running: %', _label;  -- heartbeat before a possibly slow query
  EXECUTE _query INTO _bad;
  IF _bad IS NULL THEN
    INSERT INTO _vd_results VALUES (_label, 'SKIP');
    RAISE NOTICE 'SKIP: % (precondition not met)', _label;
  ELSIF _bad = 0 THEN
    INSERT INTO _vd_results VALUES (_label, 'PASS');
    RAISE NOTICE 'PASS: %', _label;
  ELSE
    INSERT INTO _vd_results VALUES (_label, 'FAIL');
    RAISE WARNING 'FAIL: % (% bad row(s))', _label, _bad;
  END IF;
EXCEPTION
  WHEN undefined_table OR undefined_column THEN
    INSERT INTO _vd_results VALUES (_label, 'SKIP');
    RAISE NOTICE 'SKIP: % (table/column absent)', _label;
  WHEN OTHERS THEN
    INSERT INTO _vd_results VALUES (_label, 'ERROR');
    RAISE WARNING 'ERROR: % (%)', _label, SQLERRM;
END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION test_value_domains() RETURNS void AS $$
DECLARE
  _rec     RECORD;
  _js      jsonb;
  _col     text;
  _bad     bigint;
  _passed  int;
  _skipped int;
  _failed  int;
  _errored int;
BEGIN

  -- Port range (#2135: Word16 decoded as signed int2, ports > 32767 went negative).
  PERFORM check_domain(
    'pool_relay.port in 0..65535',
    $q$ SELECT count(*) FROM pool_relay
        WHERE port IS NOT NULL AND (port < 0 OR port > 65535) $q$);

  -- Epoch totals match a recompute from tx/block (#2118: cold-cache rows inflated).
  -- Same formula db-sync uses (EpochAndProtocol.hs). Excludes the in-progress (max)
  -- epoch, whose stored row may legitimately lag. Catches wrong values and missing
  -- epoch rows. NULL => SKIP when epoch sync is disabled.
  -- blk_count uses COUNT(DISTINCT block.id); this matches db-sync (incl. Byron EBBs)
  -- empirically, not by enforced invariant, so a blk_count-only FAIL may be an
  -- EBB-counting difference; check out_sum/fees/tx_count first.
  PERFORM check_domain(
    'epoch totals match recomputed tx/block totals (no missing/mismatched epoch)',
    $q$ SELECT CASE WHEN (SELECT count(*) FROM epoch) = 0 THEN NULL ELSE (
          SELECT count(*) FROM (
            SELECT b.epoch_no,
                   COALESCE(SUM(tx.out_sum), 0) AS c_out,
                   COALESCE(SUM(tx.fee), 0)     AS c_fee,
                   COUNT(tx.id)                 AS c_tx,
                   COUNT(DISTINCT b.id)         AS c_blk
            FROM block b
            LEFT JOIN tx ON tx.block_id = b.id
            WHERE b.epoch_no IS NOT NULL
              AND b.epoch_no < (SELECT MAX(epoch_no) FROM block WHERE epoch_no IS NOT NULL)
            GROUP BY b.epoch_no
          ) c
          LEFT JOIN epoch e ON e.no = c.epoch_no
          WHERE e.no IS NULL
             OR e.out_sum   <> c.c_out
             OR e.fees      <> c.c_fee
             OR e.tx_count  <> c.c_tx
             OR e.blk_count <> c.c_blk
        ) END $q$);

  -- No duplicate epoch number in "epoch". On the table form UNIQUE(no) prevents this;
  -- on the 13.7.2.1 view a regression in the #2127 finalized/current split would
  -- surface the same epoch in both halves.
  PERFORM check_domain(
    'epoch has no duplicate epoch numbers',
    $q$ SELECT count(*) FROM (
          SELECT no FROM epoch GROUP BY no HAVING count(*) > 1) d $q$);

  -- epoch_state duplicates (#2155/#2060): the table has no UNIQUE(epoch_no) and
  -- rollback does not delete it, so a rollback across an epoch boundary leaves a
  -- stale row and the re-sync inserts another. NULL => SKIP when governance is off.
  PERFORM check_domain(
    'epoch_state has at most one row per epoch',
    $q$ SELECT CASE WHEN (SELECT count(*) FROM epoch_state) = 0 THEN NULL ELSE (
          SELECT count(*) FROM (
            SELECT epoch_no FROM epoch_state GROUP BY epoch_no HAVING count(*) > 1) d
        ) END $q$);

  -- No missing epoch_state row within its range. >0 = that many epochs have no row.
  -- (A legitimately gov-toggled DB could show a gap here.)
  PERFORM check_domain(
    'epoch_state is contiguous within its range (no missing epochs)',
    $q$ SELECT CASE WHEN (SELECT count(*) FROM epoch_state) = 0 THEN NULL ELSE (
          SELECT (MAX(epoch_no) - MIN(epoch_no)) - (COUNT(DISTINCT epoch_no) - 1)
          FROM epoch_state
        ) END $q$);

  -- Non-negative lovelace on the hot tables (catches decode wraps beyond the sweep).
  PERFORM check_domain(
    'tx.fee and tx.out_sum are non-negative',
    $q$ SELECT count(*) FROM tx WHERE fee < 0 OR out_sum < 0 $q$);

  PERFORM check_domain(
    'epoch.out_sum and epoch.fees are non-negative',
    $q$ SELECT count(*) FROM epoch WHERE out_sum < 0 OR fees < 0 $q$);

  PERFORM check_domain(
    'ada_pots core pots are non-negative',
    $q$ SELECT count(*) FROM ada_pots
        WHERE treasury < 0 OR reserves < 0 OR rewards < 0 OR utxo < 0 OR fees < 0 $q$);

  -- Off-chain vote: no duplicated children (#2137: re-fetching an anchor re-inserted
  -- its children, which have no uniqueness constraint). drep_data/gov_action_data are
  -- 1:1 with the parent (>1 = dup); author/reference/external_update are 1:many, so
  -- flag exact duplicate rows (author excludes the derived "warning" column).
  PERFORM check_domain(
    'off_chain_vote_drep_data: at most one row per parent',
    $q$ SELECT count(*) FROM (
          SELECT off_chain_vote_data_id FROM off_chain_vote_drep_data
          GROUP BY off_chain_vote_data_id HAVING count(*) > 1) d $q$);

  PERFORM check_domain(
    'off_chain_vote_gov_action_data: at most one row per parent',
    $q$ SELECT count(*) FROM (
          SELECT off_chain_vote_data_id FROM off_chain_vote_gov_action_data
          GROUP BY off_chain_vote_data_id HAVING count(*) > 1) d $q$);

  PERFORM check_domain(
    'off_chain_vote_author: no exact duplicate rows',
    $q$ SELECT count(*) FROM (
          SELECT 1 FROM off_chain_vote_author
          GROUP BY off_chain_vote_data_id, name, witness_algorithm, public_key, signature
          HAVING count(*) > 1) d $q$);

  PERFORM check_domain(
    'off_chain_vote_reference: no exact duplicate rows',
    $q$ SELECT count(*) FROM (
          SELECT 1 FROM off_chain_vote_reference
          GROUP BY off_chain_vote_data_id, label, uri, hash_digest, hash_algorithm
          HAVING count(*) > 1) d $q$);

  PERFORM check_domain(
    'off_chain_vote_external_update: no exact duplicate rows',
    $q$ SELECT count(*) FROM (
          SELECT 1 FROM off_chain_vote_external_update
          GROUP BY off_chain_vote_data_id, title, uri
          HAVING count(*) > 1) d $q$);

  -- Every epoch below the tip has at least one block (no gap in block.epoch_no; #2139).
  PERFORM check_domain(
    'no epoch below the tip is missing all its blocks',
    $q$ SELECT CASE WHEN (SELECT count(*) FROM epoch) = 0 THEN NULL ELSE (
          SELECT (MAX(epoch_no) - MIN(epoch_no)) - (COUNT(DISTINCT epoch_no) - 1)
          FROM block
          WHERE epoch_no IS NOT NULL
            AND epoch_no < (SELECT MAX(epoch_no) FROM block WHERE epoch_no IS NOT NULL)
        ) END $q$);

  -- Committee quorum is well-formed: 0 <= numerator <= denominator, denominator > 0.
  PERFORM check_domain(
    'committee quorum is well-formed',
    $q$ SELECT count(*) FROM committee
        WHERE quorum_denominator <= 0
           OR quorum_numerator < 0
           OR quorum_numerator > quorum_denominator $q$);

  PERFORM check_domain(
    'new_committee quorum is well-formed',
    $q$ SELECT count(*) FROM new_committee
        WHERE quorum_denominator <= 0
           OR quorum_numerator < 0
           OR quorum_numerator > quorum_denominator $q$);

  -- Non-negativity sweep over every numeric column of every base table. The domain
  -- CHECKs are gone (see header), so this is the general net for decode wraps like
  -- #2135. information_schema resolves domains to their base type in data_type while
  -- still exposing domain_name. Excludes surrogate keys/FKs, the signed int65type,
  -- and columns that are negative by design. Each table is scanned once (all its
  -- columns in one count(*) FILTER pass). Cost note: seq-scans large tables (tx_out,
  -- ...) so on mainnet it dominates the ~10-15 min runtime; a diagnostic, not a hot path.
  FOR _rec IN
    SELECT c.table_name,
           string_agg(
             format('count(*) FILTER (WHERE %I < 0) AS %I', c.column_name, c.column_name),
             ', ' ORDER BY c.column_name) AS agg_list
    FROM information_schema.columns c
    JOIN information_schema.tables t
      ON t.table_schema = c.table_schema AND t.table_name = c.table_name
     AND t.table_type = 'BASE TABLE'
    WHERE c.table_schema = 'public'
      AND c.data_type IN ('integer', 'bigint', 'smallint', 'numeric')
      AND (c.domain_name IS NULL OR c.domain_name <> 'int65type')
      AND c.column_name <> 'id'
      AND c.column_name NOT LIKE '%\_id'
      AND (c.table_name, c.column_name) NOT IN (
            ('tx', 'deposit'),                 -- net deposit, negative when refunded
            ('drep_registration', 'deposit'))  -- negative on deregistration refund
    GROUP BY c.table_name
    ORDER BY c.table_name
  LOOP
    BEGIN
      RAISE NOTICE 'sweeping %', _rec.table_name;  -- heartbeat before a full table scan
      EXECUTE format('SELECT to_jsonb(t) FROM (SELECT %s FROM %I) t', _rec.agg_list, _rec.table_name)
        INTO _js;
      FOR _col, _bad IN SELECT key, value::bigint FROM jsonb_each_text(_js)
      LOOP
        IF _bad = 0 THEN
          INSERT INTO _vd_results VALUES (format('%s.%s is non-negative', _rec.table_name, _col), 'PASS');
          RAISE NOTICE 'PASS: %.% is non-negative', _rec.table_name, _col;
        ELSE
          INSERT INTO _vd_results VALUES (format('%s.%s is non-negative', _rec.table_name, _col), 'FAIL');
          RAISE WARNING 'FAIL: %.% has % negative row(s)', _rec.table_name, _col, _bad;
        END IF;
      END LOOP;
    EXCEPTION WHEN OTHERS THEN
      INSERT INTO _vd_results VALUES (format('%s non-negativity sweep', _rec.table_name), 'ERROR');
      RAISE WARNING 'ERROR: % sweep (%)', _rec.table_name, SQLERRM;
    END;
  END LOOP;

  -- Summary. PASS/SKIP/FAIL/ERROR counted separately: a SKIP is not coverage.
  SELECT count(*) FILTER (WHERE status = 'PASS'),
         count(*) FILTER (WHERE status = 'SKIP'),
         count(*) FILTER (WHERE status = 'FAIL'),
         count(*) FILTER (WHERE status = 'ERROR')
    INTO _passed, _skipped, _failed, _errored
    FROM _vd_results;

  RAISE NOTICE '';
  RAISE NOTICE 'Value Domain Test Results';
  RAISE NOTICE '  total:   %', _passed + _skipped + _failed + _errored;
  RAISE NOTICE '  passed:  %', _passed;
  RAISE NOTICE '  skipped: %', _skipped;
  RAISE NOTICE '  failed:  %', _failed;
  RAISE NOTICE '  errored: %', _errored;

  IF _failed > 0 OR _errored > 0 THEN
    RAISE EXCEPTION 'value-domain checks did not pass: % failed, % errored', _failed, _errored;
  END IF;
END;
$$ LANGUAGE plpgsql;

SELECT test_value_domains();

ROLLBACK;
