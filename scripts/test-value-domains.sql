-- Test value domains / data sanity on a populated database.
--
-- Unlike test-uniqueness.sql (structural) and test-referential-integrity.sql
-- (relational), this script checks that stored VALUES make sense: ranges,
-- non-negativity, agreement with recomputed totals, and absence of duplicated
-- child rows. These catch corruption that is structurally valid but wrong -
-- the class of bug that uniqueness/FK checks cannot see.
--
-- Why this is needed at all: db-sync's numeric domains (lovelace, word*type, ...)
-- were defined WITH range CHECK constraints, but migration-2-0030 DROPS all of
-- them. So a real DB has NO value-domain enforcement - the Haskell layer is the
-- only guard, and a decoder bug can write a bad value with nothing to stop it
-- (e.g. #2135 negative ports, #2118 inflated epoch sums).
--
-- Usage: psql -d <dbname> -f scripts/test-value-domains.sql
-- Or run all tests: ./scripts/run-schema-checks.sh <dbname>
--
-- Each check counts "bad" rows. 0 = PASS, >0 = FAIL (count reported).
-- A check whose table/column is absent in this schema version is SKIPped
-- (not counted as a failure), so the script runs across 13.6 and 13.7 schemas.
-- A check whose query returns NULL is also SKIPped - used to skip a check when a
-- precondition is not met (e.g. epoch sync disabled => the epoch view is empty).
--
-- Note on 13.7.2.1+: "epoch" became a VIEW (epoch_finalized table UNION ALL the
-- epoch_current view) but keeps the same columns (no, out_sum, fees, tx_count,
-- blk_count, ...), so SELECTs below work unchanged on both table and view forms.
--
-- The whole script runs read-only inside a transaction that is rolled back.

BEGIN;

-- Runs _query (must return a single count of bad rows) and reports PASS/FAIL.
-- Missing table/column => SKIP. Query returns NULL => SKIP (precondition not met).
-- So the same script works across 13.6 and 13.7 schemas.
CREATE FUNCTION check_domain(_label text, _query text) RETURNS int AS $$
DECLARE
  _bad bigint;
BEGIN
  EXECUTE _query INTO _bad;
  IF _bad IS NULL THEN
    RAISE NOTICE 'SKIP: % (precondition not met / feature disabled)', _label;
    RETURN 0;
  ELSIF _bad = 0 THEN
    RAISE NOTICE 'PASS: %', _label;
    RETURN 0;            -- 0 failures
  ELSE
    RAISE WARNING 'FAIL: % (% bad row(s))', _label, _bad;
    RETURN 1;            -- 1 failure
  END IF;
EXCEPTION
  WHEN undefined_table OR undefined_column THEN
    RAISE NOTICE 'SKIP: % (table/column not present in this schema)', _label;
    RETURN 0;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION test_value_domains() RETURNS void AS $$
DECLARE
  _total  INT := 0;
  _failed INT := 0;
  _rec    RECORD;
  _bad    bigint;
BEGIN

  -- =====================================================
  -- PORT RANGE  (regression #2135: Word16 decoded as signed int2,
  --              ports > 32767 stored negative)
  -- =====================================================

  _total := _total + 1;
  _failed := _failed + check_domain(
    'pool_relay.port in 0..65535 (no signed-16-bit wrap)',
    $q$ SELECT count(*) FROM pool_relay
        WHERE port IS NOT NULL AND (port < 0 OR port > 65535) $q$);

  -- =====================================================
  -- EPOCH VALUES MATCH RECOMPUTED tx/block TOTALS
  --   (regression #2118: cold-cache epoch rows inflated)
  --   Excludes the in-progress (max) epoch, whose stored row may legitimately
  --   lag the latest blocks on a table-backed (13.6/13.7.1) actively-syncing DB.
  --   Catches both wrong values and missing finalized-epoch rows.
  -- =====================================================

  -- Returns NULL (=> SKIP) when the epoch view/table is empty, i.e. epoch sync
  -- is disabled (--disable-epoch / disable_epoch, or the 13.7.2.1 gated view).
  -- Otherwise counts completed epochs that are missing or disagree with recompute.
  _total := _total + 1;
  _failed := _failed + check_domain(
    'epoch.out_sum/fees/tx_count/blk_count match recomputed tx/block totals',
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
             OR e.out_sum  <> c.c_out
             OR e.fees     <> c.c_fee
             OR e.tx_count <> c.c_tx
             OR e.blk_count <> c.c_blk
        ) END $q$);

  -- 13.7.2.1+: epoch is epoch_finalized UNION ALL epoch_current. A regression in
  -- the finalized/current split (the > MAX vs NOT EXISTS bug fixed in #2141) would
  -- surface the same epoch in both halves => a duplicate epoch number in the view.
  _total := _total + 1;
  _failed := _failed + check_domain(
    'epoch has no duplicate epoch numbers (finalized/current union is disjoint)',
    $q$ SELECT count(*) FROM (
          SELECT no FROM epoch GROUP BY no HAVING count(*) > 1) d $q$);

  -- =====================================================
  -- NON-NEGATIVE LOVELACE  (catches numeric-decode wraps generally)
  -- =====================================================

  _total := _total + 1;
  _failed := _failed + check_domain(
    'tx.fee and tx.out_sum are non-negative',
    $q$ SELECT count(*) FROM tx WHERE fee < 0 OR out_sum < 0 $q$);

  _total := _total + 1;
  _failed := _failed + check_domain(
    'epoch.out_sum and epoch.fees are non-negative',
    $q$ SELECT count(*) FROM epoch WHERE out_sum < 0 OR fees < 0 $q$);

  _total := _total + 1;
  _failed := _failed + check_domain(
    'ada_pots core pots are non-negative',
    $q$ SELECT count(*) FROM ada_pots
        WHERE treasury < 0 OR reserves < 0 OR rewards < 0 OR utxo < 0 OR fees < 0 $q$);

  -- =====================================================
  -- OFF-CHAIN VOTE: NO DUPLICATED CHILD ROWS
  --   (regression #2137: re-fetching an existing voting anchor re-inserted its
  --    children, which have no uniqueness constraint by design)
  --   drep_data / gov_action_data are 1:1 with the parent -> >1 per parent = dup.
  --   author / reference / external_update are 1:many -> flag EXACT duplicate rows.
  -- =====================================================

  _total := _total + 1;
  _failed := _failed + check_domain(
    'off_chain_vote_drep_data: <=1 row per parent',
    $q$ SELECT count(*) FROM (
          SELECT off_chain_vote_data_id FROM off_chain_vote_drep_data
          GROUP BY off_chain_vote_data_id HAVING count(*) > 1) d $q$);

  _total := _total + 1;
  _failed := _failed + check_domain(
    'off_chain_vote_gov_action_data: <=1 row per parent',
    $q$ SELECT count(*) FROM (
          SELECT off_chain_vote_data_id FROM off_chain_vote_gov_action_data
          GROUP BY off_chain_vote_data_id HAVING count(*) > 1) d $q$);

  _total := _total + 1;
  _failed := _failed + check_domain(
    'off_chain_vote_author: no exact duplicate rows',
    $q$ SELECT count(*) FROM (
          SELECT 1 FROM off_chain_vote_author
          GROUP BY off_chain_vote_data_id, name, witness_algorithm, public_key, signature
          HAVING count(*) > 1) d $q$);

  _total := _total + 1;
  _failed := _failed + check_domain(
    'off_chain_vote_reference: no exact duplicate rows',
    $q$ SELECT count(*) FROM (
          SELECT 1 FROM off_chain_vote_reference
          GROUP BY off_chain_vote_data_id, label, uri, hash_digest, hash_algorithm
          HAVING count(*) > 1) d $q$);

  _total := _total + 1;
  _failed := _failed + check_domain(
    'off_chain_vote_external_update: no exact duplicate rows',
    $q$ SELECT count(*) FROM (
          SELECT 1 FROM off_chain_vote_external_update
          GROUP BY off_chain_vote_data_id, title, uri
          HAVING count(*) > 1) d $q$);

  -- =====================================================
  -- SCHEMA-DRIVEN NON-NEGATIVITY SWEEP
  --   db-sync's custom DOMAIN types (lovelace, word31/63/64/128type, txindex)
  --   were created WITH CHECK (VALUE >= 0 ...) constraints, but migration-2-0030
  --   DROPS every one of those checks (for insert performance). So in a real DB
  --   there is NO DB-level value enforcement - the only guard is the Haskell type
  --   layer. A negative or out-of-range value therefore CAN be stored (this is
  --   exactly how pool_relay.port went negative, #2135). This loop checks every
  --   numeric column for negatives - raw AND domain-typed - adapting as the schema
  --   grows.
  --   Excluded: surrogate keys / FKs (id, *_id), the one signed domain (int65type,
  --   e.g. pot_transfer), and an allowlist of legitimately-signed raw columns
  --   (deposit refunds are negative by design). Only BASE TABLEs are swept (not
  --   the epoch/epoch_current views).
  --   Cost note: on mainnet this seq-scans large tables (tx_out, etc.) and can take
  --   a few minutes - it is a release-gate / diagnostic check, not a hot path.
  -- =====================================================

  FOR _rec IN
    SELECT c.table_name, c.column_name
    FROM information_schema.columns c
    JOIN information_schema.tables t
      ON t.table_schema = c.table_schema AND t.table_name = c.table_name
     AND t.table_type = 'BASE TABLE'
    WHERE c.table_schema = 'public'
      AND c.data_type IN ('integer', 'bigint', 'smallint', 'numeric')
      AND (c.domain_name IS NULL OR c.domain_name <> 'int65type')  -- exclude the one signed domain
      AND c.column_name <> 'id'
      AND c.column_name NOT LIKE '%\_id'
      AND (c.table_name, c.column_name) NOT IN (
            ('tx', 'deposit'),                 -- net deposit; negative when a tx returns deposits
            ('drep_registration', 'deposit'))  -- negative on deregistration (refund)
    ORDER BY c.table_name, c.column_name
  LOOP
    BEGIN
      EXECUTE format('SELECT count(*) FROM %I WHERE %I < 0', _rec.table_name, _rec.column_name)
        INTO _bad;
      _total := _total + 1;
      IF _bad = 0 THEN
        RAISE NOTICE 'PASS: %.% is non-negative', _rec.table_name, _rec.column_name;
      ELSE
        _failed := _failed + 1;
        RAISE WARNING 'FAIL: %.% has % negative row(s)', _rec.table_name, _rec.column_name, _bad;
      END IF;
    EXCEPTION WHEN undefined_table OR undefined_column THEN
      RAISE NOTICE 'SKIP: %.% (not present)', _rec.table_name, _rec.column_name;
    END;
  END LOOP;

  -- =====================================================
  -- EPOCH-NUMBER CONTIGUITY  (missing-epoch gap detector, e.g. #2139)
  --   A finished epoch should never be absent below the in-progress tip. Returns
  --   NULL (=> SKIP) when epoch is empty (feature disabled). >0 = that many epoch
  --   numbers have no blocks at all - a missing-data hole.
  -- =====================================================

  _total := _total + 1;
  _failed := _failed + check_domain(
    'epoch numbers are contiguous below the tip (no missing epochs)',
    $q$ SELECT CASE WHEN (SELECT count(*) FROM epoch) = 0 THEN NULL ELSE (
          SELECT (MAX(epoch_no) - MIN(epoch_no)) - (COUNT(DISTINCT epoch_no) - 1)
          FROM block
          WHERE epoch_no IS NOT NULL
            AND epoch_no < (SELECT MAX(epoch_no) FROM block WHERE epoch_no IS NOT NULL)
        ) END $q$);

  -- =====================================================
  -- COMMITTEE QUORUM SANITY  (governance-critical raw bigint columns)
  -- =====================================================

  _total := _total + 1;
  _failed := _failed + check_domain(
    'committee quorum is well-formed (0 <= numerator <= denominator, denominator > 0)',
    $q$ SELECT count(*) FROM committee
        WHERE quorum_denominator <= 0
           OR quorum_numerator < 0
           OR quorum_numerator > quorum_denominator $q$);

  _total := _total + 1;
  _failed := _failed + check_domain(
    'new_committee quorum is well-formed (0 <= numerator <= denominator, denominator > 0)',
    $q$ SELECT count(*) FROM new_committee
        WHERE quorum_denominator <= 0
           OR quorum_numerator < 0
           OR quorum_numerator > quorum_denominator $q$);

  -- =====================================================
  -- SUMMARY
  -- =====================================================

  RAISE NOTICE '';
  RAISE NOTICE '=============================================';
  RAISE NOTICE 'Value Domain Test Results';
  RAISE NOTICE '=============================================';
  RAISE NOTICE 'Total:          %', _total;
  RAISE NOTICE 'Passed/skipped: %', _total - _failed;
  RAISE NOTICE 'Failed:         %', _failed;
  RAISE NOTICE '=============================================';
END;
$$ LANGUAGE plpgsql;

SELECT test_value_domains();

ROLLBACK;

-- Functions were rolled back with the transaction, so nothing to clean up.
