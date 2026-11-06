-- Fix epoch table values by recalculating from block/tx tables and upserting.
--
-- Usage:
--   Fix a specific epoch:
--     psql -v epoch_no=620 -f scripts/fix-epoch-table.sql cexplorer
--
--   Fix all epochs:
--     psql -v epoch_no=-1 -f scripts/fix-epoch-table.sql cexplorer
--
-- Wrap in a transaction so failures don't partially rewrite the table, and stop at the
-- first error so a failed fix does not exit 0.
\set ON_ERROR_STOP on

-- Stop with a usage hint (and a non-zero exit) when -v epoch_no was not given, instead of
-- a syntax error at :epoch_no further down.
\if :{?epoch_no}
\else
  DO $$ BEGIN
    RAISE EXCEPTION 'missing -v epoch_no: run psql -v epoch_no=<epoch, or -1 for all epochs> -f scripts/fix-epoch-table.sql <dbname>';
  END $$;
\endif

-- Since 13.7.2.1 epoch is a view (migration 2-0049): db-sync stores finished epochs in
-- epoch_finalized and computes the current one from block and tx. This script only
-- repairs the old table.
DO $$
BEGIN
  IF (SELECT relkind FROM pg_class WHERE oid = to_regclass('epoch')) = 'v' THEN
    RAISE EXCEPTION 'epoch is a view since db-sync 13.7.2.1, so this script cannot repair it'
      USING HINT = 'Finished epochs are stored in epoch_finalized, which db-sync maintains; a mismatch reported by validate-epoch-table.sql points to a db-sync bug.';
  END IF;
END $$;

BEGIN;

WITH calculated AS (
  SELECT
    b.epoch_no,
    COUNT(DISTINCT b.id) AS blk_count,
    MIN(b.time) AS start_time,
    MAX(b.time) AS end_time,
    COALESCE(SUM(tx.out_sum), 0) AS out_sum,
    COALESCE(SUM(tx.fee), 0) AS fee_sum,
    COUNT(tx.id) AS tx_count
  FROM block b
  LEFT JOIN tx ON tx.block_id = b.id
  WHERE b.epoch_no IS NOT NULL
    AND (CAST(:epoch_no AS bigint) = -1 OR b.epoch_no = CAST(:epoch_no AS bigint))
  GROUP BY b.epoch_no
)
INSERT INTO epoch (no, out_sum, fees, tx_count, blk_count, start_time, end_time)
SELECT
  epoch_no,
  out_sum,
  fee_sum,
  tx_count,
  blk_count,
  start_time,
  end_time
FROM calculated
ON CONFLICT (no) DO UPDATE SET
  out_sum    = EXCLUDED.out_sum,
  fees       = EXCLUDED.fees,
  tx_count   = EXCLUDED.tx_count,
  blk_count  = EXCLUDED.blk_count,
  start_time = EXCLUDED.start_time,
  end_time   = EXCLUDED.end_time
WHERE
     epoch.out_sum    IS DISTINCT FROM EXCLUDED.out_sum
  OR epoch.fees       IS DISTINCT FROM EXCLUDED.fees
  OR epoch.tx_count   IS DISTINCT FROM EXCLUDED.tx_count
  OR epoch.blk_count  IS DISTINCT FROM EXCLUDED.blk_count
  OR epoch.start_time IS DISTINCT FROM EXCLUDED.start_time
  OR epoch.end_time   IS DISTINCT FROM EXCLUDED.end_time;

COMMIT;
