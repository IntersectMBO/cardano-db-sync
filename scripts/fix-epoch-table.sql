-- Fix epoch table values by recalculating from block/tx tables and upserting.
--
-- Usage:
--   Fix a specific epoch:
--     psql -v epoch_no=620 -f scripts/fix-epoch-table.sql cexplorer
--
--   Fix all epochs:
--     psql -v epoch_no=-1 -f scripts/fix-epoch-table.sql cexplorer
--
-- Wrap in a transaction so failures don't partially rewrite the table.

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
