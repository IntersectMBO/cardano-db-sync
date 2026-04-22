-- Validate epoch table values against recalculated values from tx/block tables.
--
-- Usage:
--   Validate a specific epoch:
--     psql -v epoch_no=620 -f scripts/validate-epoch-table.sql cexplorer
--
--   Validate all epochs:
--     psql -v epoch_no=-1 -f scripts/validate-epoch-table.sql cexplorer
--
-- Any row in the output means a mismatch. No rows = all correct.

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
),
compared AS (
  SELECT
    c.epoch_no,
    -- Calculated values
    c.out_sum AS calc_out_sum,
    c.fee_sum AS calc_fees,
    c.tx_count AS calc_tx_count,
    c.blk_count AS calc_blk_count,
    c.start_time AS calc_start_time,
    c.end_time AS calc_end_time,
    -- Stored values
    e.out_sum AS stored_out_sum,
    e.fees AS stored_fees,
    e.tx_count AS stored_tx_count,
    e.blk_count AS stored_blk_count,
    e.start_time AS stored_start_time,
    e.end_time AS stored_end_time,
    -- Differences
    c.out_sum - e.out_sum AS out_sum_diff,
    c.fee_sum - e.fees AS fees_diff,
    c.tx_count - e.tx_count AS tx_count_diff,
    c.blk_count - e.blk_count AS blk_count_diff
  FROM calculated c
  LEFT JOIN epoch e ON e.no = c.epoch_no
)
SELECT
  epoch_no,
  CASE WHEN stored_out_sum IS NULL THEN 'MISSING' ELSE '' END AS status,
  calc_out_sum,
  stored_out_sum,
  out_sum_diff,
  calc_fees,
  stored_fees,
  fees_diff,
  calc_tx_count,
  stored_tx_count,
  tx_count_diff,
  calc_blk_count,
  stored_blk_count,
  blk_count_diff,
  calc_start_time,
  stored_start_time,
  calc_end_time,
  stored_end_time
FROM compared
WHERE stored_out_sum IS NULL
   OR out_sum_diff != 0
   OR fees_diff != 0
   OR tx_count_diff != 0
   OR blk_count_diff != 0
ORDER BY epoch_no;
