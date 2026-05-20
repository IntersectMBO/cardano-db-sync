-- Hand written migration to repair epoch.out_sum / epoch.fees / epoch.tx_count / epoch.blk_count
-- corruption introduced by issue #2118 in db-sync 13.7.0.0 - 13.7.0.4. Two distinct bugs in
-- that release line could silently produce inflated rollup values or skip an epoch's row
-- entirely. Both defects are in the Hasql migration of the epoch-table maintenance path; the
-- underlying tx and block tables are unaffected, so the canonical values can always be
-- recomputed from them.
--
-- The recompute only writes rows whose stored values disagree with the recompute, so the
-- cost on an already-clean database is one aggregation pass with no row writes. On a fresh
-- sync the tx / block tables are empty and this is a no-op.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 48 THEN

    WITH calculated AS (
      SELECT
        b.epoch_no,
        COUNT(DISTINCT b.id) AS blk_count,
        MIN(b.time)          AS start_time,
        MAX(b.time)          AS end_time,
        COALESCE(SUM(tx.out_sum), 0) AS out_sum,
        COALESCE(SUM(tx.fee), 0)     AS fee_sum,
        COUNT(tx.id) AS tx_count
      FROM block b
      LEFT JOIN tx ON tx.block_id = b.id
      WHERE b.epoch_no IS NOT NULL
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
      OR epoch.end_time   IS DISTINCT FROM EXCLUDED.end_time ;

    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
