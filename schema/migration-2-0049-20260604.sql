CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 49 THEN

    DROP TABLE IF EXISTS "epoch" CASCADE ;

    CREATE TABLE "epoch_finalized" (
      "id"         BIGINT       NOT NULL PRIMARY KEY,
      "out_sum"    word128type  NOT NULL,
      "fees"       lovelace     NOT NULL,
      "tx_count"   word31type   NOT NULL,
      "blk_count"  word31type   NOT NULL,
      "no"         word31type   NOT NULL,
      "start_time" timestamp    NOT NULL,
      "end_time"   timestamp    NOT NULL
    ) ;
    ALTER TABLE "epoch_finalized" ADD CONSTRAINT "unique_epoch_finalized_no" UNIQUE ("no") ;

    CREATE TABLE "epoch_sync_enabled" (
      "singleton" BOOLEAN PRIMARY KEY DEFAULT TRUE,
      "enabled"   BOOLEAN NOT NULL,
      CONSTRAINT "epoch_sync_enabled_singleton" CHECK ("singleton" = TRUE)
    ) ;
    INSERT INTO "epoch_sync_enabled" ("enabled") VALUES (FALSE) ;

    CREATE VIEW "epoch_current" AS
      SELECT
        (b.epoch_no::bigint + 1)              AS "id",
        COALESCE(SUM(tx.out_sum), 0)::numeric AS "out_sum",
        COALESCE(SUM(tx.fee), 0)              AS "fees",
        COUNT(tx.id)::bigint                  AS "tx_count",
        COUNT(DISTINCT b.id)::bigint          AS "blk_count",
        b.epoch_no::bigint                    AS "no",
        MIN(b.time)                           AS "start_time",
        MAX(b.time)                           AS "end_time"
      FROM "block" b
      LEFT JOIN "tx" ON tx.block_id = b.id
      WHERE b.epoch_no IS NOT NULL
        AND NOT EXISTS (SELECT 1 FROM "epoch_finalized" ef WHERE ef.no = b.epoch_no)
        AND (SELECT enabled FROM "epoch_sync_enabled" WHERE singleton = TRUE)
      GROUP BY b.epoch_no ;

    CREATE VIEW "epoch" AS
      SELECT "id", "out_sum", "fees", "tx_count", "blk_count", "no", "start_time", "end_time"
        FROM "epoch_finalized"
        WHERE (SELECT enabled FROM "epoch_sync_enabled" WHERE singleton = TRUE)
      UNION ALL
      SELECT "id", "out_sum", "fees", "tx_count", "blk_count", "no", "start_time", "end_time"
        FROM "epoch_current" ;

    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;
DROP FUNCTION migrate() ;
