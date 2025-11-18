CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 45 THEN

    -- Remove duplicates first
    DELETE FROM pool_stat
    WHERE id NOT IN (
      SELECT DISTINCT ON (pool_hash_id, epoch_no) id
      FROM pool_stat
      ORDER BY pool_hash_id, epoch_no, id
    );

    -- Then add constraint if it doesn't exist
    DO $$
    BEGIN
      IF NOT EXISTS (
        SELECT 1 FROM pg_constraint
        WHERE conname = 'unique_pool_stat_epoch'
      ) THEN
        ALTER TABLE "pool_stat" ADD CONSTRAINT "unique_pool_stat_epoch"
        UNIQUE ("pool_hash_id", "epoch_no");
      END IF;
    END $$;

    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;
DROP FUNCTION migrate() ;
