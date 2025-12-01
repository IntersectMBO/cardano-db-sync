CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 45 THEN

    -- Remove duplicates
    DELETE FROM pool_stat
    WHERE id NOT IN (
      SELECT DISTINCT ON (pool_hash_id, epoch_no) id
      FROM pool_stat
      ORDER BY pool_hash_id, epoch_no, id
    );

    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;
DROP FUNCTION migrate() ;
