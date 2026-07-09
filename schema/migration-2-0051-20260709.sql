-- Remove duplicate epoch_state rows and enforce one row per epoch. See #2155.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 51 THEN

    DELETE FROM "epoch_state" WHERE id NOT IN (
      SELECT DISTINCT ON (epoch_no) id FROM "epoch_state"
      ORDER BY epoch_no, id DESC ) ;

    ALTER TABLE "epoch_state" ADD CONSTRAINT "unique_epoch_state" UNIQUE ("epoch_no") ;

    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
