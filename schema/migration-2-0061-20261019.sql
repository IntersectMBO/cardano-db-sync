-- Leios committee look-ahead: distinguish the authoritative current-epoch committee (from
-- the matured set snapshot, the one the node validates this epoch's certs against) from the
-- provisional next-epoch committee (from the mark snapshot, an early estimate labeled
-- epoch_no + 1). provisional = FALSE for authoritative rows (all existing rows), TRUE for
-- the mark-derived look-ahead; the look-ahead is replaced by the authoritative row when the
-- chain enters that epoch.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 61 THEN
    EXECUTE 'ALTER TABLE "leios_committee" ADD COLUMN "provisional" BOOLEAN NOT NULL DEFAULT FALSE' ;
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
