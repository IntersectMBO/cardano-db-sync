-- Doomsday mode: a broken chain can carry the same tx hash in more than one
-- block (e.g. a tx included in two consecutive certified Leios EBs, which the
-- ledger applies once but re-lists). db-sync's "unique_tx" constraint turns
-- that into a fatal duplicate-key error and stalls the sync. Drop the
-- uniqueness constraint so the duplicate row can be stored, and replace it
-- with a plain (non-unique) index so "WHERE hash = ..." lookups stay fast.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 55 THEN
    EXECUTE 'ALTER TABLE "tx" DROP CONSTRAINT IF EXISTS "unique_tx"' ;
    EXECUTE 'CREATE INDEX IF NOT EXISTS "idx_tx_hash" ON "tx" ("hash")' ;
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
