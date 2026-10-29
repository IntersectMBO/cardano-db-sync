-- EXPERIMENTAL (Dijkstra): provisional schema for an unreleased era; see the note at the
-- top of doc/schema.md. These columns may be amended or dropped by a later migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 53 THEN
    -- A Dijkstra sub-transaction is stored as an ordinary tx row; parent_tx_id points at
    -- the enclosing top-level tx, and is NULL for a top-level tx.
    EXECUTE 'ALTER TABLE "tx" ADD COLUMN "parent_tx_id" INT8 NULL' ;
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
