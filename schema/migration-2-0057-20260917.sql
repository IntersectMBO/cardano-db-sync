-- Dijkstra nested (sub-)transactions. A sub-transaction has its own TxId, so it is
-- stored as an ordinary tx row (its own inputs/outputs/certs) with parent_tx_id set to
-- the tx.id of the enclosing top-level transaction. NULL for a normal / top-level tx.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 57 THEN
    EXECUTE 'ALTER TABLE "tx" ADD COLUMN "parent_tx_id" INT8 NULL' ;
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
