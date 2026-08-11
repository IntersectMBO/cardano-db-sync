-- Leios (w32+) committee is now built from registered BLS keys with stake-based
-- selection (P99 coverage), not "everyone votes". Record the registered BLS
-- verification key per committee seat; NULL for a keyless seat (a pool that has
-- not registered a Leios key but still holds a seat in the selected distribution).

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 54 THEN
    EXECUTE 'ALTER TABLE "leios_committee" ADD COLUMN "bls_vkey" BYTEA NULL' ;
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
