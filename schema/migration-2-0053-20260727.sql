-- Leios BLS key registration (Dijkstra), on pool registration certificates.
-- leios_vkey: the pool's registered BLS12-381 verification key (ledger LeiosKey),
--             optional per ledger#5940; NULL for earlier eras and for Dijkstra
--             registrations that omit the key.
-- leios_pop:  the accompanying BLS12-381 proof-of-possession.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 53 THEN
    EXECUTE 'ALTER TABLE "pool_update" ADD COLUMN "leios_vkey" BYTEA NULL' ;
    EXECUTE 'ALTER TABLE "pool_update" ADD COLUMN "leios_pop" BYTEA NULL' ;
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
