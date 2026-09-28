-- The remaining Leios (Dijkstra) protocol parameters: the endorser-block limits made
-- governance-configurable in w36 — max references size, max tx size, max execution units
-- (memory and steps), and max reference-script size per endorser block. Recorded per epoch
-- on epoch_param; NULL before the Dijkstra era.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 58 THEN
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "leios_max_eb_references_size" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "leios_max_eb_txs_size" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "leios_max_eb_ex_mem" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "leios_max_eb_ex_steps" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "leios_max_ref_script_size_per_eb" word64type NULL' ;
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
