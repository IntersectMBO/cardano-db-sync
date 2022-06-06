-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 15 THEN
    EXECUTE 'ALTER TABLE "tx_out" ADD CONSTRAINT "tx_out_inline_datum_id_fkey" FOREIGN KEY("inline_datum_id") REFERENCES "datum"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "tx_out" ADD CONSTRAINT "tx_out_reference_script_id_fkey" FOREIGN KEY("reference_script_id") REFERENCES "script"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "collateral_tx_out" ADD CONSTRAINT "collateral_tx_out_inline_datum_id_fkey" FOREIGN KEY("inline_datum_id") REFERENCES "datum"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "collateral_tx_out" ADD CONSTRAINT "collateral_tx_out_reference_script_id_fkey" FOREIGN KEY("reference_script_id") REFERENCES "script"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
