-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 14 THEN
    EXECUTE 'ALTER TABLE "tx_out" ADD COLUMN "inline_datum_id" INT8 NULL' ;
    EXECUTE 'ALTER TABLE "tx_out" ADD COLUMN "reference_script_id" INT8 NULL' ;
    EXECUTE 'ALTER TABLE "collateral_tx_out" ADD COLUMN "inline_datum_id" INT8 NULL' ;
    EXECUTE 'ALTER TABLE "collateral_tx_out" ADD COLUMN "reference_script_id" INT8 NULL' ;
    EXECUTE 'ALTER TABLE "redeemer" ADD CONSTRAINT "redeemer_redeemer_data_id_fkey" FOREIGN KEY("redeemer_data_id") REFERENCES "redeemer_data"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
