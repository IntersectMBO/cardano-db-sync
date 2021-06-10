-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 5 THEN
    EXECUTE 'CREATe TABLE "collateral_tx_in"("id" SERIAL8  PRIMARY KEY UNIQUE,"tx_in_id" INT8 NOT NULL,"tx_out_id" INT8 NOT NULL,"tx_out_index" txindex NOT NULL)' ;
    EXECUTE 'ALTER TABLE "collateral_tx_in" ADD CONSTRAINT "unique_col_txin" UNIQUE("tx_out_id","tx_out_index")' ;
    EXECUTE 'ALTER TABLE "collateral_tx_in" ADD CONSTRAINT "collateral_tx_in_tx_in_id_fkey" FOREIGN KEY("tx_in_id") REFERENCES "tx"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "collateral_tx_in" ADD CONSTRAINT "collateral_tx_in_tx_out_id_fkey" FOREIGN KEY("tx_out_id") REFERENCES "tx"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;

	-- Hand written SQL statements in this block.
    -- The tx table gets a new field, so we set it NULLable first, set a default value and then make
    -- it not NULLable.
    EXECUTE 'ALTER TABLE "tx" ADD COLUMN "valid_contract" BOOLEAN' ;
    EXECUTE 'UPDATE "tx" SET "valid_contract" = TRUE' ;
    EXECUTE 'ALTER TABLE "tx" ALTER COLUMN "valid_contract" SET NOT NULL' ;

    -- Finalize.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
