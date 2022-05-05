-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 13 THEN
    EXECUTE 'CREATe TABLE "collateral_tx_out"("id" SERIAL8  PRIMARY KEY UNIQUE,"tx_id" INT8 NOT NULL,"index" txindex NOT NULL,"address" VARCHAR NOT NULL,"address_raw" BYTEA NOT NULL,"address_has_script" BOOLEAN NOT NULL,"payment_cred" hash28type NULL,"stake_address_id" INT8 NULL,"value" lovelace NOT NULL,"data_hash" hash32type NULL,"multi_assets_descr" VARCHAR NOT NULL)' ;
    EXECUTE 'ALTER TABLE "collateral_tx_out" ADD CONSTRAINT "unique_col_txout" UNIQUE("tx_id","index")' ;
    EXECUTE 'ALTER TABLE "collateral_tx_out" ADD CONSTRAINT "collateral_tx_out_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "collateral_tx_out" ADD CONSTRAINT "collateral_tx_out_stake_address_id_fkey" FOREIGN KEY("stake_address_id") REFERENCES "stake_address"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    EXECUTE 'CREATe TABLE "reference_tx_in"("id" SERIAL8  PRIMARY KEY UNIQUE,"tx_in_id" INT8 NOT NULL,"tx_out_id" INT8 NOT NULL,"tx_out_index" txindex NOT NULL)' ;
    EXECUTE 'ALTER TABLE "reference_tx_in" ADD CONSTRAINT "unique_ref_txin" UNIQUE("tx_in_id","tx_out_id","tx_out_index")' ;
    EXECUTE 'ALTER TABLE "reference_tx_in" ADD CONSTRAINT "reference_tx_in_tx_in_id_fkey" FOREIGN KEY("tx_in_id") REFERENCES "tx"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "reference_tx_in" ADD CONSTRAINT "reference_tx_in_tx_out_id_fkey" FOREIGN KEY("tx_out_id") REFERENCES "tx"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "redeemer" ADD COLUMN "redeemer_data_id" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "redeemer" DROP COLUMN "datum_id"' ;
    EXECUTE 'ALTER TABLE "datum" ADD CONSTRAINT "unique_datum" UNIQUE("hash")' ;
    EXECUTE 'ALTER TABLE "datum" DROP CONSTRAINT "unique_data"' ;
    EXECUTE 'CREATe TABLE "redeemer_data"("id" SERIAL8  PRIMARY KEY UNIQUE,"hash" hash32type NOT NULL,"tx_id" INT8 NOT NULL,"value" jsonb NULL)' ;
    EXECUTE 'ALTER TABLE "redeemer_data" ADD CONSTRAINT "unique_redeemer_data" UNIQUE("hash")' ;
    EXECUTE 'ALTER TABLE "redeemer_data" ADD CONSTRAINT "redeemer_data_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
