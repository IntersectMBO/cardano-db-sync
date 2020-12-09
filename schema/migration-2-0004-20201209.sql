-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 4 THEN
    EXECUTE 'ALTER TABLE "schema_version" ALTER COLUMN "id" TYPE INT8' ;
    EXECUTE 'ALTER TABLE "tx" ADD COLUMN "invalid_before" word64type NULL' ;
    EXECUTE 'ALTER TABLE "tx" ADD COLUMN "invalid_hereafter" word64type NULL' ;
    EXECUTE 'CREATe TABLE "ma_tx_mint"("id" SERIAL8  PRIMARY KEY UNIQUE,"policy" hash28type NOT NULL,"name" asset32type NOT NULL,"quantity" int65type NOT NULL,"tx_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "ma_tx_mint" ADD CONSTRAINT "unique_ma_tx_mint" UNIQUE("policy","name","tx_id")' ;
    EXECUTE 'ALTER TABLE "ma_tx_mint" ADD CONSTRAINT "ma_tx_mint_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id") ON DELETE RESTRICT  ON UPDATE RESTRICT' ;
    EXECUTE 'CREATe TABLE "ma_tx_out"("id" SERIAL8  PRIMARY KEY UNIQUE,"policy" hash28type NOT NULL,"name" asset32type NOT NULL,"quantity" word64type NOT NULL,"tx_out_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "ma_tx_out" ADD CONSTRAINT "unique_ma_tx_out" UNIQUE("policy","name","tx_out_id")' ;
    EXECUTE 'ALTER TABLE "ma_tx_out" ADD CONSTRAINT "ma_tx_out_tx_out_id_fkey" FOREIGN KEY("tx_out_id") REFERENCES "tx_out"("id") ON DELETE RESTRICT  ON UPDATE RESTRICT' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
