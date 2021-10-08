-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 2 THEN
    EXECUTE 'CREATe TABLE "multi_asset"("id" SERIAL8  PRIMARY KEY UNIQUE,"policy" hash28type NOT NULL,"name" asset32type NOT NULL,"fingerprint" VARCHAR NOT NULL)' ;
    EXECUTE 'ALTER TABLE "multi_asset" ADD CONSTRAINT "unique_multi_asset" UNIQUE("policy","name")' ;
    EXECUTE 'ALTER TABLE "ma_tx_mint" ADD COLUMN "ident" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "ma_tx_mint" DROP COLUMN "policy"' ;
    EXECUTE 'ALTER TABLE "ma_tx_mint" DROP COLUMN "name"' ;
    EXECUTE 'ALTER TABLE "ma_tx_mint" DROP CONSTRAINT IF EXISTS "unique_ma_tx_mint"' ;
    EXECUTE 'ALTER TABLE "ma_tx_mint" ADD CONSTRAINT "unique_ma_tx_mint" UNIQUE("ident","tx_id")' ;
    EXECUTE 'ALTER TABLE "ma_tx_out" ADD COLUMN "ident" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "ma_tx_out" DROP COLUMN "policy"' ;
    EXECUTE 'ALTER TABLE "ma_tx_out" DROP COLUMN "name"' ;
    EXECUTE 'ALTER TABLE "ma_tx_out" DROP CONSTRAINT IF EXISTS "unique_ma_tx_out"' ;
    EXECUTE 'ALTER TABLE "ma_tx_out" ADD CONSTRAINT "unique_ma_tx_out" UNIQUE("ident","tx_out_id")' ;

    EXECUTE 'ALTER TABLE "ma_tx_mint" ADD CONSTRAINT "ma_tx_mint_ident_fkey" FOREIGN KEY("ident") REFERENCES "multi_asset"("id") ON DELETE RESTRICT  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "ma_tx_out" ADD CONSTRAINT "ma_tx_out_ident_fkey" FOREIGN KEY("ident") REFERENCES "multi_asset"("id") ON DELETE RESTRICT  ON UPDATE RESTRICT' ;

    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
