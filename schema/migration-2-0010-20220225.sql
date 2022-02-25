-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 10 THEN
    EXECUTE 'ALTER TABLE "pool_metadata_ref" DROP CONSTRAINT "pool_metadata_ref_pool_id_fkey"' ;
    EXECUTE 'ALTER TABLE "pool_metadata_ref" ADD CONSTRAINT "pool_metadata_ref_pool_id_fkey" FOREIGN KEY("pool_id") REFERENCES "pool_hash"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "ma_tx_mint" DROP CONSTRAINT "ma_tx_mint_ident_fkey"' ;
    EXECUTE 'ALTER TABLE "ma_tx_mint" ADD CONSTRAINT "ma_tx_mint_ident_fkey" FOREIGN KEY("ident") REFERENCES "multi_asset"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "ma_tx_out" DROP CONSTRAINT "ma_tx_out_ident_fkey"' ;
    EXECUTE 'ALTER TABLE "ma_tx_out" ADD CONSTRAINT "ma_tx_out_ident_fkey" FOREIGN KEY("ident") REFERENCES "multi_asset"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
