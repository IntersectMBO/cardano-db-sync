-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 7 THEN
    EXECUTE 'ALTER TABLE "reward" DROP CONSTRAINT "reward_addr_id_fkey"' ;
    EXECUTE 'ALTER TABLE "reward" ADD CONSTRAINT "reward_addr_id_fkey" FOREIGN KEY("addr_id") REFERENCES "stake_address"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "orphaned_reward" DROP CONSTRAINT "orphaned_reward_addr_id_fkey"' ;
    EXECUTE 'ALTER TABLE "orphaned_reward" ADD CONSTRAINT "orphaned_reward_addr_id_fkey" FOREIGN KEY("addr_id") REFERENCES "stake_address"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "treasury" DROP CONSTRAINT "treasury_tx_id_fkey"' ;
    EXECUTE 'ALTER TABLE "treasury" ADD CONSTRAINT "treasury_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "ma_tx_mint" DROP CONSTRAINT "ma_tx_mint_tx_id_fkey"' ;
    EXECUTE 'ALTER TABLE "ma_tx_mint" ADD CONSTRAINT "ma_tx_mint_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "ma_tx_out" DROP CONSTRAINT "ma_tx_out_tx_out_id_fkey"' ;
    EXECUTE 'ALTER TABLE "ma_tx_out" ADD CONSTRAINT "ma_tx_out_tx_out_id_fkey" FOREIGN KEY("tx_out_id") REFERENCES "tx_out"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
