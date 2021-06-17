-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 7 THEN
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "ada_per_u_tx_o_word" lovelace NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "cost_models" VARCHAR NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "price_mem" lovelace NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "price_step" lovelace NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "max_tx_ex_mem" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "max_tx_ex_steps" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "max_block_ex_mem" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "max_block_ex_steps" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "max_val_size" word64type NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "collateral_percent" uinteger NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "max_collateral_inputs" uinteger NULL' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
