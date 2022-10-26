CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 23 THEN
    EXECUTE 'ALTER TABLE "stake_address" DROP COLUMN "tx_id"' ;
    EXECUTE 'ALTER TABLE "cost_model" DROP COLUMN "block_id"' ;

    EXECUTE 'ALTER TABLE "reward" DROP CONSTRAINT "reward_pool_id_fkey"' ;
    EXECUTE 'ALTER TABLE "reward" DROP CONSTRAINT "reward_addr_id_fkey"' ;
    EXECUTE 'ALTER TABLE "epoch_stake" DROP CONSTRAINT "epoch_stake_addr_id_fkey"' ;
    EXECUTE 'ALTER TABLE "epoch_stake" DROP CONSTRAINT "epoch_stake_pool_id_fkey"' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
