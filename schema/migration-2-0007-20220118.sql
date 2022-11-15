-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 7 THEN
    EXECUTE 'ALTER TABLE "pool_update" DROP CONSTRAINT "unique_pool_update"' ;
    EXECUTE 'ALTER TABLE "pool_update" ADD CONSTRAINT "unique_pool_update" UNIQUE("registered_tx_id","cert_index")' ;
    EXECUTE 'ALTER TABLE "pool_retire" DROP CONSTRAINT "unique_pool_retiring"' ;
    EXECUTE 'ALTER TABLE "pool_retire" ADD CONSTRAINT "unique_pool_retiring" UNIQUE("announced_tx_id","cert_index")' ;
    EXECUTE 'ALTER TABLE "stake_registration" DROP CONSTRAINT "unique_stake_registration"' ;
    EXECUTE 'ALTER TABLE "stake_registration" ADD CONSTRAINT "unique_stake_registration" UNIQUE("tx_id","cert_index")' ;
    EXECUTE 'ALTER TABLE "stake_deregistration" DROP CONSTRAINT "unique_stake_deregistration"' ;
    EXECUTE 'ALTER TABLE "stake_deregistration" ADD CONSTRAINT "unique_stake_deregistration" UNIQUE("tx_id","cert_index")' ;
    EXECUTE 'ALTER TABLE "delegation" DROP CONSTRAINT "unique_delegation"' ;
    EXECUTE 'ALTER TABLE "delegation" ADD CONSTRAINT "unique_delegation" UNIQUE("tx_id","cert_index")' ;
    EXECUTE 'ALTER TABLE "treasury" DROP CONSTRAINT "unique_treasury"' ;
    EXECUTE 'ALTER TABLE "treasury" ADD CONSTRAINT "unique_treasury" UNIQUE("tx_id","cert_index")' ;
    EXECUTE 'ALTER TABLE "reserve" DROP CONSTRAINT "unique_reserves"' ;
    EXECUTE 'ALTER TABLE "reserve" ADD CONSTRAINT "unique_reserves" UNIQUE("tx_id","cert_index")' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
