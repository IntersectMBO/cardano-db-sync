-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 21 THEN
    EXECUTE 'ALTER TABLE "pool_update" DROP CONSTRAINT "unique_pool_update"' ;
    EXECUTE 'ALTER TABLE "pool_retire" DROP CONSTRAINT "unique_pool_retiring"' ;
    EXECUTE 'ALTER TABLE "pool_relay" DROP CONSTRAINT "unique_pool_relay"' ;
    EXECUTE 'ALTER TABLE "stake_registration" DROP CONSTRAINT "unique_stake_registration"' ;
    EXECUTE 'ALTER TABLE "stake_deregistration" DROP CONSTRAINT "unique_stake_deregistration"' ;
    EXECUTE 'ALTER TABLE "delegation" DROP CONSTRAINT "unique_delegation"' ;
    EXECUTE 'ALTER TABLE "tx_metadata" DROP CONSTRAINT "unique_tx_metadata"' ;
    EXECUTE 'ALTER TABLE "withdrawal" DROP CONSTRAINT "unique_withdrawal"' ;
    EXECUTE 'ALTER TABLE "treasury" DROP CONSTRAINT "unique_treasury"' ;
    EXECUTE 'ALTER TABLE "reserve" DROP CONSTRAINT "unique_reserves"' ;
    EXECUTE 'ALTER TABLE "pot_transfer" DROP CONSTRAINT "unique_pot_transfer"' ;
    EXECUTE 'ALTER TABLE "ma_tx_mint" DROP CONSTRAINT "unique_ma_tx_mint"' ;
    EXECUTE 'ALTER TABLE "ma_tx_out" DROP CONSTRAINT "unique_ma_tx_out"' ;
    EXECUTE 'ALTER TABLE "redeemer" DROP CONSTRAINT "unique_redeemer"' ;
    EXECUTE 'ALTER TABLE "extra_key_witness" DROP CONSTRAINT "unique_witness"' ;
    EXECUTE 'ALTER TABLE "param_proposal" DROP CONSTRAINT "unique_param_proposal"' ;
    EXECUTE 'ALTER TABLE "epoch_param" DROP CONSTRAINT "unique_epoch_param"' ;
    EXECUTE 'ALTER TABLE "collateral_tx_out" DROP CONSTRAINT "unique_col_txout"' ;
    EXECUTE 'ALTER TABLE "collateral_tx_in" DROP CONSTRAINT "unique_col_txin"' ;
    EXECUTE 'ALTER TABLE "reference_tx_in" DROP CONSTRAINT "unique_ref_txin"' ;
    EXECUTE 'ALTER TABLE "pool_owner" DROP CONSTRAINT "unique_pool_owner"' ;
    EXECUTE 'ALTER TABLE "tx_in" DROP CONSTRAINT "unique_txin"' ;
    EXECUTE 'ALTER TABLE "ada_pots" DROP CONSTRAINT "unique_ada_pots"' ;

    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
