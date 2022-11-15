-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 25 THEN
    EXECUTE 'ALTER TABLE "block" DROP CONSTRAINT "block_previous_id_fkey"' ;
    EXECUTE 'ALTER TABLE "tx" DROP CONSTRAINT "tx_block_id_fkey"' ;
    EXECUTE 'ALTER TABLE "tx_out" DROP CONSTRAINT "tx_out_tx_id_fkey"' ;
    EXECUTE 'ALTER TABLE "collateral_tx_out" DROP CONSTRAINT "collateral_tx_out_tx_id_fkey"' ;
    EXECUTE 'ALTER TABLE "tx_in" DROP CONSTRAINT "tx_in_tx_in_id_fkey"' ;
    EXECUTE 'ALTER TABLE "collateral_tx_in" DROP CONSTRAINT "collateral_tx_in_tx_in_id_fkey"' ;
    EXECUTE 'ALTER TABLE "reference_tx_in" DROP CONSTRAINT "reference_tx_in_tx_in_id_fkey"' ;
    EXECUTE 'ALTER TABLE "ada_pots" DROP CONSTRAINT "ada_pots_block_id_fkey"' ;
    EXECUTE 'ALTER TABLE "pool_metadata_ref" DROP CONSTRAINT "pool_metadata_ref_registered_tx_id_fkey"' ;
    EXECUTE 'ALTER TABLE "pool_update" DROP CONSTRAINT "pool_update_registered_tx_id_fkey"' ;
    EXECUTE 'ALTER TABLE "pool_owner" DROP CONSTRAINT "pool_owner_pool_update_id_fkey"' ;
    EXECUTE 'ALTER TABLE "pool_retire" DROP CONSTRAINT "pool_retire_announced_tx_id_fkey"' ;
    EXECUTE 'ALTER TABLE "pool_relay" DROP CONSTRAINT "pool_relay_update_id_fkey"' ;
    EXECUTE 'ALTER TABLE "stake_registration" DROP CONSTRAINT "stake_registration_tx_id_fkey"' ;
    EXECUTE 'ALTER TABLE "stake_deregistration" DROP CONSTRAINT "stake_deregistration_tx_id_fkey"' ;
    EXECUTE 'ALTER TABLE "delegation" DROP CONSTRAINT "delegation_tx_id_fkey"' ;
    EXECUTE 'ALTER TABLE "tx_metadata" DROP CONSTRAINT "tx_metadata_tx_id_fkey"' ;
    EXECUTE 'ALTER TABLE "withdrawal" DROP CONSTRAINT "withdrawal_tx_id_fkey"' ;
    EXECUTE 'ALTER TABLE "treasury" DROP CONSTRAINT "treasury_tx_id_fkey"' ;
    EXECUTE 'ALTER TABLE "reserve" DROP CONSTRAINT "reserve_tx_id_fkey"' ;
    EXECUTE 'ALTER TABLE "pot_transfer" DROP CONSTRAINT "pot_transfer_tx_id_fkey"' ;
    EXECUTE 'ALTER TABLE "ma_tx_mint" DROP CONSTRAINT "ma_tx_mint_tx_id_fkey"' ;
    EXECUTE 'ALTER TABLE "ma_tx_out" DROP CONSTRAINT "ma_tx_out_tx_out_id_fkey"' ;
    EXECUTE 'ALTER TABLE "redeemer" DROP CONSTRAINT "redeemer_tx_id_fkey"' ;
    EXECUTE 'ALTER TABLE "script" DROP CONSTRAINT "script_tx_id_fkey"' ;
    EXECUTE 'ALTER TABLE "datum" DROP CONSTRAINT "datum_tx_id_fkey"' ;
    EXECUTE 'ALTER TABLE "redeemer_data" DROP CONSTRAINT "redeemer_data_tx_id_fkey"' ;
    EXECUTE 'ALTER TABLE "extra_key_witness" DROP CONSTRAINT "extra_key_witness_tx_id_fkey"' ;
    EXECUTE 'ALTER TABLE "param_proposal" DROP CONSTRAINT "param_proposal_registered_tx_id_fkey"' ;
    EXECUTE 'ALTER TABLE "epoch_param" DROP CONSTRAINT "epoch_param_block_id_fkey"' ;
    EXECUTE 'ALTER TABLE "pool_offline_data" DROP CONSTRAINT "pool_offline_data_pmr_id_fkey"' ;
    EXECUTE 'ALTER TABLE "pool_offline_fetch_error" DROP CONSTRAINT "pool_offline_fetch_error_pmr_id_fkey"' ;
--  This was never created, but a dependency is assumed there which needs to be cleaned up on rollbacks
--  EXECUTE 'ALTER TABLE "reverse_index" DROP CONSTRAINT "reverse_index_block_id_fkey"' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
