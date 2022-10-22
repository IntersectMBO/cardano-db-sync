-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 22 THEN
    EXECUTE 'ALTER TABLE "slot_leader" DROP CONSTRAINT "slot_leader_pool_hash_id_fkey"' ;
    EXECUTE 'ALTER TABLE "block" DROP CONSTRAINT "block_slot_leader_id_fkey"' ;
    EXECUTE 'ALTER TABLE "tx_out" DROP CONSTRAINT "tx_out_inline_datum_id_fkey"' ;
    EXECUTE 'ALTER TABLE "tx_out" DROP CONSTRAINT "tx_out_reference_script_id_fkey"' ;
    EXECUTE 'ALTER TABLE "tx_out" DROP CONSTRAINT "tx_out_stake_address_id_fkey"' ;
    EXECUTE 'ALTER TABLE "collateral_tx_out" DROP CONSTRAINT "collateral_tx_out_inline_datum_id_fkey"' ;
    EXECUTE 'ALTER TABLE "collateral_tx_out" DROP CONSTRAINT "collateral_tx_out_reference_script_id_fkey"' ;
    EXECUTE 'ALTER TABLE "collateral_tx_out" DROP CONSTRAINT "collateral_tx_out_stake_address_id_fkey"' ;
    EXECUTE 'ALTER TABLE "tx_in" DROP CONSTRAINT "tx_in_redeemer_id_fkey"' ;
    EXECUTE 'ALTER TABLE "tx_in" DROP CONSTRAINT "tx_in_tx_out_id_fkey"' ;
    EXECUTE 'ALTER TABLE "collateral_tx_in" DROP CONSTRAINT "collateral_tx_in_tx_out_id_fkey"' ;
    EXECUTE 'ALTER TABLE "reference_tx_in" DROP CONSTRAINT "reference_tx_in_tx_out_id_fkey"' ;
    EXECUTE 'ALTER TABLE "pool_metadata_ref" DROP CONSTRAINT "pool_metadata_ref_pool_id_fkey"' ;
    EXECUTE 'ALTER TABLE "pool_update" DROP CONSTRAINT "pool_update_hash_id_fkey"' ;
    EXECUTE 'ALTER TABLE "pool_update" DROP CONSTRAINT "pool_update_meta_id_fkey"' ;
    EXECUTE 'ALTER TABLE "pool_update" DROP CONSTRAINT "pool_update_reward_addr_id_fkey"' ;
    EXECUTE 'ALTER TABLE "pool_owner" DROP CONSTRAINT "pool_owner_addr_id_fkey"' ;
    EXECUTE 'ALTER TABLE "pool_retire" DROP CONSTRAINT "pool_retire_hash_id_fkey"' ;
    EXECUTE 'ALTER TABLE "stake_registration" DROP CONSTRAINT "stake_registration_addr_id_fkey"' ;
    EXECUTE 'ALTER TABLE "stake_deregistration" DROP CONSTRAINT "stake_deregistration_addr_id_fkey"' ;
    EXECUTE 'ALTER TABLE "stake_deregistration" DROP CONSTRAINT "stake_deregistration_redeemer_id_fkey"' ;
    EXECUTE 'ALTER TABLE "delegation" DROP CONSTRAINT "delegation_addr_id_fkey"' ;
    EXECUTE 'ALTER TABLE "delegation" DROP CONSTRAINT "delegation_pool_hash_id_fkey"' ;
    EXECUTE 'ALTER TABLE "delegation" DROP CONSTRAINT "delegation_redeemer_id_fkey"' ;
    EXECUTE 'ALTER TABLE "withdrawal" DROP CONSTRAINT "withdrawal_addr_id_fkey"' ;
    EXECUTE 'ALTER TABLE "withdrawal" DROP CONSTRAINT "withdrawal_redeemer_id_fkey"' ;
    EXECUTE 'ALTER TABLE "treasury" DROP CONSTRAINT "treasury_addr_id_fkey"' ;
    EXECUTE 'ALTER TABLE "reserve" DROP CONSTRAINT "reserve_addr_id_fkey"' ;
    EXECUTE 'ALTER TABLE "ma_tx_mint" DROP CONSTRAINT "ma_tx_mint_ident_fkey"' ;
    EXECUTE 'ALTER TABLE "ma_tx_out" DROP CONSTRAINT "ma_tx_out_ident_fkey"' ;
    EXECUTE 'ALTER TABLE "redeemer" DROP CONSTRAINT "redeemer_redeemer_data_id_fkey"' ;
    EXECUTE 'ALTER TABLE "param_proposal" DROP CONSTRAINT "param_proposal_cost_model_id_fkey"' ;
    EXECUTE 'ALTER TABLE "epoch_param" DROP CONSTRAINT "epoch_param_cost_model_id_fkey"' ;
    EXECUTE 'ALTER TABLE "pool_offline_data" DROP CONSTRAINT "pool_offline_data_pool_id_fkey"' ;
    EXECUTE 'ALTER TABLE "pool_offline_fetch_error" DROP CONSTRAINT "pool_offline_fetch_error_pool_id_fkey"' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
