CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_three + 1 INTO next_version FROM schema_version ;
  IF next_version <= 5 THEN

    -- Adding Indexes to Columns that reference other tables
    -- https://www.postgresql.org/message-id/20040430163539.74079.qmail@web13805.mail.yahoo.com
    -- Some of them are intentional commented out, because they already exist.
    CREATE INDEX idx_delegation_pool_hash_id ON delegation(pool_hash_id) ;
    CREATE INDEX idx_epoch_stake_pool_id ON epoch_stake(pool_id) ;
    CREATE INDEX idx_orphaned_reward_pool_id ON orphaned_reward(pool_id) ;
    CREATE INDEX idx_pool_owner_pool_hash_id ON pool_owner(pool_hash_id) ;
    CREATE INDEX idx_pool_retire_hash_id ON pool_retire(hash_id) ;
    --    CREATE INDEX idx_pool_update_hash_id ON pool_update(hash_id) ;
    CREATE INDEX idx_reward_pool_id ON reward(pool_id) ;
    CREATE INDEX idx_slot_leader_pool_hash_id ON slot_leader(pool_hash_id) ;
    CREATE INDEX idx_block_slot_leader_id ON block(slot_leader_id) ;
    --    CREATE INDEX idx_block_previous_id ON block(previous_id) ;
    CREATE INDEX idx_epoch_param_block_id ON epoch_param(block_id) ;
    CREATE INDEX idx_epoch_stake_epoch_no ON epoch_stake(epoch_no) ;
    CREATE INDEX idx_orphaned_reward_epoch_no ON orphaned_reward(epoch_no) ;
    CREATE INDEX idx_reward_earned_epoch ON reward(earned_epoch) ;
    --    CREATE INDEX idx_tx_block_id ON tx(block_id) ;
    CREATE INDEX idx_delegation_tx_id ON delegation(tx_id) ;
    CREATE INDEX idx_ma_tx_mint_tx_id ON ma_tx_mint(tx_id) ;
    CREATE INDEX idx_param_proposal_registered_tx_id ON param_proposal(registered_tx_id) ;
    CREATE INDEX idx_pool_metadata_ref_registered_tx_id ON pool_metadata_ref(registered_tx_id) ;
    CREATE INDEX idx_pool_owner_registered_tx_id ON pool_owner(registered_tx_id) ;
    CREATE INDEX idx_pool_retire_announced_tx_id ON pool_retire(announced_tx_id) ;
    CREATE INDEX idx_pool_update_registered_tx_id ON pool_update(registered_tx_id) ;
    CREATE INDEX idx_reserve_tx_id ON reserve(tx_id) ;
    CREATE INDEX idx_stake_address_registered_tx_id ON stake_address(registered_tx_id) ;
    CREATE INDEX idx_stake_deregistration_tx_id ON stake_deregistration(tx_id) ;
    CREATE INDEX idx_stake_registration_tx_id ON stake_registration(tx_id) ;
    CREATE INDEX idx_treasury_tx_id ON treasury(tx_id) ;
    CREATE INDEX idx_tx_in_tx_in_id ON tx_in(tx_in_id) ;
    CREATE INDEX idx_tx_in_tx_out_id ON tx_in(tx_out_id) ;
    CREATE INDEX idx_tx_metadata_tx_id ON tx_metadata(tx_id) ;
    CREATE INDEX idx_tx_out_tx_id ON tx_out(tx_id) ;
    CREATE INDEX idx_withdrawal_tx_id ON withdrawal(tx_id) ;
    CREATE INDEX idx_delegation_addr_id ON delegation(addr_id) ;
    CREATE INDEX idx_epoch_stake_addr_id ON epoch_stake(addr_id) ;
    CREATE INDEX idx_orphaned_reward_addr_id ON orphaned_reward(addr_id) ;
    CREATE INDEX idx_reserve_addr_id ON reserve(addr_id) ;
    CREATE INDEX idx_reward_addr_id ON reward(addr_id) ;
    CREATE INDEX idx_stake_deregistration_addr_id ON stake_deregistration(addr_id) ;
    CREATE INDEX idx_stake_registration_addr_id ON stake_registration(addr_id) ;
    CREATE INDEX idx_treasury_addr_id ON treasury(addr_id) ;
    CREATE INDEX idx_tx_out_stake_address_id ON tx_out(stake_address_id) ;
    CREATE INDEX idx_withdrawal_addr_id ON withdrawal(addr_id) ;
    CREATE INDEX idx_ma_tx_out_tx_out_id ON ma_tx_out(tx_out_id) ;
    CREATE INDEX idx_pool_update_meta_id ON pool_update(meta_id) ;
    CREATE INDEX idx_pool_relay_update_id ON pool_relay(update_id) ;

    UPDATE schema_version SET stage_three = 5 ;
    RAISE NOTICE 'DB has been migrated to stage_three version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
