
-- Adding Indexes to Columns that reference other tables.
-- Even though foreign keys are deleted, these may be needed for queries.
-- They can also speed up rollbacks by a small amount.
-- Some of them are intentional commented out, because they already exist.
CREATE INDEX IF NOT EXISTS idx_delegation_pool_hash_id ON delegation(pool_hash_id) ;
CREATE INDEX IF NOT EXISTS idx_epoch_stake_pool_id ON epoch_stake(pool_id) ;
CREATE INDEX IF NOT EXISTS idx_pool_retire_hash_id ON pool_retire(hash_id) ;
CREATE INDEX IF NOT EXISTS idx_reward_pool_id ON reward(pool_id) ;
CREATE INDEX IF NOT EXISTS idx_slot_leader_pool_hash_id ON slot_leader(pool_hash_id) ;
CREATE INDEX IF NOT EXISTS idx_block_slot_leader_id ON block(slot_leader_id) ;
CREATE INDEX IF NOT EXISTS idx_block_previous_id ON block(previous_id) ;
CREATE INDEX IF NOT EXISTS idx_epoch_param_block_id ON epoch_param(block_id) ;
CREATE INDEX IF NOT EXISTS idx_reward_earned_epoch ON reward(earned_epoch) ;
CREATE INDEX IF NOT EXISTS idx_delegation_tx_id ON delegation(tx_id) ;
CREATE INDEX IF NOT EXISTS idx_ma_tx_mint_tx_id ON ma_tx_mint(tx_id) ;
CREATE INDEX IF NOT EXISTS idx_param_proposal_registered_tx_id ON param_proposal(registered_tx_id) ;
CREATE INDEX IF NOT EXISTS idx_pool_metadata_ref_registered_tx_id ON pool_metadata_ref(registered_tx_id) ;
CREATE INDEX IF NOT EXISTS idx_pool_retire_announced_tx_id ON pool_retire(announced_tx_id) ;
CREATE INDEX IF NOT EXISTS idx_pool_update_registered_tx_id ON pool_update(registered_tx_id) ;
CREATE INDEX IF NOT EXISTS idx_reserve_tx_id ON reserve(tx_id) ;
CREATE INDEX IF NOT EXISTS idx_stake_deregistration_tx_id ON stake_deregistration(tx_id) ;
CREATE INDEX IF NOT EXISTS idx_stake_registration_tx_id ON stake_registration(tx_id) ;
CREATE INDEX IF NOT EXISTS idx_treasury_tx_id ON treasury(tx_id) ;
CREATE INDEX IF NOT EXISTS idx_tx_in_tx_in_id ON tx_in(tx_in_id) ;
CREATE INDEX IF NOT EXISTS idx_tx_in_tx_out_id ON tx_in(tx_out_id) ;
CREATE INDEX IF NOT EXISTS idx_tx_metadata_tx_id ON tx_metadata(tx_id) ;
CREATE INDEX IF NOT EXISTS idx_tx_out_tx_id ON tx_out(tx_id) ;
CREATE INDEX IF NOT EXISTS idx_withdrawal_tx_id ON withdrawal(tx_id) ;
CREATE INDEX IF NOT EXISTS idx_delegation_addr_id ON delegation(addr_id) ;
CREATE INDEX IF NOT EXISTS idx_epoch_stake_addr_id ON epoch_stake(addr_id) ;
CREATE INDEX IF NOT EXISTS idx_reserve_addr_id ON reserve(addr_id) ;
CREATE INDEX IF NOT EXISTS idx_reward_addr_id ON reward(addr_id) ;
CREATE INDEX IF NOT EXISTS idx_stake_deregistration_addr_id ON stake_deregistration(addr_id) ;
CREATE INDEX IF NOT EXISTS idx_stake_registration_addr_id ON stake_registration(addr_id) ;
CREATE INDEX IF NOT EXISTS idx_treasury_addr_id ON treasury(addr_id) ;
CREATE INDEX IF NOT EXISTS idx_tx_out_stake_address_id ON tx_out(stake_address_id) ;
CREATE INDEX IF NOT EXISTS idx_withdrawal_addr_id ON withdrawal(addr_id) ;
CREATE INDEX IF NOT EXISTS idx_ma_tx_out_tx_out_id ON ma_tx_out(tx_out_id) ;
CREATE INDEX IF NOT EXISTS idx_pool_update_meta_id ON pool_update(meta_id) ;
CREATE INDEX IF NOT EXISTS idx_pool_relay_update_id ON pool_relay(update_id) ;
--  CREATE INDEX IF NOT EXISTS idx_pool_update_hash_id ON pool_update(hash_id) ;
--  CREATE INDEX IF NOT EXISTS idx_tx_block_id ON tx(block_id) ;
