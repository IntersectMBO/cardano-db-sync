CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_three + 1 INTO next_version FROM schema_version ;
  IF next_version <= 6 THEN

    -- Indexes which optimize inserting new rewards
    CREATE INDEX idx_stake_address_hash_raw ON stake_address(hash_raw) ;
    CREATE INDEX idx_delegation_active_epoch_no ON delegation(active_epoch_no) ;
    -- CREATE INDEX idx_block_slot_no ON block(slot_no) ;
    CREATE INDEX idx_pool_update_reward_addr ON pool_update(reward_addr) ;
    CREATE INDEX idx_pool_update_active_epoch_no ON pool_update(active_epoch_no) ;

    UPDATE schema_version SET stage_three = 6 ;
    RAISE NOTICE 'DB has been migrated to stage_three version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
