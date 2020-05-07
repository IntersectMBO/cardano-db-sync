-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 14 THEN
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "block_issuer" hash32type NULL' ;
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "vrf_key" hash32type NULL' ;
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "nonce_vrf" hash32type NULL' ;
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "leader_vrf" hash32type NULL' ;
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "op_cert" hash32type NULL' ;
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "proto_version" hash32type NULL' ;
    EXECUTE 'ALTER TABLE "pool_on_data" ADD CONSTRAINT "pool_on_data_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "central_funds"("id" SERIAL8  PRIMARY KEY UNIQUE,"epoch_no" INT8 NOT NULL,"treasury" lovelace NOT NULL,"reserves" lovelace NOT NULL)' ;
    EXECUTE 'ALTER TABLE "central_funds" ADD CONSTRAINT "unique_central_funds" UNIQUE("epoch_no")' ;
    EXECUTE 'CREATe TABLE "reward"("id" SERIAL8  PRIMARY KEY UNIQUE,"reward_address" staking_addr_hash NOT NULL,"instantaneous" BOOLEAN NOT NULL,"pool_id" hash32type NOT NULL,"epoch_no" INT8 NOT NULL,"reward" lovelace NOT NULL)' ;
    EXECUTE 'ALTER TABLE "reward" ADD CONSTRAINT "unique_reward" UNIQUE("reward_address","pool_id","epoch_no","instantaneous")' ;
    EXECUTE 'CREATe TABLE "delegation"("id" SERIAL8  PRIMARY KEY UNIQUE,"rewards_address" staking_addr_hash NOT NULL,"pool_id" hash32type NOT NULL,"slot_no" INT8 NOT NULL,"amount" lovelace NOT NULL,"in_tx" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "delegation" ADD CONSTRAINT "unique_delegation" UNIQUE("rewards_address","pool_id","in_tx")' ;
    EXECUTE 'ALTER TABLE "delegation" ADD CONSTRAINT "delegation_in_tx_fkey" FOREIGN KEY("in_tx") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "stake_key_registration"("id" SERIAL8  PRIMARY KEY UNIQUE,"stakekey_address" staking_addr_hash NOT NULL,"slot_no" INT8 NOT NULL,"in_tx" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "stake_key_registration" ADD CONSTRAINT "unique_stake_key_registration" UNIQUE("stakekey_address","in_tx")' ;
    EXECUTE 'ALTER TABLE "stake_key_registration" ADD CONSTRAINT "stake_key_registration_in_tx_fkey" FOREIGN KEY("in_tx") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "stake_key_deregistration"("id" SERIAL8  PRIMARY KEY UNIQUE,"stakekey_address" staking_addr_hash NOT NULL,"slot_no" INT8 NOT NULL,"in_tx" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "stake_key_deregistration" ADD CONSTRAINT "unique_stake_key_deregistration" UNIQUE("stakekey_address","in_tx")' ;
    EXECUTE 'ALTER TABLE "stake_key_deregistration" ADD CONSTRAINT "stake_key_deregistration_in_tx_fkey" FOREIGN KEY("in_tx") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "script_registration"("id" SERIAL8  PRIMARY KEY UNIQUE,"script_address" script_hash NOT NULL,"slot_no" INT8 NOT NULL,"in_tx" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "script_registration" ADD CONSTRAINT "unique_script_registration" UNIQUE("script_address","in_tx")' ;
    EXECUTE 'ALTER TABLE "script_registration" ADD CONSTRAINT "script_registration_in_tx_fkey" FOREIGN KEY("in_tx") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "script_deregistration"("id" SERIAL8  PRIMARY KEY UNIQUE,"script_address" script_hash NOT NULL,"slot_no" INT8 NOT NULL,"in_tx" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "script_deregistration" ADD CONSTRAINT "unique_script_deregistration" UNIQUE("script_address","in_tx")' ;
    EXECUTE 'ALTER TABLE "script_deregistration" ADD CONSTRAINT "script_deregistration_in_tx_fkey" FOREIGN KEY("in_tx") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "parameter_update"("id" SERIAL8  PRIMARY KEY UNIQUE,"epoch_no" INT8 NOT NULL,"min_fee" INT8 NOT NULL,"max_fee" INT8 NOT NULL,"max_block_size" INT8 NOT NULL,"max_tx_size" INT8 NOT NULL,"max_bh_size" INT8 NOT NULL,"key_deposit" lovelace NOT NULL,"key_deposit_refund" interval NOT NULL,"key_deposit_decay" rational NOT NULL,"pool_deposit" lovelace NOT NULL,"pool_deposit_refund" interval NOT NULL,"pool_deposit_decay" rational NOT NULL,"max_epoch" INT8 NOT NULL,"n_optimal" INT8 NOT NULL,"influence" rational NOT NULL,"monetary_expand_rate" interval NOT NULL,"treasury_growth_rate" interval NOT NULL,"active_slot_coeff" interval NOT NULL,"decentralisation" interval NOT NULL,"entropy" nonce NOT NULL,"protocol_version" protocol_version NOT NULL)' ;
    EXECUTE 'ALTER TABLE "parameter_update" ADD CONSTRAINT "unique_parameter_update" UNIQUE("epoch_no")' ;
    EXECUTE 'CREATe TABLE "pool_params"("id" SERIAL8  PRIMARY KEY UNIQUE,"pool_id" hash32type NOT NULL,"ticker_id" ticker NOT NULL,"pledge" lovelace NOT NULL,"reward_address" staking_addr_hash NOT NULL,"pool_url" url NOT NULL,"metadata_hash" hash32type NOT NULL,"margin" percentage NOT NULL,"fixed_cost" INT8 NOT NULL,"registered" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "pool_params" ADD CONSTRAINT "unique_pool_params" UNIQUE("pool_id","registered")' ;
    EXECUTE 'CREATe TABLE "pool_retired"("id" SERIAL8  PRIMARY KEY UNIQUE,"pool_id" hash32type NOT NULL,"retired" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "pool_retired" ADD CONSTRAINT "unique_pool_retired" UNIQUE("pool_id","retired")' ;
    EXECUTE 'CREATe TABLE "pool_retiring"("id" SERIAL8  PRIMARY KEY UNIQUE,"pool_id" hash32type NOT NULL,"announced" INT8 NOT NULL,"retiring" INT8 NOT NULL)' ;
    EXECUTE 'CREATe TABLE "stake"("id" SERIAL8  PRIMARY KEY UNIQUE,"stakekey_address" staking_addr_hash NOT NULL,"slot_no" INT8 NOT NULL,"stake" lovelace NOT NULL)' ;
    EXECUTE 'ALTER TABLE "stake" ADD CONSTRAINT "unique_stake" UNIQUE("stakekey_address","stake")' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = 14 ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
