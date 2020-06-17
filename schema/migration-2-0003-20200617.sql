-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 3 THEN
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "vrf_key" hash32type NULL' ;
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "nonce_vrf" hash32type NULL' ;
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "leader_vrf" hash32type NULL' ;
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "op_cert" hash32type NULL' ;
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "proto_version" hash32type NULL' ;
    EXECUTE 'CREATe TABLE "stake_address"("id" SERIAL8  PRIMARY KEY UNIQUE,"hash" addr33type NOT NULL)' ;
    EXECUTE 'ALTER TABLE "stake_address" ADD CONSTRAINT "unique_stake_address" UNIQUE("hash")' ;
    EXECUTE 'CREATe TABLE "pool_chain_data"("id" SERIAL8  PRIMARY KEY UNIQUE,"tx_id" INT8 NOT NULL,"url" VARCHAR NOT NULL,"hash" hash32type NOT NULL)' ;
    EXECUTE 'ALTER TABLE "pool_chain_data" ADD CONSTRAINT "unique_pool_chain_data" UNIQUE("url")' ;
    EXECUTE 'ALTER TABLE "pool_chain_data" ADD CONSTRAINT "pool_chain_data_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "pool_meta_data"("id" SERIAL8  PRIMARY KEY UNIQUE,"url" VARCHAR NOT NULL,"hash" hash32type NOT NULL,"tx_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "pool_meta_data" ADD CONSTRAINT "unique_pool_meta_data" UNIQUE("url")' ;
    EXECUTE 'ALTER TABLE "pool_meta_data" ADD CONSTRAINT "pool_meta_data_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "pool"("id" SERIAL8  PRIMARY KEY UNIQUE,"hash" hash32type NOT NULL,"pledge" INT8 NOT NULL,"reward_addr_id" INT8 NOT NULL,"meta" INT8 NULL,"margin" DOUBLE PRECISION NOT NULL,"fixed_cost" INT8 NOT NULL,"registered" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "pool" ADD CONSTRAINT "unique_pool" UNIQUE("hash","registered")' ;
    EXECUTE 'ALTER TABLE "pool" ADD CONSTRAINT "pool_reward_addr_id_fkey" FOREIGN KEY("reward_addr_id") REFERENCES "stake_address"("id")' ;
    EXECUTE 'ALTER TABLE "pool" ADD CONSTRAINT "pool_meta_fkey" FOREIGN KEY("meta") REFERENCES "pool_meta_data"("id")' ;
    EXECUTE 'ALTER TABLE "pool" ADD CONSTRAINT "pool_registered_fkey" FOREIGN KEY("registered") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "pool_owner"("id" SERIAL8  PRIMARY KEY UNIQUE,"hash" hash32type NOT NULL,"pool_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "pool_owner" ADD CONSTRAINT "unique_pool_owner" UNIQUE("hash")' ;
    EXECUTE 'ALTER TABLE "pool_owner" ADD CONSTRAINT "pool_owner_pool_id_fkey" FOREIGN KEY("pool_id") REFERENCES "pool"("id")' ;
    EXECUTE 'CREATe TABLE "pool_retire"("id" SERIAL8  PRIMARY KEY UNIQUE,"pool_id" INT8 NOT NULL,"announced_tx_id" INT8 NOT NULL,"retiring_epoch" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "pool_retire" ADD CONSTRAINT "unique_pool_retiring" UNIQUE("pool_id")' ;
    EXECUTE 'ALTER TABLE "pool_retire" ADD CONSTRAINT "pool_retire_pool_id_fkey" FOREIGN KEY("pool_id") REFERENCES "pool"("id")' ;
    EXECUTE 'ALTER TABLE "pool_retire" ADD CONSTRAINT "pool_retire_announced_tx_id_fkey" FOREIGN KEY("announced_tx_id") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "central_funds"("id" SERIAL8  PRIMARY KEY UNIQUE,"epoch_no" INT8 NOT NULL,"treasury" lovelace NOT NULL,"reserves" lovelace NOT NULL)' ;
    EXECUTE 'ALTER TABLE "central_funds" ADD CONSTRAINT "unique_central_funds" UNIQUE("epoch_no")' ;
    EXECUTE 'CREATe TABLE "reward"("id" SERIAL8  PRIMARY KEY UNIQUE,"addr_id" INT8 NOT NULL,"instantaneous" BOOLEAN NOT NULL,"pool_id" INT8 NOT NULL,"epoch_no" INT8 NOT NULL,"reward" lovelace NOT NULL)' ;
    EXECUTE 'ALTER TABLE "reward" ADD CONSTRAINT "unique_reward" UNIQUE("addr_id","pool_id","epoch_no","instantaneous")' ;
    EXECUTE 'ALTER TABLE "reward" ADD CONSTRAINT "reward_addr_id_fkey" FOREIGN KEY("addr_id") REFERENCES "stake_address"("id")' ;
    EXECUTE 'ALTER TABLE "reward" ADD CONSTRAINT "reward_pool_id_fkey" FOREIGN KEY("pool_id") REFERENCES "pool"("id")' ;
    EXECUTE 'CREATe TABLE "delegation"("id" SERIAL8  PRIMARY KEY UNIQUE,"addr_id" INT8 NOT NULL,"pool_id" INT8 NOT NULL,"slot_no" INT8 NOT NULL,"amount" lovelace NOT NULL,"tx_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "delegation" ADD CONSTRAINT "unique_delegation" UNIQUE("addr_id","pool_id","tx_id")' ;
    EXECUTE 'ALTER TABLE "delegation" ADD CONSTRAINT "delegation_addr_id_fkey" FOREIGN KEY("addr_id") REFERENCES "stake_address"("id")' ;
    EXECUTE 'ALTER TABLE "delegation" ADD CONSTRAINT "delegation_pool_id_fkey" FOREIGN KEY("pool_id") REFERENCES "pool"("id")' ;
    EXECUTE 'ALTER TABLE "delegation" ADD CONSTRAINT "delegation_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "stake_key_registration"("id" SERIAL8  PRIMARY KEY UNIQUE,"addr_id" INT8 NOT NULL,"slot_no" INT8 NOT NULL,"tx_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "stake_key_registration" ADD CONSTRAINT "unique_stake_key_registration" UNIQUE("addr_id","tx_id")' ;
    EXECUTE 'ALTER TABLE "stake_key_registration" ADD CONSTRAINT "stake_key_registration_addr_id_fkey" FOREIGN KEY("addr_id") REFERENCES "stake_address"("id")' ;
    EXECUTE 'ALTER TABLE "stake_key_registration" ADD CONSTRAINT "stake_key_registration_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "stake_key_deregistration"("id" SERIAL8  PRIMARY KEY UNIQUE,"addr_id" INT8 NOT NULL,"slot_no" INT8 NOT NULL,"tx_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "stake_key_deregistration" ADD CONSTRAINT "unique_stake_key_deregistration" UNIQUE("addr_id","tx_id")' ;
    EXECUTE 'ALTER TABLE "stake_key_deregistration" ADD CONSTRAINT "stake_key_deregistration_addr_id_fkey" FOREIGN KEY("addr_id") REFERENCES "stake_address"("id")' ;
    EXECUTE 'ALTER TABLE "stake_key_deregistration" ADD CONSTRAINT "stake_key_deregistration_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "script_registration"("id" SERIAL8  PRIMARY KEY UNIQUE,"addr_id" INT8 NOT NULL,"slot_no" INT8 NOT NULL,"tx_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "script_registration" ADD CONSTRAINT "unique_script_registration" UNIQUE("addr_id","tx_id")' ;
    EXECUTE 'ALTER TABLE "script_registration" ADD CONSTRAINT "script_registration_addr_id_fkey" FOREIGN KEY("addr_id") REFERENCES "stake_address"("id")' ;
    EXECUTE 'ALTER TABLE "script_registration" ADD CONSTRAINT "script_registration_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "script_deregistration"("id" SERIAL8  PRIMARY KEY UNIQUE,"addr_id" INT8 NOT NULL,"slot_no" INT8 NOT NULL,"tx_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "script_deregistration" ADD CONSTRAINT "unique_script_deregistration" UNIQUE("addr_id","tx_id")' ;
    EXECUTE 'ALTER TABLE "script_deregistration" ADD CONSTRAINT "script_deregistration_addr_id_fkey" FOREIGN KEY("addr_id") REFERENCES "stake_address"("id")' ;
    EXECUTE 'ALTER TABLE "script_deregistration" ADD CONSTRAINT "script_deregistration_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "stake"("id" SERIAL8  PRIMARY KEY UNIQUE,"addr_id" INT8 NOT NULL,"slot_no" INT8 NOT NULL,"stake" lovelace NOT NULL)' ;
    EXECUTE 'ALTER TABLE "stake" ADD CONSTRAINT "unique_stake" UNIQUE("addr_id","stake")' ;
    EXECUTE 'ALTER TABLE "stake" ADD CONSTRAINT "stake_addr_id_fkey" FOREIGN KEY("addr_id") REFERENCES "stake_address"("id")' ;
    EXECUTE 'CREATe TABLE "param_update"("id" SERIAL8  PRIMARY KEY UNIQUE,"epoch_no" INT8 NOT NULL,"min_fee" INT8 NOT NULL,"max_fee" INT8 NOT NULL,"max_block_size" INT8 NOT NULL,"max_tx_size" INT8 NOT NULL,"max_bh_size" INT8 NOT NULL,"key_deposit" lovelace NOT NULL,"pool_deposit" lovelace NOT NULL,"max_epoch" INT8 NOT NULL,"n_optimal" INT8 NOT NULL,"influence" DOUBLE PRECISION NOT NULL,"monetary_expand_rate" interval NOT NULL,"treasury_growth_rate" interval NOT NULL,"active_slot_coeff" interval NOT NULL,"decentralisation" interval NOT NULL,"entropy" hash32type NOT NULL,"protocol_version" BYTEA NOT NULL,"min_coin" lovelace NOT NULL)' ;
    EXECUTE 'ALTER TABLE "param_update" ADD CONSTRAINT "unique_param_update" UNIQUE("epoch_no")' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = 3 ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
