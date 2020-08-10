-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 3 THEN
    EXECUTE 'CREATe TABLE "pool_hash"("id" SERIAL8  PRIMARY KEY UNIQUE,"hash" hash28type NOT NULL)' ;
    EXECUTE 'ALTER TABLE "pool_hash" ADD CONSTRAINT "unique_pool_hash" UNIQUE("hash")' ;
    EXECUTE 'ALTER TABLE "slot_leader" ALTER COLUMN "hash" TYPE hash32type' ;
    EXECUTE 'ALTER TABLE "slot_leader" ADD COLUMN "pool_hash_id" INT8 NULL' ;
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "epoch_slot_no" uinteger NULL' ;
    EXECUTE 'ALTER TABLE "block" ALTER COLUMN "tx_count" TYPE INT8' ;
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "vrf_key" hash32type NULL' ;
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "op_cert" hash32type NULL' ;
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "proto_version" VARCHAR NULL' ;
    EXECUTE 'ALTER TABLE "tx" ADD COLUMN "deposit" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "tx_out" ADD COLUMN "address_raw" BYTEA NOT NULL' ;
    EXECUTE 'ALTER TABLE "tx_out" ADD COLUMN "payment_cred" hash28type NULL' ;
    EXECUTE 'ALTER TABLE "meta" ALTER COLUMN "network_name" SET NOT NULL' ;
    EXECUTE 'ALTER TABLE "meta" DROP COLUMN "protocol_const"' ;
    EXECUTE 'ALTER TABLE "meta" DROP COLUMN "slot_duration"' ;
    EXECUTE 'ALTER TABLE "meta" DROP COLUMN "slots_per_epoch"' ;
    EXECUTE 'ALTER TABLE "epoch" ALTER COLUMN "out_sum" TYPE word128type' ;
    EXECUTE 'CREATe TABLE "stake_address"("id" SERIAL8  PRIMARY KEY UNIQUE,"hash_raw" addr29type NOT NULL,"view" VARCHAR NOT NULL,"registered_tx_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "stake_address" ADD CONSTRAINT "unique_stake_address" UNIQUE("hash_raw")' ;
    EXECUTE 'ALTER TABLE "stake_address" ADD CONSTRAINT "stake_address_registered_tx_id_fkey" FOREIGN KEY("registered_tx_id") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "pool_meta_data"("id" SERIAL8  PRIMARY KEY UNIQUE,"url" VARCHAR NOT NULL,"hash" hash32type NOT NULL,"registered_tx_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "pool_meta_data" ADD CONSTRAINT "unique_pool_meta_data" UNIQUE("url","hash")' ;
    EXECUTE 'ALTER TABLE "pool_meta_data" ADD CONSTRAINT "pool_meta_data_registered_tx_id_fkey" FOREIGN KEY("registered_tx_id") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "pool_update"("id" SERIAL8  PRIMARY KEY UNIQUE,"hash_id" INT8 NOT NULL,"cert_index" INT4 NOT NULL,"vrf_key" hash32type NOT NULL,"pledge" word64type NOT NULL,"reward_addr_id" INT8 NOT NULL,"meta" INT8 NULL,"margin" DOUBLE PRECISION NOT NULL,"fixed_cost" lovelace NOT NULL,"registered_tx_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "pool_update" ADD CONSTRAINT "unique_pool_update" UNIQUE("hash_id","registered_tx_id")' ;
    EXECUTE 'ALTER TABLE "pool_update" ADD CONSTRAINT "pool_update_hash_id_fkey" FOREIGN KEY("hash_id") REFERENCES "pool_hash"("id")' ;
    EXECUTE 'ALTER TABLE "pool_update" ADD CONSTRAINT "pool_update_reward_addr_id_fkey" FOREIGN KEY("reward_addr_id") REFERENCES "stake_address"("id")' ;
    EXECUTE 'ALTER TABLE "pool_update" ADD CONSTRAINT "pool_update_meta_fkey" FOREIGN KEY("meta") REFERENCES "pool_meta_data"("id")' ;
    EXECUTE 'ALTER TABLE "pool_update" ADD CONSTRAINT "pool_update_registered_tx_id_fkey" FOREIGN KEY("registered_tx_id") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "pool_owner"("id" SERIAL8  PRIMARY KEY UNIQUE,"hash" hash28type NOT NULL,"pool_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "pool_owner" ADD CONSTRAINT "unique_pool_owner" UNIQUE("hash")' ;
    EXECUTE 'ALTER TABLE "pool_owner" ADD CONSTRAINT "pool_owner_pool_id_fkey" FOREIGN KEY("pool_id") REFERENCES "pool_hash"("id")' ;
    EXECUTE 'CREATe TABLE "pool_retire"("id" SERIAL8  PRIMARY KEY UNIQUE,"update_id" INT8 NOT NULL,"cert_index" INT4 NOT NULL,"announced_tx_id" INT8 NOT NULL,"retiring_epoch" uinteger NOT NULL)' ;
    EXECUTE 'ALTER TABLE "pool_retire" ADD CONSTRAINT "unique_pool_retiring" UNIQUE("update_id")' ;
    EXECUTE 'ALTER TABLE "pool_retire" ADD CONSTRAINT "pool_retire_update_id_fkey" FOREIGN KEY("update_id") REFERENCES "pool_update"("id")' ;
    EXECUTE 'ALTER TABLE "pool_retire" ADD CONSTRAINT "pool_retire_announced_tx_id_fkey" FOREIGN KEY("announced_tx_id") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "pool_relay"("id" SERIAL8  PRIMARY KEY UNIQUE,"update_id" INT8 NOT NULL,"ipv4" VARCHAR NULL,"ipv6" VARCHAR NULL,"dns_name" VARCHAR NULL,"dns_srv_name" VARCHAR NULL,"port" INT4 NULL)' ;
    EXECUTE 'ALTER TABLE "pool_relay" ADD CONSTRAINT "unique_pool_relay" UNIQUE("update_id","ipv4","ipv6","dns_name")' ;
    EXECUTE 'ALTER TABLE "pool_relay" ADD CONSTRAINT "pool_relay_update_id_fkey" FOREIGN KEY("update_id") REFERENCES "pool_update"("id")' ;
    EXECUTE 'CREATe TABLE "reserve"("id" SERIAL8  PRIMARY KEY UNIQUE,"addr_id" INT8 NOT NULL,"cert_index" INT4 NOT NULL,"amount" lovelace NOT NULL,"tx_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "reserve" ADD CONSTRAINT "unique_reserves" UNIQUE("addr_id","tx_id")' ;
    EXECUTE 'ALTER TABLE "reserve" ADD CONSTRAINT "reserve_addr_id_fkey" FOREIGN KEY("addr_id") REFERENCES "stake_address"("id")' ;
    EXECUTE 'ALTER TABLE "reserve" ADD CONSTRAINT "reserve_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "withdrawal"("id" SERIAL8  PRIMARY KEY UNIQUE,"addr_id" INT8 NOT NULL,"amount" lovelace NOT NULL,"tx_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "withdrawal" ADD CONSTRAINT "unique_withdrawal" UNIQUE("addr_id","tx_id")' ;
    EXECUTE 'ALTER TABLE "withdrawal" ADD CONSTRAINT "withdrawal_addr_id_fkey" FOREIGN KEY("addr_id") REFERENCES "stake_address"("id")' ;
    EXECUTE 'ALTER TABLE "withdrawal" ADD CONSTRAINT "withdrawal_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "delegation"("id" SERIAL8  PRIMARY KEY UNIQUE,"addr_id" INT8 NOT NULL,"cert_index" INT4 NOT NULL,"update_id" INT8 NOT NULL,"tx_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "delegation" ADD CONSTRAINT "unique_delegation" UNIQUE("addr_id","update_id","tx_id")' ;
    EXECUTE 'ALTER TABLE "delegation" ADD CONSTRAINT "delegation_addr_id_fkey" FOREIGN KEY("addr_id") REFERENCES "stake_address"("id")' ;
    EXECUTE 'ALTER TABLE "delegation" ADD CONSTRAINT "delegation_update_id_fkey" FOREIGN KEY("update_id") REFERENCES "pool_update"("id")' ;
    EXECUTE 'ALTER TABLE "delegation" ADD CONSTRAINT "delegation_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "stake_registration"("id" SERIAL8  PRIMARY KEY UNIQUE,"addr_id" INT8 NOT NULL,"cert_index" INT4 NOT NULL,"tx_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "stake_registration" ADD CONSTRAINT "unique_stake_registration" UNIQUE("addr_id","tx_id")' ;
    EXECUTE 'ALTER TABLE "stake_registration" ADD CONSTRAINT "stake_registration_addr_id_fkey" FOREIGN KEY("addr_id") REFERENCES "stake_address"("id")' ;
    EXECUTE 'ALTER TABLE "stake_registration" ADD CONSTRAINT "stake_registration_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "stake_deregistration"("id" SERIAL8  PRIMARY KEY UNIQUE,"addr_id" INT8 NOT NULL,"cert_index" INT4 NOT NULL,"tx_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "stake_deregistration" ADD CONSTRAINT "unique_stake_deregistration" UNIQUE("addr_id","tx_id")' ;
    EXECUTE 'ALTER TABLE "stake_deregistration" ADD CONSTRAINT "stake_deregistration_addr_id_fkey" FOREIGN KEY("addr_id") REFERENCES "stake_address"("id")' ;
    EXECUTE 'ALTER TABLE "stake_deregistration" ADD CONSTRAINT "stake_deregistration_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "tx_metadata"("id" SERIAL8  PRIMARY KEY UNIQUE,"key" word64type NOT NULL,"json" jsonb NOT NULL,"tx_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "tx_metadata" ADD CONSTRAINT "unique_tx_metadata" UNIQUE("key","tx_id")' ;
    EXECUTE 'ALTER TABLE "tx_metadata" ADD CONSTRAINT "tx_metadata_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "reward"("id" SERIAL8  PRIMARY KEY UNIQUE,"addr_id" INT8 NOT NULL,"cert_index" INT4 NOT NULL,"amount" lovelace NOT NULL,"tx_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "reward" ADD CONSTRAINT "unique_reward" UNIQUE("addr_id","tx_id")' ;
    EXECUTE 'ALTER TABLE "reward" ADD CONSTRAINT "reward_addr_id_fkey" FOREIGN KEY("addr_id") REFERENCES "stake_address"("id")' ;
    EXECUTE 'ALTER TABLE "reward" ADD CONSTRAINT "reward_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "stake"("id" SERIAL8  PRIMARY KEY UNIQUE,"addr_id" INT8 NOT NULL,"tx_id" INT8 NOT NULL,"stake" lovelace NOT NULL)' ;
    EXECUTE 'ALTER TABLE "stake" ADD CONSTRAINT "unique_stake" UNIQUE("addr_id","stake")' ;
    EXECUTE 'ALTER TABLE "stake" ADD CONSTRAINT "stake_addr_id_fkey" FOREIGN KEY("addr_id") REFERENCES "stake_address"("id")' ;
    EXECUTE 'ALTER TABLE "stake" ADD CONSTRAINT "stake_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "treasury"("id" SERIAL8  PRIMARY KEY UNIQUE,"addr_id" INT8 NOT NULL,"cert_index" INT4 NOT NULL,"amount" lovelace NOT NULL,"tx_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "treasury" ADD CONSTRAINT "unique_treasury" UNIQUE("addr_id","tx_id")' ;
    EXECUTE 'ALTER TABLE "treasury" ADD CONSTRAINT "treasury_addr_id_fkey" FOREIGN KEY("addr_id") REFERENCES "stake_address"("id")' ;
    EXECUTE 'ALTER TABLE "treasury" ADD CONSTRAINT "treasury_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id")' ;
    EXECUTE 'CREATe TABLE "param_update"("id" SERIAL8  PRIMARY KEY UNIQUE,"epoch_no" uinteger NOT NULL,"key" hash28type NOT NULL,"min_fee_a" uinteger NULL,"min_fee_b" uinteger NULL,"max_block_size" uinteger NULL,"max_tx_size" uinteger NULL,"max_bh_size" uinteger NULL,"key_deposit" lovelace NULL,"pool_deposit" lovelace NULL,"max_epoch" uinteger NULL,"n_optimal" uinteger NULL,"influence" DOUBLE PRECISION NULL,"monetary_expand_rate" DOUBLE PRECISION NULL,"treasury_growth_rate" DOUBLE PRECISION NULL,"active_slot_coeff" DOUBLE PRECISION NULL,"entropy" hash32type NULL,"protocol_version" VARCHAR NULL,"min_u_tx_o_value" lovelace NULL,"min_pool_cost" lovelace NULL,"registered_tx_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "param_update" ADD CONSTRAINT "unique_param_update" UNIQUE("key","registered_tx_id")' ;
    EXECUTE 'ALTER TABLE "param_update" ADD CONSTRAINT "param_update_registered_tx_id_fkey" FOREIGN KEY("registered_tx_id") REFERENCES "tx"("id")' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = 3 ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
