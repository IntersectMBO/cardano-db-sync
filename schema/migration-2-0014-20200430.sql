-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 14 THEN

    -- TX.

    -- BLOCK.
    -- For Shelley. All of them can be NULLABLE since we have one table for both
    -- Byron and Shelley.
    EXECUTE 'ALTER TABLE block ADD COLUMN "block_issuer" hash32type NULL';
    EXECUTE 'ALTER TABLE block ADD COLUMN "vrf_key" hash32type NULL';
    EXECUTE 'ALTER TABLE block ADD COLUMN "nonce_vrf" hash32type NULL';
    EXECUTE 'ALTER TABLE block ADD COLUMN "leader_vrf" hash32type NULL';
    EXECUTE 'ALTER TABLE block ADD COLUMN "op_cert" hash32type NULL';
    EXECUTE 'ALTER TABLE block ADD COLUMN "proto_version" hash32type NULL';

    -- TXIN.
    -- One diff, the index.
    
    -- TXOUT.
    
    -- NEW for SHELLEY

    -- CENTRAL_FUNDS
    -- DO we connect up with the EPOCH?
    -- FOREIGN KEY (epoch_no) REFERENCES epoch (id)

    CREATE TABLE central_funds (
        id                      bigint              NOT NULL,
        epoch_no                uinteger            NOT NULL,
        treasury                lovelace            NOT NULL,
        reserves                lovelace            NOT NULL,

        PRIMARY KEY (id),
        CONSTRAINT central_funds_uk UNIQUE (epoch_no)
    );
    

    CREATE SEQUENCE public.central_funds_id_seq
        START WITH 1
        INCREMENT BY 1
        NO MINVALUE
        NO MAXVALUE
        CACHE 1;
    
    ALTER SEQUENCE public.central_funds_id_seq OWNED BY public.central_funds.id;

    -- Basic rewards data.
    -- What reward has been earned in the epoch by delegating to the specified pool id.
    -- This design allows rewards to be discriminated based on how they are earned.

    CREATE TABLE reward (
        id                      bigint              NOT NULL,
        reward_address          staking_addr_hash   NOT NULL,
        instantaneous           boolean             NOT NULL,
        pool_id                 hash32type          NOT NULL,  -- Arbitrary if an instantaneous reward - needed for primary key
        epoch_no                uinteger            NOT NULL,
        reward                  lovelace            NOT NULL,


        PRIMARY KEY (id),
        CONSTRAINT reward_uk UNIQUE (reward_address,pool_id,epoch_no,instantaneous)
    );

    CREATE SEQUENCE public.reward_id_seq
        START WITH 1
        INCREMENT BY 1
        NO MINVALUE
        NO MAXVALUE
        CACHE 1;
    
    ALTER SEQUENCE public.reward_id_seq OWNED BY public.reward.id;

    -- Basic delegation data.
    -- What has been delegated to a pool and when.  Where will the rewards be recorded.
    -- The epoch can be deduced from the unique slot number.
    -- Note the overlap with the rewards table in the first two fields.
    
    CREATE TABLE delegation (
        id                      bigint              NOT NULL,
        rewards_address         staking_addr_hash   NOT NULL,
        pool_id                 hash32type          NOT NULL,
        slot_no                 uinteger            NOT NULL,      -- technically redundant, but useful
        amount                  lovelace            NOT NULL,
        in_tx                   bigint              NOT NULL REFERENCES tx (id) ON DELETE CASCADE,
    
        PRIMARY KEY (id),
        CONSTRAINT delegation_uk UNIQUE (rewards_address,pool_id,in_tx)
    );

    CREATE SEQUENCE public.delegation_id_seq
        START WITH 1
        INCREMENT BY 1
        NO MINVALUE
        NO MAXVALUE
        CACHE 1;
    
    ALTER SEQUENCE public.delegation_id_seq OWNED BY public.delegation.id;

    -- When was a staking key registered

    CREATE TABLE stake_key_registration (
        id                      bigint              NOT NULL,
        stakekey_address        staking_addr_hash   NOT NULL,
        slot_no                 uinteger            NOT NULL,
        in_tx                   bigint              NOT NULL REFERENCES tx (id) ON DELETE CASCADE,
    
        PRIMARY KEY (id),
        CONSTRAINT stake_key_registration_uk UNIQUE (stakekey_address,in_tx)
    );

    CREATE SEQUENCE public.stake_key_registration_id_seq
        START WITH 1
        INCREMENT BY 1
        NO MINVALUE
        NO MAXVALUE
        CACHE 1;
    
    ALTER SEQUENCE public.stake_key_registration_id_seq OWNED BY public.stake_key_registration.id;

    -- When was a staking key unregistered
    
    CREATE TABLE stake_key_deregistration (
       id                       bigint              NOT NULL,
       stakekey_address         staking_addr_hash   NOT NULL,
       slot_no                  uinteger            NOT NULL,
       in_tx                    bigint              NOT NULL REFERENCES tx (id) ON DELETE CASCADE,
    
       PRIMARY KEY (id),
       CONSTRAINT stake_key_deregistration_uk UNIQUE (stakekey_address,in_tx)
    );
    
    CREATE SEQUENCE public.stake_key_deregistration_id_seq
        START WITH 1
        INCREMENT BY 1
        NO MINVALUE
        NO MAXVALUE
        CACHE 1;
    
    ALTER SEQUENCE public.stake_key_deregistration_id_seq OWNED BY public.stake_key_deregistration.id;
    
    -- When was a script registered
    
    CREATE TABLE script_registration (
       id                       bigint              NOT NULL,
       script_address           script_hash         NOT NULL,
       slot_no                  uinteger            NOT NULL,
       in_tx                    bigint              NOT NULL REFERENCES tx (id) ON DELETE CASCADE,
    
       PRIMARY KEY (id),
       CONSTRAINT script_registration_uk UNIQUE (script_address,in_tx)
    );

    CREATE SEQUENCE public.script_registration_id_seq
        START WITH 1
        INCREMENT BY 1
        NO MINVALUE
        NO MAXVALUE
        CACHE 1;
    
    ALTER SEQUENCE public.script_registration_id_seq OWNED BY public.script_registration.id;

    -- When was a script unregistered
    
    CREATE TABLE script_deregistration (
       id                       bigint              NOT NULL,
       script_address           script_hash         NOT NULL,
       slot_no                  uinteger            NOT NULL,
       in_tx                    bigint              NOT NULL REFERENCES tx (id) ON DELETE CASCADE,
    
       PRIMARY KEY (id),
       CONSTRAINT script_deregistration_uk UNIQUE (script_address,in_tx)
    );

    CREATE SEQUENCE public.script_deregistration_id_seq
        START WITH 1
        INCREMENT BY 1
        NO MINVALUE
        NO MAXVALUE
        CACHE 1;
    
    ALTER SEQUENCE public.script_deregistration_id_seq OWNED BY public.script_deregistration.id;

    -- Parameter update - there can only be one successful update per epoch.
    
    CREATE TABLE parameter_update (
       id                       bigint              NOT NULL,
       epoch_no                 uinteger            NOT NULL,
       min_fee                  uinteger            NOT NULL,
       max_fee                  uinteger            NOT NULL,
       max_block_size           uinteger            NOT NULL,
       max_tx_size              uinteger            NOT NULL,
       max_bh_size              uinteger            NOT NULL,
       key_deposit              lovelace            NOT NULL,
       key_deposit_refund       interval            NOT NULL,
       key_deposit_decay        rational            NOT NULL,
       pool_deposit             lovelace            NOT NULL,
       pool_deposit_refund      interval            NOT NULL,
       pool_deposit_decay       rational            NOT NULL,
       max_epoch                uinteger            NOT NULL,
       n_optimal                uinteger            NOT NULL,
       influence                rational            NOT NULL,
       monetary_expansion_rate  interval            NOT NULL,
       treasury_growth_rate     interval            NOT NULL,
       active_slot_coeff        interval            NOT NULL,
       decentralisation         interval            NOT NULL,
       entropy                  nonce,                              -- NULL if the nonce is not present
       protocol_version         protocol_version    NOT NULL,

       PRIMARY KEY (id),
       CONSTRAINT parameter_update_uk UNIQUE (epoch_no)
    );

    CREATE SEQUENCE public.parameter_update_id_seq
        START WITH 1
        INCREMENT BY 1
        NO MINVALUE
        NO MAXVALUE
        CACHE 1;
    
    ALTER SEQUENCE public.parameter_update_id_seq OWNED BY public.parameter_update.id;

    -- Pool Parameters.
    -- Records important pool-specific data.
    
    CREATE TABLE pool_params (
       id                       bigint              NOT NULL,
       pool_id                  hash32type          NOT NULL,   -- hash of the pool VRF key
       --owners                   hash ARRAY          NOT NULL,   -- key hashes for the owners - at least one owner is needed TODO(KS): Normal form?
       ticker_id                ticker              NOT NULL,
       pledge                   lovelace            NOT NULL,
       reward_address           staking_addr_hash   NOT NULL,   -- overall pool rewards
       pool_url                 url,
       metadata_hash            hash32type,
       margin                   percentage          NOT NULL,
       fixed_cost               uinteger            NOT NULL,
       registered               uinteger            NOT NULL,   -- in which slot was this metadata registered/re-registered
    
       PRIMARY KEY (id),
       CONSTRAINT pool_params_uk UNIQUE (pool_id,registered)
    );

    CREATE SEQUENCE public.pool_params_id_seq
        START WITH 1
        INCREMENT BY 1
        NO MINVALUE
        NO MAXVALUE
        CACHE 1;
    
    ALTER SEQUENCE public.pool_params_id_seq OWNED BY public.pool_params.id;

    -- Pool Retirement
    
    CREATE TABLE pool_retired (
       id                       bigint              NOT NULL,

       pool_id                  hash32type          NOT NULL,   -- hash of the pool VRF key
       retired                  uinteger            NOT NULL,   -- when retirement occurred (epoch no)
    
       PRIMARY KEY (id),
       CONSTRAINT pool_retired_uk UNIQUE (pool_id,retired)
    );
    
    -- Announcement of Pool Retirement
    
    CREATE TABLE pool_retiring (
       id                       bigint              NOT NULL,
       pool_id                  hash32type          NOT NULL,   -- hash of the pool VRF key
       announced                uinteger            NOT NULL,   -- which slot was retirement announced
       retiring                 uinteger            NOT NULL,   -- in which epoch is retirement planned
    
       PRIMARY KEY (id),
       CONSTRAINT pool_retiring_uk UNIQUE (pool_id,announced)
    );

    CREATE SEQUENCE public.pool_retiring_id_seq
        START WITH 1
        INCREMENT BY 1
        NO MINVALUE
        NO MAXVALUE
        CACHE 1;
    
    ALTER SEQUENCE public.pool_retiring_id_seq OWNED BY public.pool_retiring.id;
    
    -- How much stake is held by a staking key.
    
    CREATE TABLE stake (
       id                       bigint              NOT NULL,
       stakekey_address         staking_addr_hash   NOT NULL,
       slot_no                  uinteger            NOT NULL,   -- when was the change made
       stake                    lovelace            NOT NULL,
 
       PRIMARY KEY (id),
       CONSTRAINT stake_uk UNIQUE (stakekey_address,stake)
    );

    CREATE SEQUENCE public.stake_id_seq
        START WITH 1
        INCREMENT BY 1
        NO MINVALUE
        NO MAXVALUE
        CACHE 1;
    
    ALTER SEQUENCE public.stake_id_seq OWNED BY public.stake.id;

    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = 14 ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
