-- Hand written migration to create the custom types with 'DOMAIN' statements.

CREATE FUNCTION migrate() RETURNS void AS $$

DECLARE
  next_version int;

BEGIN
  SELECT stage_one + 1 INTO next_version FROM "schema_version";
  IF next_version = 3 THEN
    -- DOMAIN
    EXECUTE 'CREATE DOMAIN nonnegative AS integer CHECK (VALUE > 0);';

    -- Are these hashes or addr_hashes?
    EXECUTE 'CREATE DOMAIN payment_addr_hash AS hash28type;';
    EXECUTE 'CREATE DOMAIN staking_addr_hash AS hash28type;';
    EXECUTE 'CREATE DOMAIN script_hash AS hash28type;';

    EXECUTE 'CREATE DOMAIN signature AS hash32type;';
    EXECUTE 'CREATE DOMAIN vrf_certificate AS hash32type;';
    EXECUTE 'CREATE DOMAIN nonce as hash28type;';

    -- Tickers may be 1-5 bytes
    EXECUTE 'CREATE DOMAIN ticker AS bytea CHECK (octet_length (VALUE) = 5);';

    -- URL for pool description.
    EXECUTE 'CREATE DOMAIN url AS bytea CHECK (octet_length (VALUE) = 64);';

    -- Percentages in the range 0%-100% -- recorded to two decimal places
    EXECUTE 'CREATE DOMAIN percentage AS decimal (5,2) CHECK (VALUE >= 0 AND VALUE <= 100);';

    -- TYPES
    EXECUTE 'CREATE TYPE interval AS (minval uinteger,maxval uinteger);';
    EXECUTE 'CREATE TYPE rational AS (num uinteger,denom nonnegative);';
    EXECUTE 'CREATE TYPE pointer AS (blockid hash32type,tx_idx txindex,cert_idx uinteger);';
    EXECUTE 'CREATE TYPE address AS (payment_address payment_addr_hash,staking_address staking_addr_hash,pointer_address pointer);';
    EXECUTE 'CREATE TYPE operational_certificate AS (hot_vkey hash32type,sequence_number uinteger,kes_period uinteger,sigma signature);';
    EXECUTE 'CREATE TYPE protocol_version AS (major uinteger,minor uinteger);';
    EXECUTE 'CREATE TYPE tx_metadata AS (label uinteger,metadata jsonb);';
    EXECUTE 'CREATE TYPE withdrawal AS (withdraw_from hash32type,amount lovelace);';

    UPDATE "schema_version" SET stage_one = next_version;
    RAISE NOTICE 'DB has been migrated to stage_one version %', next_version;
  END IF;
END;

$$ LANGUAGE plpgsql;

SELECT migrate();

DROP FUNCTION migrate();
