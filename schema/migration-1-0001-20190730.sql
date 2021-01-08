-- Hand written migration to create the custom types with 'DOMAIN' statements.

CREATE FUNCTION migrate() RETURNS void AS $$

DECLARE
  next_version int;

BEGIN
  SELECT stage_one + 1 INTO next_version FROM "schema_version";
  IF next_version = 1 THEN
    -- Lovelace used to be bounded to [0, 45000000000000000] but things like 'pool_update.pledge'
    -- can be any Word64 value so we need this :cry:.
    EXECUTE 'CREATE DOMAIN lovelace AS numeric (20, 0) CHECK (VALUE >= 0 AND VALUE <= 18446744073709551615);';

    EXECUTE 'CREATE DOMAIN txindex AS smallint CHECK (VALUE >= 0);';
    EXECUTE 'CREATE DOMAIN uinteger AS integer CHECK (VALUE >= 0);';

    -- Blocks, transactions and merkle roots use a 32 byte hash.
    EXECUTE 'CREATE DOMAIN hash32type AS bytea CHECK (octet_length (VALUE) = 32);';

    -- Addresses use a 28 byte hash (as do StakeholdIds).
    EXECUTE 'CREATE DOMAIN hash28type AS bytea CHECK (octet_length (VALUE) = 28);';

    -- Stake addresses are a 28 byte hash prepended with a byte describing the address.
    EXECUTE 'CREATE DOMAIN addr29type AS bytea CHECK (octet_length (VALUE) = 29);';

    -- 'maxBound :: Word128' as a decimal has 39 digits, so we only need to check that it
    -- is positive.
    EXECUTE 'CREATE DOMAIN word128type AS numeric (39, 0) CHECK (VALUE >= (0)::numeric AND VALUE <= (340282366920938463463374607431768211455)::numeric);';

    -- 'maxBound :: Word64' as a decimal has 20 digits but not all 20 digit values are less than
    -- 'maxBound :: Word64'.
    EXECUTE 'CREATE DOMAIN word64type AS numeric (20, 0) CHECK (VALUE >= 0 AND VALUE <= 18446744073709551615);';

    UPDATE "schema_version" SET stage_one = 1;
    RAISE NOTICE 'DB has been migrated to stage_one version %', next_version;
  END IF;
END;

$$ LANGUAGE plpgsql;

SELECT migrate();

DROP FUNCTION migrate();
