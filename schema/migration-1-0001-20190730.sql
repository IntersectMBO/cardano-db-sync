-- Hand written migration to create the custom types with 'DOMAIN' statements.

CREATE FUNCTION migrate() RETURNS void AS $$

DECLARE
  next_version int;

BEGIN
  SELECT stage_one + 1 INTO next_version FROM "schema_version";
  IF next_version = 1 THEN
    EXECUTE 'CREATE DOMAIN lovelace AS bigint CHECK (VALUE >= 0 AND VALUE <= 45000000000000000);';
    EXECUTE 'CREATE DOMAIN txindex AS smallint CHECK (VALUE >= 0 AND VALUE < 1024);';
    EXECUTE 'CREATE DOMAIN uinteger AS integer CHECK (VALUE >= 0);';

    -- Blocks, transactions and merkel roots use a 32 byte hash.
    EXECUTE 'CREATE DOMAIN hash32type AS bytea CHECK (octet_length (VALUE) = 32);';

    -- Addresses use a 28 byte hash (as do StakeholdIds).
    EXECUTE 'CREATE DOMAIN hash28type AS bytea CHECK (octet_length (VALUE) = 28);';

    UPDATE "schema_version" SET stage_one = 1;
    RAISE NOTICE 'DB has been migrated to stage_one version %', next_version;
  END IF;
END;

$$ LANGUAGE plpgsql;

SELECT migrate();

DROP FUNCTION migrate();
