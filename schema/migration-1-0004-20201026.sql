-- Hand written migration to create the custom types with 'DOMAIN' statements.

CREATE FUNCTION migrate() RETURNS void AS $$

DECLARE
  next_version int;

BEGIN
  SELECT stage_one + 1 INTO next_version FROM "schema_version";
  IF next_version = 4 THEN
    -- Asset identifiers are ByteStrings of up to 32 bytes in length.
    -- The empty ByteString is a valid value.
    EXECUTE 'CREATE DOMAIN asset32type AS bytea CHECK (octet_length (VALUE) <= 32);';

    -- This is horrible. Basically a 'Word64' with an extra sign bit.
    EXECUTE 'CREATE DOMAIN int65type AS numeric (20, 0) CHECK (VALUE >= -18446744073709551615 AND VALUE <= 18446744073709551615);';

    UPDATE "schema_version" SET stage_one = next_version;
    RAISE NOTICE 'DB has been migrated to stage_one version %', next_version;
  END IF;
END;

$$ LANGUAGE plpgsql;

SELECT migrate();

DROP FUNCTION migrate();
