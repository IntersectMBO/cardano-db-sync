-- Hand written migration to create the custom types with 'DOMAIN' statements.

CREATE FUNCTION migrate() RETURNS void AS $$

DECLARE
  next_version int;

BEGIN
  SELECT stage_one + 1 INTO next_version FROM "schema_version";
  IF next_version = 3 THEN
    -- Used as the sum of tx outputs for an epoch.
    -- Persistent does not support more precision than 'Int64' (support for 'Word64'
    -- is done as a 'cast' to/from 'Int64' resulting in values greater than
    -- 'maxBound :: Int64' being represented in the database as negative values.
    -- Instead we we use 'Word128'.
    EXECUTE 'CREATE DOMAIN outsum AS word128type;';

    UPDATE "schema_version" SET stage_one = next_version;
    RAISE NOTICE 'DB has been migrated to stage_one version %', next_version;
  END IF;
END;

$$ LANGUAGE plpgsql;

SELECT migrate();

DROP FUNCTION migrate();
