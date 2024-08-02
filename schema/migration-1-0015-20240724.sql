-- Hand written migration to create the custom types with 'DOMAIN' statements.

CREATE FUNCTION migrate() RETURNS void AS $$

DECLARE
  next_version int;

BEGIN
  SELECT stage_one + 1 INTO next_version FROM "schema_version";
  IF next_version = 15 THEN

    ALTER TYPE anchorType ADD VALUE 'vote' AFTER 'other';
    ALTER TYPE anchorType ADD VALUE 'committee_dereg' AFTER 'vote';
    ALTER TYPE anchorType ADD VALUE 'constitution' AFTER 'committee_dereg';

    UPDATE "schema_version" SET stage_one = next_version;
    RAISE NOTICE 'DB has been migrated to stage_one version %', next_version;
  END IF;
END;

$$ LANGUAGE plpgsql;

SELECT migrate();

DROP FUNCTION migrate();
