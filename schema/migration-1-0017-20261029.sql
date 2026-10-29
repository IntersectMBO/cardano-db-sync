-- EXPERIMENTAL (Dijkstra): provisional schema for an unreleased era; see the note at the
-- top of doc/schema.md. Postgres cannot remove a value from an enum type once it has been
-- added, so this value is permanent even if the feature behind it changes.

-- Hand written migration to add the Dijkstra Plutus V4 script language to the scripttype
-- enum. The Haskell ScriptType already carries PlutusV4; this makes the database accept
-- it. ALTER TYPE ... ADD VALUE cannot run in the same transaction that uses the new
-- value, so it lives in its own stage-one migration.

CREATE FUNCTION migrate() RETURNS void AS $$

DECLARE
  next_version int;

BEGIN
  SELECT stage_one + 1 INTO next_version FROM "schema_version";
  IF next_version = 17 THEN

    ALTER TYPE scripttype ADD VALUE 'plutusV4' AFTER 'plutusV3';

    UPDATE "schema_version" SET stage_one = next_version;
    RAISE NOTICE 'DB has been migrated to stage_one version %', next_version;
  END IF;
END;

$$ LANGUAGE plpgsql;

SELECT migrate();

DROP FUNCTION migrate();
