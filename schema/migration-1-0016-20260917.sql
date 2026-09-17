-- Hand written migration to add the Dijkstra Plutus V4 script language to the
-- scripttype enum. The Haskell ScriptType already carries PlutusV4; this makes the
-- database enum accept it (ALTER TYPE ... ADD VALUE cannot run in the same
-- transaction that uses the new value, so it lives in its own stage-one migration).

CREATE FUNCTION migrate() RETURNS void AS $$

DECLARE
  next_version int;

BEGIN
  SELECT stage_one + 1 INTO next_version FROM "schema_version";
  IF next_version = 16 THEN

    ALTER TYPE scripttype ADD VALUE 'plutusV4' AFTER 'plutusV3';

    UPDATE "schema_version" SET stage_one = next_version;
    RAISE NOTICE 'DB has been migrated to stage_one version %', next_version;
  END IF;
END;

$$ LANGUAGE plpgsql;

SELECT migrate();

DROP FUNCTION migrate();
