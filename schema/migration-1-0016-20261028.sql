-- Hand written migration to add the Dijkstra 'guard' script purpose to the
-- scriptpurposetype enum. A guard redeemer runs the script of a credential that
-- gates a Dijkstra transaction ('guards' / 'requiredTopLevelGuards'); until now such
-- redeemers were recorded as 'propose'. ALTER TYPE ... ADD VALUE cannot run in the
-- same transaction that uses the new value, so it lives in its own stage-one
-- migration.

CREATE FUNCTION migrate() RETURNS void AS $$

DECLARE
  next_version int;

BEGIN
  SELECT stage_one + 1 INTO next_version FROM "schema_version";
  IF next_version = 16 THEN

    ALTER TYPE scriptpurposetype ADD VALUE 'guard' AFTER 'propose';

    UPDATE "schema_version" SET stage_one = next_version;
    RAISE NOTICE 'DB has been migrated to stage_one version %', next_version;
  END IF;
END;

$$ LANGUAGE plpgsql;

SELECT migrate();

DROP FUNCTION migrate();
