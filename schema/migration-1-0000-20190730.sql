-- Hand written migration that creates a 'schema_version' table and initializes it.

CREATE FUNCTION init() RETURNS void AS $$

DECLARE
    emptyDB boolean;

BEGIN
  SELECT NOT EXISTS (SELECT 1 FROM information_schema.tables WHERE table_name='schema_version') INTO emptyDB;
  IF emptyDB THEN
    EXECUTE 'CREATE TABLE "schema_version" (id SERIAL PRIMARY KEY UNIQUE, stage_one INT8 NOT NULL, stage_two INT8 NOT NULL, stage_three INT8 NOT NULL);';
    EXECUTE 'INSERT INTO "schema_version" (stage_one, stage_two, stage_three) VALUES (0, 0, 0);';

    RAISE NOTICE 'DB has been initialized';
  END IF;
END;

$$ LANGUAGE plpgsql;

SELECT init();

DROP FUNCTION init();
