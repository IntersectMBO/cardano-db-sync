CREATE FUNCTION migrate() RETURNS void AS $$

BEGIN
  EXECUTE 'ALTER DOMAIN lovelace DROP CONSTRAINT IF EXISTS lovelace_check; ';
END;

$$ LANGUAGE plpgsql;

SELECT migrate();

DROP FUNCTION migrate();
