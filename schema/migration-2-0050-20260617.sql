-- Repair pool_relay.port rows where the old int2 encoder wrapped values >32767 to negative. See #2135.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 50 THEN

    UPDATE pool_relay
      SET port = port + 65536
      WHERE port < 0 ;

    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
