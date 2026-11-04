-- Strip the quotes that 13.8.0.0 stored around pool relay IPs. Only rows it wrote are
-- quoted, so match on the quote rather than rewriting every row.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 53 THEN

    UPDATE "pool_relay" SET "ipv4" = btrim("ipv4", '"') WHERE "ipv4" LIKE '"%' ;
    UPDATE "pool_relay" SET "ipv6" = btrim("ipv6", '"') WHERE "ipv6" LIKE '"%' ;

    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
