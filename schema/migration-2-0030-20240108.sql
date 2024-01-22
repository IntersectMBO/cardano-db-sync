-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 30 THEN
    EXECUTE 'alter domain addr29type drop constraint addr29type_check' ;
    EXECUTE 'alter domain asset32type drop constraint asset32type_check' ;
    EXECUTE 'alter domain hash28type drop constraint hash28type_check' ;
    EXECUTE 'alter domain hash32type drop constraint hash32type_check' ;
    EXECUTE 'alter domain int65type drop constraint int65type_check' ;
    EXECUTE 'alter domain lovelace drop constraint lovelace_check' ;
    EXECUTE 'alter domain txindex drop constraint txindex_check' ;
    EXECUTE 'alter domain word128type drop constraint word128type_check' ;
    EXECUTE 'alter domain word31type drop constraint word31type_check' ;
    EXECUTE 'alter domain word63type drop constraint word63type_check' ;
    EXECUTE 'alter domain word64type drop constraint word64type_check' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
