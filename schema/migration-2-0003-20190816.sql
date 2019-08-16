-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 3 THEN
    EXECUTE 'ALTER TABLE "block" ALTER COLUMN "hash" TYPE hash32type' ;
    EXECUTE 'ALTER TABLE "block" ALTER COLUMN "merkel_root" TYPE hash32type' ;
    EXECUTE 'ALTER TABLE "tx" ALTER COLUMN "hash" TYPE hash32type' ;
    EXECUTE 'ALTER TABLE "tx_out" ALTER COLUMN "address" TYPE hash28type' ;
    EXECUTE 'ALTER TABLE "tx_in" DROP COLUMN "tx_out_id"' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = 3 ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
