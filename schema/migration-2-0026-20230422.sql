-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 26 THEN
    EXECUTE 'ALTER TABLE "tx_out" ADD COLUMN "address_id" INT8 NOT NULL' ;
    EXECUTE 'ALTER TABLE "tx_out" DROP COLUMN "address"' ;
    EXECUTE 'ALTER TABLE "tx_out" DROP COLUMN "address_raw"' ;
    EXECUTE 'ALTER TABLE "tx_out" DROP COLUMN "address_has_script"' ;
    EXECUTE 'ALTER TABLE "tx_out" DROP COLUMN "payment_cred"' ;
    EXECUTE 'CREATe TABLE "address"("id" SERIAL8  PRIMARY KEY UNIQUE,"address" VARCHAR NOT NULL,"address_raw" BYTEA NOT NULL,"has_script" BOOLEAN NOT NULL,"payment_cred" hash28type NULL)' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
