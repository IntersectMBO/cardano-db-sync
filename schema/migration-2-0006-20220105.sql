-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 6 THEN
    EXECUTE 'CREATe TABLE "extra_key_witness"("id" SERIAL8  PRIMARY KEY UNIQUE,"hash" hash28type NOT NULL,"tx_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "extra_key_witness" ADD CONSTRAINT "unique_witness" UNIQUE("hash")' ;
    EXECUTE 'ALTER TABLE "extra_key_witness" ADD CONSTRAINT "extra_key_witness_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
