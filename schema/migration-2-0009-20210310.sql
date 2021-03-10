-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 9 THEN
    EXECUTE 'CREATe TABLE "ada_pots"("id" SERIAL8  PRIMARY KEY UNIQUE,"slot_no" uinteger NOT NULL,"epoch_no" uinteger NOT NULL,"treasury" lovelace NOT NULL,"reserves" lovelace NOT NULL,"rewards" lovelace NOT NULL,"utxo" lovelace NOT NULL,"deposits" lovelace NOT NULL,"fees" lovelace NOT NULL,"block_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "ada_pots" ADD CONSTRAINT "unique_ada_pots" UNIQUE("block_id")' ;
    EXECUTE 'ALTER TABLE "ada_pots" ADD CONSTRAINT "ada_pots_block_id_fkey" FOREIGN KEY("block_id") REFERENCES "block"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
