-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 23 THEN
    EXECUTE 'CREATe TABLE "reverse_index"("id" SERIAL8  PRIMARY KEY UNIQUE,"block_id" INT8 NOT NULL,"min_ids" VARCHAR NULL)' ;
    EXECUTE 'ALTER TABLE "stake_address" DROP COLUMN "tx_id"' ;
    EXECUTE 'ALTER TABLE "tx_in" DROP CONSTRAINT "unique_txin"' ;
    EXECUTE 'ALTER TABLE "ada_pots" DROP CONSTRAINT "unique_ada_pots"' ;
    EXECUTE 'ALTER TABLE "cost_model" DROP COLUMN "block_id"' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
