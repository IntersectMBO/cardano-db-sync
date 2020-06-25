-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 2 THEN
    EXECUTE 'CREATe TABLE "tx_body"("id" SERIAL8  PRIMARY KEY UNIQUE,"hash" hash32type NOT NULL,"body" bytea NOT NULL)' ;
    EXECUTE 'ALTER TABLE "tx_body" ADD CONSTRAINT "unique_tx_body" UNIQUE("hash")' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = 2 ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
