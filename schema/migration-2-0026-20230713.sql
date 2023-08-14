-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 26 THEN
    EXECUTE 'CREATe TABLE "epoch_stake_progress"("id" SERIAL8  PRIMARY KEY UNIQUE,"epoch_no" word31type NOT NULL,"completed" BOOLEAN NOT NULL)' ;
    EXECUTE 'ALTER TABLE "epoch_stake_progress" ADD CONSTRAINT "unique_epoch_stake_progress" UNIQUE("epoch_no")' ;
    EXECUTE 'CREATe TABLE "extra_migrations"("id" SERIAL8  PRIMARY KEY UNIQUE,"token" VARCHAR NOT NULL,"description" VARCHAR NULL)' ;
    EXECUTE 'ALTER TABLE "tx" ALTER COLUMN "deposit" DROP NOT NULL' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
