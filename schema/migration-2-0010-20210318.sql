-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 10 THEN
    EXECUTE 'ALTER TABLE "reserve" ALTER COLUMN "amount" TYPE int65type' ;
    EXECUTE 'ALTER TABLE "reward" ADD COLUMN "type" rewardtype NOT NULL' ;
    EXECUTE 'ALTER TABLE "orphaned_reward" ADD COLUMN "type" rewardtype NOT NULL' ;
    EXECUTE 'ALTER TABLE "treasury" ALTER COLUMN "amount" TYPE int65type' ;
    EXECUTE 'CREATe TABLE "pot_transfer"("id" SERIAL8  PRIMARY KEY UNIQUE,"cert_index" INT4 NOT NULL,"treasury" int65type NOT NULL,"reserves" int65type NOT NULL,"tx_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "pot_transfer" ADD CONSTRAINT "unique_pot_transfer" UNIQUE("tx_id","cert_index")' ;
    EXECUTE 'ALTER TABLE "pot_transfer" ADD CONSTRAINT "pot_transfer_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
