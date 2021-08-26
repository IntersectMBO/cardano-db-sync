-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 23 THEN
    EXECUTE 'ALTER TABLE "redeemer" ADD COLUMN "datum_id" INT8 NOT NULL' ;
    EXECUTE 'CREATe TABLE "datum"("id" SERIAL8  PRIMARY KEY UNIQUE,"hash" hash32type NOT NULL,"tx_id" INT8 NOT NULL,"value" jsonb NULL)' ;
    EXECUTE 'ALTER TABLE "datum" ADD CONSTRAINT "unique_data" UNIQUE("hash")' ;
    EXECUTE 'ALTER TABLE "datum" ADD CONSTRAINT "datum_tx_id_fkey" FOREIGN KEY("tx_id") REFERENCES "tx"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD CONSTRAINT "param_proposal_cost_models_id_fkey" FOREIGN KEY("cost_models_id") REFERENCES "cost_models"("id") ON DELETE RESTRICT  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD CONSTRAINT "epoch_param_cost_models_id_fkey" FOREIGN KEY("cost_models_id") REFERENCES "cost_models"("id") ON DELETE RESTRICT  ON UPDATE RESTRICT' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
