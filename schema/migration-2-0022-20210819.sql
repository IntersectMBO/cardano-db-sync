-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 22 THEN
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "cost_models_id" INT8 NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" DROP COLUMN "cost_models"' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "cost_models_id" INT8 NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" DROP COLUMN "cost_models"' ;
    EXECUTE 'CREATe TABLE "cost_models"("id" SERIAL8  PRIMARY KEY UNIQUE,"costs" jsonb NOT NULL,"block_id" INT8 NOT NULL)' ;
    EXECUTE 'ALTER TABLE "cost_models" ADD CONSTRAINT "unique_cost_models" UNIQUE("costs")' ;
    EXECUTE 'ALTER TABLE "cost_models" ADD CONSTRAINT "cost_models_block_id_fkey" FOREIGN KEY("block_id") REFERENCES "block"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
