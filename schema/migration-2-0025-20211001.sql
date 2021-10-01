-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 25 THEN
    EXECUTE 'ALTER TABLE "param_proposal" DROP CONSTRAINT "param_proposal_cost_models_id_fkey"' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD CONSTRAINT "param_proposal_cost_models_id_fkey" FOREIGN KEY("cost_models_id") REFERENCES "cost_models"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    EXECUTE 'ALTER TABLE "epoch_param" DROP CONSTRAINT "epoch_param_cost_models_id_fkey"' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD CONSTRAINT "epoch_param_cost_models_id_fkey" FOREIGN KEY("cost_models_id") REFERENCES "cost_models"("id") ON DELETE CASCADE  ON UPDATE RESTRICT' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
