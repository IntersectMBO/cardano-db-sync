-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 34 THEN
    EXECUTE 'ALTER TABLE "gov_action_proposal" ALTER COLUMN "description" TYPE jsonb USING description::jsonb' ;
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "pvtpp_security_group" DOUBLE PRECISION NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "pvtpp_security_group" DOUBLE PRECISION NULL' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
