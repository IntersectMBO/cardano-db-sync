-- Repair the collateral protocol parameters that a signed 16-bit encoder wrapped.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 52 THEN

    UPDATE "param_proposal" SET "collateral_percent" = "collateral_percent" + 65536 WHERE "collateral_percent" < 0 ;
    UPDATE "param_proposal" SET "max_collateral_inputs" = "max_collateral_inputs" + 65536 WHERE "max_collateral_inputs" < 0 ;
    UPDATE "epoch_param" SET "collateral_percent" = "collateral_percent" + 65536 WHERE "collateral_percent" < 0 ;
    UPDATE "epoch_param" SET "max_collateral_inputs" = "max_collateral_inputs" + 65536 WHERE "max_collateral_inputs" < 0 ;

    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
