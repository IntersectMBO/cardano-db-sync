-- The EB closure size on a certifying block (has_leios_cert): the certified endorser
-- block's transaction closure is spliced into this block's body, so this records the
-- sum of those transactions' sizes -- the quantity bounded by maxEndorserBlockTxsSize.
-- NULL on non-certifying blocks. The tx count is already available as block.tx_count.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 60 THEN
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "eb_closure_size" word63type NULL' ;
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
