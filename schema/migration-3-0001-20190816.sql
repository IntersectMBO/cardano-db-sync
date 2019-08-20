-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_three + 1 INTO next_version FROM schema_version ;
  IF next_version = 1 THEN
    -- TODO: Is there a way to avoid 'NOT IN' using an 'OUTER JOIN'?
    -- Does the 'OUTER JOIN' actually perform better?
    EXECUTE 'CREATe VIEW utxo AS SELECT tx_out.* FROM tx_out LEFT OUTER JOIN tx_in ON tx_out.tx_id = tx_in.tx_out_id AND tx_out.index = tx_in.tx_out_index WHERE tx_in.tx_in_id IS NULL' ;

    UPDATE schema_version SET stage_three = 1;
    RAISE NOTICE 'DB has been migrated to stage_three version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
