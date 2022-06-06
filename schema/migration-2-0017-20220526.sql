-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 17 THEN
    EXECUTE 'ALTER TABLE "param_proposal" ADD COLUMN "coins_per_utxo_size" lovelace NULL' ;
    EXECUTE 'ALTER TABLE "param_proposal" DROP COLUMN "coins_per_utxo_word"' ;
    EXECUTE 'ALTER TABLE "epoch_param" ADD COLUMN "coins_per_utxo_size" lovelace NULL' ;
    EXECUTE 'ALTER TABLE "epoch_param" DROP COLUMN "coins_per_utxo_word"' ;
    -- Hand written SQL statements can be added here.
    UPDATE schema_version SET stage_two = next_version ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
