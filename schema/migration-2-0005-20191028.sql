-- Persistent generated migration.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 5 THEN
    EXECUTE 'ALTER TABLE "tx" ADD COLUMN "out_sum" lovelace' ;

    -- -------------------------------------------------------------------------
    -- Hand written SQL to update the out_sum column of the tx table.
    update tx
      set out_sum =
    	(select sum (tx_out.value)
    	  from tx as tx_inner inner join tx_out on tx_inner.id = tx_out.tx_id
          where tx.id = tx_inner.id
          ) ;

    -- Make the new column non-NULLable.
    alter table tx alter column out_sum set not null ;
    -- -------------------------------------------------------------------------

    UPDATE schema_version SET stage_two = 5 ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
