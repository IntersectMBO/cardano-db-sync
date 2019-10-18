-- Persistent generated migration with hand written SQL.

CREATE FUNCTION migrate() RETURNS void AS $$
DECLARE
  next_version int ;
BEGIN
  SELECT stage_two + 1 INTO next_version FROM schema_version ;
  IF next_version = 3 THEN
    EXECUTE 'ALTER TABLE "block" ADD COLUMN "time" timestamp NULL' ;

    -- ---------------------------------------------------------------------------------------------
    -- Hand written SQL to populate the new column and then make it non-NULL.
    -- First timestamp the genesis block.
    update block set time = (select start_time from meta) where epoch_no is null ;

    -- Then add timestamps to the EBBs.
    update block
      set time =
        (select cast (quote_literal (epoch_no * (select slot_duration from meta) * 21.6) as interval)
            + (select start_time from meta))
      where slot_no is null and epoch_no is not null ;

    -- Then add timestamp to the main blocks.
    update block
      set time =
        (select cast (quote_literal (slot_no * (select slot_duration from meta) * 0.001) as interval)
            + (select start_time from meta))
      where slot_no is not null ;

    -- Finally make the timestamp non-NULLable.
    alter table block alter column time set not null ;
    -- ---------------------------------------------------------------------------------------------

    UPDATE schema_version SET stage_two = 3 ;
    RAISE NOTICE 'DB has been migrated to stage_two version %', next_version ;
  END IF ;
END ;
$$ LANGUAGE plpgsql ;

SELECT migrate() ;

DROP FUNCTION migrate() ;
